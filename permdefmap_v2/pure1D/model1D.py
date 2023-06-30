#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 15 17:21:38 2020

@author: hamish
"""

import numpy as np

# 1-D model class
class Model1D:
    """1-D version of permdefmap model.
    
    Parameters
    ----------
    Parameters marked as calculated have their values calculated in the
    model setup and do not need to entered manually.
    
    Geometry
    points : array of float
        Distances of points along line of model.
    npt : int
        Number of points (calculated).
    nseg : int
        Number of segments (calculated).
    dx : array of float
        Distance between points in the model (calculated).
    faults : array of bool
        If the point is a fault (calculated).
    nfault : int
        Number of faults.
    
    A priori parameters
    slipcap : array of float
        Slip rate capacity of fault points. Zero for points that are
        not faults.
    straincap : array of float
        Strain rate capacity of segments.
    pot : array of float
        Force potentials at points.
    startvel : float
        Velocity at first point of line.
    endvel : float
        Velocity at last point of line.
    
    Observations
    nslipob : int
        Number of slip rate observations.
    ptofslipob : list of int
        Point of slip rate observation.
    slipobvalue : list of float
        Value of slip rate observation.
    slipobse : list of float
        Standard error of slip rate observation.
    nstrainob : int
        Number of strain rate observations.
    segofstrainob : list of int
        Segment of strain rate observation.
    strainobvalue : list of float
        Value of strain rate observation.
    strainobse : list of float
        Standard error of strain rate observation.
    
    Results (solution suffixes omitted)
    tau : float
        Lagrange multiplier for a priori solution.
    All of the following attributes include a single character suffix
    indicating which solution the result is for. The suffixes are:
        0 - a priori solution
        1 - a posteriori solution
        d - difference (a posteriori minus a priori solutions)
    For simplicity, these suffices are omitted in the following list
    and each item on the list refers to four attributes e.g. for 
    velminus, the three object attributes are velm0, velm1, and velmd
    (and velm is not an attribute itself).
    velm, velp : array of float
        Velocity on the minus and plus sides of point.
    slip : array of float
        Slip rate at point on fault.
    strain : array of float
        Strain rate on segment.
    trac : array of float
        Traction at point on fault.
    stress : array of float
        Stress on segment.
    
    Misfits (solution suffixes omitted)
    Misfits are normalised by the standard error of the observation so
    a misfit of 1.0 indicates that the prediction is 1.0 times the
    standard error away from the observed value. The solution suffix
    can be 0 or 1.
    rss : float
        Residual sum of squares (sum of the squared misfits to
        observations).
    slipobmis : array of float
        Misfit to slip rate observation.
    strainobmis : array of float
        Misfit to strain rate observation.
    """
    # initialise model
    def __init__(self, points):
        self.points = np.array(points)
        self.npt = len(points)
        self.nseg = self.npt - 1
        self.dx = self.points[1:] - self.points[:-1]
        self.slipcap = np.zeros((self.npt))
        self.straincap = np.zeros((self.nseg))
        self.pot = np.zeros((self.npt))
        self.startvel = 0.
        self.endvel = 0.
        self.faults = np.zeros((self.npt),dtype=np.bool)
        self.nfault = 0
        
        # observationseg
        self.nslipob = 0
        self.ptofslipob = []
        self.slipobvalue = []
        self.slipobse = []
        self.nstrainob = 0
        self.segofstrainob = []
        self.strainobvalue = []
        self.strainobse = []
        
        # results (0 = apriori, 1 = aposteriori, d = difference = apri-apost)
        self.tau = 0.             # Lagrange multiplier for apriori
        # velocities
        self.velm0 = np.zeros((self.npt))
        self.velp0 = np.zeros((self.npt))
        self.velm1 = np.zeros((self.npt))
        self.velp1 = np.zeros((self.npt))
        self.velmd = np.zeros((self.npt))
        self.velpd = np.zeros((self.npt))
        # slip rates
        self.slip0 = np.zeros((self.npt))
        self.slip1 = np.zeros((self.npt))
        self.slipd = np.zeros((self.npt))
        # strain rates
        self.strain0 = np.zeros((self.nseg))
        self.strain1 = np.zeros((self.nseg))
        self.straind = np.zeros((self.nseg))
        # tractions on faults
        self.trac0 = np.zeros((self.npt))
        self.trac1 = np.zeros((self.npt))
        self.tracd = np.zeros((self.npt))
        # stresses in segments
        self.stress0 = np.zeros((self.nseg))
        self.stress1 = np.zeros((self.nseg))
        self.stressd = np.zeros((self.nseg))
        
        # misfits
        self.rss0 = 0.
        self.rss1 = 0.
        self.slipobmis0 = []
        self.slipobmis1 = []
        self.strainobmis0 = []
        self.strainobmis1 = []
    
    def addFault(self, point, slipcap):
        """Add a fault to model.
        
        Parameters
        ----------
        point : int
            Point where fault is located.
        slipcap : float
            Slip rate capacity of fault
        """
        self.nfault += 1
        self.faults[point] = True
        self.slipcap[point] = slipcap
        
    def addSlipOb(self, point, value, se):
        """Add slip rate observation to model.
        
        Parameters
        ----------
        point : int
            Point of slip rate observation.
        value : float
            Value of slip rate observation.
        se : float
            Standard error of slip rate observation.
        """
        if not self.faults[point]:
            print('No fault at this point. Cannot add slip-rate observation.')
        else:
            self.nslipob += 1
            self.ptofslipob.append(point)
            self.slipobvalue.append(value)
            self.slipobse.append(se)
    
    def addStrainOb(self, seg, value, se):
        """Add strain rate observation to model.
        
        Parameters
        ----------
        seg : int
            Segment of strain rate observation.
        value : float
            Value of strain rate observation.
        se : float
            Standard error of strain rate observation.
        """
        self.nstrainob += 1
        self.segofstrainob.append(seg)
        self.strainobvalue.append(value)
        self.strainobse.append(se)
    
    def writeModel(self, file='model_setup.dat'):
        """Write model setup out to file
        
        Parameters
        ----------
        file : str, defualt='model_setup.dat'
            Name of file to output model to.
        """
        
        f = open(file)
        f.write(str(self.npt) + '\n')
        for p in range(self.npt):
            f.write(str(p)+' '+str(self.points[p])+' '+str(self.pot[p]) + '\n')
        f.write(str(self.nseg) + '\n')
        for s in range(self.nseg):
            f.write(str(s)+' '+str(self.straincap[s]) + '\n')
        f.write(str(np.sum(self.faults)) + '\n')
        for p in range(self.npt)[self.faults]:
            f.write(str(p)+' '+str(self.slipcap[p] + '\n'))
        f.close()
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        