#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
model.py - defines the 'Model' class of objects for use in permdefmap

Contains the structure of the Model objects, covering its variables and its
functions.

"""

import numpy as np
from .fortran import setup, writeInput
from .postprocessing import readResults, readFixedValues, readMisfit

class Model:
    """Structure containing information required for permdefmap model.
    
    Attributes
    ----------
    Number of...
    ngp : int
        Number of grid points in model.
    nside : int
        Number of element sides.
    nel : int
        Number of elements.
    nfault : int
        Number of faults.
    nvline : int
        Number of velocity lines (lines where velocities are specified
        within or on the border of the model).
    nrigidb : int
        Number of rigid boundary lines that move at plate rate.
    nvelob : int
        Number of individual velocity observations (not components).
    nslipob : int
        Number of fault slip rate observations.
    nstrainob : int
        Number of strain rate (in element) observations.
    nsliplink : int
        Number of fault slip rate constraints.
    nstrainlink : int
        Number of strain rate constraints.
    
    Geometry
    earthradius : float
        Planetary radius.
    gplong, gplat : array of float
        Longitude and latitude in degrees of grid points.
    gp1ofside, gp2ofside : array of int
        Index of grid points 1 and 2 of side.
    el1ofside, el2ofside : array of int
        Index of elements 1 and 2 of side.
    gp1ofel, gp2ofel, gp3ofel : array of int
        Index of grid points 1, 2, and 3 of element.
    side1ofel,side2ofel,side3ofel : array of int
        Index of sides 1, 2, and 3 of element. Side 1 is opposite grid
        point 1.
    nfaultseg : array of int
        Number of fault segments on fault.
    sideonfault : array of int
        Indices of sides on each fault.
    gponfault : array of int
        Indices of grid points on each fault.
        
    Velocity lines and rigid boundaries
    nvlinegp : array of int
        Number of grid points (excluding fault intersections) on
        velocity line.
    nvlineside : array of int
        Number of sides on velocity line.
    nvlinefault : array of int
        Number of fault intersections on velocity line.
    gponvline : array of int
        Indices of grid points (including fault intersections) on each
        velocity line.
    sideonvline : array of int
        Indices of sides on each velocity line.
    faultonvline : array of int
        Indices of fault intersecting each velocity line.
    interpvline : array of bool
        True if the velocity at this grid point or side on the velocity
        line needs to be interpolated. False if the velocity is already
        specified.
    vlinex, vliney : array of float
        x and y components of velocities specified at grid point or
        side on velocity line.
    vlineplusx, vlineminusx, vlineplusy, vlineminusy : array of float
        x and y components of velocities specified for the plus and
        minus sides of a fault intersecting the velocity line. Will be
        ignored if interpvline is True for this grid point or side. The
        direction of the positive normal to the fault segment is the
        plus side of fault.
    nrigidbside : array of int
        Number of sides on rigid boundary.
    sideonrigidb : array of int
        Indices of sides on each rigid boundary.
    gponrigidb : array of int
        Indices of grid points on each rigid boundary.
    eulerlong, eulerlat : array of float
        Longitude and latitude of Euler pole of rotation defining
        velocities along a rigid boundary
    eulerrate : array of float
        Rotation rate for Euler pole in radians/[time unit].
    
    Derived Geometry (calculated using class functions)
    nelatgp : array of int
        Number of elements meeting at grid point.
    nsegatgp : array of int
        Number of fault segments meeting at grid point.
    nvlineatgp : array of int
        Number of velocity line segments meeting at grid point.
    nrigidbatgp : array of int
        Number of rigid boundary segments meeting at grid point.
    faultonside : array of int
        Index of fault on side.
    segonside : array of int
        Index of fault segment (from start of fault) on side.
    nsideofelonfault : array of int
        Number of sides of a given element which are on any fault.
    ngpofelonfault : array of int
        Number of grid points of a given element which are on any fault.
    faultptonvline : array of int
        Fault index of point on velocity line.
    faultsignonvline : array of int
        Sign of fault on velocity line. +1 indicates the velocity line
        is in the same direction as the fault normal. -1 indicates the
        directions are opposite.
    
    Optional geometry (subfaults and subregions)
    nsubf : int
        Number of subfaults.
    subfofseg : array of int
        Index of subfault for each fault segment.
    subfofside : array of int
        Index of sbufault for each element side.
    subfname : list of str
        Name of subfault.
    nsubr : int
        Number of subregions.
    subrofel : array of int
        Index of subregion for each element.
    subrname : list of str
        Name of subregion.
    
    A priori parameters (see Haines & Sutherland, 2019 for definitions)
    potxx, potyy, potxy : array of float
        xx, yy, and xy components of force potentials at grid points.
    straincapc, straincapcc, straincapcs : array of float
        c, cc, and cs components of strain rate capacity in elements.
    straincaps, straincapsc, straincapss
        s, sc, and ss components of strain rate capacity in element.
    slipcapc, slipcaps : array of float
        c and s components of slip rate capacity on fault segments.
    
    Observations
    veloblong, veloblat : array of float
        Longitude and latitude of velocity observation.
    velobx, veloby : array of float
        x and y components of velocity obsevation.
    velobxse, velobyse : array of float
        Standard error of x and y components of velocity observation.
    velobcorr : array of float
        Correlation coefficient between x and y components of velocity
        observation.
    faultofslipob : array of int
        Index of fault of slip rate observation.
    sideofslipob : array of int
        Index of side of slip rate observation.
    nslipobcomp : array of int
        Number of components for slip rate observation. Must be 1 or 2
        for all observations.
    slipobcoefft, slipobcoeffn : array of float
        Transverse and normal coefficients for the vector(s) defining
        the direction and scaling factor for each component of the slip
        rate observation.
    slipobvalue, slipobse : array of float
        Value and standard error of each component of slip rate
        observation.
    slipobcorr : array of float
        Correlation coefficient between components of slip rate
        observation.
    elofstrainob : array of int
        Index of element of strain rate observation.
    nstrainobcomp : array of int
        Number of components for strain rate observation. Must be 1, 2,
        or 3 for all observations.
    strainobcoeffxx, strainobcoeffyy, strainobcoeffxy : array of float
        xx, yy, and xy coefficients for the 'vector(s)' defining the
        'direction' and scaling factor for each component of the strain
        rate observation.
    strainobvalue, strainobse : array of float
        Value and standard error of each component of strain rate
        observation.
    strainobcorr : array of float
        Correlation coefficients between components of strain rate
        observation in the order 1-2, 1-3, 2-3.
    
    Observational correlation constraints (link)
    faultofsliplink : array of int
        Index of fault of slip rate constraint.
    nsliplinkside : array of int
        Number of sides for slip rate constraint.
    sideofsliplink : array of int
        Indices of sides for slip rate constraint.
    sliplinkcoefft, sliplinkcoeffn : array of float
        Transverse and normal coefficients for the vector(s) defining
        the direction and scaling factor for each contribution to the
        slip rate constraint.
    sliplinkref, sliplinkscale : array of float
        Reference value and scale factor of each contribution to the
        slip rate constraint.
    nstrainlinkel : array of int
        Number of elements for strain rate constraint.
    elofstrainlink : array of int
        Indices of elements for strain rate constraint.
    strainlinkcoeffxx, strainlinkcoeffyy, strainlinkcoeffxy : array of float
        xx, yy, and xy coefficients for the 'vector(s)' defining the
        'direction' and scaling factor for each contribution to the
        strain rate constraint.
    strainlinkref, strainlinkscale : array of float
        Reference value and scale of each contribution to the strain
        rate constraint.
    
    Geometry calculated in setup
    ellong, ellat : array of float
        Longitude and latitude of element centres.
    elarea : array of float
        Area of element.
    nvelobinel : array of int
        Number of velocity observations in element.
    nstrainobinel : array of int
        Number of strain rate observations in element.
    nstrainlinkinel : array of int
        Number of strain rate constraints in element.
    seglong, seglat : array of float
        Longitude and latitude of fault segment centres.
    seglength, segtaz : array of float
        Length and azimuth of fault segments.
    segtx, segty : array of float
        x and y components of unit transverse/tangent vector for fault
        segments.
    segnx, segny : array of float
        x and y components of unit normal vector for fault segments.
    segnaz : array of float
        Azimuth of normal vector for fault segments.
    nslipobonseg : array of int
        Number of slip rate observations on fault segment.
    nsliplinkonseg : array of int
        Number of slip rate constraints on fault segment.
    
    Fixed values calculated from a priori parameters and constraints
    vlineoutx, vlineouty : array of float
        x and y components of velocities at grid point or side on
        velocity line (including interpolations, faults left blank).
    vlineoutmag, vlineoutaz : arry of float
        Magnitude and azimuth of velocities at grid point or side on
        velocity line (including interpolations, faults left blank).
    vlineoutmx, vlineoutmy, vlineoutpx, vlineoutpy : array of float
        x and y components of velocity on minus (m) and plus (p) sides
        of faults on velocity line.
    vlineoutmmag, vlineoutmaz, vlineoutpmag, vlineoutpaz : array of float
        Magnitude (mag) and azimuth (az) of velocity on minus (m) and
        plus (p) sides of faults on velocity line.
    rigidbvelx, rigidbvely : array of float
        x and y components of velocity at grid points on rigid
        boundary.
    rigidbvelmag, rigidbvelaz : array of float
        Magnitude and azimuth of velocity at grid points on rigid
        boundary.
    forcex, forcey : array of float
        x and y component of force at centre of element.
    forcemag, forceaz : array of float
        Magnitude and azimuth of force at centre of element.
    
    Results (solution suffixes omitted)
    All of the following attributes include a single character suffix
    indicating which solution the result is for. The suffixes are:
        0 - a priori solution
        1 - a posteriori solution
        d - difference (a posteriori minus a priori solutions)
        c - constraint part of solution
    For simplicity, these suffices are omitted in the following list
    and each item on the list refers to four attributes e.g. for 
    velinelx, the four object attributes are velinelx0, velinelx1,
    velinelxd, and velinelxc (and velinelx is not an attribute itself).
    
    Results in elements
    velinelx, velinely : array of float
        x and y component of velocity at centre of element.
    velinelmag, velinelaz : array of float
        Magnitude and azimuth of velocity at centre of element.
    strainxx, strainyy, strainxy : array of float
        xx, yy, and xy component of strain rate at centre of element.
    strainmax, strainmin, strainaz : array of float
        Maximum principle axis, minimum principle axis, and azimuth of
        maximum principle axis of strain rate at centre of element.
    stressxx, stressyy, stressxy : array of float
        xx, yy, and xy component of stress at centre of element.
    stressmax, stressmin, stressaz : array of float
        Maximum principle axis, minimum principle axis, and azimuth of
        maximum principle axis of stress at centre of element.
    velatvertx, velatverty : array of float
        x and y component of velocity at element vertices.
    rotationxy : array of float
        Rotation at centre of element.
        
    Results on faults (t=transverse, n=normal)
    slipt, slipn : array of float
        t and n components of slip rate at centre of fault segment.
    tract, tracn : array of float
        t and n components of traction at centre of fault segment.
    slipx, slipy : array of float
        x and y components of slip rate at centre of fault segment.
    tracx, tracy : array of float
        x and y components of traction at centre of fault segment.
    slipmag, slipaz : array of float
        Magnitude and azimuth (in xy-coordinates) of slip rate at
        centre of fault segment.
    tracmag, tracaz : array of float
        Magnitude and azimuth (in xy-coordinates) of traction at
        centre of fault segment.
    
    Observation misfits (solution suffixes omitted)
    Misfits are normalised by the standard error of the observation so
    a misfit of 1.0 indicates that the prediction is 1.0 times the
    standard error away from the observed value. The solution suffix
    can be 0 or 1.
    rss : float
        Residual sum of squares (sum of the squared misfits to
        observations)
    velobxmis, velobymis : array of float
        Misfit to x and y components of velocity observation.
    slipobmis : array of float
        Misfit to slip rate observation.
    strainobmis : array of float
        Misfit to strain rate observation.
    sliplinkmis : array of float
        Misfit to slip rate constraint observation.
    strainlinkmis : array of float
        Misfit to strain rate constraint observation.
    """
    # Model initialisation
    def __init__(self, load=False):
        # Fixed Fortran parameters
        self.maxgp = 40000
        self.maxs = 3 * self.maxgp
        self.maxe = 2 * self.maxgp
        self.maxf = 600
        self.maxfs = 100
        self.maxvl = 10
        self.maxvls = 200
        self.maxvlj = 2*self.maxvls + 1
        self.maxrb = 10
        self.maxrbs = 200
        self.maxvo = 5000
        self.maxfo = self.maxf * self.maxfs
        self.maxeo = self.maxe
        self.maxduc = 2 * self.maxf
        self.maxec = 100
        self.maxece = 500
        self.maxsf = self.maxf * self.maxfs
        self.maxsvl = self.maxvl * self.maxvls
        self.maxsrb = self.maxrb * self.maxrbs
        self.maxpf = self.maxsf + self.maxf
        self.maxpvl = self.maxsvl + self.maxvl
        self.maxprb = self.maxsrb + self.maxrb
        self.maxfp = 6
        # Number of ...
        self.ngp = 0
        self.nside = 0
        self.nel = 0
        self.nfault = 0
        self.nvline = 0
        self.nrigidb = 0
        self.nvelob = 0
        self.nslipob = 0
        self.nstrainob = 0
        self.nsliplink = 0
        self.nstrainlink = 0
        # Geometry
        self.gp1onside = -np.ones((self.maxs), dtype=np.int32)
        self.gp2onside = -np.ones((self.maxs), dtype=np.int32)
        self.el1onside = -np.ones((self.maxs), dtype=np.int32)
        self.el2onside = -np.ones((self.maxs), dtype=np.int32)
        self.side1ofel = -np.ones((self.maxe), dtype=np.int32)
        self.side2ofel = -np.ones((self.maxe), dtype=np.int32)
        self.side3ofel = -np.ones((self.maxe), dtype=np.int32)
        self.gp1ofel = -np.ones((self.maxe), dtype=np.int32)
        self.gp2ofel = -np.ones((self.maxe), dtype=np.int32)
        self.gp3ofel = -np.ones((self.maxe), dtype=np.int32)
        
        self.nfaultseg = np.zeros((self.maxf), dtype=np.int32)
        self.nvlineside = np.zeros((self.maxvl), dtype=np.int32)
        self.nvlfault = np.zeros((self.maxvl), dtype=np.int32)
        self.nvlinegp = np.zeros((self.maxvl), dtype=np.int32)
        self.nrigidbside = np.zeros((self.maxrb), dtype=np.int32)
        
        self.interpvline = np.zeros((self.maxvlj, self.maxvl), dtype=np.bool)
        self.faultsignonvline = np.zeros((1+self.maxvls, self.maxvl), dtype=np.int32)
        
        self.nelatgp = np.zeros((self.maxgp), dtype=np.int32)
        self.nsegatgp = np.zeros((self.maxgp), dtype=np.int32)
        self.nvlineatgp = np.zeros((self.maxgp), dtype=np.int32)
        self.nrigidbatgp = np.zeros((self.maxgp), dtype=np.int32)
        self.nsideofelonfault = np.zeros((self.maxe), dtype=np.int32)
        self.ngpofelonfault = np.zeros((self.maxe), dtype=np.int32)
        
        self.faultonside = -np.ones((self.maxs), dtype=np.int32)
        self.segonside = -np.ones((self.maxs), dtype=np.int32)
        self.sideonfault = -np.ones((self.maxfs, self.maxf), dtype=np.int32)
        self.gponfault = -np.ones((1+self.maxfs, self.maxf), dtype=np.int32)
        self.gponvline = -np.ones((1+self.maxvls, self.maxvl), dtype=np.int32)
        self.sideonvline = -np.ones((self.maxvls, self.maxvl), dtype=np.int32)
        self.faultonvline = -np.ones((1+self.maxvls, self.maxvl), dtype=np.int32)
        self.faultptonvline = -np.ones((1+self.maxvls, self.maxvl), dtype=np.int32)
        self.sideonrigidb = -np.ones((self.maxrbs, self.maxrb), dtype=np.int32)
        self.gponrigidb = -np.ones((1+self.maxrbs, self.maxrb), dtype=np.int32)
        # Observation location
#        self.evo = -np.ones((self.maxvo), dtype=np.int32)
        
        self.nslipobcomp = np.zeros((self.maxfo), dtype=np.int32)
        self.nstrainobcomp = np.zeros((self.maxeo), dtype=np.int32)
        self.nsliplinkside = np.zeros((self.maxduc), dtype=np.int32)
        self.nstrainlinkel = np.zeros((self.maxec), dtype=np.int32)
        
        self.faultofslipob = -np.ones((self.maxfo), dtype=np.int32)
        self.sideofslipob = -np.ones((self.maxfo), dtype=np.int32)
        self.elofstrainob = -np.ones((self.maxeo), dtype=np.int32)
        self.faultofsliplink = -np.ones((self.maxduc), dtype=np.int32)
        self.sideofsliplink = -np.ones((self.maxfs, self.maxduc), dtype=np.int32)
        self.elofstrainlink = -np.ones((self.maxece, self.maxec), dtype=np.int32)
        # Subfaults and subregions
        self.subfofseg = -np.ones((self.maxfs, self.maxf), dtype=np.int32)
        self.subfofside = -np.ones((self.maxs), dtype=np.int32)
        self.nsubf = 0
        self.subrofel = -np.ones((self.maxe), dtype=np.int32)
        self.nsubr = 0
        # Names
        self.faultname = ['' for _ in range(self.maxf)]
        self.subfname = ['' for _ in range(self.maxf)]
        self.subrname = ['' for _ in range(self.maxe)]
        self.rigidbname = ['' for _ in range(self.maxrb)]
        self.vlinename = ['' for _ in range(self.maxvl)]
        # Coordinates
        self.earthradius = 0.
        self.gplong = np.zeros((self.maxgp))
        self.gplat = np.zeros((self.maxgp))
        # Apriori model parameters
        self.potxx = np.zeros((self.maxgp))
        self.potyy = np.zeros((self.maxgp))
        self.potxy = np.zeros((self.maxgp))
        self.straincapc = np.zeros((self.maxe))
        self.straincapcc = np.zeros((self.maxe))
        self.straincapcs = np.zeros((self.maxe))
        self.straincaps = np.zeros((self.maxe))
        self.straincapsc = np.zeros((self.maxe))
        self.straincapss = np.zeros((self.maxe))
        self.slipcapc = np.zeros((self.maxfs, self.maxf))
        self.slipcaps = np.zeros((self.maxfs, self.maxf))
        self.vlinex = np.zeros((self.maxvlj, self.maxvl))
        self.vliney = np.zeros((self.maxvlj, self.maxvl))
        self.vlineminusx = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineminusy = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineplusx = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineplusy = np.zeros((1+self.maxvls, self.maxvl))
        self.eulerlat = np.zeros((self.maxrb))
        self.eulerlong = np.zeros((self.maxrb))
        self.eulerrate = np.zeros((self.maxrb))
        # Derived inputs
        self.vlineoutx = np.zeros((self.maxvlj, self.maxvl))
        self.vlineouty = np.zeros((self.maxvlj, self.maxvl))
        self.vlineoutmag = np.zeros((self.maxvlj, self.maxvl))
        self.vlineoutaz = np.zeros((self.maxvlj, self.maxvl))
        self.vlineoutmx = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineoutmy = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineoutmmag = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineoutmaz = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineoutpx = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineoutpy = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineoutpmag = np.zeros((1+self.maxvls, self.maxvl))
        self.vlineoutpaz = np.zeros((1+self.maxvls, self.maxvl))
        self.rigidbvelx = np.zeros((1+2*self.maxrbs, self.maxrb))
        self.rigidbvely = np.zeros((1+2*self.maxrbs, self.maxrb))
        self.rigidbvelmag = np.zeros((1+2*self.maxrbs, self.maxrb))
        self.rigidbvelaz = np.zeros((1+2*self.maxrbs, self.maxrb))
        # Observation values
        self.veloblong = np.zeros((self.maxvo))
        self.veloblat = np.zeros((self.maxvo))
        self.velobx = np.zeros((self.maxvo))
        self.veloby = np.zeros((self.maxvo))
        self.velobxse = np.zeros((self.maxvo))
        self.velobyse = np.zeros((self.maxvo))
        self.velobcorr = np.zeros((self.maxvo))
#        self.w1vo = np.zeros((self.maxvo))
#        self.w2vo = np.zeros((self.maxvo))
#        self.w3vo = np.zeros((self.maxvo))
        self.slipobcoefft = np.zeros((2, self.maxfo))
        self.slipobcoeffn = np.zeros((2, self.maxfo))
        self.slipobvalue = np.zeros((2, self.maxfo))
        self.slipobse = np.zeros((2, self.maxfo))
        self.slipobcorr = np.zeros((self.maxfo))
        self.strainobcoeffxx = np.zeros((3, self.maxeo))
        self.strainobcoeffyy = np.zeros((3, self.maxeo))
        self.strainobcoeffxy = np.zeros((3, self.maxeo))
        self.strainobvalue = np.zeros((3, self.maxeo))
        self.strainobse = np.zeros((3, self.maxeo))
        self.strainobcorr = np.zeros((3, self.maxeo))
        self.sliplinkcoefft = np.zeros((self.maxfs, self.maxduc))
        self.sliplinkcoeffn = np.zeros((self.maxfs, self.maxduc))
        self.sliplinkref = np.zeros((self.maxfs, self.maxduc))
        self.sliplinkscale = np.zeros((self.maxfs, self.maxduc))
        self.strainlinkcoeffxx = np.zeros((self.maxece, self.maxec))
        self.strainlinkcoeffyy = np.zeros((self.maxece, self.maxec))
        self.strainlinkcoeffxy = np.zeros((self.maxece, self.maxec))
        self.strainlinkref = np.zeros((self.maxece, self.maxec))
        self.strainlinkscale = np.zeros((self.maxece, self.maxec))
        # Solution
        # 0 = apriori; 1 = aposteriori; d = difference = aposteriori-apriori;
        # c = constraints
        # Elements
        self.ellong = np.zeros((self.maxe))
        self.ellat = np.zeros((self.maxe))
        self.nvelobinel = np.zeros((self.maxe))
        self.nstrainobinel = np.zeros((self.maxe), dtype = np.int32)
        self.nstrainlinkinel = np.zeros((self.maxe), dtype = np.int32)
        self.elarea = np.zeros((self.maxe))
        self.forcex = np.zeros((self.maxe))
        self.forcey = np.zeros((self.maxe))
        self.forcemag = np.zeros((self.maxe))
        self.forceaz = np.zeros((self.maxe))
        self.velinelx0 = np.zeros((self.maxe))
        self.velinely0 = np.zeros((self.maxe))
        self.velinelaz0 = np.zeros((self.maxe))
        self.velinelmag0 = np.zeros((self.maxe))
        self.strainxx0 = np.zeros((self.maxe))
        self.strainyy0 = np.zeros((self.maxe))
        self.strainxy0 = np.zeros((self.maxe))
        self.stressxx0 = np.zeros((self.maxe))
        self.stressyy0 = np.zeros((self.maxe))
        self.stressxy0 = np.zeros((self.maxe))
        self.strainmax0 = np.zeros((self.maxe))
        self.strainmin0 = np.zeros((self.maxe))
        self.strainaz0 = np.zeros((self.maxe))
        self.stressmax0 = np.zeros((self.maxe))
        self.stressmin0 = np.zeros((self.maxe))
        self.stressaz0 = np.zeros((self.maxe))
        self.velinelx1 = np.zeros((self.maxe))
        self.velinely1 = np.zeros((self.maxe))
        self.velinelaz1 = np.zeros((self.maxe))
        self.velinelmag1 = np.zeros((self.maxe))
        self.strainxx1 = np.zeros((self.maxe))
        self.strainyy1 = np.zeros((self.maxe))
        self.strainxy1 = np.zeros((self.maxe))
        self.stressxx1 = np.zeros((self.maxe))
        self.stressyy1 = np.zeros((self.maxe))
        self.stressxy1 = np.zeros((self.maxe))
        self.strainmax1 = np.zeros((self.maxe))
        self.strainmin1 = np.zeros((self.maxe))
        self.strainaz1 = np.zeros((self.maxe))
        self.stressmax1 = np.zeros((self.maxe))
        self.stressmin1 = np.zeros((self.maxe))
        self.stressaz1 = np.zeros((self.maxe))
        self.velinelxd = np.zeros((self.maxe))
        self.velinelyd = np.zeros((self.maxe))
        self.velinelazd = np.zeros((self.maxe))
        self.velinelmagd = np.zeros((self.maxe))
        self.strainxxd = np.zeros((self.maxe))
        self.strainyyd = np.zeros((self.maxe))
        self.strainxyd = np.zeros((self.maxe))
        self.stressxxd = np.zeros((self.maxe))
        self.stressyyd = np.zeros((self.maxe))
        self.stressxyd = np.zeros((self.maxe))
        self.strainmaxd = np.zeros((self.maxe))
        self.strainmind = np.zeros((self.maxe))
        self.strainazd = np.zeros((self.maxe))
        self.stressmaxd = np.zeros((self.maxe))
        self.stressmind = np.zeros((self.maxe))
        self.stressazd = np.zeros((self.maxe))
        self.velinelxc = np.zeros((self.maxe))
        self.velinelyc = np.zeros((self.maxe))
        self.velinelazc = np.zeros((self.maxe))
        self.velinelmagc = np.zeros((self.maxe))
        self.strainxxc = np.zeros((self.maxe))
        self.strainyyc = np.zeros((self.maxe))
        self.strainxyc = np.zeros((self.maxe))
        self.stressxxc = np.zeros((self.maxe))
        self.stressyyc = np.zeros((self.maxe))
        self.stressxyc = np.zeros((self.maxe))
        self.strainmaxc = np.zeros((self.maxe))
        self.strainminc = np.zeros((self.maxe))
        self.strainazc = np.zeros((self.maxe))
        self.stressmaxc = np.zeros((self.maxe))
        self.stressminc = np.zeros((self.maxe))
        self.stressazc = np.zeros((self.maxe))
        # elements extras
        self.velatvertx0 = np.zeros((3, self.maxe))
        self.velatverty0 = np.zeros((3, self.maxe))
        self.rotationxy0 = np.zeros((self.maxe))
        self.velatvertx1 = np.zeros((3, self.maxe))
        self.velatverty1 = np.zeros((3, self.maxe))
        self.rotationxy1 = np.zeros((self.maxe))
        self.velatvertxd = np.zeros((3, self.maxe))
        self.velatvertyd = np.zeros((3, self.maxe))
        self.rotationxyd = np.zeros((self.maxe))
        self.velatvertxc = np.zeros((3, self.maxe))
        self.velatvertyc = np.zeros((3, self.maxe))
        self.rotationxyc = np.zeros((self.maxe))
        # faults
        self.nslipobonseg = np.zeros((self.maxfs, self.maxf), dtype = np.int32)
        self.nsliplinkonseg = np.zeros((self.maxfs, self.maxf), dtype = np.int32)
        self.seglong = np.zeros((self.maxfs, self.maxf))
        self.seglat = np.zeros((self.maxfs, self.maxf))
        self.seglength = np.zeros((self.maxfs, self.maxf))
        self.segtaz = np.zeros((self.maxfs, self.maxf))
        self.segtx = np.zeros((self.maxfs, self.maxf))
        self.segty = np.zeros((self.maxfs, self.maxf))
        self.segnx = np.zeros((self.maxfs, self.maxf))
        self.segny = np.zeros((self.maxfs, self.maxf))
        self.segnaz = np.zeros((self.maxfs, self.maxf))
        self.slipt0 = np.zeros((self.maxfs, self.maxf))
        self.slipn0 = np.zeros((self.maxfs, self.maxf))
        self.tract0 = np.zeros((self.maxfs, self.maxf))
        self.tracn0 = np.zeros((self.maxfs, self.maxf))
        self.slipx0 = np.zeros((self.maxfs, self.maxf))
        self.slipy0 = np.zeros((self.maxfs, self.maxf))
        self.tracx0 = np.zeros((self.maxfs, self.maxf))
        self.tracy0 = np.zeros((self.maxfs, self.maxf))
        self.slipaz0 = np.zeros((self.maxfs, self.maxf))
        self.slipmag0 = np.zeros((self.maxfs, self.maxf))
        self.tracaz0 = np.zeros((self.maxfs, self.maxf))
        self.tracmag0 = np.zeros((self.maxfs, self.maxf))
        self.slipt1 = np.zeros((self.maxfs, self.maxf))
        self.slipn1 = np.zeros((self.maxfs, self.maxf))
        self.tract1 = np.zeros((self.maxfs, self.maxf))
        self.tracn1 = np.zeros((self.maxfs, self.maxf))
        self.slipx1 = np.zeros((self.maxfs, self.maxf))
        self.slipy1 = np.zeros((self.maxfs, self.maxf))
        self.tracx1 = np.zeros((self.maxfs, self.maxf))
        self.tracy1 = np.zeros((self.maxfs, self.maxf))
        self.slipaz1 = np.zeros((self.maxfs, self.maxf))
        self.slipmag1 = np.zeros((self.maxfs, self.maxf))
        self.tracaz1 = np.zeros((self.maxfs, self.maxf))
        self.tracmag1 = np.zeros((self.maxfs, self.maxf))
        self.sliptd = np.zeros((self.maxfs, self.maxf))
        self.slipnd = np.zeros((self.maxfs, self.maxf))
        self.tractd = np.zeros((self.maxfs, self.maxf))
        self.tracnd = np.zeros((self.maxfs, self.maxf))
        self.slipxd = np.zeros((self.maxfs, self.maxf))
        self.slipyd = np.zeros((self.maxfs, self.maxf))
        self.tracxd = np.zeros((self.maxfs, self.maxf))
        self.tracyd = np.zeros((self.maxfs, self.maxf))
        self.slipazd = np.zeros((self.maxfs, self.maxf))
        self.slipmagd = np.zeros((self.maxfs, self.maxf))
        self.tracazd = np.zeros((self.maxfs, self.maxf))
        self.tracmagd = np.zeros((self.maxfs, self.maxf))
        self.sliptc = np.zeros((self.maxfs, self.maxf))
        self.slipnc = np.zeros((self.maxfs, self.maxf))
        self.tractc = np.zeros((self.maxfs, self.maxf))
        self.tracnc = np.zeros((self.maxfs, self.maxf))
        self.slipxc = np.zeros((self.maxfs, self.maxf))
        self.slipyc = np.zeros((self.maxfs, self.maxf))
        self.tracxc = np.zeros((self.maxfs, self.maxf))
        self.tracyc = np.zeros((self.maxfs, self.maxf))
        self.slipazc = np.zeros((self.maxfs, self.maxf))
        self.slipmagc = np.zeros((self.maxfs, self.maxf))
        self.tracazc = np.zeros((self.maxfs, self.maxf))
        self.tracmagc = np.zeros((self.maxfs, self.maxf))
        # Observation misfits
        self.rss0 = 0.
        self.velobxmis0 = np.zeros((self.maxvo))
        self.velobymis0 = np.zeros((self.maxvo))
        self.slipobmis0 = np.zeros((2, self.maxfo))
        self.strainobmis0 = np.zeros((3, self.maxeo))
        self.sliplinkmis0 = np.zeros((self.maxfs, self.maxduc))
        self.strainlinkmis0 = np.zeros((self.maxece, self.maxec))
        self.rss1 = 0.
        self.velobxmis1 = np.zeros((self.maxvo))
        self.velobymis1 = np.zeros((self.maxvo))
        self.slipobmis1 = np.zeros((2, self.maxfo))
        self.strainobmis1 = np.zeros((3, self.maxeo))
        self.sliplinkmis1 = np.zeros((self.maxfs, self.maxduc))
        self.strainlinkmis1 = np.zeros((self.maxece, self.maxec))
        
        # Load from setup if desired
        if load:
            self.loadFromSetup()
    
    # Object methods to simplify manipulation of model
    def loadFromSetup(self):
        """
        Reads the file setup_input.dat and loads those variables into
        the model, converting indices to 0-indexed for Python.
        """
        (self.ngp, self.nside, self.nel, self.nfault, self.nvline,
            self.nrigidb, self.nvelob, self.nslipob, self.nstrainob,
            self.nsliplink, self.nstrainlink, gp1s, gp2s, s1e, s2e, s3e,
            self.nfaultseg, sf, self.nvlinegp, self.nvlineside,
            self.nvlinefault, gpvl, svl, fvl, self.nrigidbside, srb, ffo, sfo,
            self.nslipobcomp, eeo, self.nstrainobcomp, fduc,
            self.nsliplinkside, sduc, self.nstrainlinkel, eec,
            self.interpvline, self.earthradius, self.gplong, self.gplat,
            self.potxx, self.potyy, self.potxy, self.straincapc,
            self.straincapcc, self.straincapcs, self.straincaps,
            self.straincapsc, self.straincapss, self.slipcapc, self.slipcaps,
            self.vlinex, self.vliney, self.vlineminusx, self.vlineminusy,
            self.vlineplusx, self.vlineplusy, self.eulerlat, self.eulerlong,
            self.eulerrate, self.veloblong, self.veloblat, self.velobx,
            self.veloby, self.velobxse, self.velobyse, self.velobcorr,
            self.slipobcoefft, self.slipobcoeffn, self.slipobvalue,
            self.slipobse, self.slipobcorr, self.strainobcoeffxx,
            self.strainobcoeffyy, self.strainobcoeffxy, self.strainobvalue,
            self.strainobse, reo12, reo13, reo23, self.sliplinkcoefft,
            self.sliplinkcoeffn, self.sliplinkref, self.sliplinkscale,
            self.strainlinkcoeffxx, self.strainlinkcoeffyy,
            self.strainlinkcoeffxy, self.strainlinkref,
            self.strainlinkscale) = setup.input()
        
        # Correct for Fortran indexing from 1
        # Geometry
        self.gp1onside = gp1s - 1
        self.gp2onside = gp2s - 1
        self.side1ofel = s1e - 1
        self.side2ofel = s2e - 1
        self.side3ofel = s3e - 1
        self.sideonfault = sf - 1
        self.gponvline = gpvl - 1
        self.sideonvline = svl - 1
        self.faultonvline = fvl - 1
        self.sideonrigidb = srb - 1
        # Observation and observational constraint set up
        self.faultofslipob = ffo - 1
        self.sideofslipob = sfo - 1
        self.elofstrainob = eeo - 1
        self.faultofsliplink = fduc - 1
        self.sideofsliplink = sduc - 1
        self.elofstrainlink = eec - 1  
        self.strainobcorr = np.array([reo12, reo13, reo23])
    
    def writeForSetup(self):
        """
        Takes variables certain from the model and writes them out to the file 
        setup_input.dat, converting indices to 1-indexed for Fortran.
        """
        writeInput.writei(
            self.ngp, self.nside, self.nel, self.nfault,
            self.nvline, self.nrigidb, self.nvelob, self.nslipob,
            self.nstrainob, self.nsliplink, self.nstrainlink, self.gp1onside+1,
            self.gp2onside+1, self.side1ofel+1, self.side2ofel+1,
            self.side3ofel+1, self.nfaultseg, self.sideonfault+1,
            self.nvlinegp, self.nvlineside, self.nvlinefault, self.gponvline+1,
            self.sideonvline+1, self.faultonvline+1, self.nrigidbside,
            self.sideonrigidb+1, self.faultofslipob+1, self.sideofslipob+1,
            self.nslipobcomp, self.elofstrainob+1, self.nstrainobcomp,
            self.faultofsliplink+1, self.nsliplinkside, self.sideofsliplink+1,
            self.nstrainlinkel, self.elofstrainlink+1, self.interpvline,
            self.earthradius, self.gplong, self.gplat, self.potxx, self.potyy,
            self.potxy, self.straincapc, self.straincapcc, self.straincapcs,
            self.straincaps, self.straincapsc, self.straincapss, self.slipcapc,
            self.slipcaps, self.vlinex, self.vliney, self.vlineminusx,
            self.vlineminusy, self.vlineplusx, self.vlineplusy, self.eulerlat,
            self.eulerlong, self.eulerrate, self.veloblong, self.veloblat,
            self.velobx, self.veloby, self.velobxse, self.velobyse,
            self.velobcorr, self.slipobcoefft, self.slipobcoeffn,
            self.slipobvalue, self.slipobse, self.slipobcorr,
            self.strainobcoeffxx, self.strainobcoeffyy, self.strainobcoeffxy,
            self.strainobvalue, self.strainobse, self.strainobcorr[0,:],
            self.strainobcorr[1,:], self.strainobcorr[2,:],
            self.sliplinkcoefft, self.sliplinkcoeffn, self.sliplinkref,
            self.sliplinkscale, self.strainlinkcoeffxx, self.strainlinkcoeffyy,
            self.strainlinkcoeffxy, self.strainlinkref, self.strainlinkscale)
            
    def readAllOutput(self):
        """
        Reads all of the output files for information that can be saved in the
        model.
        """
        
        readResults.readElements(self, 'all')
        readResults.readFaults(self, 'all')
        readFixedValues.readForces(self)
        readFixedValues.readFixedVels(self)
        readMisfit.readMisfit(self, 'both')
    
    def loadAllGeometry(self):
        """
        Loads all of the geometry by calling the functions that can also be
        called individually below.
        """
        self.getFaultPoints()
        self.getElementPoints()
        self.getRigidBoundaryPoints()
        # Velocity line points are already determined in setup
        
        self.getSideElements()
        self.getSideFaults()
        self.getPointSegments()
        
        self.elementArea()
        self.faultSegmentLength()
        self.faultNormals()
        
        self.getVelocityLineFaultPoints()
    
    def findElement(self, long, lat, weights=True, flag=True, el=0):
        """
        Function to find the element containing a given longitude and latitude.
        
        This is a wrapper for setup.finde applied to this Model object.
        It accounts for the difference in indexing between Python and
        Fortran and it accepts and returns values with Python 0-index.
        
        Parameters
        ----------
        long, lat : float
            Longitude and latitude of point.
        weights : bool, default=True
            Return the weights indicating location of point within
            element.
        flag : bool, default=True
            Return the flag indicating success or failure to find
            element.
        el : int, default=0
            Starting guess for element containing point.
        
        Returns
        -------
        out : int or list
            If both weights and flag are False, out is int indicating
            index of element of point. If weights or flag are True,
            out is list with element followed by three weights (float)
            and/or flag (int).
        """
        isf = self.faultonside[:self.nside] >= 0
        
        # Load fault sides if not already done so
        if self.nfault > 0 and np.sum(isf) == 0:
            self.getSideFaults()
            isf = self.faultonside[:self.nside] >= 0
        
        # Run function
        result = setup.finde(
            long, lat, el+1, self.gplong[:self.ngp],
            self.gplat[:self.ngp], self.el1onside[:self.nside]+1,
            self.el2onside[:self.nside]+1, isf, self.gp1ofel[:self.nel]+1,
            self.gp2ofel[:self.nel]+1, self.gp3ofel[:self.nel]+1,
            self.side1ofel[:self.nel]+1, self.side2ofel[:self.nel]+1,
            self.side3ofel[:self.nel]+1)
        
        # Process output
        if result[4] == 2:
            print('Unable to find element')
            if not flag:
                return
        out = [result[0] - 1]
        if weights:
            out.extend(result[1:4])
        if flag:
            out.append(result[4])
        if len(out) == 1:
            out = out[0]
        return out
    
    def elementArea(self):
        """
        Calculates the area of each element.
        
        This is automatically done inside setup and printed to a file
        in output. This method allows for the calculation to be
        performed and the result used before input into setup.
        """
        # Load element points if not already done so
        if self.gp1ofel[0] == -1:
            self.getElementPoints()
        
        for e in range(self.nel):
            out = setup.evalue(
                self.earthradius,
                self.earthradius*np.radians(self.gplong[self.gp1ofel[e]]),
                self.earthradius*np.radians(self.gplat[self.gp1ofel[e]]),
                self.earthradius*np.radians(self.gplong[self.gp2ofel[e]]),
                self.earthradius*np.radians(self.gplat[self.gp2ofel[e]]),
                self.earthradius*np.radians(self.gplong[self.gp3ofel[e]]),
                self.earthradius*np.radians(self.gplat[self.gp3ofel[e]]))
            self.elarea[e] = np.sum(out[0])
            
    def faultSegmentLength(self):
        """
        Calculates the length of each fault segment.
        
        This is automatically done inside setup and printed to a file
        in output. This method allows for the calculation to be
        performed and the result used before input into setup.
        """
        # Load fault points if not already done so
        if self.nfault > 0 and self.gponfault[0,0] == -1:
            self.getFaultPoints()
        
        for f in range(self.nfault):
            for s in range(self.nfaultseg[f]):
                out = setup.fvalue(
                    self.earthradius,
                    self.earthradius * np.radians(self.gplong[self.gponfault[s, f]]),
                    self.earthradius * np.radians(self.gplat[self.gponfault[s, f]]),
                    self.earthradius * np.radians(self.gplong[self.gponfault[s+1, f]]),
                    self.earthradius * np.radians(self.gplat[self.gponfault[s+1, f]]))
                self.seglength[s, f] = np.sum(out[0])
            # The other variables are junk for this purpose
            
    def faultNormals(self):
        """
        Calculates the unit normal and transverse vectors for fault segments.
        
        This is automatically done and printed to a file in output.
        This method allows the calculation to be performed and used
        before running the model.
        """
        # Load fault points if not already done so
        if self.nfault > 0 and self.gponfault[0,0] == -1:
            self.getFaultPoints()
        
        for f in range(self.nfault):
            for s in range(self.nfaultseg[f]):
                y1 = self.gplat[self.gponfault[s, f]]
                y2 = self.gplat[self.gponfault[s+1, f]]
                dx = ((self.gplong[self.gponfault[s+1, f]]
                        - self.gplong[self.gponfault[s, f]])
                        * np.cos(np.radians((y1+y2) / 2)))
                dy = y2 - y1
                dl = np.hypot(dx, dy)
                self.segtx[s, f] = dx / dl
                self.segty[s, f] = dy / dl
                self.segnx[s, f] = -dy / dl
                self.segny[s, f] = dx / dl
            
    def getFaultPoints(self):
        """
        Determines the gridpoints on each fault.
        """
        for f in range(self.nfault):
            # Use first two segments to determine direction of fault
            if (self.gp1onside[self.sideonfault[0, f]]
                    == self.gp1onside[self.sideonfault[1, f]]):
                self.gponfault[0, f] = self.gp2onside[self.sideonfault[0, f]]
                self.gponfault[1, f] = self.gp1onside[self.sideonfault[0, f]]
                self.gponfault[2, f] = self.gp2onside[self.sideonfault[1, f]]
            
            elif (self.gp1onside[self.sideonfault[0, f]]
                    == self.gp2onside[self.sideonfault[1, f]]):
                self.gponfault[0, f] = self.gp2onside[self.sideonfault[0, f]]
                self.gponfault[1, f] = self.gp1onside[self.sideonfault[0, f]]
                self.gponfault[2, f] = self.gp1onside[self.sideonfault[1, f]]
            
            elif (self.gp2onside[self.sideonfault[0, f]]
                    == self.gp1onside[self.sideonfault[1, f]]):
                self.gponfault[0, f] = self.gp1onside[self.sideonfault[0, f]]
                self.gponfault[1, f] = self.gp2onside[self.sideonfault[0, f]]
                self.gponfault[2, f] = self.gp2onside[self.sideonfault[1, f]]
            
            elif (self.gp2onside[self.sideonfault[0, f]]
                    == self.gp2onside[self.sideonfault[1, f]]):
                self.gponfault[0, f] = self.gp1onside[self.sideonfault[0, f]]
                self.gponfault[1, f] = self.gp2onside[self.sideonfault[0, f]]
                self.gponfault[2, f] = self.gp1onside[self.sideonfault[1, f]]
            else:
                print('Unable to match points of first two segments on fault '
                      +str(f))
            
            # Add in rest of the segments
            for s in range(2, self.nfaultseg[f]):
                if (self.gp1onside[self.sideonfault[s, f]] 
                        == self.gponfault[s, f]):
                    self.gponfault[s+1, f] = self.gp2onside[
                        self.sideonfault[s, f]]
                elif (self.gp2onside[self.sideonfault[s, f]]
                        == self.gponfault[s, f]):
                    self.gponfault[s+1, f] = self.gp1onside[
                        self.sideonfault[s, f]]
                else:
                    print('Unable to match points of segment '+str(s)+
                          ' on fault '+str(f))
    
    def getRigidBoundaryPoints(self):
        """
        Determines the gridpoints on each rigid boundary.
        """
        for rb in range(self.nrigidb):
            # Use first two segments to determine direction of rigid boundary
            if (self.gp1onside[self.sideonrigidb[0, rb]]
                    ==self.gp1onside[self.sideonrigidb[1, rb]]):
                self.gponrigidb[0, rb] = self.gp2onside[self.sideonrigidb[0, rb]]
                self.gponrigidb[1, rb] = self.gp1onside[self.sideonrigidb[0, rb]]
                self.gponrigidb[2, rb] = self.gp2onside[self.sideonrigidb[1, rb]]
            elif (self.gp1onside[self.sideonrigidb[0, rb]]
                    == self.gp2onside[self.sideonrigidb[1, rb]]):
                self.gponrigidb[0, rb] = self.gp2onside[self.sideonrigidb[0, rb]]
                self.gponrigidb[1, rb] = self.gp1onside[self.sideonrigidb[0, rb]]
                self.gponrigidb[2, rb] = self.gp1onside[self.sideonrigidb[1, rb]]
            elif (self.gp2onside[self.sideonrigidb[0, rb]]
                    == self.gp1onside[self.sideonrigidb[1, rb]]):
                self.gponrigidb[0, rb] = self.gp1onside[self.sideonrigidb[0, rb]]
                self.gponrigidb[1, rb] = self.gp2onside[self.sideonrigidb[0, rb]]
                self.gponrigidb[2, rb] = self.gp2onside[self.sideonrigidb[1, rb]]
            elif (self.gp2onside[self.sideonrigidb[0, rb]]
                    == self.gp2onside[self.sideonrigidb[1, rb]]):
                self.gponrigidb[0, rb] = self.gp1onside[self.sideonrigidb[0, rb]]
                self.gponrigidb[1, rb] = self.gp2onside[self.sideonrigidb[0, rb]]
                self.gponrigidb[2, rb] = self.gp1onside[self.sideonrigidb[1, rb]]
            else:
                print('Unable to match points of first two segments on rigid '+
                      'boundary '+str(rb))
            # Add in rest of the segments
            for s in range(2,self.nrigidbside[rb]):
                if (self.gp1onside[self.sideonrigidb[s, rb]]
                        == self.gponrigidb[s, rb]):
                    self.gponrigidb[s+1, rb] = self.gp2onside[
                        self.sideonrigidb[s, rb]]
                elif (self.gp2onside[self.sideonrigidb[s, rb]]
                        == self.gponrigidb[s, rb]):
                    self.gponrigidb[s+1, rb] = self.gp1onside[
                        self.sideonrigidb[s, rb]]
                else:
                    print('Unable to match points of segment '+str(s)+'on '+
                          'rigid boundary '+str(rb))
    
    def getElementPoints(self):
        """
        Determines the gridpoints of each element.
        """
        for e in range(self.nel):
            # Point 1 is the point opposite side 1
            # Match point on sides 2 and 3
            if (self.gp1onside[self.side2ofel[e]]
                    == self.gp1onside[self.side3ofel[e]]
                    or self.gp1onside[self.side2ofel[e]]
                    == self.gp2onside[self.side3ofel[e]]):
                # If True, point 1 is gp1 on side 2
                self.gp1ofel[e] = self.gp1onside[self.side2ofel[e]]
                # Determine points 2 and 3
                if (self.gp2onside[self.side2ofel[e]]
                        == self.gp1onside[self.side1ofel[e]]):
                    self.gp3ofel[e] = self.gp1onside[self.side1ofel[e]]
                    self.gp2ofel[e] = self.gp2onside[self.side1ofel[e]]
                elif (self.gp2onside[self.side2ofel[e]]
                        == self.gp2onside[self.side1ofel[e]]):
                    self.gp2ofel[e] = self.gp1onside[self.side1ofel[e]]
                    self.gp3ofel[e] = self.gp2onside[self.side1ofel[e]]
                else:
                    print('Unable to match sides on element '+str(e+1))
            elif (self.gp2onside[self.side2ofel[e]]
                    == self.gp1onside[self.side3ofel[e]]
                    or self.gp2onside[self.side2ofel[e]]
                    == self.gp2onside[self.side3ofel[e]]):
                # Else if True, point 1 is gp2 on side 2
                self.gp1ofel[e] = self.gp2onside[self.side2ofel[e]]
                # Determine points 2 and 3
                if (self.gp1onside[self.side2ofel[e]]
                        == self.gp1onside[self.side1ofel[e]]):
                    self.gp3ofel[e] = self.gp1onside[self.side1ofel[e]]
                    self.gp2ofel[e] = self.gp2onside[self.side1ofel[e]]
                elif (self.gp1onside[self.side2ofel[e]]
                        == self.gp2onside[self.side1ofel[e]]):
                    self.gp2ofel[e] = self.gp1onside[self.side1ofel[e]]
                    self.gp3ofel[e] = self.gp2onside[self.side1ofel[e]]
                else:
                    print('Unable to match sides on element '+str(e))
            else:
                print('Unable to match sides on element '+str(e))

    def getSideElements(self):
        """
        Determines the elements of each side.
        """
        # Reset arrays if necessary
        if self.el1onside[0] >= 0:
            self.el1onside = -np.ones((self.maxs), dtype=np.int32)
            self.el2onside = -np.ones((self.maxs), dtype=np.int32)
        
        for e in range(self.nel):
            sides = [self.side1ofel[e], self.side2ofel[e], self.side3ofel[e]]
            for s in sides:
                if self.el1onside[s] == -1:
                    self.el1onside[s] = e
                elif self.el2onside[s] == -1:
                    self.el2onside[s] = e
                else:
                    print('Impossibly many elements on side '+str(s))
                    
    def getSideFaults(self):
        """
        Determines the fault and segment number of each side.
        """
        for f in range(self.nfault):
            for s in range(self.nfaultseg[f]):
                self.faultonside[self.sideonfault[s, f]] = f
                self.segonside[self.sideonfault[s, f]] = s

    def getPointSegments(self):
        """
        Determines the number of fault, velocity line, and rigid
        boundary segments at a gridpoints.
        
        Useful for identifying fault terminations and triple junctions.
        """
        # Check fault and rigid boundary points loaded
        if self.nfault > 0 and self.gponfault[0,0] == -1:
            self.getFaultPoints()
        if self.nrigidb > 0 and self.gponrigidb[0,0] == -1:
            self.getRigidBoundaryPoints()
        if self.gp1ofel[0] == -1:
            self.getElementPoints()
        
        # Reset number of points
        self.nelatgp = np.zeros((self.maxgp), dtype=np.int32)
        self.nsegatgp = np.zeros((self.maxgp), dtype=np.int32)
        self.nvlineatgp = np.zeros((self.maxgp), dtype=np.int32)
        self.nrigidbatgp = np.zeros((self.maxgp), dtype=np.int32)
        
        # Elements/total segments at gridpoint
        for e in range(self.nel):
            self.nelatgp[self.gp1ofel[e]] += 1
            self.nelatgp[self.gp2ofel[e]] += 1
            self.nelatgp[self.gp3ofel[e]] += 1
        
        # Fault segments at gridpoint
        for f in range(self.nfault):
            self.nsegatgp[self.gponfault[0, f]] += 1
            for gp in range(1, self.nfaultseg[f]):
                self.nsegatgp[self.gponfault[gp, f]] += 2
            self.nsegatgp[self.gponfault[self.nfaultseg[f],f]] += 1
        
        # Velocity lines at gridpoint
        for vl in range(self.nvline):
            self.nvlineatgp[self.gponvline[0, vl]] += 1
            for gp in range(1, self.nvlineside[vl]):
                self.nvlineatgp[self.gponvline[gp, vl]] += 2
            self.nvlineatgp[self.gponvline[self.nvlineside[vl], vl]] += 1
        
        # Rigid boundaries at gridpoint
        for rb in range(self.nrigidb):
            self.nrigidbatgp[self.gponrigidb[0, rb]] += 1
            for gp in range(1, self.nrigidbside[rb]):
                self.nrigidbatgp[self.gponrigidb[gp, rb]] += 2
            self.nrigidbatgp[self.gponrigidb[self.nrigidbside[rb], rb]] += 1
    
    def getElementFaults(self):
        """
        Determines the number of sides and gridpoints of an element
        that are on a fault.
        """
        if self.nfault == 0:
            # No faults in the model; all elements touch zero faults
            return
        
        # Check point segments, side elements, and side faults loaded
        if self.el1onside[0] == -1:
            self.getSideElements()
        if self.nelatgp[0] == -1:
            self.getPointSegments()
        if np.sum(self.faultonside) == -self.maxs:
            self.getSideFaults()
        
        for e in range(self.nel):
            # Number of element sides on faults
            self.nsideofelonfault[e] = (self.faultonside[self.side1ofel[e]] >= 0
                + self.faultonside[self.side2ofel[e]] >= 0
                + self.faultonside[self.side3ofel[e]] >= 0)
            # Number of element gridpoints on faults
            self.ngpofelonfault[e] = (self.nsegatgp[self.gp1ofel[e]] >= 0
                + self.nsegatgp[self.gp2ofel[e]] >= 0
                + self.nsegatgp[self.gp3ofel[e]] >= 0)
    
    def getVelocityLineFaultPoints(self):
        """
        Determines the point index of a fault where it meets a velocity
        line.
        
        The function also determines the sign of faults on velocity
        lines. +1 means that the positive normal to the fault is in the
        same direction as the positive tangent to velocity line.
        -1 means that the fault normal is in the opposite direction to
        the velocity line tangent. The positive normal is to the right
        of the fault when travelling from start to end.
        """
        # Check fault points and normals loaded
        if self.nfault > 0 and self.segtx[0,0] == 0 and self.segty[0,0] == 0:
            self.faultNormals()
        
        for i in range(self.nvline):
            for j in range(self.nvlineside[i] + 1):
                # Identify velocity line points which are on faults
                if self.faultonvline[j, i] >= 0:
                    f = self.faultonvline[j, i]
                    gp = self.gponvline[j, i]
                    # Calculate v-line tangent. Note magnitude irrelevant
                    if j == 0:
                        # If first point on v-line
                        y1 = self.gplat[self.gponvline[j, i]]
                        y2 = self.gplat[self.gponvline[j+1, i]]
                        dx = ((self.gplong[self.gponvline[j+1, i]]
                               - self.gplong[self.gponvline[j, i]])
                              * np.cos(np.radians((y1+y2) / 2)))
                        dy = y2 - y1
                    elif j == self.nvlineside[i]:
                        # If last point on v-line
                        y1 = self.gplat[self.gponvline[j-1, i]]
                        y2 = self.gplat[self.gponvline[j, i]]
                        dx = ((self.gplong[self.gponvline[j, i]]
                               - self.gplong[self.gponvline[j-1, i]])
                              * np.cos(np.radians((y1+y2) / 2)))
                        dy = y2 - y1
                    else:
                        # Non-endpoints get average of segments either side
                        y1 = self.gplat[self.gponvline[j, i]]
                        y2 = self.gplat[self.gponvline[j+1, i]]
                        dx1 = ((self.gplong[self.gponvline[j+1, i]]
                                - self.gplong[self.gponvline[j, i]])
                               * np.cos(np.radians((y1+y2) / 2)))
                        dy1 = y2 - y1
                        y1 = self.gplat[self.gponvline[j-1, i]]
                        y2 = self.gplat[self.gponvline[j, i]]
                        dx2 = ((self.gplong[self.gponvline[j, i]]
                                - self.gplong[self.gponvline[j-1, i]])
                               * np.cos(np.radians((y1+y2) / 2)))
                        dy2 = y2 - y1
                        dx = dx1/np.hypot(dx1, dy1) + dx2/np.hypot(dx2, dy2)
                        dy = dy1/np.hypot(dx1, dy1) + dy2/np.hypot(dx2, dy2)
                    # Find the matching point on the fault
                    for k in range(self.nfaultseg[f] + 1):
                        if gp == self.gponfault[k, f]:
                            self.faultptonvline[j, i] = k
                            if k == 0:
                                nx = self.segnx[k, f]
                                ny = self.segny[k, f]
                            elif k == self.nfaultseg[k-1]:
                                nx = self.segnx[k-1, f]
                                ny = self.segny[k-1, f]
                            else:
                                nx = self.segnx[k, f] + self.segnx[k-1, f]
                                ny = self.segny[k, f] + self.segny[k-1, f]                                
                            # Calculate sign using dot product
                            self.faultsignonvline[j, i] = np.sign(dx*nx + dy*ny)
                            break
                    else:
                        print('Unable to find point '+str(gp)+' on fault '
                              +str(f)+' for v-line '+str(i))
        
    def getSlipObsOnFault(self, fault):
        """Returns array of slip rate observations on fault.
        
        Parameters
        ----------
        fault : int
            Index of fault.
        
        Returns
        -------
        obs : array of int
            Array of slip rate observations on fault.
        """
        
        sides = self.sideonfault[:self.nfaultseg[fault], fault]
        obs = np.arange(self.maxfo)[np.isin(self.sideofslipob, sides)]
        
        return obs








