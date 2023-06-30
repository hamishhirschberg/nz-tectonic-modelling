#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 26 12:21:40 2020

addStrainrateObs.py

@author: hamish
"""
import numpy as np

def addNew(model, elofstrainob, strainobse, strainobvalue=[0,0,0],
                     strainobcoeffxx=[1,0,0], strainobcoeffyy=[0,1,0],
                     strainobcoeffxy=[0,0,1], nstrainobcomp=3,
                     strainobcorr=[0,0,0]):
    """Add strain rate observations to a model.
    
    This function adds one or more strain rate observations to a model.
    Each input parameter is a list or array of the values of that
    parameter for each input observation. The parameters use the same
    naming convention as the permdefmap model parameters. The default
    values of the coefficients correspond to the components being
    observations of exx, eyy, and exy.
    
    Parameters
    ----------
    model : permdefmap model
        Model strain rate observations are being added to.
    elofstrainob : int or list of int
        Elements of strain rate observations.
    strainobse : float or array of float
        Standard error of strain rate observation. The first axis of
        the array should have size 3.
    strainobvalue : float or array of float, default = [0,0,0]
        Value of strain rate observation. The first axis of the array
        should have size 3.
    strainobcoeffxx : float or array of float, default = [1,0,0]
        xx coefficient of strain rate observation. The first axis of
        the array should have size 3.
    strainobcoeffyy : float or array of float, default = [0,1,0]
        yy coefficient of strain rate observation. The first axis of
        the array should have size 3.
    strainobcoeffxy : float or array of float, default = [0,0,1]
        xy coefficient of strain rate observation. The first axis of
        the array should have size 3.
    nstrainobcomp : int or array of int, default = 3
        Number of components in strain rate observation.
    strainobcorr : float or array of float, default = [0,0,0]
        Correlation of strain rate observation components. The first
        axis of the array should have size 3.
    """
    # Number of observations to be added
    nobs = np.size(elofstrainob)
    # Add attributes for the observations
    model.elofstrainob[model.nstrainob:model.nstrainob+nobs] = elofstrainob
    model.nstrainobcomp[model.nstrainob:model.nstrainob+nobs] = nstrainobcomp
    # Reshape any arrays that need it so they can be broadcast correctly
    if np.size(strainobcoeffxx) == 3:
        strainobcoeffxx = np.array(strainobcoeffxx).reshape((3,1))
    model.strainobcoeffxx[:,model.nstrainob:model.nstrainob+nobs] = strainobcoeffxx
    if np.size(strainobcoeffyy) == 3:
        strainobcoeffyy = np.array(strainobcoeffyy).reshape((3,1))
    model.strainobcoeffyy[:,model.nstrainob:model.nstrainob+nobs] = strainobcoeffyy
    if np.size(strainobcoeffxy) == 3:
        strainobcoeffxy = np.array(strainobcoeffxy).reshape((3,1))
    model.strainobcoeffxy[:,model.nstrainob:model.nstrainob+nobs] = strainobcoeffxy
    if np.size(strainobvalue) == 3:
        strainobvalue = np.array(strainobvalue).reshape((3,1))
    model.strainobvalue[:,model.nstrainob:model.nstrainob+nobs] = strainobvalue
    if np.size(strainobse) == 3:
        strainobse = np.array(strainobse).reshape((3,1))
    model.strainobse[:,model.nstrainob:model.nstrainob+nobs] = strainobse
    if np.size(strainobcorr)  ==  3:
        strainobcorr = np.array(strainobcorr).reshape((3,1))
    model.strainobcorr[:,model.nstrainob:model.nstrainob+nobs] = strainobcorr
    model.nstrainob += nobs

def removeStrainOb(model,ob):
    """Removes a single strain rate observation.
    
    Parameters
    ----------
    model : permdefmap model
        Model the observation is being removed from.
    ob : int
        Index of the observation to be removed.
    """
    # Shift all later observations forward by one
    model.elofstrainob[ob:model.nstrainob-1] = model.elofstrainob[ob+1:model.nstrainob]
    model.nstrainobcomp[ob:model.nstrainob-1] = model.nstrainobcomp[ob+1:model.nstrainob]
    model.strainobcoeffxx[:, ob:model.nstrainob-1] = model.strainobcoeffxx[:, ob+1:model.nstrainob]
    model.strainobcoeffyy[:, ob:model.nstrainob-1] = model.strainobcoeffyy[:, ob+1:model.nstrainob]
    model.strainobcoeffxy[:, ob:model.nstrainob-1] = model.strainobcoeffxy[:, ob+1:model.nstrainob]
    model.strainobvalue[:, ob:model.nstrainob-1] = model.strainobvalue[:, ob+1:model.nstrainob]
    model.strainobse[:, ob:model.nstrainob-1] = model.strainobse[:, ob+1:model.nstrainob]
    model.reo12[ob:model.nstrainob-1] = model.reo12[ob+1:model.nstrainob]
    model.reo13[ob:model.nstrainob-1] = model.reo13[ob+1:model.nstrainob]
    model.reo23[ob:model.nstrainob-1] = model.reo23[ob+1:model.nstrainob]
    model.nstrainob -= 1

def addStrainrateInterp(model,interp,unc=0.1):
    """
    Function to add strain rates interpolated from one permdefmap model to
     another. The two models are assumed to have the same units. This function
     finds the observation's element based on long and lat, making it slow.
     
    This function assumes that interp is a tab/space separated text file with
     data on each line following the order:
         longitude, latitude,
         exx, eyy, exy,
     with optional errors:
         s_exx, s_eyy, s_exy,
     and thence optional error correlations:
         c_exx_eyy, c_exx_exy, c_eyy_exy
    
    An assumed relative uncertainty can be provided if no uncertainty is
     otherwise known. This defaults to 0.1.
    """
    if model.gp1e[1]==0:
        print('Element gridpoints not loaded. Loading gridpoints now.')
        model.getElementPoints()
    file = open(interp,'r')
    line=file.readline().split()
    while line:
        lon=float(line[0])
        lat=float(line[1])
        # find element containing this point
        for e in range(model.nel):
            lons=[model.long[model.gp1e[e]-1],model.long[model.gp2e[e]-1],model.long[model.gp3e[e]-1],model.long[model.gp1e[e]-1]]
            lats=[model.lat[model.gp1e[e]-1],model.lat[model.gp2e[e]-1],model.lat[model.gp3e[e]-1],model.lat[model.gp1e[e]-1]]
            test=0
            for i in range(3):
                test+=((lat-lats[i])*(lons[i+1]-lons[i])>(lon-lons[i])*(lats[i+1]-lats[i]))
            if test==0 or test==3:
                # point is in this element
                model.elofstrainob[model.nstrainob]=e+1
                # observation orientation
                model.strainobcoeffxx[0,model.nstrainob]=1.
                model.strainobcoeffyy[1,model.nstrainob]=1.
                model.strainobcoeffxy[2,model.nstrainob]=1.
                # observation components
                model.nstrainobcomp[model.nstrainob]=3
                model.strainobvalue[0,model.nstrainob]=float(line[2])
                model.strainobvalue[1,model.nstrainob]=float(line[3])
                model.strainobvalue[2,model.nstrainob]=float(line[4])
                # observation uncertainties
                if len(line)>5:
                    model.strainobse[0,model.nstrainob]=float(line[5])
                    model.strainobse[1,model.nstrainob]=float(line[6])
                    model.strainobse[2,model.nstrainob]=float(line[7])
                else:
                    model.strainobse[0,model.nstrainob]=abs(float(line[2]))*unc
                    model.strainobse[1,model.nstrainob]=abs(float(line[3]))*unc
                    model.strainobse[2,model.nstrainob]=abs(float(line[4]))*unc
                # observation uncertainty correlations
                if len(line)>8:
                    model.reo12[model.nstrainob]=float(line[8])
                    model.reo13[model.nstrainob]=float(line[9])
                    model.reo23[model.nstrainob]=float(line[10])
                model.nstrainob += 1
                break
        else:
            print('Unable to find element for strain rate obsrvation at '+\
                  str(round(lon,6))+' '+str(round(lat,6)))
        line=file.readline().split()
                    
def addStrainrateOrthog(model,interp,unc=0.1):
    """
    Function to add strain rates interpolated from one permdefmap model to
     another. The two models are assumed to have the same units and the same
     geometry. The funciton requires the element of the observation in the file.
     
    This function assumes that interp is a tab/space separated text file with
     data on each line following the order:
         longitude, latitude, element
         exx, eyy, exy,
     with optional errors:
         s_exx, s_eyy, s_exy,
     and thence optional error correlations:
         c_exx_eyy, c_exx_exy, c_eyy_exy
    
    An assumed relative uncertainty can be provided if no uncertainty is
     otherwise known. This defaults to 0.1.
    """
    if model.gp1e[1]==0:
        print('Element gridpoints not loaded. Loading gridpoints now.')
        model.getElementPoints()
    file = open(interp,'r')
    line=file.readline().split()
    while line:
        # point is in this element
        model.elofstrainob[model.nstrainob]=int(line[2])
        # observation orientation
        model.strainobcoeffxx[0,model.nstrainob]=1.
        model.strainobcoeffyy[1,model.nstrainob]=1.
        model.strainobcoeffxy[2,model.nstrainob]=1.
        # observation components
        model.nstrainobcomp[model.nstrainob]=3
        model.strainobvalue[0,model.nstrainob]=float(line[3])
        model.strainobvalue[1,model.nstrainob]=float(line[4])
        model.strainobvalue[2,model.nstrainob]=float(line[5])
        # observation uncertainties
        if len(line)>6:
            model.strainobse[0,model.nstrainob]=float(line[6])
            model.strainobse[1,model.nstrainob]=float(line[7])
            model.strainobse[2,model.nstrainob]=float(line[8])
        else:
            model.strainobse[0,model.nstrainob]=abs(float(line[3]))*unc
            model.strainobse[1,model.nstrainob]=abs(float(line[4]))*unc
            model.strainobse[2,model.nstrainob]=abs(float(line[5]))*unc
        # observation uncertainty correlations
        if len(line)>9:
            model.reo12[model.nstrainob]=float(line[9])
            model.reo13[model.nstrainob]=float(line[10])
            model.reo23[model.nstrainob]=float(line[11])
        model.nstrainob += 1
        line=file.readline().split()

def sumPresent(model):
    """Sum strain rate observations in same element.
    
    Function to sum multiple strain rate observations in the same
    element. It is intended for situations where there are
    contributions from multiple sources to the strain rate in an
    element that have been evaluated separately. The variance of the
    output observation is the sum of the variances of the input
    observations for that element. The function only sums observations
    that have all three components. Other observations are just moved
    to the start of the list of observations.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain rates are being summed.
    """
    
    # Elements with observations
    el=np.zeros((model.nel), dtype=np.bool)
    # Value of observation in each element
    obs=np.zeros((3,model.nel))
    # Variance of observations
    obvar=np.zeros((3,model.nel))
    
    # Observations which have < 3 components
    part=np.zeros_like(model.nstrainobcomp, dtype=np.bool)
    
    for ob in range(model.nstrainob):
        if model.nstrainobcomp[ob] < 3:
            # Track observations with < 3 components
            part[ob] = True
        else:
            # Track element of observation
            e = model.elofstrainob[ob]
            el[e] = True
            # Rotate observation into xy-coordinates. Note: cannot assume that
            # coefficient matrix is orthogonal.
            A = np.array([[model.strainobcoeffxx[0,ob], model.strainobcoeffyy[0,ob], model.strainobcoeffxy[0,ob]],
                          [model.strainobcoeffxx[1,ob], model.strainobcoeffyy[1,ob], model.strainobcoeffxy[1,ob]],
                          [model.strainobcoeffxx[2,ob], model.strainobcoeffyy[2,ob], model.strainobcoeffxy[2,ob]]])
            b = np.array([model.strainobvalue[0,ob], model.strainobvalue[1,ob], model.strainobvalue[2,ob]])
            Ainvb = np.linalg.solve(A, b)
            # Add rotated observation to element's total
            obs[:,e] += Ainvb
            # Add variances
            b = np.array([model.strainobse[0,ob], model.strainobse[1,ob], model.strainobse[2,ob]])
            Ainvb = np.linalg.solve(A, b)
            obvar[:,e] += Ainvb * Ainvb
    
    # Move partial observations
    npart = np.sum(part)
    model.elofstrainob[:npart] = model.elofstrainob[part]
    model.strainobcoeffxx[:, :npart] = model.strainobcoeffxx[:, part]
    model.strainobcoeffyy[:, :npart] = model.strainobcoeffyy[:, part]
    model.strainobcoeffxy[:, :npart] = model.strainobcoeffxy[:, part]
    model.nstrainobcomp[:npart] = model.nstrainobcomp[part]
    model.strainobvalue[:, :npart] = model.strainobvalue[:, part]
    model.strainobse[:, :npart] = model.strainobse[:, part]
    model.strainobcorr[:, :npart] = model.strainobcorr[:, part]
    
    # Add summed observtions. Note: not all elements might have an observation
    model.nstrainob = npart + np.sum(el)
    model.elofstrainob[npart:model.nstrainob] = np.arange(model.nel)[el]
    model.strainobcoeffxx[:, npart:model.nstrainob] = [[1], [0], [0]]
    model.strainobcoeffyy[:, npart:model.nstrainob] = [[0], [1], [0]]
    model.strainobcoeffxy[:, npart:model.nstrainob] = [[0], [0], [1]]
    model.nstrainobcomp[npart:model.nstrainob] = 3
    model.strainobvalue[:, npart:model.nstrainob] = obs + 0.
    model.strainobse[:, npart:model.nstrainob] = np.sqrt(obvar)
    model.strainobcorr[:, npart:model.nstrainob] = 0.
    














