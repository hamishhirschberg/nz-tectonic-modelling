#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Add velocity observations to model.

Created on Fri Apr 29 16:28:13 2022

@author: Hamish Hirschberg
"""

import shapefile
#import numpy
from ..geometry import eulerVelPlate

def fromShapefile(model, file, velx, vely, velxse, velyse, corr=None,
                  unitconv=1., plate=None):
    """Add velocity observations from Shapefile.
    
    Parameters
    ----------
    model : permdefmap model
        Add velocity observations to this model.
    file : string
        File name of Shapefile with velocity observations.
    velx, vely : string
        Field names in the Shapefile for the x (east) and y (north)
        components of velocity.
    velxse, velyse : string
        Field names in the Shapefile for the standard error in the x and y
        components of velocity.
    corr : string, default=None
        Field name in the Shapefile for the correlation between the
        variances in the x and y components of velocity. Default of None
        indicates that there is no correlation field, in which case the
        correlation is assumed to be zero.
    unitconv : float
        Multiply velocities and standard errors by this value to convert
        them to the velocity units used in the model.
    plate : int, default=None
        Velocities are in the reference frame of this plate. This function
        will convert the velocities to the reference from of the model
        using the model's plate rotation parameters. Default of None
        indicates that the velocities are in the model reference frame and
        do not need to be rotated.
    """
    
    # Import/processing of Shapefile
    sf = shapefile.Reader(file)
    # Shape data
    shape = sf.shapes()
    # Number of observations
    nobs = len(shape)
    # Field (attribute) definitions and records as lists
    fields = sf.fields
    records = sf.records()
    # Dictionary linking fields to index
    fieldind = {'':0}
    for i in range(len(fields)-1):
        fieldind[fields[i+1][0]] = i
    
    # Loop through each observation and process it
    for i in range(nobs):
        # Location
        model.veloblong[model.nvelob] = shape[i].points[0][0]
        model.veloblat[model.nvelob] = shape[i].points[0][1]
        
        # Observation and standard error
        model.velobx[model.nvelob] = records[i][fieldind[velx]] * unitconv
        model.veloby[model.nvelob] = records[i][fieldind[vely]] * unitconv
        model.seoux[model.nvelob] = records[i][fieldind[velxse]] * unitconv
        model.seouy[model.nvelob] = records[i][fieldind[velyse]] * unitconv
        
        # Correlation if present
        if corr is None:
            model.velobcorr[model.nvelob] = 0.
        else:
            model.velobcorr[model.nvelob] = records[i][fieldind[corr]]
        
        # Rotate to plate if desired
        if plate is not None:
            plateve, platevn = eulerVelPlate(model, plate, model.veloblong[i],
                                             model.veloblat[i])
            model.velobx[model.nvelob] += plateve
            model.veloby[model.nvelob] += platevn
        
        model.nvelob += 1


















