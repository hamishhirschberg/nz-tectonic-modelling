#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 27 15:29:26 2020

addSubfaults.py - program to add 'subfault' identifier to segments.
A 'subfault' is a section of a fault comprising one or more model fault segments
that has similar fault properties or another reason for being adjusted in a
uniform manner.

@author: hamish
"""

import shapefile
import numpy as np

def addSubfaults(model, faults, subfaultName='FZ_Name',
                 subfaultNumber='Subfault_2', segmentNumber='Seg_Num',
                 faultNumber='Fault_Num', wrapTo360=False):
    """Adds subfault identifiers to segments
    
    A 'subfault' is a section of a fault comprising one or more model fault
    segments that have similar fault properties or another reason for being 
    considered in a collective manner. Subfaults do not affect the model
    inversion but can be used for analysis or adjustment. Subfaults are
    extracted from a shapefile with the same fault geometry as the model.
    
    Parameters
    ----------
    model : permdefmap model
        Add subfaults to this model.
    faults : str
        Name of shapefile with subfault information. Normally, this will be
        the same shapefile used to generate the model's fault geometry.
    subfaultName : str, default='FZ_Name'
        Name of the shapefile attribute containing subfault names.
    subfaultNumber : str, default='Subfault_2'
        Name of the shapefile attribute containing subfault indices.
    segmentNumber : str, default='Seg_Num'
        Name of the shapefile attribute containing segment indices.
    faultNumber : str, default='Fault_Num'
        Name of the shapefile attribute containing fault indices.
    wrapTo360 : bool, default=False
        Convert longitude values from the range [-180,180] to the range
        [0,360].
    """
    # Check if gridpoints have been loaded into model
    if model.nfault == 0:
        print('No faults present in the model. Cannot add subfaults.')
        return
    elif model.gponfault[1,1]==0:
        print('Fault gridpoints not loaded. Loading gridpoints now.')
        model.getFaultPoints()
    
    # Input/processing of shapefile
    sf_faults = shapefile.Reader(faults)
    # Shape data
    f_shape_raw = sf_faults.shapes()
    # Number of faults in the shapefile
    nfsf = len(f_shape_raw)
    # Field definitions (attribute definitions) as a list
    f_field = sf_faults.fields
    # Records (attribute values) as a list
    f_record_raw = sf_faults.records()
    # Dictionary linking attribute name to index
    i_f_field={'':0}
    for i in range(1, len(f_field)):
        i_f_field[f_field[i][0]] = i - 1
    # Reorder the shapes and records by 'Seg_Num'
    f_shape = [[] for _ in range(nfsf)]
    f_record = [[] for _ in range(nfsf)]
    for f in range(nfsf):
        i = f_record_raw[f][i_f_field[segmentNumber]]
        f_shape[i] = f_shape_raw[f]
        f_record[i] = f_record_raw[f]
    # Fault points, model fault number
    f_XY = []
    mf = []
    # Loop through each fault and process it
    for f in range(nfsf):
        f_XY.append(f_shape[f].points)
        subf = f_record[f][i_f_field[subfaultNumber]]
        model.subfname[subf] = f_record[f][i_f_field[subfaultName]]
        mf = f_record[f][i_f_field[faultNumber]]
        for fs in range(len(f_XY[f])-1):
            long0 = f_XY[f][fs][0]
            long1 = f_XY[f][fs+1][0]
            if wrapTo360:
                if long0 <= 0:
                    long0 += 360
                if long1 <= 0:
                    long1 += 360
            lat0 = f_XY[f][fs][1]
            lat1 = f_XY[f][fs+1][1]
            for s in range(model.nfaultseg[mf]):
                lo = (model.gplong[model.gponfault[s,mf]]
                      + model.gplong[model.gponfault[s+1,mf]]) / 2
                la = (model.gplat[model.gponfault[s,mf]]
                      + model.gplat[model.gponfault[s+1,mf]]) / 2
                if ((long0>lo) != (long1>lo)) and ((lat0>la) != (lat1>la)):
                    model.subfofseg[s,mf] = subf
                    model.subfofside[model.sideonfault[s,mf]] = subf
    # Calculate number of subfaults
    model.nsubf = np.max(model.subfofseg) + 1

def obSubfaults(model):
    """Determine the subfaults of slip rate observations.
    
    Parameters
    ----------
    model : permdefmap model
        Model with the observations and subfaults.
    
    Returns
    -------
    subfofslipob : array of int
        Indices of the subfault each slip rate observation is on.
    """
    
    if model.nsubf == 0:
        print('No subfaults loaded.')
        return
    
    subfofslipob = -np.ones((model.maxfo), dtype=np.int)
    
    for i in range(model.nslipob):
        subfofslipob[i] = model.subfofside[model.sideofslipob[i]]
    
    return subfofslipob






























