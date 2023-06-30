#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 15:45:01 2020

addSliprateObs.py

@author: Hamish Hirschberg
"""
import shapefile
import numpy as np

def pointsFromShapeFile(model, file, unitconv=1.):
    """Adds slip rate observations specified at points.
    
    This function adds slip rate observations from a Shapefile to a model
    using observations that are specified at points. The segment of the
    observation is the one where the observation falls into the
    segment's bounding box. This assumes that the fault does not zigzag
    excessively and the observation has been snapped to the fault trace.
    
    Parameters
    ----------
    model : permdefmap model
        The model that the observation is being added to.
    file : string
        The file name of the shapefile containing the observations.
    unitconv : float
        Multiply slip rates and standard errors by this value to convert
        them to the slip rate (velocity) units used in the model.
    """
    # check if fault gridpoints have been loaded into model
    if model.nfault == 0:
        print('No faults present in the model. Cannot add slip rate observations.')
        return
    elif model.gponfault[1,1]==0:
        print('Fault gridpoints not loaded. Loading gridpoints now.')
        model.getFaultPoints()
    
    #input/processing of shapefile
    sf = shapefile.Reader(file)
    # Shape data
    shape = sf.shapes()
    # Number of slip rate observations
    nobs = len(shape)
    # Field definitions (attribute definitions) as a list
    fields = sf.fields
    # Records (attribute values) as a list
    records = sf.records()
    # Dictionary linking attribute name to index
    fieldind = {'':0}
    for i in range(1,len(fields)):
        fieldind[fields[i][0] ] = (i-1)
    # Loop through each observation and process it
    for i in range(nobs):
        # longitude and latitude of observation
        lo = shape[i].points[0][0]
        la = shape[i].points[0][1]
        # Fault observation is on
        f = records[i][fieldind['Fault_Num']]
        # Dip and error
        dip = np.radians(records[i][fieldind['Dip_Best']])
        dipse = np.radians(records[i][fieldind['Dip_Max']] -
                           records[i][fieldind['Dip_Min']]) * 0.25
        # Use rake to determine cdut and cdun
        rake = np.radians(records[i][fieldind['Rake_Best']])
        # Positive for normal slip
        if np.sin(rake) > 0:
            coeffn=1.
        else:
            coeffn=-1.
        # Positive for dextral slip
        if np.cos(rake) > 0:
            coefft=1.
        else:
            coefft=-1.
        # Strike slip rate
        strike_sr=records[i][fieldind['StrSR_Best']]
        if strike_sr != None:
            Vht=float(strike_sr)*0.001
            # Strike slip rate error - treat Max and Min as defining 95% confidence interval (or 2 sigma)
            err_Vht=0.25*(records[i][fieldind['StrSR_Max']]-records[i][fieldind['StrSR_Min']])*0.001
        else:
            Vht=-1      # if no strike slip rate in observation
            err_Vht=-1
        # Fault-normal (shortening) component of slip
        short_sr=records[i][fieldind['Short_Best']]
        dip_sr=records[i][fieldind['DipSR_Best']]
        vert_sr=records[i][fieldind['VerSR_Best']]
        # Shortening rate
        if short_sr != None:
            Vhn=float(short_sr)*0.001
            err_Vhn=0.25*(records[i][fieldind['Short_Max']]-records[i][fieldind['Short_Min']])*0.001
        # Dip slip rate
        elif dip_sr != None:
            Vd=float(dip_sr)*0.001
            Vhn=Vd*np.cos(dip)
            # Dip slip rate error
            err_Vd=0.25*(records[i][fieldind['DipSR_Max']]-records[i][fieldind['DipSR_Min']])*0.001
            err_Vhn=err_Vd*np.cos(dip)+Vd*np.sin(dip)*dipse
        # Vertical slip rate
        elif vert_sr != None:
            Vv=float(vert_sr)*0.001
            Vhn=Vv/np.tan(dip)
            # Vertical slip rate error
            err_Vv=0.25*(records[i][fieldind['VerSR_Max']]-records[i][fieldind['VerSR_Min']])*0.001
            err_Vhn=err_Vv/np.tan(dip)+Vv*dipse/np.pow(np.sin(dip),2)     
        else:
            Vhn=-1      # If no dip or vertical slip rate in observation
            err_Vhn=-1
    
        # add observation information to model
        i = model.nslipob
        nc = 0            # number of components
        if (Vht != -1):
            model.slipobcoefft[nc, i] = coefft
            model.slipobcoeffn[nc, i] = 0.
            model.slipobvalue[nc, i] = Vht
            model.slipobse[nc, i] = err_Vht
            nc += 1
        if (Vhn != -1):
            model.slipobcoefft[nc, i] = 0.
            model.slipobcoeffn[nc, i] = coeffn
            model.slipobvalue[nc, i] = Vhn
            model.slipobse[nc, i] = err_Vhn
            nc += 1
        if nc == 2:
            model.slipobcorr[i] = 0.
        elif nc == 0:
            name = records[i][fieldind['Obs_Name']]
            print('Observation '+str(i)+' ('+name+') has no slip rate. Ignoring.')
            continue
        
        model.nslipobcomp[i] = nc       # number of observation components
        
        # calculate which segment of fault observation is on
        model.faultofslipob[i] = f
        for s in range(model.nfaultseg[f]):
            if (((model.gplong[model.gponfault[s, f]] > lo)
                 != (model.gplong[model.gponfault[s+1, f]] >= lo))
                and ((model.gplat[model.gponfault[s, f]] > la)
                    != (model.gplat[model.gponfault[s+1, f]] >= la))):
                model.sideofslipob[i] = model.sideonfault[s, f]
                break
        else:
            print('Unable to find segment for slip-rate observation '+str(i)+' on fault '+str(f))
        
        model.nslipob += 1      # update number of observations

def removeObs(model, obs):
    """Remove specified slip rate observations.
    
    Parameters
    ----------
    model : permdefmap model
        Model to remove observations from.
    obs : array of float
        Indices of slip rate observations to remove
    """
    
    # Get boolean array of observations removed and staying
    remove = np.isin(np.arange(model.maxfo), obs)
    stay = np.logical_not(remove)
    nstay = np.sum(stay)
    
    # Remove the observations
    model.faultofslipob[:nstay] = model.faultofslipob[stay]
    model.sideofslipob[:nstay] = model.sideofslipob[stay]
    model.nslipobcomp[:nstay] = model.nslipobcomp[stay]
    model.slipobcoefft[:, :nstay] = model.slipobcoefft[:, stay]
    model.slipobcoeffn[:, :nstay] = model.slipobcoeffn[:, stay]
    model.slipobvalue[:, :nstay] = model.slipobvalue[:, stay]
    model.slipobse[:, :nstay] = model.slipobse[:, stay]
    model.slipobcorr[:nstay] = model.slipobcorr[stay]
    model.nslipob -= np.sum(remove)
    
def removeEndObs(model):
    """Remove slip rate observations from end segments.
    
    This function removes slip rate observations from segments that are
    fault terminations. Observations adjacent to triple junctions are
    not affected.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the end observations are being removed.
    """
    for i in range(model.nslipob-1,-1,-1):
        gp1 = model.gp1onside[model.sideofslipob[i]]
        gp2 = model.gp2onside[model.sideofslipob[i]]
        # Determine if segment is a fault termination
        if (model.nsegatgp[gp1] + model.nvlineatgp[gp1] == 1
                or model.nsegatgp[gp2] + model.nvlineatgp[gp2] == 1):
            # remove observation on end segment and shunt forward remaining observations
            model.faultofslipob[i:model.nslipob] = model.faultofslipob[i+1:model.nslipob+1]
            model.sideofslipob[i:model.nslipob] = model.sideofslipob[i+1:model.nslipob+1]
            model.nslipobcomp[i:model.nslipob] = model.nslipobcomp[i+1:model.nslipob+1]
            model.slipobcoefft[:, i:model.nslipob] = model.slipobcoefft[:, i+1:model.nslipob+1]
            model.slipobcoeffn[:, i:model.nslipob] = model.slipobcoeffn[:, i+1:model.nslipob+1]
            model.slipobvalue[:, i:model.nslipob] = model.slipobvalue[:, i+1:model.nslipob+1]
            model.slipobse[:, i:model.nslipob] = model.slipobse[:, i+1:model.nslipob+1]
            model.slipobcorr[i:model.nslipob] = model.slipobcorr[i+1:model.nslipob+1]
            model.nslipob -= 1
    
def removeFaultObs(model,faults):
    """Remove slip rate observations from specific faults.
    
    Function to remove all slip rate observations from specific faults.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the observations are being removed.
    faults : list or int
        List of indices of faults to remove the observations from. A
        single fault can be given as an int.
    """
    # Check type of faults variable
    if type(faults) is int:
        faults = [faults]
    
    # Loop through observations backwards
    for i in range(model.nslipob-1,-1,-1):
        if model.faultofslipob[i] in faults:
            # Remove observation on end segment and shunt forward remaining observations
            model.faultofslipob[i:model.nslipob] = model.faultofslipob[i+1:model.nslipob+1]
            model.sideofslipob[i:model.nslipob] = model.sideofslipob[i+1:model.nslipob+1]
            model.nslipobcomp[i:model.nslipob] = model.nslipobcomp[i+1:model.nslipob+1]
            model.slipobcoefft[:, i:model.nslipob] = model.slipobcoefft[:, i+1:model.nslipob+1]
            model.slipobcoeffn[:, i:model.nslipob] = model.slipobcoeffn[:, i+1:model.nslipob+1]
            model.slipobvalue[:, i:model.nslipob] = model.slipobvalue[:, i+1:model.nslipob+1]
            model.slipobse[:, i:model.nslipob] = model.slipobse[:, i+1:model.nslipob+1]
            model.slipobcorr[i:model.nslipob] = model.slipobcorr[i+1:model.nslipob+1]
            model.nslipob -= 1
       
def halveEndObs(model):
    """Halve slip rate observations at ends of faults.
    
    This function halves the observations on the end segments of
    faults. It halves each component of the observation and their
    standard errors, maintaining the same ratio between all parts of
    that observation and their standard errors.
    
    Be careful when applying this that it is not applied to observations
    that do not need it. It is most likely to be needed by observations
    that have been automatically assigned to all segments along a fault.
    It should not be needed where observations have been applied
    directly to a single segment.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the observations are being modified.
    """
    for i in range(model.nslipob):
        gp1 = model.gp1onside[model.sideofslipob[i]]
        gp2 = model.gp2onside[model.sideofslipob[i]]
        # Determine if segment is a fault termination
        if (model.nsegatgp[gp1] + model.nvlineatgp[gp1] == 1
                or model.nsegatgp[gp2] + model.nvlineatgp[gp2] == 1):
            model.slipobvalue[:, i] /= 2.
            model.slipobse[:, i] /= 2.
    
    
    
    
    
    
    
    
    
    
    
    