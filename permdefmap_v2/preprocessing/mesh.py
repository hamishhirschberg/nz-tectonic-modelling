#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 15 11:56:50 2023

mesh.py - a collection of functions that manipulate the mesh geometry
of a permdefmap model.

@author: Hamish Hirschberg
"""

def mergeFaults(model, fault1, fault2):
    """Merge two faults that share an endpoint.
    
    This functions combines two faults which share an endpoint into a
    single, continuous fault in the model. The model solution
    constraints set fault slip rates at fault endpoints (excluding
    triple junctions) to zero. Merging the faults into a continuous
    fault removes that constraint.
    
    The faults are merged by extending the fault with the lower index.
    The properties of the fault with higher index are copied into the
    extension of the lower index fault. Observations on the higher
    index fault are reclassified as being on the lower index fault.
    The higher index fault is then deleted and all other faults with a
    higher index have their index reduced by one.
    
    It is assumed that this function is run in preprocessing before the
    model is inverted. Therefore, this function does not change any
    inversion result arrays. It also does not change any arrays
    populated by model.loadAllGeometry().
    
    Parameters
    ----------
    model : permdefmap model
        Merge faults in this model.
    fault1, fault2 : int
        Indices of two faults to be merged.
    """
    
    import numpy as np
    
    # Make fault1 the first fault
    if fault2 < fault1:
        fault1, fault2 = fault2, fault1
    elif fault2 == fault1:
        raise RuntimeError('Cannot merge fault '+str(fault1)+' with itself.')
    
    # Get endpoints
    model.getFaultPoints()
    nseg1 = model.nfaultseg[fault1]
    nseg2 = model.nfaultseg[fault2]
    gp11 = model.gponfault[0, fault1]
    gp21 = model.gponfault[0, fault2]
    gp12 = model.gponfault[nseg1, fault1]
    gp22 = model.gponfault[nseg2, fault2]
    
    # Find shared endpoints and which end of the faults they are at
    if (gp11 == gp21):
        flip1 = True
        flip2 = False
    elif (gp11 == gp22):
        flip1 = True
        flip2 = True
    elif (gp12 == gp21):
        flip1 = False
        flip2 = False
    elif (gp12 == gp22):
        flip1 = False
        flip2 = True
    else:
        raise RuntimeError('Faults '+str(fault1)+' and '+str(fault2)
                            +' do not share a common endpoint.')
    
    # Flip fault properties if necessary
    if flip1:
        model.sideonfault[:nseg1,fault1] = np.flip(model.sideonfault[:nseg1,fault1])
        model.slipcapc[:nseg1,fault1] = np.flip(model.slipcapc[:nseg1,fault1])
        model.slipcaps[:nseg1,fault1] = np.flip(model.slipcaps[:nseg1,fault1])
    if flip2:
        model.sideonfault[:nseg2,fault2] = np.flip(model.sideonfault[:nseg2,fault2])
        model.slipcapc[:nseg2,fault2] = np.flip(model.slipcapc[:nseg2,fault2])
        model.slipcaps[:nseg2,fault2] = np.flip(model.slipcaps[:nseg2,fault2])
    
    # Merge fault2 into fault1
    nsegt = nseg1 + nseg2
    model.nfaultseg[fault1] = nsegt
    model.sideonfault[nseg1:nsegt,fault1] = model.sideonfault[:nseg2,fault2]
    model.slipcapc[nseg1:nsegt,fault1] = model.slipcapc[:nseg2,fault2]
    model.slipcaps[nseg1:nsegt,fault1] = model.slipcaps[:nseg2,fault2]
    model.faultofsliplink[model.faultofsliplink==fault2] = fault1
    model.faultofslipob[model.faultofslipob==fault2] = fault1
    model.faultonvline[model.faultonvline==fault2] = fault1
    
    # Remove fault 2
    model.faultname[fault2:model.nfault] = model.faultname[fault2+1:model.nfault+1]
    model.nfaultseg[fault2:model.nfault] = model.nfaultseg[fault2+1:model.nfault+1]
    model.sideonfault[:,fault2:model.nfault] = model.sideonfault[:,fault2+1:model.nfault+1]
    model.slipcapc[:,fault2:model.nfault] = model.slipcapc[:,fault2+1:model.nfault+1]
    model.slipcaps[:,fault2:model.nfault] = model.slipcaps[:,fault2+1:model.nfault+1]
    model.faultofsliplink[model.faultofsliplink>fault2] -= 1
    model.faultofslipob[model.faultofslipob>fault2] -= 1
    model.faultonvline[model.faultonvline>fault2] -= 1
    model.nfault -= 1
    
def removeFault(model, fault, convertObs=True):
    """Remove fault.
    
    This function removes a fault from the model geometry. It does not
    change the mesh geometry as it converts the fault segments from
    locations where velocity discontinuities can occur into regular
    sides with continuous velocities across them.
    
    It is assumed that this function is run in preprocessing before the
    model is inverted. Therefore, this function does not change any
    inversion result arrays. It also does not change any arrays
    populated by model.loadAllGeometry().
    
    Parameters
    ----------
    model : permdefmap model
        Merge faults in this model.
    fault : int
        Index of the fault to be removed.
    convertObs : bool, default=True
    """
    
    import numpy as np
    from slip2strainRate import someObs
    
    # Convert slip rate observations to strain rate observations
    if convertObs:
        obs = np.arange(model.maxfo)[model.faultofslipob==fault]
        someObs(model, obs)
    
    # Remove fault from arrays of properties
    model.faultname[fault:model.nfault] = model.faultname[fault+1:model.nfault+1]
    model.nfaultseg[fault:model.nfault] = model.nfaultseg[fault+1:model.nfault+1]
    model.sideonfault[:,fault:model.nfault] = model.sideonfault[:,fault+1:model.nfault+1]
    model.slipcapc[:,fault:model.nfault] = model.slipcapc[:,fault+1:model.nfault+1]
    model.slipcaps[:,fault:model.nfault] = model.slipcaps[:,fault+1:model.nfault+1]
    model.faultofsliplink[model.faultofsliplink>fault] -= 1
    model.faultofslipob[model.faultofslipob>fault] -= 1
    model.faultonvline[model.faultonvline>fault] -= 1
    model.nfault -= 1
            
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    