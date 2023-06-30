#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 20 10:48:32 2020

@author: hamish
"""

import numpy as np

def potFromTractionStress(model, prop=1.):
    """Adjust force potentials from traction and stress.
    
    This is the pure 1-D version of adjsuting the force potentials
    based on the difference between the a posteriori and a priori
    tractions and stresses.
    
    Parameters
    ---------
    model : 1D permdefmap model
        Adjust force potentials for this model.
    prop : float or list of arrays, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied. As a float,
        it is applied uniformly to all segments and points. As a list,
        the first item is an array of the proportion for each point
        (although only points on faults are used) and the second item
        is an array of the proportion for each segment.
    """
    
    # Separate slip and strain proportions
    if type(prop) == list:
        slipprop = prop[0]
        strainprop = prop[1]
    else:
        slipprop = prop
        strainprop = prop
    
    dpot = np.zeros(model.npt)
    # Changes on segments
    dpot[:-1] += model.stressd * strainprop
    dpot[1:] += model.stressd * strainprop
    # Change at point is average of changes on bordering segments.
    dpot[1:-1] *= 0.5
    
    # Changes on faults, which take priority over changes from segments.
    dpot[model.faults] = (model.tracd*slipprop)[model.faults]
    
    # Apply changes
    model.pot += dpot

def strainCapUniformly(model, prop=1., strainmin=1e-12):
    """Adjust strain-rate capacity uniformly.
    
    This is the pure 1-D version of adjusting the strain-rate capacity
    based on the ratio between the a posteriori and a priori strain
    rates. This function adjusts the strain-rate capacity by the same
    factor for all segments.
    
    Parameters
    ----------
    model : 1D permdefmap model
        Adjust strain-rate capacities for this model.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied.
    strainmin : float, default=1e-12
        Minimum relative strain rate to avoid numerical problems
        associated with dividing by a small number.
    """
    
    # A priori
    # Magnitude of stress * strain
    mag0 = model.strain0 * model.stress0
    # Sum of segment lengths
    ltot = abs(model.points[-1] - model.points[0])
    # Total magnitude, weighted by segment length
    magtot0 = np.sum(mag0 * model.dx)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = strainmin * magtot0 / ltot
    # Set segments smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = magmin
    
    # A posteriori
    # Magnitude of a priori stress * a posteriori strain rate
    mag1 = model.strain1 * model.stress0
    # Prevent elements weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set elements smaller than min mag to value of min mag
    mag1[mag1 < strainmin] = strainmin
    # Calculate ratio of a posteriori and a priori strain rates for all elements
    ratio = mag1 / mag0
    # Scaling factor weighted by length
    scale=(np.sum(ratio * model.dx) / ltot) ** prop

    model.straincap = model.straincap * scale

def strainCapBySegment(model, prop=1., strainmin=1e-12):
    """Adjust strain-rate capacity by segment.
    
    This is the pure 1-D version of adjusting the strain-rate capacity
    based on the ratio between the a posteriori and a priori strain
    rates. This function adjusts the strain-rate capacity by different
    factors for each segment.
    
    Parameters
    ----------
    model : 1D permdefmap model
        Adjust strain-rate capacities for this model.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied.
    strainmin : float, default=1e-12
        Minimum relative strain rate to avoid numerical problems
        associated with dividing by a small number.
    """
    
    # A priori
    # Magnitude of stress * strain
    mag0 = model.strain0 * model.stress0
    # Sum of segment lengths
    ltot = abs(model.points[-1] - model.points[0])
    # Total magnitude, weighted by segment length
    magtot0 = np.sum(mag0 * model.dx)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = strainmin * magtot0 / ltot
    # Set segments smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = magmin
    
    # A posteriori
    # Magnitude of a priori stress * a posteriori strain rate
    mag1 = model.strain1 * model.stress0
    # Prevent elements weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set elements smaller than min mag to value of min mag
    mag1[mag1 < strainmin] = strainmin
    # Scaling factor is ratio of a posteriori and a priori strain rates
    scale = (mag1 / mag0) ** prop

    model.straincap = model.straincap * scale
    
def slipCapUniformly(model, prop=1., slipmin=1e-12):
    """Adjust slip-rate capacity uniformly.
    
    This is the pure 1-D version of adjusting the slip-rate capacity
    based on the ratio between the a posteriori and a priori slip
    rates. This function adjusts the slip-rate capacity by the same
    factor for all faults.
    
    Parameters
    ----------
    model : 1D permdefmap model
        Adjust slip-rate capacities for this model.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied.
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    """
    
    # A priori
    # Magnitude of traction * slip rate for all faults
    mag0 = model.slip0 * model.trac0
    # Total magnitude
    magtot0 = np.sum(mag0)
    # Minimum magnitude
    magmin = slipmin * magtot0 / model.nfault
    # Set faults smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = magmin
    
    # A posteriori
    # Magnitude of a priori traction * a posteriori slip rate
    mag1 = model.trac0 * model.slip1
    # Prevent faults weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set segments smaller than min mag to value of min mag
    mag1[np.all([mag1<magmin, mag1], axis=0)] = magmin
    # Calculate corrected ratio of apost and apri slip rates for all faults
    slipratio = np.divide(mag1, mag0, where=model.faults)
    # Average fault ratio
    scale = np.mean(slipratio[model.faults]) ** prop
    
    # Apply adjustment to slip rate capacities
    model.slipcap = scale * model.slipcap

def slipCapByFault(model, prop=1., slipmin=1e-12):
    """Adjust slip-rate capacity by fault.
    
    This is the pure 1-D version of adjusting the slip-rate capacity
    based on the ratio between the a posteriori and a priori slip
    rates. This function adjusts the slip-rate capacity by different
    factors for each fault.
    
    Parameters
    ----------
    model : 1D permdefmap model
        Adjust slip-rate capacities for this model.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied.
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    """
    
    # A priori
    # Magnitude of traction * slip rate for all faults
    mag0 = model.slip0 * model.trac0
    # Total magnitude
    magtot0 = np.sum(mag0)
    # Minimum magnitude
    magmin = slipmin * magtot0 / model.nfault
    # Set faults smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = magmin
    
    # A posteriori
    # Magnitude of a priori traction * a posteriori slip rate
    mag1 = model.trac0 * model.slip1
    # Prevent faults weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set segments smaller than min mag to value of min mag
    mag1[np.all([mag1<magmin, mag1], axis=0)] = magmin
    # Calculate corrected ratio of apost and apri slip rates for all faults
    scale = np.divide(mag1, mag0, where=model.faults) ** prop
    
    # Apply adjustment to slip rate capacities
    model.slipcap = scale * model.slipcap
    
def adjustMaterialRatio(model,prop=1,dtau=0,adj=1):
    """
    This function adjusts the strain- and slip-rate capacities based on the 
     ratio between the aposteriori and apriori strain and slip rates.
    prop adjusts how big the adjustment is.
    dtau accounts for a change in the stress tau.
    """
    import numpy as np
    
    if type(prop)==list:
        kfac=prop[0][model.faults]
        efac=prop[1]
    else:
        kfac=prop
        efac=prop
    
    if type(adj)==list:
        kadj=adj[0][model.faults]
        eadj=adj[1]
    else:
        kadj=adj
        eadj=adj
    
    kratio=np.divide(model.slip1,model.slip0+model.slipcap*dtau,where=model.faults)
#    kratio[kratio<-1]=-1        # prevent wrong-way weakening
#    model.slipcap[model.faults]=np.power(np.abs(kratio[model.faults]*kadj),kfac)*model.slipcap[model.faults]
    model.slipcap[model.faults]=np.abs(1-kfac+kfac*kratio[model.faults])*kadj*model.slipcap[model.faults]
    
    eratio=np.divide(model.strain1,model.strain0+model.straincap*dtau)
#    eratio[eratio<-1]=-1
#    model.straincap=np.abs(eratio*eadj)**efac*model.straincap
    model.straincap=np.abs(1-efac+efac*eratio)*eadj*model.straincap
    
    
    
    
    
    
    
    
    
    
    