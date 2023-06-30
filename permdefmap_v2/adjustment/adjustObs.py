#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 22 11:06:13 2021

@author: Hamish Hirschberg

adjustObs - a collection of functions to adjust certain observations in the
 model so that a given feature can be fit.
"""

def strainStyleMag(model, style, stylese, soln=0, minmag=0., comp=0,
                   obrange=None):
    """Adjust strain rate observations based on style.
    
    This function adjusts strain rate observations and their standard
    errors to match the observed strain style based on a given strain
    rate magnitude. The magnitude is taken from the magnitude of strain
    rate in the previous iteration.
    
    model : permdefmap model
        Model where the observations are being adjusted.
    style : array of float
        Strain rate style in elements.
    stylese : array of float
        Standard error of strain rate style.
    soln : 0 (default) or 1
        Determines whether the a priori (0) or a posteriori (1)
        solution is used for strain rate magnitude.
    minmag : float, default=0.
        Elements with a strain rate magnitude smaller than minmag will
        use minmag instead when calculating standard errors for their
        observations.
    comp : int, default=0
        Index of the observation component that constrains strain rate
        style.
    obrange : iterable
        Indices of observations to be adjusted. Default is all
        observations.
    """
    import numpy as np
    
    if obrange is None:
        obrange = range(model.nstrainob)
    
    if str(soln) == '0' or soln == 'apri' or soln == 'apriori':
        # Magnitude of strain rate from previous iteration
        mag = np.sqrt(model.strainxx0[:model.nel]**2 
                      + model.strainyy0[:model.nel]**2
                      + 2 * model.strainxy0[:model.nel]**2)
    elif str(soln) == '1' or soln == 'apost' or soln == 'aposteriori':
        mag = np.sqrt(model.strainxx1[:model.nel]**2 
                      + model.strainyy1[:model.nel]**2
                      + 2 * model.strainxy1[:model.nel]**2)
    else:
        print('Solution not recognised.')
        return
    
    for i in obrange:
        # Element of observation
        e = model.elofstrainob[i]
        
        model.strainobvalue[comp,i] = mag[e] * style[e]
        if mag[e] < minmag:
            # Use minmag to calculate std error if magnitude is too small
            model.strainobse[comp,i] = minmag * stylese[e]
        else:
            model.strainobse[comp,i] = mag[e] * stylese[e]
        
def strainDirecMag(model, direcse, soln=0, minmag=0, comp=2, obrange=None):
    """Adjust strain rate observations based on direction.
    
    This function adjusts strain rate observations and their standard
    errors to match the observed strain direction based on a given strain
    rate magnitude. The magnitude is taken from the magnitude of strain
    rate in the previous iteration.
    
    model : permdefmap model
        Model where the observations are being adjusted.
    direcse : array of float
        Standard error of strain rate direction in radians.
    soln : 0 (default) or 1
        Determines whether the a priori (0) or a posteriori (1)
        solution is used for strain rate magnitude.
    minmag : float, default=0.
        Elements with a strain rate magnitude smaller than minmag will
        use minmag instead when calculating standard errors for their
        observations.
    comp : int, default=0
        Index of the observation component that constrains strain rate
        direction.
    obrange : iterable
        Indices of observations to be adjusted. Default is all
        observations.
    """
    import numpy as np
    
    if obrange is None:
        obrange  =  range(model.nstrainob)
    
    if str(soln) == '0' or soln == 'apri' or soln == 'apriori':
        mag = np.sqrt(0.25 * (model.strainxx0[:model.nel]
                              - model.strainyy0[:model.nel]) ** 2
                      + model.strainxy0[:model.nel]**2)
    elif str(soln) == '1' or soln == 'apost' or soln == 'aposteriori':
        mag = np.sqrt(0.25 * (model.strainxx1[:model.nel]
                              - model.strainyy1[:model.nel]) ** 2
                      + model.strainxy1[:model.nel]**2)
    else:
        print('Solution not recognised.')
        return
    
    for i in obrange:
        e = model.elofstrainob[i]
        
        model.strainobvalue[comp,i] = mag[e]
        if mag[e] < minmag:
            # use minmag to calculate std error if magnitude is too small
            model.strainobse[comp,i] = minmag * direcse[e]
        else:
            model.strainobse[comp,i] = mag[e] * direcse[e]
    
def strainAllMag(model, style, stylese, direcse, soln = 0, minmag = 0,
                 obrange = None):
    """Adjusts magnitudes of all strain rate observation components.
    
    This function adjusts the magnitudes of all strain rate observation
    components based on the magnitude of the observation in the
    previous iteration. It assumes that the three components are:
        0: style*mag: exx+eyy
        1: max shear*mag: e1-e2  =  exx'-eyy'
        2: zero shear direction: 2*exy' = 0
    Standard error in direction assumed to be in radians.
    
    model : permdefmap model
        Model where the observations are being adjusted.
    style : array of float
        Strain rate style in elements.
    stylese : array of float
        Standard error of strain rate style.
    direcse : array of float
        Standard error of strain rate direction in radians.
    soln : 0 (default) or 1
        Determines whether the a priori (0) or a posteriori (1)
        solution is used for strain rate magnitude.
    minmag : float, default = 0.
        Elements with a strain rate magnitude smaller than minmag will
        use minmag instead when calculating standard errors for their
        observations.
    obrange : iterable
        Indices of observations to be adjusted. Default is all
        observations.
    """
    import numpy as np
    
    if obrange is None:
        obrange = range(model.nstrainob)
    
    if str(soln) == '0' or soln == 'apri' or soln == 'apriori':
        mag = np.sqrt(model.strainxx0[:model.nel]**2
                      + model.strainyy0[:model.nel]**2
                      + 2 * model.strainxy0[:model.nel]**2)
    elif str(soln) == '1' or soln == 'apost' or soln == 'aposteriori':
        mag = np.sqrt(model.strainxx1[:model.nel]**2
                      + model.strainyy1[:model.nel]**2
                      + 2 * model.strainxy1[:model.nel]**2)
    else:
        print('Solution not recognised.')
        return
    
    for i in obrange:
        e = model.elofstrainob[i]
        
        # Strain style observation
        model.strainobvalue[0,i] = mag[e-1] * style[e-1]
        # Max shear observation
        model.strainobvalue[1,i] = mag[e-1] * np.sqrt(2-style[e-1]**2)
        # Zero shear observation  =  0
        if mag[e-1] < minmag:
            # use minmag to calculate std error if magnitude is too small
            model.strainobse[0,i] = minmag * stylese[e-1]
            model.strainobse[2,i] = 2 * minmag * direcse[e-1]
        else:
            model.strainobse[0,i] = mag[e-1] * stylese[e-1]
            model.strainobse[2,i] = 2 * mag[e-1] * direcse[e-1]
        model.strainobse[1,i] = (np.abs(style[e-1]) / np.sqrt(2-style[e-1]**2)
                                 * model.strainobse[0,i]
                                 + 0.5 * model.strainobse[2,i])
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    