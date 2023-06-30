#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 26 16:34:01 2022

@author: Hamish Hirschberg
"""

def styleFromObs(model, interp=False):
    """Calculate strain rate style from observations.
    
    Style is defined as the dilatational strain rate divided by the
    strain rate magnitude. It is in the range -sqrt(2) to +sqrt(2).
    Currently this function assumes that there is one observation per
    element, the observations are arranged in the same order as the
    elements, and the observation components are in the order xx, yy, xy.
    Later versions will likely remove these assumptions.
    
    Parameters
    ----------
    model : permdefmap model
        Calculate strain rate style for this model.
    interp : boolean, default=False
        Interpolate style from elements with calculated style into elements
        with no calculated style (i.e. style is nan). Interpolation uses
        scipy interpolate's radial basis functions with the thin-plate
        function.
    
    Returns
    -------
    style : array of float
        array of strain rate style in each element.
    """
    
    from scipy import interpolate
    import numpy as np
    
    # Calculate observation magnitude and then style.
    mag = np.sqrt(model.strainobvalue[0,:model.nel]**2
                  + model.strainobvalue[1,:model.nel]**2
                  + 2*model.strainobvalue[2,:model.nel]**2)
    style = (model.strainobvalue[0,:model.nel]
             + model.strainobvalue[1,:model.nel]) / mag
    
    if interp:
        # Element longitude and latitude.
        ellong = (model.gplong[model.gp1ofel[:model.nel]] 
                  + model.gplong[model.gp2ofel[:model.nel]]
                  + model.gplong[model.gp3ofel[:model.nel]]) / 3
        ellat = (model.gplat[model.gp1ofel[:model.nel]] 
                 + model.gplat[model.gp2ofel[:model.nel]]
                 + model.gplat[model.gp3ofel[:model.nel]]) / 3
        # Elements with and without style calculation
        hasstyle = np.isfinite(style)
        nostyle = np.isnan(style)
        # Perform interpolation
        stylerbf = interpolate.Rbf(ellong[hasstyle], ellat[hasstyle],
                                   style[hasstyle], function='thin_plate')
        # Store interpolated values
        style[nostyle] = stylerbf(ellong[nostyle], ellat[nostyle])
    
    return style
    
def directionFromObs(model, interp=False):
    """Calculate strain rate direction from observations.
    
    Direction is returned in terms of the cos(2*theta) and sin(2*theta) where
    theta is the angle of maximum (extensional) strain rate anti-clockwise
    from the x-axis.
    Currently this function assumes that there is one observation per
    element, the observations are arranged in the same order as the
    elements, and the observation components are in the order xx, yy, xy.
    Later versions will likely remove these assumptions.
    
    Parameters
    ----------
    model : permdefmap model
        Calculate strain rate style for this model.
    interp : boolean, default=False
        Interpolate style from elements with calculated style into elements
        with no calculated style (i.e. style is nan). Interpolation uses
        scipy interpolate's radial basis functions with the thin-plate
        function.
    
    Returns
    -------
    costh, sinth : array of float
        Arrays of cos(2*theta) and sin(2*theta) in each element.
    """
    
    from scipy import interpolate
    import numpy as np
    
    # Calculate shear strain rates.
    shear1 = (model.strainobvalue[0,:model.nel]
              - model.strainobvalue[1,:model.nel])
    shear2 = 2 * model.strainobvalue[2,:model.nel]
    shearm = np.hypot(shear1, shear2)
    # Calculate cos, sin of direction
    costh = shear1 / shearm
    sinth = shear2 / shearm
    
    if interp:
        # Element longitude and latitude.
        ellong = (model.gplong[model.gp1ofel[:,model.nel]] 
                  + model.gplong[model.gp2ofel[:,model.nel]]
                  + model.gplong[model.gp3ofel[:,model.nel]]) / 3
        ellat = (model.gplat[model.gp1ofel[:,model.nel]] 
                 + model.gplat[model.gp1ofel[:,model.nel]]
                 + model.gplat[model.gp1ofel[:,model.nel]]) / 3
        # Elements with and without style calculation
        hasstyle = np.isfinite(costh)
        nostyle = np.isnan(costh)
        # Perform interpolation
        cosrbf = interpolate.Rbf(ellong[hasstyle], ellat[hasstyle],
                                 costh[hasstyle], function='thin_plate')
        sinrbf = interpolate.Rbf(ellong[hasstyle], ellat[hasstyle],
                                 sinth[hasstyle], function='thin_plate')
        # Store interpolated values
        costh[nostyle] = cosrbf(ellong[nostyle], ellat[nostyle])
        sinth[nostyle] = sinrbf(ellong[nostyle], ellat[nostyle])
        # Rescale values to ensure magnitude of 1
        intmag = np.hypot(costh, sinth)
        costh[nostyle] /= intmag[nostyle]
        sinth[nostyle] /= intmag[nostyle]
    
    return costh, sinth









