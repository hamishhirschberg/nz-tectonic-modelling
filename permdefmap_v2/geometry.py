#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 25 10:53:16 2020

misc.py - a collection of miscellaneous functions that I don't know where else
 to put at the moment. Not all of them are directly related to the permdefmap
 module.
 
This module used to contain some plotting functions which have since moved to
 plotting. They are maintained here for legacy purposes by calling the function
 in plotting.

@author: hamish
"""

import numpy as np
from .postprocessing.readResults import readFaults
from .preprocessing.subfaults import obSubfaults

def eulerVel(eulerlong, eulerlat, eulerrate, ptlong, ptlat,
             earthradius=6.371e6):
    """Velocity of point from Euler pole and rotation rate.
    
    Parameters
    ----------
    eulerlong, eulerlat : float
        Longitude and latitude of Euler pole.
    eulerrate : float
        Rotation rate around the Euler pole in rad/yr (or
        rad/[time units]).
    ptlong, ptlat : float
        Longitude and latitude of points of interest.
    earthradius : float, default=6.371e6
        Radius of Earth/planet in m (or [length units]).
    
    Returns
    -------
    velx, vely : float
        x (east) and y (north) components of velocity at points
        of interest.
    """
    
    # Convert longitudes and latitudes to radians
    eulerlong = np.radians(eulerlong)
    eulerlat = np.radians(eulerlat)
    ptlong = np.radians(ptlong)
    ptlat = np.radians(ptlat)
    
    # East and north velocities of point
    velx = (earthradius * eulerrate 
            * (np.sin(eulerlat) * np.cos(ptlat) 
               - np.cos(eulerlat) * np.sin(ptlat) * np.cos(ptlong-eulerlong)))
    vely = (earthradius * eulerrate
            * np.cos(eulerlat) * np.sin(ptlong-eulerlong))
    
    return [velx, vely]

def eulerVelPlate(model, plate, ptlong, ptlat):
    """Velocity of point relative to plate in model.
    
    This function simplifies the implementation of eulerVel when used in
    the context of a permdefmap model.
    
    Parameters
    ----------
    model : permdefmap model
        Model that includes the plate.
    plate : int
        Index of plate.
    ptlong, ptlat : float
        Longitude and latitude of points of interest.
        
    Returns
    -------
    velx, vely : float
        x (east) and y (north) components of velocity at points
        of interest.
    """
    
    return eulerVel(model.eulerlong[plate], model.eulerlat[plate],
                    model.eulerrate[plate], ptlong, ptlat, model.earthradius)

def principleValues(axx, ayy, axy, az=True, deg=True):
    """Principal values and direction of strain, stress, etc.
    
    Function to calculate the principle values of a symmetric 2x2 rank 2
    tensor (e.g. strain, stress in 2D) and the angle of the axis of the
    larger of the principal values.
    
    Parameters
    ----------
    axx, ayy, axy : float
        xx, yy, and xy components of tensor.
    az : boolean, default=True
        Returns angle as an azimuth clockwise from north (y-axis).
        Otherwise, returns angle measured anti-clockwise from the x-axis
        (east).
    deg : boolean, default=True
        Returns angle in degrees. Otherwise returns angle in radians.
    
    Returns
    -------
    amax, amin : float
        Maximum and minimum principal values.
    angle : float
        Angle of the axis of the maximum principal value.
    """
    
    areal = (axx+ayy) * 0.5
    shear1 = (axx-ayy) * 0.5
    shearmag = np.hypot(shear1, axy)
    amax = areal + shearmag
    amin = areal - shearmag
    angle = np.arctan2(axy, shear1) * 0.5
    if az:
        angle = np.pi/2 - angle
    if deg:
        angle = np.degrees(angle)
    return amax, amin, angle

def rotateAxes(axx,ayy,axy,theta,deg=True):
    """Rotate axes of strain, stress, etc.
    
    Function to rotate axes relative to symmetric 2x2 rank 2 tensor (e.g.
    strain, stress, in 2D) by a given angle in an anti-clockwise direction,
    equivalent to rotating the tensor itself by the same angle in a
    clockwise direction.
    
    Parameters
    ----------
    axx, ayy, axy : float
        xx, yy, and xy components of tensor in original coordinates.
    theta : float
        Angle to rotate the coordinates by.
    deg : boolean, default=True
        Angle is in degrees. Otherwise, angle is in radians.
    
    Returns
    -------
    bxx, byy, bxy : float
        xx, yy, and xy components of tensor in rotated coordinates.
    """
    
    if deg:
        theta = np.radians(theta)
        
    bxx = axx*np.cos(theta)**2 + ayy*np.sin(theta)**2 + axy*np.sin(2*theta)
    byy = axx*np.sin(theta)**2 + ayy*np.cos(theta)**2 - axy*np.sin(2*theta)
    bxy = 0.5*(ayy-axx)*np.sin(2*theta) + axy*np.cos(2*theta)
    
    return bxx, byy, bxy

def invertStrainCap(model,capten=False):
    """Inverse of strain rate capacity.
    
    Parameters
    ----------
    model : permdefmap model
        Invert strain rate capacity from this model.
    capten : boolean, default=False
       Also return strain rate capacity as fourth rank tensor.
    
    Returns
    -------
    capinv : array
        Inverse of strain rate capacity as a fourth rank tensor.
    cap : array, optional
        Strain rate capacity as a fourth rank tensor.
    """
    
    # Convert components to matrix
    # Note scaling factors sqrt(2) and 2
    capm = np.zeros((model.nel, 3, 3))
    capm[:,0,0] = (0.5 * (model.straincapc[:model.nel]
                          + model.straincapcc[:model.nel])
                   + 0.125 * (model.straincaps[:model.nel]
                              - model.straincapsc[:model.nel]))
    capm[:,0,1] = -0.125 * (model.straincaps[:model.nel]
                            - model.straincapsc[:model.nel])
    capm[:,0,2] = (-0.25 * model.straincapcs[:model.nel]
                   + 0.125 * model.straincapss[:model.nel]) * np.sqrt(2)
    capm[:,1,0] = capm[:,0,1]
    capm[:,1,1] = (0.5 * (model.straincapc[:model.nel]
                          - model.straincapcc[:model.nel])
                   + 0.125 * (model.straincaps[:model.nel]
                              - model.straincapsc[:model.nel]))
    capm[:,1,2] = (-0.25 * model.straincapcs[:model.nel]
                   - 0.125 * model.straincapss[:model.nel]) * np.sqrt(2)
    capm[:,2,0] = capm[:,0,2]
    capm[:,2,1] = capm[:,1,2]
    capm[:,2,2] = (0.25 * model.straincapc[:model.nel]
                   + 0.125 * (model.straincaps[:model.nel]
                              + model.straincapsc[:model.nel])) * 2
    # Invert matrix representation
    capinvm = np.linalg.inv(capm)
    
    # Convert inverse in matrix representation to 4th rank tensor
    capinv = np.zeros((2, 2, 2, 2, model.maxe))
    capinv[0,0,0,0,:model.nel] = capinvm[:,0,0]
    capinv[0,0,0,1,:model.nel] = capinvm[:,0,2] * np.sqrt(0.5)
    capinv[0,0,1,0,:model.nel] = capinvm[:,0,2] * np.sqrt(0.5)
    capinv[0,0,1,1,:model.nel] = capinvm[:,0,1]
    capinv[0,1,0,0,:model.nel] = capinvm[:,2,0] * np.sqrt(0.5)
    capinv[0,1,0,1,:model.nel] = capinvm[:,2,2] * 0.5
    capinv[0,1,1,0,:model.nel] = capinvm[:,2,2] * 0.5
    capinv[0,1,1,1,:model.nel] = capinvm[:,2,1] * np.sqrt(0.5)
    capinv[1,0,0,0,:model.nel] = capinvm[:,2,0] * np.sqrt(0.5)
    capinv[1,0,0,1,:model.nel] = capinvm[:,2,2] * 0.5
    capinv[1,0,1,0,:model.nel] = capinvm[:,2,2] * 0.5
    capinv[1,0,1,1,:model.nel] = capinvm[:,2,1] * np.sqrt(0.5)
    capinv[1,1,0,0,:model.nel] = capinvm[:,1,0]
    capinv[1,1,0,1,:model.nel] = capinvm[:,1,2] * np.sqrt(0.5)
    capinv[1,1,1,0,:model.nel] = capinvm[:,1,2] * np.sqrt(0.5)
    capinv[1,1,1,1,:model.nel] = capinvm[:,1,1]
    
    if capten:
        # Convert matrix representation to 4th rank tensor if desired
        cap = np.zeros((2,2,2,2,model.maxe))
        cap[0,0,0,0,:model.nel] = capm[:,0,0]
        cap[0,0,0,1,:model.nel] = capm[:,0,2] * np.sqrt(0.5)
        cap[0,0,1,0,:model.nel] = capm[:,0,2] * np.sqrt(0.5)
        cap[0,0,1,1,:model.nel] = capm[:,0,1]
        cap[0,1,0,0,:model.nel] = capm[:,2,0] * np.sqrt(0.5)
        cap[0,1,0,1,:model.nel] = capm[:,2,2] * 0.5
        cap[0,1,1,0,:model.nel] = capm[:,2,2] * 0.5
        cap[0,1,1,1,:model.nel] = capm[:,2,1] * np.sqrt(0.5)
        cap[1,0,0,0,:model.nel] = capm[:,2,0] * np.sqrt(0.5)
        cap[1,0,0,1,:model.nel] = capm[:,2,2] * 0.5
        cap[1,0,1,0,:model.nel] = capm[:,2,2] * 0.5
        cap[1,0,1,1,:model.nel] = capm[:,2,1] * np.sqrt(0.5)
        cap[1,1,0,0,:model.nel] = capm[:,1,0]
        cap[1,1,0,1,:model.nel] = capm[:,1,2] * np.sqrt(0.5)
        cap[1,1,1,0,:model.nel] = capm[:,1,2] * np.sqrt(0.5)
        cap[1,1,1,1,:model.nel] = capm[:,1,1]
        
        return capinv, cap
    
    else:
        return capinv

def strainCapMag(model):
    """Magnitude of strain rate capacities.
    
    Calculates the magnitude of strain rate capacities in each element.
    The magnitude is defined as sqrt(cap[ijkl] * cap[ijkl]), where the
    multiplication is summed over the indices i, j, k, and l (Einstein
    summation convention).
    
    Parameters
    ----------
    model : permdefmap model
        Calculate strain rate capacity magnitudes for this model.
    
    Returns
    -------
    capmag : array of float
        Magnitude of strain rate capacity in each element.
    """
    
    # Calculate magnitude squared first.
    capsq = (0.75*model.straincapc**2 + 0.125*model.straincaps**2
             + 0.5*model.straincapc*model.straincaps
             + 0.5*model.straincapcc**2 + 0.125*model.straincapsc**2
             + 0.5*model.straincapcs**2 + 0.125*model.straincapss**2)
    
    return np.sqrt(capsq)

def slipCapMag(model):
    """Magnitude of slip rate capacities.
    
    Calculates the magnitude of slip rate capacities on each fault segment.
    The magnitude is defines as sqrt(cap[ij] * cap[ij]), where the
    multiplication is summed over the indices i and j (Einstein summation
    convention).
    
    Parameters
    ----------
    model : permdefmap model
        Calculate slip rate capacity magnitude for this model.
    
    Returns
    -------
    capmag : array of float
        Magnitude of slip rate capacity on each fault segment.
    """
    
    # Calculate squared magnitude first.
    capsq = (2*model.slipcapc**2 + model.slipcaps**2
             + 2*model.slipcapc*model.slipcaps)
    
    return np.sqrt(capsq)

def forcesFromPots(model, potxx, potyy, potxy):
    """Find forces from given force potentials.
    
    Calculates forces in elements from given force potentials. The force
    potentials do not have to be the same as the force potentials used in
    the model and may only represent a portion of the force potentials in
    the model.
    
    Parameters
    ----------
    model : permdefmap model
        Calculate forces in elements for this model. Note that the force
        potentials in the model are not assumed to be the force potentials
        for this calculation.
    potxx, potyy, potxy : float or array of float
        Force potentials at grid points in model. These do not have to be
        the same as the force potentials used in the model. A single
        float for one component is interpreted as the uniform value of
        the component at all grid points.
    
    Returns
    -------
    forcex, forcey : array of float
        Forces in elements from the given force potentials.
    """
    
    from .fortran.setup import evalue
    import numpy as np
    
    # Allow for uniform potentials
    if np.size(potxx) == 1:
        potxx = np.ones((model.ngp)) * potxx
    if np.size(potyy) == 1:
        potyy = np.ones((model.ngp)) * potyy
    if np.size(potxy) == 1:
        potxy = np.ones((model.ngp)) * potxy
    
    forcex = np.zeros((model.nel))
    forcey = np.zeros((model.nel))
    
    # Loop through each element
    for e in range(model.nel):
        # Element grid points
        gp1 = model.gp1ofel[e]
        gp2 = model.gp2ofel[e]
        gp3 = model.gp3ofel[e]
        # Calculate geometric parameters of element
        out = evalue(model.earthradius,
            model.earthradius*np.radians(model.gplong[gp1]),
            model.earthradius*np.radians(model.gplat[gp1]),
            model.earthradius*np.radians(model.gplong[gp2]),
            model.earthradius*np.radians(model.gplat[gp2]),
            model.earthradius*np.radians(model.gplong[gp3]),
            model.earthradius*np.radians(model.gplat[gp3]))
        # Load parameters into more useful variables
        area = np.sum(out[0])
        dxsum = np.array(out[6:9])
        nysum = np.array(out[9:12])
        dy = np.array(out[12:15])
        potxxe = potxx[[gp1,gp2,gp3]]
        potyye = potyy[[gp1,gp2,gp3]]
        potxye = potxy[[gp1,gp2,gp3]]
        # Calculate derivatives
        dpotxxdx = np.sum(dxsum*potxxe) / area
        dpotxydx = np.sum(dxsum*potxye) / area
        dpotxydy = 2.0 * np.sum(nysum*potxye) / area - np.sum(dy*potxye)
        dpotyydy = np.sum(nysum*(potyye-potxxe)) / area + np.sum(dy*potxxe)
        # Calculate forces
        forcex[e] = - (dpotxxdx + dpotxydy)
        forcey[e] = - (dpotxydx + dpotyydy)
    
    return forcex, forcey
#    return dpotxxdx, dpotxydx, dpotxydy, dpotyydy

def findPointFromSides(model, side1, side2, oneindex=True):
    """Find point common to two sides.
    
    This function takes two sides that share a common point in a model and
    finds that point. The function will print the longitude and latitude of
    the point and return the index of the point. If the two sides do not
    share a common point, the function will print a message and return -1.
     
    The function can take the side indices and return the point index in
    either counting from zero (Python index) or counting from one (Fortran
    indexing that error messages from setup use). The latter option allows
    this function to be used to help identify and locate problem points
    when setting up the model geometry.
    
    Parameters
    ----------
    model : permdefmap model
        Model that includes the sides.
    side1, side2 : int
        Index of sides that share a common point.
    oneindex : boolean, default=True
        Whether indices are counted from one (True) or from zero (False).
        The option to count from one is included to align with the error
        messages from setup.
    
    Returns
    -------
    gp : int
        Index of gridpoint common to both sides. Returns -1 if not found.
    """
    
    if oneindex:
        side1 -= 1
        side2 -= 1
    
    if (model.gp1onside[side1] == model.gp1onside[side2] 
            or model.gp1onside[side1] == model.gp2onside[side2]):
        gp = model.gp1onside[side1]
        print('Point: '+str(model.long[gp])+' '+str(model.lat[gp]))
        if oneindex:
            gp += 1
        return gp
    elif (model.gp2onside[side1] == model.gp1onside[side2]
            or model.gp2onside[side1] == model.gp2onside[side2]):
        gp = model.gp2onside[side1]
        print('Point: '+str(model.long[gp])+' '+str(model.lat[gp]))
        if oneindex:
            gp += 1
        return gp
    else:
        print('Sides do not share a common point')
        return -1
    
def pointAverageFromElements(model, data):
    """Calculate values at grid points from average of surrounding elements.
    
    This function calculates values at grid points from the average of
    the values in surrounding elements, weighted by the areas of the
    elements. It does not consider any effects from surrounding fault
    segments.
    
    Parameters
    ----------
    model : permdefmap model
        Model that includes grid points and elements.
    data : array of float
        Data values in elements.
    
    Returns
    -------
    datagp : array of float
        Data values averaged at grid points.
    """
    
    # Data and weight sums at grid points
    datasum = np.zeros((model.ngp))
    wsum = np.zeros((model.ngp))
    
    # Loop through elements
    for e in range(model.nel):
        # Element grid points
        gp1 = model.gp1ofel[e]
        gp2 = model.gp2ofel[e]
        gp3 = model.gp3ofel[e]
        datasum[[gp1, gp2, gp3]] += data[e] * model.elarea[e]
        wsum[[gp1, gp2, gp3]] += model.elarea[e]
    
    return datasum / wsum

def segAverageFromElements(model, data):
    """Calculate values on fault segments from average of adjoining elements.
    
    This function calculates values on fault segments from the average
    of the elements on either side. It is not weighted by the area of
    the elements.
    
    Parameters
    ----------
    model : permdefmap model
        Model that includes fault segments and elements.
    data : array of float
        Data values in elements.
    
    Returns
    -------
    dataseg : array of float
        Data values averaged on fault segments.
    """
    # Initialise segment data
    dataseg = np.zeros_like(model.seglength)
    
    # Loop through fault segments
    for i in range(model.nfault):
        for j in range(model.nfaultseg[i]):
            side = model.sideonfault[j,i]
            el1 = model.el1onside[side]
            el2 = model.el2onside[side]
            dataseg[j,i] = (data[el1]+data[el2]) * 0.5
    
    return dataseg

def subfaultCentres(model):
    """Indices of central segments of subfaults
    
    Function to return the indices of central segments of subfaults. If
    a subfault has an even number of segments, the convention is that the
    latter of the two middle segments is returned. Fault segment length
    is not considered. Subfaults need to be loaded first.
    
    Parameters
    ----------
    model : permdefmap model
        Determine central segments for this model
    
    Returns
    -------
    midseg : array of int
        Array with indices along faults of central segments of subfaults
    fault : array of int
        Array of indices indicating which fault segment is on
    """
    
    midseg = np.zeros(model.nsubf, dtype=np.int)
    fault = np.zeros(model.nsubf, dtype=np.int)
    
    # Loop through subfaults
    for i in range(model.nsubf):
        # Segments on subfault
        segi, faulti = np.where(model.subfofseg==i)
        # Central segment array index
        s = int(len(segi)/2)
        midseg[i] = segi[s]
        fault[i] = faulti[s]
    
    return midseg, fault

def faultObCentres(model):
    """Indices of central segments of slip rate observations
    
    Function to return the indices of central segments of slip rate
    observations on faults where the observation has been repeated for all
    segments on a subfault. Excludes observations on end segments. If a
    subfault has an even number of segments, the convention is that the
    latter of the two middle segments is returned. Fault segment length is
    not considered. Subfaults need to be loaded first.
    
    Parameters
    ----------
    model : perdmefmap model
        Determine central segments for this model
    
    Returns
    -------
    midobi : array of int
        Array with indices of observations that are on central segments
    """
    
    # Check geometry is loaded
    if model.nelatgp[0] == 0:
        model.getPointSegments()
    
    subfofob = obSubfaults(model)       # Subfault of observations
    nobonsubf = np.zeros(model.nsubf, dtype=np.int)    # Number of observations on subfault
    midob = np.zeros(model.maxfo, dtype=np.bool)     # Fault obs on middle segments
    
    for i in range(model.nsubf):
        nobonsubf[i] = np.sum(subfofob==i)
        segs = model.sideofslipob[subfofob==i]
        if nobonsubf[i] == 2:
            s = segs[1]
            gp1 = model.gp1onside[s]
            gp2 = model.gp2onside[s]
            # Check if second segment is end segment
            if model.nsegatgp[gp1]==1 or model.nsegatgp[gp2]==1:
                # Then use first segment instead
                s = segs[0]
                midob[model.sideofslipob==s] = True
            else:
                # Note: segments cannot both be end segments
                # Use second segment as default
                midob[model.sideofslipob==s] = True
        elif nobonsubf[i] == 1:
            s = segs[0]
            gp1 = model.gp1onside[s]
            gp2 = model.gp2onside[s]
            if not (model.nsegatgp[gp1]==1 or model.nsegatgp[gp2]==1):
                # Only use the single segment if it is not an end segment
                midob[model.sideofslipob==s] = True
        else:
            # Longer subfaults, use the middle segment
            # Subfaults with even number of segments use latter of two middle segments
            s = segs[int(len(segs)/2)]
            midob[model.sideofslipob==s] = True
    
    # Convert booleans to indices
    midobi = np.arange(model.maxfo)[midob]
    return midobi

def smoothAtEls(model, data, smooth, weight=None, space=None, newlong=None,
                newlat=None):
    """Smooth quantity at element centres.
    
    This function smooths an input data quantity that is measured at
    element centres. By default, the returned data is also at element
    centres but can alternatively be returned at other points.
    
    Parameters
    ----------
    model : permdefmap model
        Smooth quantities at elements in this model.
    data : array of float
        Quantities to be smoothed.
    smooth : float
        The sigma of the Gaussian filter used for smoothing, in the model's
        length units.
    weight : array of float or str 'area', default=None
        Weight data by this factor. Default of None applies no weight.
        'area' weights data by element area (this assumes the area has been
        loaded into the model).
    space : float, default=None
        Spacing in model length units of the grid used for interpolation.
        Default is the same distance as smooth.
    newlong, newlat : array of float, default=None
        Interpolate smoothed quantities onto these grid points with
        these longitudes and latitudes. If both newlong and newlat are None,
        the smoothed quantities will be calculated and stored at the
        element centres of the input model. If only one of newlong or newlat
        is None, an error will be raised.
        
    Returns
    -------
    smoothed : array of float
        Smoothed quantities at desired points.
    """
    
    from scipy import interpolate, ndimage
    import warnings
    
    # Set spacing=smoothing if not set
    if space is None:
        space = smooth
    
    # Set weight from input
    if weight is None:
        weight = np.ones((model.nel))
    elif isinstance(weight, str) and (weight == 'area' or weight == 'a'):
        weight = model.elarea[:model.nel]
    else:
        weight = weight[:model.nel]
    
    # Convert smoothing and spacing to degrees
    midlat = (np.min(model.ellat[:model.nel])
              + np.max(model.ellat[:model.nel])) / 2
    smoothy = np.degrees(smooth / model.earthradius)
    smoothx = smoothy / np.cos(np.radians(midlat))
    spacey = np.degrees(space / model.earthradius)
    spacex = spacey / np.cos(np.radians(midlat))
    
    # Create grid covering model domain plus a small spare
    longr = np.arange(np.min(model.ellong[:model.nel]) - 2*smoothx,
                      np.max(model.ellong[:model.nel]) + 2*smoothx, spacex)
    latr = np.arange(np.min(model.ellat[:model.nel]) - 2*smoothy,
                     np.max(model.ellat[:model.nel]) + 2*smoothy, spacey)
    glong, glat = np.meshgrid(longr, latr)
    
    # Data multiplied by weight
    dataw = data[:model.nel] * weight
    
    # Grid data and weights
    datawg = interpolate.griddata((model.ellong[:model.nel],model.ellat[:model.nel]),
                                   dataw, (glong,glat), method='linear')
    wg = interpolate.griddata((model.ellong[:model.nel],model.ellat[:model.nel]),
                              weight, (glong,glat), method='linear')
    
    # Replace NaNs with zeros to prevent propogation
    datawg[np.isnan(datawg)] = 0.
    wg[np.isnan(wg)] = 0.
    
    # Smooth weight-data and weights. They must be smoothed with the same
    # method for the next step to be valid.
    dataws = ndimage.gaussian_filter(datawg, smooth/space, mode='nearest',
                                      truncate=3.)
    ws = ndimage.gaussian_filter(wg, smooth/space, mode='nearest', truncate=3.)
    
    # Retrieve regular smoothed velocities
    # Note: this will produce NaN in areas outside of model domain.
    # These are expected so the 0 / 0 warning is caught
    with warnings.catch_warnings():
        warnings.simplefilter('ignore', RuntimeWarning)
        datas = dataws / ws
    
    # Choose output points
    if (newlong is None) or (newlat is None):
        if (newlong is None) and (newlat is None):
            # If no points provided, use model element centres
            points = np.array([model.ellat[:model.nel],
                       model.ellong[:model.nel]]).transpose()
        else:
            # Raise error if only one of long or lat provided
            raise RuntimeError('Only one of longitude and latitude provided. '
                               +'Either both need to be provided or neither.')
    else:
        # If points provided, interpolate onto those points
        points = np.array([newlat, newlong]).transpose()
    
    # Output smoothed data
    smoothed = interpolate.interpn((latr,longr), datas, points)
    return smoothed
    
def getFaultObliquity(model,soln,fault,segs='all',lenWeight=True):
    """
    This function calculates the mean angle between the fault and the slip
     vector for the given segments of the fault.
    
    The function calculates the average angle by summing the sines and cosines
     of the angles of each segment and finding the average from this sum. The 
     averages can optionally be weighted by the length of the segment (default).
    """
    
    # identify which part of the solution is desired
    soln=str(soln).lower()      # convert to lowercase string
    if soln == '0' or soln == 'apriori' or soln == 'apri':
        azdu=model.azdu0
    elif soln == '1' or soln == 'aposteriori' or soln == 'apost':
        azdu=model.azdu1
    elif soln == 'd' or soln == 'difference' or soln == 'diff':
        azdu=model.azdud
    elif soln == 'c' or soln == 'constraint' or soln == 'con':
        azdu=model.azduc
    else:
        print('Solution not recognised.')
    
    # check that solution on fault is loaded
    if azdu[0,0] == 0.:
        readFaults(model,soln)
        if soln == '0' or soln == 'apriori' or soln == 'apri':
            azdu=model.azdu0
        elif soln == '1' or soln == 'aposteriori' or soln == 'apost':
            azdu=model.azdu1
        elif soln == 'd' or soln == 'difference' or soln == 'diff':
            azdu=model.azdud
        elif soln == 'c' or soln == 'constraint' or soln == 'con':
            azdu=model.azduc
    
    if segs=='all':
        segs=range(1,model.nfs[fault-1]+1)
    elif isinstance(segs,int):
        return np.radians(azdu[segs-1,fault-1]-model.azfs[segs-1,fault-1])
    
    s=0.
    c=0.
    if lenWeight:
        for i in segs:
            angle=np.radians(azdu[i-1,fault-1]-model.azfs[i-1,fault-1])
            s+=np.sin(angle)*model.lenfs[i-1,fault-1]
            c+=np.cos(angle)*model.lenfs[i-1,fault-1]
    else:
        for i in segs:
            angle=np.radians(azdu[i-1,fault-1]-model.azfs[i-1,fault-1])
            s+=np.sin(angle)
            c+=np.cos(angle)
    
    return np.degrees(np.arctan2(s,c))

def pointFaultDist(model,angle=False):
    """
    Calculates the distance between points in the model and the nearest point
     on a fault.
     
    This assumes that the distance to the nearest fault is going to be small
     and therefore uses a flat earth approximation to reduce computational time.
    
    If angle is True, then the distance will be returned as an angle in degrees.
    """
    
    # load fault points
    if model.gpf[0,0] == 0:
        model.getFaultPoints()
    
    fgplong=np.zeros((model.maxfs+1,model.maxf))
    fgplat =np.zeros((model.maxfs+1,model.maxf))
    fpoints=np.zeros((model.maxfs+1,model.maxf),dtype=np.bool)
    dist=np.zeros((model.maxgp))
    
    # convert gridpoints to lat/long
    for f in range(model.nf):
        fpoints[:model.nfs[f]+1,f]=True
        for p in range(model.nfs[f]+1):
            fgplong[p,f]=model.long[model.gpf[p,f]-1]
            fgplat[p,f] = model.lat[model.gpf[p,f]-1]
    
    for gp in range(model.ngp):
        ang=np.hypot((model.long[gp]-fgplong[fpoints])*np.cos((np.radians(model.lat[gp]+fgplat[fpoints])*0.5)),model.lat[gp]-fgplat[fpoints])
        dist[gp]=np.min(ang)
        
    if not angle:
        dist=np.radians(dist)*model.re
    
    return dist

    
    
    
    
    
    
    
    
    
    
    
    
    
    
        
        