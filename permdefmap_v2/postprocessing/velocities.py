#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 19 15:24:59 2021

velocities.py - collection of functions for analysing the velocity results

@author: Hamish Hirschberg
"""

def velocityDiffPoints(model, points, soln, rotate=False, covar=None):
    """Difference in velocity between two points.
    
    Calculates the difference in velocity between two points either in
    terms of xy-coordinates or rotated to transverse/normal relative to the
    vector between the two points. Optionally outputs the estimated
    variance of the difference. Velocity difference is based on the
    velocities at the centre of the elements containing the specified
    points.
    
    Parameters
    ----------
    model : permdefmap model
        Calculate velocities from this model.
    points : list of float
        Coordinates of points given as [long0, lat0, long1, lat1].
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
    rotate : bool, default=False
        Rotate the velocities to transverse/normal relative to vector from
        point 0 to point 1.
    covar : numpy array, optional
        Covariance matrix for velocities, used to calculate variance of
        result. If covar=None, no variance is calculated for the result.
    
    Returns
    -------
    vel : list with [float, float]
        Difference in velocity between the two points. Order is
        x-component, y-component if rotate=False or transverse, normal
        components if rotate=True.
    var : list with [float, float, float], optional
        Variance in components of velocity difference. Order of first two
        items in list matches the order of components of `vel`. The last
        item is the covariance of the two components.
    """
    
    import numpy as np
    
    # choose solution and load velocities
    if str(soln)=='0' or soln=='pre' or soln=='apriori':
        velx = model.velinelx0
        vely = model.velinely0
    elif str(soln)=='1' or soln=='post' or soln=='aposteriori':
        velx = model.velinelx1
        vely = model.velinely1
    elif str(soln)=='c' or soln=='con' or soln=='constraint':
        velx = model.velinelxc
        vely = model.velinelyc
    elif str(soln)=='d' or soln=='diff' or soln=='difference':
        velx = model.velinelxd
        vely = model.velinelyd
    else:
        raise Exception('Solution '+str(soln)+' not recognised.')
    
    el1, flag = model.finde(points[0], points[1], False)
    if flag == 2:
        raise Exception('Unable to find point ('+str(points[0])+','
                        +str(points[1])+') in mesh.')
    el2, flag = model.finde(points[2], points[3], False, el=el1)
    if flag == 2:
        raise Exception('Unable to find point ('+str(points[2])+','
                        +str(points[3])+') in mesh.')
    
    uxdiff = velx[el2] - velx[el1]
    uydiff = vely[el2] - vely[el1]
    
    if not covar is None:
        nel = int(np.size(covar, axis=0) / 2)
        # Calculate variance of velocity difference
        varx = covar[el1,el1] + covar[el2,el2] - 2*covar[el1,el2]
        vary = (covar[nel+el1,nel+el1] + covar[nel+el2,nel+el2]
                - 2*covar[nel+el1,nel+el2])
        covarxy = (covar[el1,nel+el1] + covar[el2,nel+el2]
                   - covar[el1,nel+el2] - covar[el2,nel+el1])
        
        if not rotate:
            # Return velocities with variances
            return [uxdiff, uydiff], [varx, vary, covarxy]
    
    if rotate:
        # Rotate velocities to along profile
        # Calculate direction vector
        dy = points[3] - points[1]
        dx = ((points[2] - points[0])
              * np.cos(np.radians((points[1]+points[3]) * 0.5)))
        dl = np.hypot(dx, dy)
        tx = dx / dl        # = +ny
        ty = dy / dl        # = -nx
        utdiff = uxdiff*tx + uydiff*ty
        undiff = -uxdiff*ty + uydiff*tx
        
        if not covar is None:
            # Calculate rotated variances
            vart = varx*tx**2 + vary*ty**2 + 2*covarxy*tx*ty
            varn = varx*ty**2 + vary*tx**2 - 2*covarxy*ty*tx
            covartn = tx*ty*(vary-varx) + (tx**2-ty**2)*covarxy
            
            return [utdiff, undiff], [vart, varn, covartn]
        else:
            return [utdiff, undiff]
    else:
        return [uxdiff, uydiff]

def smoothVels(model, soln, newlong, newlat, smooth, weight=None, space=None,
               fname=None, strain=False, plate=None):
    """Smooth modelled velocity field.
    
    This function smooths the modelled velocity field and outputs the
    velocities or strain rates at arbitrary points. The velocities are
    interpolated onto a grid in xy-space and smoothed using a Gaussian
    filter.
    
    Parameters
    ----------
    model : permdefmap model
        Smooth the velocity field produced by this model.
    soln : string
        Part of the solution to be read in. The solution can be specified
        as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
    newlong, newlat : array of float
        Longitude and latitude of points to interpolate the velocities onto.
    smooth : float
        The sigma of the Gaussian filter used for smoothing in model length
        units. smooth=0 will return the velocities without smoothing but
        requires setting space. For computational simplicity, the
        longitudinal spacing is calculated for the middle latitude of the
        model and no correction is made in the smoothing for lengths of
        parallels varying with latitude (the correction is still made in
        strain rate calculations).
    weight : array of float, default=None
        Weight of velocities in the interpolation, such as the inverse of
        the standard error. Default of None does not apply a weight.
    space : float, default=None
        Spacing in model length units of the grid used for interpolation.
        Default is the same distance as smooth. If smooth is not set,
        space must be set.
    fname : str, default=None
        Print velocities (or strain rates if strain=True) to file with this
        name. If fname is not set, the velocities are returned as a
        variable instead.
    strain : bool, default=False
        Return strain rate (True) instead of velocities (False).
    plate : int, default=None
        Calculate velocities relative to this plate. Default uses the same
        reference frame as the model.
    
    Returns
    -------
    velout : array of float, optional
        Smoothed, interpolated velocities. If strain=True, strain rates are
        returned instead. Only returned if fname=None.
    """
    import numpy as np
    from scipy import interpolate, ndimage
    from .readResults import readElements
    from ..geometry import eulerVel
    import warnings
    
    # Identify which solution(s)/part of solution is desired
    soln=str(soln).lower()      # Convert to lowercase string
    if soln == '0' or soln == 'apriori' or soln == 'apri':
        ux = model.velinelx0[:model.nel]
        uy = model.velinely0[:model.nel]
    elif soln == '1' or soln == 'aposteriori' or soln == 'apost':
        ux = model.velinelx1[:model.nel]
        uy = model.velinely1[:model.nel]
    elif soln == 'd' or soln == 'difference' or soln == 'diff':
        ux = model.velinelxd[:model.nel]
        uy = model.velinelyd[:model.nel]
    elif soln == 'c' or soln == 'constraint' or soln == 'con':
        ux = model.velinelxc[:model.nel]
        uy = model.velinelyc[:model.nel]
    elif soln == 'a' or soln == 'all' or soln == 'b' or soln == 'both':
        raise Exception('Multiple parts of solution are not implemented for '
                        + 'this function. Please run separately.')
    else:
        raise Exception('Part of solution '+soln+' not recognised.')
    
    # Check that velocities are loaded (assumes the first point isn't going to
    # be exactly zero)
    if ux[0] == 0.:
        readElements(model,soln)
        if soln == '0' or soln == 'apriori' or soln == 'apri':
            ux = model.velinelx0[:model.nel]
            uy = model.velinely0[:model.nel]
        elif soln == '1' or soln == 'aposteriori' or soln == 'apost':
            ux = model.velinelx1[:model.nel]
            uy = model.velinely1[:model.nel]
        elif soln == 'd' or soln == 'difference' or soln == 'diff':
            ux = model.velinelxd[:model.nel]
            uy = model.velinelyd[:model.nel]
        elif soln == 'c' or soln == 'constraint' or soln == 'con':
            ux = model.velinelxc[:model.nel]
            uy = model.velinelyc[:model.nel]
    
    # Rotate velocities to given plate if desired
    if plate is not None:
        euler = eulerVel(model.eulerlong[plate], model.eulerlat[plate],
                         model.eulerrate[plate], model.ellong[:model.nel],
                         model.ellat[:model.nel], model.earthradius)
        ux = ux - euler[0]
        uy = uy - euler[1]
    
    # Set spacing=smoothing if not set
    if space is None:
        space = smooth
    
    if weight is None:
        weight = np.ones((model.nel))
        
    # Convert smoothing and spacing to degrees
    midlat = (np.min(model.gplat[:model.ngp])
              + np.max(model.gplat[:model.ngp])) / 2
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
    
    # Velocity multiplied by weight
    uxw = ux * weight[:model.nel]
    uyw = uy * weight[:model.nel]
    # Grid velocities and weights
    uxwg = interpolate.griddata((model.ellong[:model.nel],model.ellat[:model.nel]),
                                uxw, (glong,glat), method='linear')
    uywg = interpolate.griddata((model.ellong[:model.nel],model.ellat[:model.nel]),
                                uyw, (glong,glat), method='linear')
    wg = interpolate.griddata((model.ellong[:model.nel],model.ellat[:model.nel]),
                              weight[:model.nel], (glong,glat), method='linear')
    
    # No smoothing if smooth == 0
    if smooth == 0:
        uxs = uxwg / wg
        uys = uywg / wg
    else:
        # Replace NaN with zeros to prevent propagation
        uxwg[np.isnan(uxwg)]=0.
        uywg[np.isnan(uywg)]=0.
        wg[np.isnan(wg)]=0.
        
        # Smooth weight-velocities and weights using gaussian
        # They must be smoothed using same method for next step to be valid
        uxws = ndimage.gaussian_filter(uxwg, smooth/space, mode='nearest',
                                       truncate=3.)
        uyws = ndimage.gaussian_filter(uywg, smooth/space, mode='nearest',
                                       truncate=3.)
        ws = ndimage.gaussian_filter(wg, smooth/space, mode='nearest',
                                     truncate=3.)
        # Retrieve regular smoothed velocities
        # Note: this will produce NaN in areas outside of model domain
        # These are expected so the 0 / 0 warning is caught
        with warnings.catch_warnings():
            warnings.simplefilter('ignore', RuntimeWarning)
            uxs = uxws / ws
            uys = uyws / ws
    
    points = np.array([newlat,newlong]).transpose()
    
    # Retrieve strain rates or velocities
    if strain:
        # Calculate the gradients
        uxsdy, uxsdx = np.gradient(uxs, np.radians(spacey), np.radians(spacex))
        uysdy, uysdx = np.gradient(uys, np.radians(spacey), np.radians(spacex))
        # Latitudes in radians
        latrad = np.radians(latr[:,np.newaxis])
        # Convert to strain rates in spherical coordinates
        exx = (uxsdx - uys*np.sin(latrad)) / (np.cos(latrad)*model.earthradius)
        eyy = uysdy / model.earthradius
        exy = ((uysdx+uxs*np.sin(latrad)) / (2*np.cos(latrad)*model.earthradius)
                + uxsdy / (2*model.earthradius))
        
        strain = np.zeros((3,len(points)))
        strain[0,:] = interpolate.interpn((latr,longr), exx, points)
        strain[1,:] = interpolate.interpn((latr,longr), eyy, points)
        strain[2,:] = interpolate.interpn((latr,longr), exy, points)
        
        if fname:
            outf = open(fname, 'w')
            for i in range(len(newlong)):
                outf.write(str(newlong[i])+'\t'+str(newlat[i])+'\t'
                           +str(strain[0,i])+'\t'+str(strain[1,i])+'\t'
                           +str(strain[2,i])+'\n')
            outf.close()
        else:
            return strain
    else:
        # Interpolate onto points
        velout = np.zeros((2,len(points)))
        velout[0,:] = interpolate.interpn((latr,longr), uxs, points)
        velout[1,:] = interpolate.interpn((latr,longr), uys, points)
    
        if fname:
            outf = open(fname, 'w')
            for i in range(len(newlong)):
                outf.write(str(newlong[i])+'\t'+str(newlat[i])+'\t'
                           +str(velout[0,i])+'\t'+str(velout[1,i])+'\n')
            outf.close()
        else:
            return velout











