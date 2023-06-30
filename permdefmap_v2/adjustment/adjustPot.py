#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 24 16:02:54 2020

@author: hamish

adjustPhi.py - a collection of functions to adjust force potentials at points 
in permdefmap models.
"""

import numpy as np

def fromTractionStress(model, prop=1.):
    """Adjusts force potentials using traction and stress differences.
    
    This function adjusts force potentials with the aim of fitting
    observations exactly without consideration of the phyical likelihood
    of the resultant force potentials. The potentials at points on
    faults are adjusted based on the difference of a posteriori minus
    a priori tractions on adjacent fault segments. Potentials at points
    off faults are adjusted based on the difference of a posteriori
    minus a priori stresses in adjacent elements.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the force potentials are being adjusted. The
        difference between a priori and a posteriori results need to
        have been read into the model but this function does not check
        for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied.
    """
    
    # Arrays of output force potential adjustments
    dpotxx = np.zeros((model.ngp))
    dpotyy = np.zeros((model.ngp))
    dpotxy = np.zeros((model.ngp))
    
    # Calculate force potential adjustments in elements
    dpotel = np.array([[model.stressxxd, model.stressxyd],
                       [model.stressxyd, model.stressyyd]])
#    # adjust for change in stress
#    if np.array(dstress).any():
#        dpotel[0,0,:]=2*dpotel[0,0,:]-dstress[0,:]
#        dpotel[1,1,:]=2*dpotel[1,1,:]-dstress[1,:]
#        dpotel[0,1,:]=2*dpotel[0,1,:]-dstress[2,:]
#        dpotel[1,0,:]=2*dpotel[1,0,:]-dstress[2,:]
    # Assign force potentials to gridpoints
    for e in range(model.nel):
        dpotxx[model.gp1ofel[e]] += dpotel[0,0,e]
        dpotxx[model.gp2ofel[e]] += dpotel[0,0,e]
        dpotxx[model.gp3ofel[e]] += dpotel[0,0,e]
        dpotyy[model.gp1ofel[e]] += dpotel[1,1,e]
        dpotyy[model.gp2ofel[e]] += dpotel[1,1,e]
        dpotyy[model.gp3ofel[e]] += dpotel[1,1,e]
        dpotxy[model.gp1ofel[e]] += dpotel[0,1,e]
        dpotxy[model.gp2ofel[e]] += dpotel[0,1,e]
        dpotxy[model.gp3ofel[e]] += dpotel[0,1,e]
    # Account for multiple elements at each point by dividing by num els at pt
    dpotxx /= model.nelatgp[:model.ngp]
    dpotyy /= model.nelatgp[:model.ngp]
    dpotxy /= model.nelatgp[:model.ngp]
    # Identify points on faults
    faults = model.nsegatgp[:model.ngp] > 0.5
    # Reset points on faults
    dpotxx[faults] = 0
    dpotyy[faults] = 0
    dpotxy[faults] = 0
    
    # Calculate force potential adjustments for faults
    dpottn = model.tractd
    dpotnn = model.tracnd
    dpottt = np.zeros_like(dpotnn)
#    if np.array(dtrac).any():
#        dpottn=2*dpottn-dtrac[0,:,:]
#        dpotnn=2*dpotnn-dtrac[1,:,:]
    
    for f in range(model.nfault):
        for s in range(model.nfaultseg[f]):
            # Rotation matrix for fault segment
            rot = np.array([[model.segtx[s,f], model.segty[s,f]],
                            [model.segnx[s,f], model.segny[s,f]]])
            # Calculate tt-component based on elements adjacent to segment
            dpote = (dpotel[:,:,model.el1onside[model.sideonfault[s,f]]]
                     + dpotel[:,:,model.el2onside[model.sideonfault[s,f]]]) * 0.5
            dpottt[s,f] = np.einsum('ij,i,j', dpote, rot[0,:], rot[0,:])
            # Rotate from tn to xy coordinates for segment
            dpotseg = np.einsum('ki,lj,kl', rot, rot,
                                [[dpottt[s,f], dpottn[s,f]],
                                 [dpottn[s,f], dpotnn[s,f]]])
            # Assign force potentials to segments
            dpotxx[model.gponfault[s,f]] += dpotseg[0,0]
            dpotxx[model.gponfault[s+1,f]] += dpotseg[0,0]
            dpotyy[model.gponfault[s,f]] += dpotseg[1,1]
            dpotyy[model.gponfault[s+1,f]] += dpotseg[1,1]
            dpotxy[model.gponfault[s,f]] += dpotseg[1,0]
            dpotxy[model.gponfault[s+1,f]] += dpotseg[1,0]
    # Account for multiple segments at each point
    dpotxx[faults] /= model.nsegatgp[:model.ngp][faults]
    dpotyy[faults] /= model.nsegatgp[:model.ngp][faults]
    dpotxy[faults] /= model.nsegatgp[:model.ngp][faults]
    
    # Add to existing force potentials
    model.potxx[:model.ngp] += dpotxx * prop
    model.potyy[:model.ngp] += dpotyy * prop
    model.potxy[:model.ngp] += dpotxy * prop
    
def fromTractionStressWeighted(model, prop=1., change=False):
    """Adjusts force potentials using weighted traction and stress differences.
    
    This function adjusts force potentials with the aim of fitting
    observations exactly without consideration of the phyical likelihood
    of the resultant force potentials. The potentials at points on
    faults are adjusted based on the difference of a posteriori minus
    a priori tractions on adjacent fault segments, weighted by the slip
    rate capacity and length of each segment. Potentials at points off
    faults are adjusted based on the difference of a posteriori minus
    a priori stresses in adjacent elements, weighted by the strain rate
    capacity and area of the elements.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the force potentials are being adjusted. The
        difference between a priori and a posteriori results need to
        have been read into the model but this function does not check
        for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied.
    change : bool, default=False
        If True, returns the change to force potentials.
    
    Returns
    -------
    change : array of float, optional
        Change to force potentials as an array of xx, yy, and xy components.
    """
    
    from ..geometry import strainCapMag, slipCapMag
    
    # Arrays of output force potential adjustments
    dpotxx = np.zeros((model.ngp))
    dpotyy = np.zeros((model.ngp))
    dpotxy = np.zeros((model.ngp))
    # Array of weight sum (area * cap)
    areacap = np.zeros((model.ngp))
    
    # Calculate force potential adjustments in elements
    dpotel = np.array([[model.stressxxd, model.stressxyd],
                       [model.stressxyd, model.stressyyd]])
#    # adjust for change in stress
#    if np.array(dstress).any():
#        dpotel[0,0,:]=2*dpotel[0,0,:]-dstress[0,:]
#        dpotel[1,1,:]=2*dpotel[1,1,:]-dstress[1,:]
#        dpotel[0,1,:]=2*dpotel[0,1,:]-dstress[2,:]
#        dpotel[1,0,:]=2*dpotel[1,0,:]-dstress[2,:]
    strainmag = strainCapMag(model)
    # Assign force potentials to gridpoints
    for e in range(model.nel):
        # Weighting contribution
        weight = model.elarea[e] * strainmag[e]
        areacap[model.gp1ofel[e]] += weight
        areacap[model.gp2ofel[e]] += weight
        areacap[model.gp3ofel[e]] += weight
        # Weighted force potentials
        dpotxx[model.gp1ofel[e]] += dpotel[0,0,e] * weight
        dpotxx[model.gp2ofel[e]] += dpotel[0,0,e] * weight
        dpotxx[model.gp3ofel[e]] += dpotel[0,0,e] * weight
        dpotyy[model.gp1ofel[e]] += dpotel[1,1,e] * weight
        dpotyy[model.gp2ofel[e]] += dpotel[1,1,e] * weight
        dpotyy[model.gp3ofel[e]] += dpotel[1,1,e] * weight
        dpotxy[model.gp1ofel[e]] += dpotel[0,1,e] * weight
        dpotxy[model.gp2ofel[e]] += dpotel[0,1,e] * weight
        dpotxy[model.gp3ofel[e]] += dpotel[0,1,e] * weight
    # Divide weighted sum by sum of weights to get average
    dpotxx /= areacap
    dpotyy /= areacap
    dpotxy /= areacap
    # Identify points on faults
    faults = model.nsegatgp[:model.ngp] > 0.5
    # Reset points on faults
    dpotxx[faults] = 0
    dpotyy[faults] = 0
    dpotxy[faults] = 0
    
    # Calculate force potential adjustments for faults
    dpottn = model.tractd
    dpotnn = model.tracnd
    dpottt = np.zeros_like(dpotnn)
    # Array of weight sum (length * cap)
    segcap = np.zeros((model.ngp))
#    if np.array(dtrac).any():
#        dpottn=2*dpottn-dtrac[0,:,:]
#        dpotnn=2*dpotnn-dtrac[1,:,:]
    
    slipmag = slipCapMag(model)
    for f in range(model.nfault):
        for s in range(model.nfaultseg[f]):
            # Rotation matrix for fault segment
            rot = np.array([[model.segtx[s,f], model.segty[s,f]],
                            [model.segnx[s,f], model.segny[s,f]]])
            # Calculate tt-component based on elements adjacent to segment
            dpote = (dpotel[:,:,model.el1onside[model.sideonfault[s,f]]]
                     + dpotel[:,:,model.el2onside[model.sideonfault[s,f]]]) * 0.5
            dpottt[s,f] = np.einsum('ij,i,j', dpote, rot[0,:], rot[0,:])
            # Rotate from tn to xy coordinates for segment
            dpotseg = np.einsum('ki,lj,kl', rot, rot,
                                [[dpottt[s,f], dpottn[s,f]],
                                 [dpottn[s,f], dpotnn[s,f]]])
            # Contribution to weight sum
            weight = model.seglength[s,f] * slipmag[s,f]
            segcap[model.gponfault[s,f]] += weight
            segcap[model.gponfault[s+1,f]] += weight
            # Assign force potentials to segments
            dpotxx[model.gponfault[s,f]] += dpotseg[0,0] * weight
            dpotxx[model.gponfault[s+1,f]] += dpotseg[0,0] * weight
            dpotyy[model.gponfault[s,f]] += dpotseg[1,1] * weight
            dpotyy[model.gponfault[s+1,f]] += dpotseg[1,1] * weight
            dpotxy[model.gponfault[s,f]] += dpotseg[1,0] * weight
            dpotxy[model.gponfault[s+1,f]] += dpotseg[1,0] * weight
    # Account for multiple segments at each point
    dpotxx[faults] /= segcap[faults]
    dpotyy[faults] /= segcap[faults]
    dpotxy[faults] /= segcap[faults]
    
    # Add to existing force potentials
    model.potxx[:model.ngp] += dpotxx * prop
    model.potyy[:model.ngp] += dpotyy * prop
    model.potxy[:model.ngp] += dpotxy * prop
    
    if change:
        return np.array([dpotxx, dpotyy, dpotxy]) * prop
    
def fromTractionOnlyWeighted(model, prop=1., change=False):
    """Adjusts force potentials using weighted traction differences only.
    
    This function adjusts force potentials with the aim of fitting
    observations exactly without consideration of the phyical likelihood
    of the resultant force potentials. The potentials at points on
    faults are adjusted based on the difference of a posteriori minus
    a priori tractions on adjacent fault segments, weighted by the slip
    rate capacity and length of each segment. Potentials at points off
    faults are not adjusted.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the force potentials are being adjusted. The
        difference between a priori and a posteriori results need to
        have been read into the model but this function does not check
        for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied.
    change : bool, default=False
        If True, returns the change to force potentials.
    
    Returns
    -------
    change : array of float, optional
        Change to force potentials as an array of xx, yy, and xy components.
    """
    
    from ..geometry import slipCapMag
    
    # Arrays of output force potential adjustments
    dpotxx = np.zeros((model.ngp))
    dpotyy = np.zeros((model.ngp))
    dpotxy = np.zeros((model.ngp))
    
    # Calculate force potential adjustments in elements
    dpotel = np.array([[model.stressxxd, model.stressxyd],
                       [model.stressxyd, model.stressyyd]])
    
    # Identify points on faults
    faults = model.nsegatgp[:model.ngp] > 0.5
    
    # Calculate force potential adjustments for faults
    dpottn = model.tractd
    dpotnn = model.tracnd
    dpottt = np.zeros_like(dpotnn)
    # Array of weight sum (length * cap)
    segcap = np.zeros((model.ngp))
#    if np.array(dtrac).any():
#        dpottn=2*dpottn-dtrac[0,:,:]
#        dpotnn=2*dpotnn-dtrac[1,:,:]
    
    slipmag = slipCapMag(model)
    for f in range(model.nfault):
        for s in range(model.nfaultseg[f]):
            # Rotation matrix for fault segment
            rot = np.array([[model.segtx[s,f], model.segty[s,f]],
                            [model.segnx[s,f], model.segny[s,f]]])
            # Calculate tt-component based on elements adjacent to segment
            dpote = (dpotel[:,:,model.el1onside[model.sideonfault[s,f]]]
                     + dpotel[:,:,model.el2onside[model.sideonfault[s,f]]]) * 0.5
            dpottt[s,f] = np.einsum('ij,i,j', dpote, rot[0,:], rot[0,:])
            # Rotate from tn to xy coordinates for segment
            dpotseg = np.einsum('ki,lj,kl', rot, rot,
                                [[dpottt[s,f], dpottn[s,f]],
                                 [dpottn[s,f], dpotnn[s,f]]])
            # Contribution to weight sum
            weight = model.seglength[s,f] * slipmag[s,f]
            segcap[model.gponfault[s,f]] += weight
            segcap[model.gponfault[s+1,f]] += weight
            # Assign force potentials to segments
            dpotxx[model.gponfault[s,f]] += dpotseg[0,0] * weight
            dpotxx[model.gponfault[s+1,f]] += dpotseg[0,0] * weight
            dpotyy[model.gponfault[s,f]] += dpotseg[1,1] * weight
            dpotyy[model.gponfault[s+1,f]] += dpotseg[1,1] * weight
            dpotxy[model.gponfault[s,f]] += dpotseg[1,0] * weight
            dpotxy[model.gponfault[s+1,f]] += dpotseg[1,0] * weight
    # Account for multiple segments at each point
    dpotxx[faults] /= segcap[faults]
    dpotyy[faults] /= segcap[faults]
    dpotxy[faults] /= segcap[faults]
    
    # Add to existing force potentials
    model.potxx[:model.ngp] += dpotxx * prop
    model.potyy[:model.ngp] += dpotyy * prop
    model.potxy[:model.ngp] += dpotxy * prop
    
    if change:
        return np.array([dpotxx, dpotyy, dpotxy]) * prop
    
def fromStressOnlyWeighted(model, prop=1., change=False):
    """Adjusts force potentials using weighted stress differences.
    
    This function adjusts force potentials with the aim of fitting
    observations exactly without consideration of the phyical likelihood
    of the resultant force potentials. Potentials are adjusted based on
    the difference of a posteriori minus a priori stresses in adjacent
    elements, weighted by the strain rate capacity and area of the
    elements. This function does not consider faults.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the force potentials are being adjusted. The
        difference between a priori and a posteriori results need to
        have been read into the model but this function does not check
        for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment that should be applied.
    change : bool, default=False
        If True, returns the change to force potentials.
    
    Returns
    -------
    change : array of float, optional
        Change to force potentials as an array of xx, yy, and xy components.
    """
    
    from ..geometry import strainCapMag
    
    # Arrays of output force potential adjustments
    dpotxx = np.zeros((model.ngp))
    dpotyy = np.zeros((model.ngp))
    dpotxy = np.zeros((model.ngp))
    # Array of weight sum (area * cap)
    areacap = np.zeros((model.ngp))
    
    # Calculate force potential adjustments in elements
    dpotel = np.array([[model.stressxxd, model.stressxyd],
                       [model.stressxyd, model.stressyyd]])
#    # adjust for change in stress
#    if np.array(dstress).any():
#        dpotel[0,0,:]=2*dpotel[0,0,:]-dstress[0,:]
#        dpotel[1,1,:]=2*dpotel[1,1,:]-dstress[1,:]
#        dpotel[0,1,:]=2*dpotel[0,1,:]-dstress[2,:]
#        dpotel[1,0,:]=2*dpotel[1,0,:]-dstress[2,:]
    strainmag = strainCapMag(model)
    # Assign force potentials to gridpoints
    for e in range(model.nel):
        # Weighting contribution
        weight = model.elarea[e] * strainmag[e]
        areacap[model.gp1ofel[e]] += weight
        areacap[model.gp2ofel[e]] += weight
        areacap[model.gp3ofel[e]] += weight
        # Weighted force potentials
        dpotxx[model.gp1ofel[e]] += dpotel[0,0,e] * weight
        dpotxx[model.gp2ofel[e]] += dpotel[0,0,e] * weight
        dpotxx[model.gp3ofel[e]] += dpotel[0,0,e] * weight
        dpotyy[model.gp1ofel[e]] += dpotel[1,1,e] * weight
        dpotyy[model.gp2ofel[e]] += dpotel[1,1,e] * weight
        dpotyy[model.gp3ofel[e]] += dpotel[1,1,e] * weight
        dpotxy[model.gp1ofel[e]] += dpotel[0,1,e] * weight
        dpotxy[model.gp2ofel[e]] += dpotel[0,1,e] * weight
        dpotxy[model.gp3ofel[e]] += dpotel[0,1,e] * weight
    # Divide weighted sum by sum of weights to get average
    dpotxx /= areacap
    dpotyy /= areacap
    dpotxy /= areacap
    
    # Add to existing force potentials
    model.potxx[:model.ngp] += dpotxx * prop
    model.potyy[:model.ngp] += dpotyy * prop
    model.potxy[:model.ngp] += dpotxy * prop
    
    if change:
        return np.array([dpotxx, dpotyy, dpotxy]) * prop
    
def smoothPot(model, smooth, weight=None, space=None, gplong=None, gplat=None,
              noRigid=False, pots=None):
    """Smooth force potentials.
    
    Parameters
    ----------
    model : permdefmap model
        Smooth force potentials in this model.
    smooth : float
        The sigma of the Gaussian filter used for smoothing, in the model's
        length units. Smooth=0 will perform no smoothing and only perform
        interpolation.
    weight : array of float, default=None
        Potentials are weighted by this factor. Default of None applies no
        weight.
    space : float, default=None
        Spacing in model length units of the grid used for interpolation.
        Default is the same distance as smooth.
    gplong, gplat : array of float, default=None
        Interpolate smoothed force potentials onto these grid points with
        these longitudes and latitudes. If both gplong and gplat are None,
        the smoothed force potentials will be calculated and stored at the
        grid points of the input model. If only one of gplong or gplat is
        None, an error will be raised.
    noRigid : bool, default=False
        Before smoothing, remove force potentials on grid points on a
        rigid boundary.
    pots : array of float, default=None
        Array of [xx, yy, xy] component arrays of force potentials grid
        points. Default uses force potentials from the model.
    
    Returns
    -------
    potxx, potyy, potxy : array of float, optional
        The xx, yy, and xy components of smoothed force potentials at grid
        points given by gplong and gplat. Not returned if gplong and gplat
        are None.
    """
    
    from scipy import interpolate, ndimage
    import warnings
    
    # Set spacing=smoothing if not set
    if space is None:
        space = smooth
    
    # Set weight from input
    if weight is None:
        weight = np.ones((model.ngp))
    else:
        weight = weight[:model.ngp]
    
    # Set weight of points on rigid boundary if desired
    if noRigid:
        onrigidb = model.nrigidbatgp[:model.ngp] > 0
        weight[onrigidb] = 0
    
    # Choose force potentials to use
    if pots is None:
        pxx = model.potxx[:model.ngp]
        pyy = model.potyy[:model.ngp]
        pxy = model.potxy[:model.ngp]
    else:
        pxx, pyy, pxy = pots
    
    # Convert smoothing and spacing to degrees
    midlat = (np.min(model.gplat[:model.ngp])
              + np.max(model.gplat[:model.ngp])) / 2
    smoothy = np.degrees(smooth / model.earthradius)
    smoothx = smoothy / np.cos(np.radians(midlat))
    spacey = np.degrees(space / model.earthradius)
    spacex = spacey / np.cos(np.radians(midlat))
    
    # Create grid covering model domain plus a small spare
    longr = np.arange(np.min(model.gplong[:model.ngp]) - 2*smoothx - spacex,
                      np.max(model.gplong[:model.ngp]) + 2*smoothx + spacex, spacex)
    latr = np.arange(np.min(model.gplat[:model.ngp]) - 2*smoothy - spacey,
                     np.max(model.gplat[:model.ngp]) + 2*smoothy + spacey, spacey)
    glong, glat = np.meshgrid(longr, latr)
    
    # Potentials multiplied by weights
    potxxw = model.potxx[:model.ngp] * weight[:model.ngp]
    potyyw = model.potyy[:model.ngp] * weight[:model.ngp]
    potxyw = model.potxy[:model.ngp] * weight[:model.ngp]
    
    # Grid potentials and weights
    potxxwg = interpolate.griddata((model.gplong[:model.ngp],model.gplat[:model.ngp]),
                                   potxxw, (glong,glat), method='linear')
    potyywg = interpolate.griddata((model.gplong[:model.ngp],model.gplat[:model.ngp]),
                                   potyyw, (glong,glat), method='linear')
    potxywg = interpolate.griddata((model.gplong[:model.ngp],model.gplat[:model.ngp]),
                                   potxyw, (glong,glat), method='linear')
    wg = interpolate.griddata((model.gplong[:model.ngp],model.gplat[:model.ngp]),
                              weight[:model.ngp], (glong,glat), method='linear')
    
    if smooth == 0:
        # If no smoothing is required...
        potxxs = potxxwg / wg
        potyys = potyywg / wg
        potxys = potxywg / wg
        if (gplong is None) and (gplat is None):
            warnings.warn('No smoothing or new interpolation points specified. '
                          + 'Beware that input force potentials will not be '
                          + 'returned exactly.')
    else:
        # Otherwise, smooth.
        # Replace NaNs with zeros to prevent propogation
        potxxwg[np.isnan(potxxwg)] = 0.
        potyywg[np.isnan(potyywg)] = 0.
        potxywg[np.isnan(potxywg)] = 0.
        wg[np.isnan(wg)] = 0.
        
        # Smooth weight-potentials and weights. They must be smoothed with the same
        # method for the next step to be valid.
        potxxws = ndimage.gaussian_filter(potxxwg, smooth/space, mode='nearest',
                                          truncate=3.)
        potyyws = ndimage.gaussian_filter(potyywg, smooth/space, mode='nearest',
                                          truncate=3.)
        potxyws = ndimage.gaussian_filter(potxywg, smooth/space, mode='nearest',
                                          truncate=3.)
        ws = ndimage.gaussian_filter(wg, smooth/space, mode='nearest', truncate=3.)
        
        # Retrieve regular smoothed velocities
        # Note: this will produce NaN in areas outside of model domain.
        # These are expected so the 0 / 0 warning is caught
        with warnings.catch_warnings():
            warnings.simplefilter('ignore', RuntimeWarning)
            potxxs = potxxws / ws
            potyys = potyyws / ws
            potxys = potxyws / ws
    
    
    # Store smoothed force potentials
    if (gplong is None) or (gplat is None):
        if (gplong is None) and (gplat is None):
            # If no grid points provided, store in input model
            points = np.array([model.gplat[:model.ngp],
                       model.gplong[:model.ngp]]).transpose()
            model.potxx[:model.ngp] = interpolate.interpn((latr,longr), potxxs, points)
            model.potyy[:model.ngp] = interpolate.interpn((latr,longr), potyys, points)
            model.potxy[:model.ngp] = interpolate.interpn((latr,longr), potxys, points)
            
            return
        
        else:
            # Raise error if only one of long or lat provided
            raise RuntimeError('Only one of longitude and latitude provided. '
                               +'Either both need to be provided or neither.')
    else:
        # If grid points provided, interpolate onto those grid points
        points = np.array([gplat, gplong]).transpose()
        potxx = interpolate.interpn((latr,longr), potxxs, points)
        potyy = interpolate.interpn((latr,longr), potyys, points)
        potxy = interpolate.interpn((latr,longr), potxys, points)
        
        return potxx, potyy, potxy

























