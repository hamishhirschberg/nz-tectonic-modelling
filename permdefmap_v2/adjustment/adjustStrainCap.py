#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 15:52:22 2019

This is the Python version of the Fortran code 'adjust_L_in_setup_input.f' that
adjusts the strain-rate capacity in elements based on the relative magnitudes
of the strain rates in the aposteriori and apriori solutions.

This function takes a permdefmap model as an argument. Optionally, it can also
take a minimum relative value of strain rate, emin, which defaults to 1e-12.

@author: hamish
"""

import numpy as np

def byElement(model, prop=1., dstress=0., strainmin=1e-12):
    """Adjusts strain-rate capacity on an element-by-element basis.
    
    This function adjusts the strain-rate capacity on an element-by-
    element basis based on the relative values of a posteriori and
    a priori strain rate. The adjustment is approximately the a posteriori
    strain rate divided by the a priori strain rate.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    dstress : float, default=0.
        The estimated change in stress from the adjustment. This allows
        the adjustment to more accurately calculated so that the new
        a priori solution will more closely match the old a posteriori
        solution.
    strainmin : float, default=1e-12
        Minimum relative strain rate to avoid numerical problems
        associated with dividing by a small number.
    """
    # A priori for each element
    # Magnitude of stress * strain
    mag0 = (model.stressxx0*model.strainxx0 + model.stressyy0*model.strainyy0
            + 2*model.stressxy0*model.strainxy0)[:model.nel]
    if np.array(dstress).any():
        mag0 += (dstress[0,:model.nel]*model.strainxx0[:model.nel]
                 + dstress[1,:model.nel]*model.strainyy0[:model.nel]
                 + 2*dstress[2,:model.nel]*model.strainxy0[:model.nel])
    # Sum of element areas
    atot = np.sum(model.elarea[:model.nel])
    # Total magnitude, weighted by element area
    magtot0 = np.sum(model.elarea[:model.nel] * mag0)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = strainmin * magtot0 / atot
    # Check if magtot0 positive
    if magtot0 <= 0:
        print('Sum of magnitudes in adjustment <= 0.')
        print('Min mag = '+str(magmin))
    # Set elements smaller than min mag to value of min mag
    mag0[mag0<magmin] = magmin
    
    # A posteriori
    # Magnitude of a priori stress * a posteriori strain rate
    mag1 = (model.stressxx0*model.strainxx1 + model.stressyy0*model.strainyy1
            + 2*model.stressxy0*model.strainxy1)[:model.nel]
    # Prevent elements weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set elements smaller than min mag to value of min mag
    mag1[mag1<magmin] = magmin
    # Calculate corrected ratio of apost and apri strain rates for all elements
    scale = np.divide(mag1, mag0, where=mag0!=0.) ** prop
    
    # Adjust strain-rate capacity on each element by its ratio
    model.straincapc[:model.nel] *= scale
    model.straincapcc[:model.nel] *= scale
    model.straincapcs[:model.nel] *= scale
    model.straincaps[:model.nel] *= scale
    model.straincapsc[:model.nel] *= scale
    model.straincapss[:model.nel] *= scale

    
def uniformly(model, prop=1., dstress=0., strainmin=1e-12):
    """Adjusts strain-rate capacity uniformly.
    
    This function adjusts the strain-rate capacity by a uniform factor
    for all elements based on the relative values of a posteriori and
    a priori strain rate. The adjustment is approximately the a posteriori
    strain rate divided by the a priori strain rate.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    dstress : float, default=0.
        The estimated change in stress from the adjustment. This allows
        the adjustment to more accurately calculated so that the new
        a priori solution will more closely match the old a posteriori
        solution.
    strainmin : float, default=1e-12
        Minimum relative strain rate to avoid numerical problems
        associated with dividing by a small number.
    """
    # A priori for each element
    # Magnitude of stress * strain
    mag0 = (model.stressxx0*model.strainxx0 + model.stressyy0*model.strainyy0
            + 2*model.stressxy0*model.strainxy0)
    if np.array(dstress).any():
        # Apply stress adjustment if desired
        mag0 += ((0.5*model.straincapc + 0.5*model.straincapcc
                  + 0.125*model.straincaps - 0.125*model.straincapsc)
                 * dstress[0,:] * model.stressxx0)         # lxxxx
        mag0 += ((0.5*model.straincapc - 0.5*model.straincapcc
                  + 0.125*model.straincaps - 0.125*model.straincapsc)
                 * dstress[1,:] * model.stressyy0)         # lyyyy
        mag0 += -(0.125 * (model.straincaps-model.straincapsc)
                  * (dstress[0,:]*model.stressyy0
                     + dstress[1,:]*model.stressxx0))      # lxxyy
        mag0 += ((model.straincapc + 0.5*model.straincaps
                  + 0.5*model.straincapsc)
                 * dstress[2,:] * model.stressxy0)         # lxyxy
        mag0 += ((-0.5*model.straincapcs + 0.25*model.straincapss)
                 * (dstress[0,:]*model.stressxy0
                    + dstress[2,:]*model.stressxx0))       # lxxxy
        mag0 += ((-0.5*model.straincapcs - 0.25*model.straincapss)
                 * (dstress[1,:]*model.stressxy0
                    + dstress[2,:]*model.stressyy0))       # lxyyy
    # Sum of element areas
    atot = np.sum(model.elarea)
    # Total magnitude, weighted by element area
    magtot0 = np.sum(model.elarea * mag0)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = strainmin * magtot0 / atot
    # Set elements smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = magmin
    
    # A posteriori
    # Magnitude of a priori stress * a posteriori strain rate
    mag1 = (model.stressxx0*model.strainxx1 + model.stressyy0*model.strainyy1
            + 2*model.stressxy0*model.strainxy1)
    # Prevent elements weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set elements smaller than min mag to value of min mag
    mag1[np.all([mag1<magmin, mag1], axis=0)] = magmin
    # Calculate ratio of a posteriori and a priori strain rates for all elements
    ratio = np.divide(mag1, mag0, where=mag0!=0.)
    # Scaling factor weighted by area
    scale = (np.sum(model.elarea*ratio) / atot) ** prop
    
    # Adjust strain-rate capacity on each element by its ratio
    model.straincapc = scale * model.straincapc
    model.straincapcc = scale * model.straincapcc
    model.straincapcs = scale * model.straincapcs
    model.straincaps = scale * model.straincaps
    model.straincapsc = scale * model.straincapsc
    model.straincapss = scale * model.straincapss

def uniformStress(model, postProp=0., aveRange=None, newStress=None):
    """Adjusts strain rate capacity to produce uniform stress magnitudes.
    
    This function adjusts strain rate capacity to produce approximately
    uniform a priori stress magnitudes in all elements. Strain-rate
    capacities in elements with large apriori stress magnitudes are increased
    to make the elements weaker. Strain-rate capacities in elements with
    small a priori stress magnitudes are reduced to make the elements
    stronger.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    postProp : float or array of float, default=0.
        Perform adjustment based on a proportion of the a posteriori stress
        relative to a priori stress. A proportion of 0 uses just the
        a priori stress. A proportion of 1 uses the a posteriori stress.
        A proportion of 0.5 uses the average of the a priori and
        a posteriori stress. This allows the adjustment to be applied with
        another adjustment, e.g. to force potentials, with the proportion
        being the same for both adjustments.
    aveRange : range or array of int or array of bool, default=None
        Average stress over this range of elements. Any variable that
        is accepted by a numpy array as an index can be used. Default
        averages over all elements.
    newStress : float, default=None
        New stress magnitude that elements are adjusted towards. Magnitude
        is sqrt(0.5*s_ij*s_ij). Ignored if aveRange is set. Default
        calculates new stress magnitude using average from previous
        iteration.
    """
    # Set range to calculate stress average over
    if aveRange is None:
        aveRange = np.arange(model.nel)
    else:
        newStress = None
    
    if newStress is None:
        # A priori stress magnitude
        mag0 = np.sqrt(model.stressxx0**2 + model.stressyy0**2 
                      + 2*model.stressxy0**2)
        # Total area of all elements
        atot = np.sum(model.elarea[aveRange])
        # Sum of stress magnitudes, weighted by area
        magtot = np.sum(model.elarea[aveRange] * mag0[aveRange])
        # Mean stress magnitude, weightedy by area
        magave = magtot / atot
    else:
        # Use given stress magnitude, converted to convention.
        magave = newStress * np.sqrt(2)
    
    # Calculate stress based on proportion of a posteriori used
    stressxx = (model.stressxx0[:model.nel]
                + model.stressxxd[:model.nel]*postProp)
    stressyy = (model.stressyy0[:model.nel]
                + model.stressyyd[:model.nel]*postProp)
    stressxy = (model.stressxy0[:model.nel]
                + model.stressxyd[:model.nel]*postProp)
    
    # New stress magnitude
    mag1 = np.sqrt(stressxx**2 + stressyy**2 + 2*stressxy**2)
    
    scale = np.divide(mag1, magave)
    # Scale strain-rate capacity
    model.straincapc[:model.nel] *= scale
    model.straincapcc[:model.nel] *= scale
    model.straincapcs[:model.nel] *= scale
    model.straincaps[:model.nel] *= scale
    model.straincapsc[:model.nel] *= scale
    model.straincapss[:model.nel] *= scale
    
def powerLaw(model, n, postProp=0., aveRange=None):
    """Adjusts strain rate capacity to produce power law rheology.
    
    This function adjusts strain rate capacity to approximately produce
    a power law rheology in all elements. The power law is
    T = B * E**(1/n)
    for stress magnitude T, strain rate magnitude E, power law exponent n,
    and scaling factor B.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    n : int
        Power law exponent.
    postProp : float or array of float, default=0.
        Perform adjustment based on a proportion of the a posteriori B-value
        relative to a priori B-value. A proportion of 0 uses just the
        a priori B-value. A proportion of 1 uses the a posteriori B-value.
        A proportion of 0.5 uses the average of the a priori and
        a posteriori B-value. This allows the adjustment to be applied with
        another adjustment, e.g. to force potentials, with the proportion
        being the same for both adjustments.
    aveRange: range or array of int or array of bool, default=None
        Average B-value over this range of elements. Any variable that
        is accepted by a numpy array as an index can be used. Default
        averages over all elements.
    """
    
    if aveRange is None:
        aveRange = np.arange(model.nel)
    # A priori stress magnitude
    tmag0 = np.sqrt(model.stressxx0**2 + model.stressyy0**2 
                  + 2*model.stressxy0**2)
    # A priori strain rate magnitude
    emag0 = np.sqrt(model.strainxx0**2 + model.strainyy0**2 
                  + 2*model.strainxy0**2)
    # A priori B value
    bmag0 = tmag0 * emag0**(-1/n)
    # Total area of all elements
    atot = np.sum(model.elarea[aveRange])
    # Sum of stress magnitudes, weighted by area
    magtot = np.sum(model.elarea[aveRange] * bmag0[aveRange])
    
    # Calculate stress based on proportion of a posteriori used
    stressxx = (model.stressxx0[:model.nel]
                + model.stressxxd[:model.nel]*postProp[:model.nel])
    stressyy = (model.stressyy0[:model.nel]
                + model.stressyyd[:model.nel]*postProp[:model.nel])
    stressxy = (model.stressxy0[:model.nel] 
                + model.stressxyd[:model.nel]*postProp[:model.nel])
    
    # Calculate strain rate based on proportion of a posteriori used
    strainxx = (model.strainxx0[:model.nel]
                + model.strainxxd[:model.nel]*postProp[:model.nel])
    strainyy = (model.strainyy0[:model.nel]
                + model.strainyyd[:model.nel]*postProp[:model.nel])
    strainxy = (model.strainxy0[:model.nel]
                + model.strainxyd[:model.nel]*postProp[:model.nel])
    
    # A posteriori stress magnitude
    tmag1 = np.sqrt(stressxx**2 + stressyy**2 + 2*stressxy**2)
    # A posteriori strain rate magnitude
    emag1 = np.sqrt(strainxx**2 + strainyy**2 + 2*strainxy**2)
    # A posteriori B value
    bmag1 = tmag1 * emag1**(-1/n)
    
    scale = np.divide(bmag1*atot, magtot, where=magtot!=0.)
    # Scale strain-rate capacity
    # Scale strain-rate capacity
    model.straincapc[:model.nel] *= scale
    model.straincapcc[:model.nel] *= scale
    model.straincapcs[:model.nel] *= scale
    model.straincaps[:model.nel] *= scale
    model.straincapsc[:model.nel] *= scale
    model.straincapss[:model.nel] *= scale
 
def yieldStress(model, yieldStress, postProp=0.):
    """Adjusts strain rate capacity to account for yield stress.
    
    This function adjusts strain rate capacity to account for yield
    stress in each element. Strain rate capacities in elements where
    modelled stresses exceed the yield stress are weakened so that
    the predicted stress equals the yield stress. Strain rate
    capacities in other elements are not adjusted.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    yieldStress : float
        Yield stress magnitude that elements are adjusted so that they
        do not exceed. Magnitude is sqrt(0.5*s_ij*s_ij).
    postProp : float or array of float, default=0.
        Perform adjustment based on a proportion of the a posteriori stress
        relative to a priori stress. A proportion of 0 uses just the
        a priori stress. A proportion of 1 uses the a posteriori stress.
        A proportion of 0.5 uses the average of the a priori and
        a posteriori stress. This allows the adjustment to be applied with
        another adjustment, e.g. to force potentials, with the proportion
        being the same for both adjustments.
    """
    
    # Calculate current stress based on proportion of a posteriori used
    stressxx = (model.stressxx0[:model.nel]
                + model.stressxxd[:model.nel]*postProp)
    stressyy = (model.stressyy0[:model.nel]
                + model.stressyyd[:model.nel]*postProp)
    stressxy = (model.stressxy0[:model.nel]
                + model.stressxyd[:model.nel]*postProp)
    
    # New stress magnitude
    mag1 = np.sqrt(stressxx**2 + stressyy**2 + 2*stressxy**2)
    
    # Stress relative to yield stress
    scale = np.divide(mag1, yieldStress*np.sqrt(2))
    # Only adjust elements that exceed yeild stress
    scale[scale<1] = 1
    # Scale strain-rate capacity
    model.straincapc[:model.nel] *= scale
    model.straincapcc[:model.nel] *= scale
    model.straincapcs[:model.nel] *= scale
    model.straincaps[:model.nel] *= scale
    model.straincapsc[:model.nel] *= scale
    model.straincapss[:model.nel] *= scale   
# -----------------------------------------------------------------------------
# Functions below this line are in testing

def byElementTest(model, prop=1., dstress=0., maxchange=1e6):
    """Adjusts strain-rate capacity on an element-by-element basis.
    
    This function adjusts the strain-rate capacity on an element-by-
    element basis based on the relative values of a posteriori and
    a priori strain rate. The adjustment is approximately the a posteriori
    strain rate divided by the a priori strain rate.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    dstress : float, default=0.
        The estimated change in stress from the adjustment. This allows
        the adjustment to more accurately calculated so that the new
        a priori solution will more closely match the old a posteriori
        solution.
    maxchange : float, default=1e6
        Maximum relative change. Useful for avoiding problems with small
        numbers.
    """
    # A priori for each element
    # Magnitude of stress * strain
    mag0 = (model.stressxx0*model.strainxx0 + model.stressyy0*model.strainyy0
            + 2*model.stressxy0*model.strainxy0)[:model.nel]
    # Correction if change in stress or force potentials
    if np.array(dstress).any():
        mag0 += (dstress[0,:model.nel]*model.strainxx0[:model.nel]
                 + dstress[1,:model.nel]*model.strainyy0[:model.nel]
                 + 2*dstress[2,:model.nel]*model.strainxy0[:model.nel])
    
    # A posteriori
    # Magnitude of a priori stress * a posteriori strain rate
    mag1 = (model.stressxx0*model.strainxx1 + model.stressyy0*model.strainyy1
            + 2*model.stressxy0*model.strainxy1)[:model.nel]
    # Prevent elements weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Calculate ratio of apost and apri strain rates for all elements
    ratio = np.abs(np.divide(mag1, mag0, where=mag0!=0.))
    ratio[ratio>maxchange] = maxchange
    ratio[ratio<1/maxchange] = 1/maxchange
    scale = ratio ** prop
    
    # Adjust strain-rate capacity on each element by its ratio
    model.straincapc[:model.nel] *= scale
    model.straincapcc[:model.nel] *= scale
    model.straincapcs[:model.nel] *= scale
    model.straincaps[:model.nel] *= scale
    model.straincapsc[:model.nel] *= scale
    model.straincapss[:model.nel] *= scale  

def anisotropically(model, strainmin=1e-12):
    """Adjusts strain rate capacity anisotropically.
    
    This function adjusts the strain rate capacity on an element-by-
    element basis anisotropically with a different factor for each
    component of strain rate capacity. The adjustment is based on the
    relative values of a posteriori and a priori strain rate components.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    strainmin : float, default=1e-12
        Minimum relative strain rate to prevent capacities becoming too
        small.
    """
    
    # A priori strain rate times stress for each element
    mag0 = (model.stressxx0*model.strainxx0 + model.stressyy0*model.strainyy0
            + 2*model.stressxy0*model.strainxy0)[:model.nel]
    
    # c-component of capacity, preventing negative capacities
    mag1 = ((model.strainxx1 + model.strainyy1)
            * (model.strainxx0 + model.strainyy0))[:model.nel]
    mag1[mag1<0.] = np.min([np.abs(mag1), model.straincapc[:model.nel]], axis=0)[mag1<0.]
    minmag = (np.sum(mag1*model.elarea[:model.nel])
              / np.sum(model.elarea[:model.nel])) * strainmin
    mag1[mag1<minmag] = minmag
    model.straincapc[:model.nel] = mag1 / mag0
    # cc-component of capacity
    mag1 = (model.strainxx1 * model.strainxx0
            - model.strainyy1 * model.strainyy0)[:model.nel]
    model.straincapcc[:model.nel] = mag1 / mag0
    
    # s- and sc-components of capacity, preventing negative s-components
    magd = ((model.strainxx1 - model.strainyy1)
            * (model.strainyy0 - model.strainxx0))[:model.nel]
    magxy = 4 * (model.strainxy1 * model.strainxy0)[:model.nel]
    mag1 = magd - magxy - 2*(model.strainxx1*model.strainyy0 
                             + model.strainyy1*model.strainxx0)[:model.nel]
    mag1[mag1<0.] = np.min([np.abs(mag1), model.straincapc[:model.nel]], axis=0)[mag1<0.]
    minmag = (np.sum(mag1*model.elarea[:model.nel])
              / np.sum(model.elarea[:model.nel])) * strainmin
    mag1[mag1<minmag] = minmag
    model.straincaps[:model.nel] = mag1 / mag0
    model.straincapsc[:model.nel] = (magd+magxy) / mag0
    
    # cs-component of capacity
    mag1 = -((model.strainxx1+model.strainyy1) * model.strainxy0
             + model.strainxy1 * (model.strainxx0+model.strainyy0))[:model.nel]
    model.straincapcs[:model.nel] = mag1 / mag0
    # cs-component of capacity
    mag1 = 2* ((model.strainxx1-model.strainyy1) * model.strainxy0
             + model.strainxy1 * (model.strainxx0-model.strainyy0))[:model.nel]
    model.straincapss[:model.nel] = mag1 / mag0
    
    # Check that c**2 > cs**2 + cc**2
    capratio = np.hypot(model.straincapcs, model.straincapcc) / model.straincapc
    bigger = capratio >= 1
    model.straincapcs[bigger] *= 0.999999 / capratio[bigger]
    model.straincapcc[bigger] *= 0.999999 / capratio[bigger]
    
    # Check that s**2 > ss**2 + sc**2
    capratio = np.hypot(model.straincapss, model.straincapsc) / model.straincaps
    bigger = capratio >= 1
    model.straincapss[bigger] *= 0.999999 / capratio[bigger]
    model.straincapsc[bigger] *= 0.999999 / capratio[bigger]

def partiallyAnisotropically(model):
    """Adjusts strain rate capacity in a partially anisotropic manner.
    
    This function adjusts the strain rate capacity on an element-by-
    element basis in a partially anisotropic manner by adjusting the
    c- and s-components of strain rate capacity by different factors.
    The adjustment to the c-component is based on all components and
    the adjustment to the s-component is based on shear components.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    strainmin : float, default=1e-12
        Minimum relative strain rate to prevent capacities becoming too
        small.
    """
    
def anisotropicallyC(model, prop=1., maxchange=1e6):
    """Adjusts strain rate capacity c-components anisotropically.
    
    This function adjusts the c-, cc-, and cs-components of strain rate
    capacity on an element-by-element basis in an anisotropic manner.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the strain-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    maxchange : float, default=1e6
        Maximum relative change. Useful for avoiding problems with small
        numbers.
    """
    
#    # Apriori for each element
#    # Magnitude of stress * strain
#    mag0 = (model.stressxx0*model.strainxx0 + model.stressyy0*model.strainyy0
#            - 2*model.stressxy0*model.strainxy0)[:model.nel]
#    
#    # A posteriori
#    # Magnitude of a priori stress * a posteriori strain rate
#    mag1 = (model.stressxx0*model.strainxx1 + model.stressyy0*model.strainyy1
#            - 2*model.stressxy0*model.strainxy1)[:model.nel]
#    # Calculate corrected ratio of apost and apri strain rates for all elements
#    ratio = np.divide(mag1, mag0, where=mag0!=0.)
#    # Prevent elements from weakening when signs differ
#    ratio[ratio < -1.] = -1
#    ratio = np.abs(ratio)
#    # Apply max change
#    ratio[ratio>maxchange] = maxchange
#    ratio[ratio<1/maxchange] = 1/maxchange
#    scale = ratio ** prop
#    diffc = model.straincapc[:model.nel] * (scale-1)
#    
#    # Adjust c-component
#    model.straincapc[:model.nel] += diffc
#    model.straincaps[:model.nel] *= scale
    
    # Calculate adjustment to cs-component
#    numcs = 2*model.stressxy0[:model.nel]*diffc - 4*model.strainxyd[:model.nel]
    numcs = -4*model.strainxyd[:model.nel]
    dil0 = model.stressxx0[:model.nel] + model.stressyy0[:model.nel]
    model.straincapcs[:model.nel] += numcs / dil0
    
    # Calculate adjustment to cc-component
#    numcc = (-(model.stressxx0[:model.nel]-model.stressyy0[:model.nel]) * diffc
#             + 2 * (model.strainxxd[:model.nel]-model.strainyyd[:model.nel]))
    numcc = 2 * (model.strainxxd[:model.nel]-model.strainyyd[:model.nel])
    model.straincapcc[:model.nel] += numcc / dil0
    
    # Check that c**2 > cs**2 + cc**2
    capratio = np.hypot(model.straincapcs, model.straincapcc) / model.straincapc
    bigger = capratio >= 1
    model.straincapcs[bigger] *= 0.999999 / capratio[bigger]
    model.straincapcc[bigger] *= 0.999999 / capratio[bigger]
    



    
    
    
    
    
    
    
    
    
    
    
    
    
    
    