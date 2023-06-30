#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 18 11:21:15 2019

This is a collection of functions to adjust slip-rate capacity (k) using a 
variety of similar methods based on the relative values of a priori and
a posteriori slip rates.

These functions take a permdefmap model as an argument. Optionally, they can also
take a minimum relative value of slip rate, dumin, which defaults to 1e-12.

@author: Hamish Hirschberg
"""
import numpy as np

def byFault(model, prop=1., dstress=0., slipmin=1e-12):
    """Adjusts slip-rate capacity on a fault-by-fault basis.
    
    This function adjusts the slip-rate capacity on a fault-by-fault
    basis using the relative values of a priori and a posteriori slip
    rates on the fault segments. The slip-rate capacity for all
    segments on a single fault will change by the same factor but that
    factor will be different for each fault. This will keep the
    relative values of slip-rate capacity the same between different
    segments on the same fault but will change the relative values
    between segments on different faults. The adjustment is
    approximately the a prosteriori slip divided by the a priori slip.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the slip-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    dstress : array of float, default=0.
        The estimated change in stress from the adjustment. This allows
        the adjustment to more accurately calculated so that the new
        a priori solution will more closely match the old a posteriori
        solution. This is particularly useful when the adjustment is
        performed after another adjustment in the same iteration.
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    """
    # A priori for each segment
    # Magnitude of traction * slip rate for all segments
    mag0 = model.tract0*model.slipt0 + model.tracn0*model.slipn0
    if np.array(dstress).any():
        # Apply stress adjustment if desired
        mag0 += (model.tract0 * (model.slipcapc+model.slipcaps) * dstress[0][:,:]
                + model.tracn0 * model.slipcapc * dstress[1][:,:])
    # Sum length of fault segments
    lsegtot = np.sum(model.seglength)
    # Total magnitude, weighted by segment length
    magtot0 = np.sum(model.seglength * mag0)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = slipmin * magtot0 / lsegtot
    # Set segments smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = slipmin
    
    # A posteriori
    # Magnitude of a priori traction * a posteriori slip rate
    mag1 = model.tract0*model.slipt1 + model.tracn0*model.slipn1
    # Prevent faults weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set segments smaller than min mag to value of min mag
    mag1[np.all([mag1<magmin, mag1], axis=0)] = magmin
    # Calculate corrected ratio of apost and apri slip rates for all segments
    slipratio = np.divide(mag1, mag0, where=mag0!=0.)
    # Total length of each fault
    lftot = np.sum(model.seglength, axis=0)
    # Sum of segment ratios for each fault, weighted by segment length
    fratio = np.sum(model.seglength*slipratio, axis=0)
    
    # Scaling factor for each fault, accounting for adjustment proportion factor
    scale = np.divide(fratio, lftot, where=lftot!=0.) ** prop
    # Apply adjustment to slip rate capacities
    model.slipcapc = scale * model.slipcapc
    model.slipcaps = scale * model.slipcaps
    
def uniformly(model, prop=1., dstress=0., slipmin=1e-12):
    """Adjusts slip rate capacity uniformly.
    
    This function adjusts slip rate capacity uniformly for all segments
    on all faults based on the relative values of a priori and
    a posteriori slip rates on fault segments. The slip rate capacity
    is adjusted by the same factor for all segments across all faults.
    The adjustment is approximately the a prosteriori slip divided by
    the a priori slip.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the slip-rate capacities are being adjusted. The
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
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    """
    # A priori for each segment
    # Magnitude of traction * slip rate for all segments
    mag0 = model.tract0*model.slipt0 + model.tracn0*model.slipn0
    if np.array(dstress).any():
        # Apply stress adjustment if desired
        mag0 += (model.tract0 * (model.slipcapc+model.slipcaps) * dstress[0][:,:]
                + model.tracn0 * model.slipcapc * dstress[1][:,:])
    # Sum length of fault segments
    lsegtot = np.sum(model.seglength)
    # Total magnitude, weighted by segment length
    magtot0 = np.sum(model.seglength * mag0)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = slipmin * magtot0 / lsegtot
    # Set segments smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = slipmin
    
    # A posteriori
    # Magnitude of a priori traction * a posteriori slip rate
    mag1 = model.tract0*model.slipt1 + model.tracn0*model.slipn1
    # Prevent faults weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set segments smaller than min mag to value of min mag
    mag1[np.all([mag1<magmin, mag1], axis=0)] = magmin
    # Calculate corrected ratio of apost and apri slip rates for all segments
    slipratio = np.divide(mag1, mag0, where=mag0!=0.)
    # Total length of all fault segments
    ltot = np.sum(model.seglength)
    # Sum of segment ratios for each fault, weighted by segment length    
    ratio = np.sum(model.seglength * slipratio)
    
    # Uniform scaling factor
    scale = np.divide(ratio, ltot, where=ltot!=0.) ** prop
    # Apply adjustment to slip rate capacities
    model.slipcapc = scale * model.slipcapc
    model.slipcaps = scale * model.slipcaps
    
def bySegment(model, prop=1., dstress=0., slipmin=1e-12):
    """Adjusts slip rate capacity on a segment-by-segment basis.
    
    This function adjusts the slip rate capacity on a segment-by-
    segment basis based on the relative values of the a posteriori and
    a priori slip rates. The adjustment is approximately the
    a posteriori slip rate divided by the a priori slip rate.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the slip-rate capacities are being adjusted. The
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
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    """
    # A priori for each segment
    # Magnitude of traction * slip rate for all segments
    mag0 = model.tract0*model.slipt0 + model.tracn0*model.slipn0
    if np.array(dstress).any():
        # Apply stress adjustment if desired
        mag0 += (model.tract0 * (model.slipcapc+model.slipcaps) * dstress[0][:,:]
                + model.tracn0 * model.slipcapc * dstress[1][:,:])
    # Sum length of fault segments
    lsegtot = np.sum(model.seglength)
    # Total magnitude, weighted by segment length
    magtot0 = np.sum(model.seglength * mag0)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = slipmin * magtot0 / lsegtot
    # Set segments smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = magmin
    
    # A posteriori
    # Magnitude of a priori traction * a posteriori slip rate
    mag1 = model.tract0*model.slipt1 + model.tracn0*model.slipn1
    # Prevent faults weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set segments smaller than min mag to value of min mag
    mag1[np.all([mag1<magmin, mag1], axis=0)] = magmin
    # Calculate corrected ratio of apost and apri slip rates for all segments
    scale = np.divide(mag1, mag0, where=mag0!=0.) ** prop
    
    # Adjust slip-rate capacity on each segment by its ratio
    model.slipcapc = scale * model.slipcapc
    model.slipcaps = scale * model.slipcaps
# -----------------------------------------------------------------------------
# Functions below this line are in testing

def anisotropically(model, prop=1., slipmin=1e-12):
    """Adjusts slip rate capacity anisotropically.
    
    This function adjusts slip rate capacity anisotropically on a
    semgent-by-segment basis based on the relative values of the
    a posteriori and a priori slip rates. The adjustment is approximately
    the a posteriori slip rate divided by the a priori slip rate. The
    c-component of slip rate capacity is based on the normal component
    of slip rate. The s-component is based on the transverse component
    of slip rate and the c-component of capacity.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the slip-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    """
    
    # Set minimum relative slip rate
    magtot = np.sum(model.slipmag0)
    lsegtot = np.sum(model.seglength)
    magmin = slipmin * magtot / lsegtot
    
    # Set c-component of slip rate capacity
    # A priori with min slip rate
    slip0 = np.abs(model.slipn0)
    slip0[slip0<magmin] = magmin
    # A posteriori with min slip rate
    slip1 = model.slipn1 * np.sign(model.slipn0)
    slip1[slip1<magmin] = magmin
    # Caculate scale for c-component
    scale = np.divide(slip1, slip0, where=slip0!=0) ** prop
    model.slipcapc *= scale
    
    # Set s-component of slip rate capacity
    # A priori with min slip rate
    slip0 = np.abs(model.slipt0)
    slip0[slip0<magmin] = magmin
    # A posteriori with min slip rate
    slip1 = model.slipt1 * np.sign(model.slipt0)
    slip1[slip1<magmin] = magmin
    # Caculate scale for c-component
    scale = np.divide(slip1, slip0, where=slip0!=0) ** prop
    model.slipcaps *= scale


def anisotropicallyPlus(model, prop=1., slipmin=1e-12, capmin=1e-6):
    """Adjusts slip rate capacity anisotropically.
    
    This function adjusts slip rate capacity anisotropically on a
    semgent-by-segment basis based on the relative values of the
    a posteriori and a priori slip rates. The adjustment is approximately
    the a posteriori slip rate divided by the a priori slip rate. The
    c-component of slip rate capacity is based on the normal component
    of slip rate. The s-component is based on the transverse component
    of slip rate and the c-component of capacity.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the slip-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    capmin : float, default=1e-6
        Minimum s-component of slip rate capacity relative to c-component
        of slip rate capacity.
    """
    
    # Set minimum relative slip rate
    magtot = np.sum(model.slipmag0)
    lsegtot = np.sum(model.seglength)
    magmin = slipmin * magtot / lsegtot
    
    # Set combined (c+s)-component of slip rate capacity
    # A priori with min slip rate
    slip0 = np.abs(model.slipt0)
    slip0[slip0<magmin] = magmin
    # A posteriori with min slip rate
    slip1 = model.slipt1 * np.sign(model.slipt0)
    slip1[slip1<magmin] = magmin
    # Caculate scale for (c+s)-component
    scale = np.divide(slip1, slip0, where=slip0!=0) ** prop
    slipcap = model.slipcapc + model.slipcaps
    slipcap *= scale
    
    # Set c-component of slip rate capacity
    # A priori with min slip rate
    slip0 = np.abs(model.slipn0)
    slip0[slip0<magmin] = magmin
    # A posteriori with min slip rate
    slip1 = model.slipn1 * np.sign(model.slipn0)
    slip1[slip1<magmin] = magmin
    # Caculate scale for c-component
    scale = np.divide(slip1, slip0, where=slip0!=0) ** prop
    model.slipcapc *= scale
    
    # Convert (c+s)-component into s-component of slip rate capacity
    model.slipcaps = slipcap - model.slipcapc
    slipcapmin = model.slipcapc * capmin
    smallcap = model.slipcaps < slipcapmin
    model.slipcaps[smallcap] = slipcapmin[smallcap]

def anisotropicallyCross(model, prop=1., slipmin=1e-12):
    """Adjusts slip rate capacity anisotropically.
    
    This function adjusts slip rate capacity anisotropically on a
    semgent-by-segment basis based on the relative values of the
    a posteriori and a priori slip rates. The adjustment is approximately
    the a posteriori slip rate divided by the a priori slip rate. The
    c-component of slip rate capacity is based on the normal component
    of slip rate. The s-component is based on the transverse component
    of slip rate and the c-component of capacity.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the slip-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    """
    
    # Set minimum relative slip rate
    magtot = np.sum(model.slipmag0)
    lsegtot = np.sum(model.seglength)
    magmin = slipmin * magtot / lsegtot
    
    # Set c-component of slip rate capacity
    # A priori with min slip rate
    slip0 = np.abs(model.slipn0)
    slip0[slip0<magmin] = magmin
    # A posteriori with min slip rate
    slip1 = model.slipn1 * np.sign(model.slipn0)
    slip1[slip1<magmin] = magmin
    # Caculate scale for c-component
    scale = np.divide(slip1, slip0, where=slip0!=0) ** prop
    model.slipcapc *= scale
    
    # A priori for each segment
    # Magnitude of traction * slip rate for all segments
    mag0 = model.tracn0*model.slipt0 - model.tract0*model.slipn0
    # Store sign of mag0 then get absolute value
    sign = np.sign(mag0)
    mag0 = np.abs(mag0)
    # Sum length of fault segments
    lsegtot = np.sum(model.seglength)
    # Total magnitude, weighted by segment length
    magtot0 = np.sum(model.seglength * mag0)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = slipmin * magtot0 / lsegtot
    # Set segments smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = magmin
    
    # A posteriori
    # Magnitude of a priori traction * a posteriori slip rate
    mag1 = model.tracn0*model.slipt1 - model.tract0*model.slipn1
    # Set relaive to original sign of mag0
    mag1 *= sign
    # Prevent faults weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set segments smaller than min mag to value of min mag
    mag1[np.all([mag1<magmin, mag1], axis=0)] = magmin
    # Calculate corrected ratio of apost and apri slip rates for all segments
    scale = np.divide(mag1, mag0, where=mag0!=0.) ** prop
    
    # Adjust slip-rate capacity on each segment by its ratio
    model.slipcaps = scale * model.slipcaps
    
def anisotropicallyMinus(model, prop=1., slipmin=1e-12):
    """Adjusts slip rate capacity anisotropically.
    
    This function adjusts slip rate capacity anisotropically on a
    semgent-by-segment basis based on the relative values of the
    a posteriori and a priori slip rates. The adjustment is approximately
    the a posteriori slip rate divided by the a priori slip rate. The
    c-component of slip rate capacity is based on the normal component
    of slip rate. The s-component is based on the transverse component
    of slip rate and the c-component of capacity.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the slip-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    """
    
    # Set minimum relative slip rate
    magtot = np.sum(model.slipmag0)
    lsegtot = np.sum(model.seglength)
    magmin = slipmin * magtot / lsegtot
    
    # Set c-component of slip rate capacity
    # A priori with min slip rate
    slip0 = np.abs(model.slipn0)
    slip0[slip0<magmin] = magmin
    # A posteriori with min slip rate
    slip1 = model.slipn1 * np.sign(model.slipn0)
    slip1[slip1<magmin] = magmin
    # Caculate scale for c-component
    scale = np.divide(slip1, slip0, where=slip0!=0) ** prop
    model.slipcapc *= scale
    
    # A priori for each segment
    # Magnitude of traction * slip rate for all segments
    mag0 = model.tract0*model.slipt0 - model.tracn0*model.slipn0
    # Store sign of mag0 then get absolute value
    sign = np.sign(mag0)
    mag0 = np.abs(mag0)
    # Sum length of fault segments
    lsegtot = np.sum(model.seglength)
    # Total magnitude, weighted by segment length
    magtot0 = np.sum(model.seglength * mag0)
    # Minimum magnitude scaled by weighted average of magnitude
    magmin = slipmin * magtot0 / lsegtot
    # Set segments smaller than min mag to value of min mag
    mag0[np.all([mag0<magmin, mag0], axis=0)] = magmin
    
    # A posteriori
    # Magnitude of a priori traction * a posteriori slip rate
    mag1 = model.tract0*model.slipt1 - model.tracn0*model.slipn1
    # Set relaive to original sign of mag0
    mag1 *= sign
    # Prevent faults weakening when a posteriori and a priori signs differ
    mag1[mag1 < -mag0] = -mag0[mag1 < -mag0]
    # Convert all magnitudes to positive values
    mag1 = np.abs(mag1)
    # Set segments smaller than min mag to value of min mag
    mag1[np.all([mag1<magmin, mag1], axis=0)] = magmin
    # Calculate corrected ratio of apost and apri slip rates for all segments
    scale = np.divide(mag1, mag0, where=mag0!=0.) ** prop
    
    # Adjust slip-rate capacity on each segment by its ratio
    model.slipcaps = scale * model.slipcaps
    
def anisotropicallyDip(model, dip, dipunc, prop=1., slipmin=1e-12, capmin=1e-6):
    """Adjusts slip rate capacity anisotropically.
    
    This function adjusts slip rate capacity anisotropically on a
    semgent-by-segment basis based on the relative values of the
    a posteriori and a priori slip rates. The adjustment is approximately
    the a posteriori slip rate divided by the a priori slip rate. The
    c-component of slip rate capacity is based on the normal component
    of slip rate. The s-component is based on the transverse component
    of slip rate and the c-component of capacity.
    
    Parameters
    ----------
    model : permdefmap model
        Model where the slip-rate capacities are being adjusted. The
        a priori and a posteriori results need to have been read into
        the model but this function does not check for that.
    prop : float, default=1.
        A value, typically between 0 and 1, indicating the proportion
        of the standard adjustment should be applied.
    slipmin : float, default=1e-12
        Minimum relative slip rate to avoid numerical problems
        associated with dividing by a small number.
    capmin : float, default=1e-6
        Minimum s-component of slip rate capacity relative to c-component
        of slip rate capacity.
    """
    
    # Set minimum relative slip rate
    magtot = np.sum(model.slipmag0 * model.seglength)
    lsegtot = np.sum(model.seglength)
    magmin = slipmin * magtot / lsegtot
    
    # Set combined (c+s)-component of slip rate capacity
    # A priori with min slip rate
    slip0 = np.abs(model.slipt0)
    slip0[slip0<magmin] = magmin
    # A posteriori with min slip rate
    slip1 = model.slipt1 * np.sign(model.slipt0)
    # Account for a posteriori in opposite direction to a priori
    slip1[slip1<-slip0] = -slip0[slip1<-slip0]
    slip1 = np.abs(slip1)
    slip1[slip1<magmin] = magmin
    # Caculate scale for (c+s)-component
    scale = np.divide(slip1, slip0, where=slip0!=0) ** prop
    slipcap = model.slipcapc + model.slipcaps
    slipcapnew = slipcap * scale
    
    # Set c-component of slip rate capacity
    # A priori with min slip rate
    slip0 = np.abs(model.slipn0)
    slip0[slip0<magmin] = magmin
    # A posteriori with min slip rate
    slip1 = model.slipn1 * np.sign(model.slipn0)
    # Account for a posteriori in opposite direction to a priori
    slip1[slip1<-slip0] = -slip0[slip1<-slip0]
    slip1 = np.abs(slip1)
    slip1[slip1<magmin] = magmin
    # Caculate scale for c-component
    scale = np.divide(slip1, slip0, where=slip0!=0) ** prop
    capcnew = model.slipcapc * scale
    
    # Check resulting dip is within uncertainty
    # Calculate cos**2 of effective dip
    ceff = capcnew / slipcapnew
    # Max dip and min cos**2
    dipmax = np.min([np.ones_like(dip)*(np.pi/2-capmin),dip+dipunc],axis=0)
    cmin = np.cos(dipmax) ** 2
    # Min dip and max cos**2
    dipmin = np.max([np.ones_like(dip)*capmin,dip-dipunc],axis=0)
    cmax = np.cos(dipmin) ** 2
    
    # Identify dips outside constraints
    dipover = ceff > cmax
    dipunder = ceff < cmin
    dipbad = np.logical_or(dipover, dipunder)
    # Get new effective cos**2 for dips outside constraints
    ceff[dipover] = cmax[dipover]
    ceff[dipunder] = cmin[dipunder]
    
    # Apply weighted combination of capacity adjustments for bad dips
    mag0 = (model.tract0[dipbad]*model.slipt0[dipbad] + model.tracn0[dipbad]*model.slipn0[dipbad]) * ceff[dipbad]
    mag1 = (model.tract0[dipbad] * model.slipt0[dipbad] * slipcapnew[dipbad] * ceff[dipbad]
            + model.tracn0[dipbad] * model.slipn0[dipbad] * capcnew[dipbad])
    slipcapnew[dipbad] = mag1 / mag0
    capcnew[dipbad] = slipcapnew[dipbad] * ceff[dipbad]
    
    # Store new capacities
    hasseg = model.sideonfault >= 0
    model.slipcapc[hasseg] = capcnew[hasseg]
    model.slipcaps[hasseg] = slipcapnew[hasseg] - model.slipcapc[hasseg]











