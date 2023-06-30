#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 28 11:40:15 2019

@author: Hamish Hirschberg
"""

from .fortran import preGmsh, postGmsh, setup, invert, output, writeInput
from .fortran import invertAlphaf, invertAlphafx, invertPre, outputExtra

from .preprocessing import gmshGeom, maskStress, maskStrain
from .preprocessing import addStrainrateObs, addVelocityObs, addSliprateObs
from .preprocessing import subfaults, subregions, getNames
from .preprocessing import mesh, slip2strainRate

from .adjustment import adjustSlipCap, adjustStrainCap, adjustPot, adjustObs

from .postprocessing import readResults, readModel, readMisfit, extractRss
from .postprocessing import readFixedValues, getVariance, velocities, forces

from . import geometry, plotting

from .model import Model

def invertModel(model, alphaf=1.):
    """Perform standard inversion of permdefmap model.
    
    Parameters
    ----------
    model : permdefmap model
        Perform standard a priori and a posteriori invserion on this model.
        Results are read into this model
    alphaf : float, default=1.
        alpha weighting of the dynamic (a priori) part in the a posteriori
        inversion. Larger values weight the dynamic part more. Smaller
        values weight the observational part more.
    """
    # Write model to files used by Fortran codes
    model.writeForSetup()
    
    # Invert model
    setup.setup()
    invertAlphaf.invalf(alphaf)
    output.output()
    
    # Read in results
    model.readAllOutput()