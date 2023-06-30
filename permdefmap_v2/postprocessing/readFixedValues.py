#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 13 09:30:09 2021

readFixedValues.py

@author: Hamish Hirschberg

Collection of functions to read values that are fixed within an
iteration of the permanent deformation map method.
"""
def readFixedVels(model):
    """Reads in fixed velocities along velocity lines and rigid boundaries.
    
    This functions reads the input file 'fixed_velocity_values.log',
    which is the same for all solutions. It saves the values of fixed
    velocities into a permdefmap model.
    
    Parameters
    ----------
    model : permdefmap model
        Model into which the values are stored.    
    """
    
    file='fixed_velocity_values.log'
    fdat=open(file,'r')
    # Velocity lines
    for i in range(7):
        fdat.readline()
    for i in range(model.nvline):
        fdat.readline()
        for j in range(2*model.nvlineside[i] + 1):
            gp = int(j/2)
            fdat.readline()
            # Faults have minus/plus sides as two separate entries
            if j%2 == 0 and model.faultonvline[gp,i] >= 0:
                (model.vlineoutmx[gp,i],
                 model.vlineoutmy[gp,i]) = fdat.readline().split()
                (model.vlineoutmaz[gp,i],
                 model.vlineoutmmag[gp,i]) = fdat.readline().split()
                fdat.readline()
                (model.vlineoutpx[gp,i],
                 model.vlineoutpy[gp,i]) = fdat.readline().split()
                (model.vlineoutpaz[gp,i],
                 model.vlineoutpmag[gp,i]) = fdat.readline().split()
            # Segments and other gridpoints have one entry each
            else:
                (model.vlineoutx[j,i],
                 model.vlineouty[j,i]) = fdat.readline().split()
                (model.vlineoutaz[j,i],
                 model.vlineoutmag[j,i]) = fdat.readline().split()
    
    # Rigid boundaries
    for i in range(5):
        fdat.readline()
    for i in range(model.nrigidb):
        fdat.readline()
        for j in range(2*model.nrigidbside[i] + 1):
            fdat.readline()
            (model.rigidbvelx[j,i],
             model.rigidbvely[j,i]) = fdat.readline().split()
            (model.rigidbvelaz[j,i],
             model.rigidbvelmag[j,i]) = fdat.readline().split()

def readForces(model, capOut=False):
    """Reads in forces at element centres.
    
    This function reads the input file 'forces_and_rate_capacities.log',
    which is the same for all solutions. It saves the values of forces
    at element centres into a permdefmap model.
    
    Parameters
    ----------
    model : permdefmap model
        Model into which the values are stored.
    capOut : bool, default=False
        Returns effective capacities used in the model which account for
        the inversion of the model occurring at grid points.
    """
    
    import numpy as np
    
    file = 'forces_and_rate_capacities.log'
    fdat = open(file, 'r')
    # Fault information
    for i in range(7):
        fdat.readline()     # Ignore comment lines
    # All fault information can be found elsewhere
    for i in range(model.nfault):
        fdat.readline()
        for j in range(model.nfaultseg[i]):
            for k in range(3):
                fdat.readline()
    # Element information
    straincapc = np.zeros((model.nel))
    straincapcc = np.zeros((model.nel))
    straincapcs = np.zeros((model.nel))
    straincaps = np.zeros((model.nel))
    straincapsc = np.zeros((model.nel))
    straincapss = np.zeros((model.nel))
    for i in range(15):
        fdat.readline()     # Ignore comment lines
    for i in range(model.nel):
        fdat.readline()     # Ignore information that can be found elsewhere
        model.forcex[i], model.forcey[i] = fdat.readline().split()
        model.forcemag[i], model.forceaz[i] = fdat.readline().split()
        straincapc[i], straincapcc[i], straincapcs[i] = fdat.readline().split()
        straincaps[i], straincapsc[i], straincapss[i] = fdat.readline().split()
        for j in range(2):
            fdat.readline()
        for j in range(3):
            for k in range(5):
                fdat.readline()
    fdat.close()
    if capOut:
        return (straincapc, straincapcc, straincapcs, straincaps, straincapsc,
                straincapss)