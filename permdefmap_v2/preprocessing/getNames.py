#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 22 11:07:05 2021

@author: Hamish Hirschberg
"""

def getNames(model):
    """Load names from 'raw_input_when_using_gmsh.dat'.
    
    This function reads the file 'raw_input_when_using_gmsh.dat' and
    extracts the names of faults, rigid boundaries, and velocity lines from
    the comments of the file. This is specific to the way that the comments
    are currently entered into the file and will need to be edited if this
    comment format changes.
    
    Parameters
    ----------
    model : permdefmap model
        Store names in this model.
    """
    
    file = open('raw_input_when_using_gmsh.dat','r')
    
    # Earth radius
    for i in range(3):
        file.readline()
    
    # Velocity lines
    for i in range(model.nvline):
        line = file.readline()
        model.vlinename[i] = line.split('#')[2].strip()
        npoint = int(line.split()[1])
        for j in range(npoint):
            file.readline()
            line = file.readline()
            if abs(int(line.split()[0])) == 3:
                file.readline()
                    
    # Rigid boundaries
    for i in range(2):
        file.readline()
    for i in range(model.nrigidb):
        line = file.readline()
        model.rigidbname[i] = line.split(',')[5].strip()
        npoint = int(line.split()[1])
        for j in range(npoint):
            file.readline()
    
    # External boundaries
    file.readline()
    line = file.readline()
    nextb = int(line.split()[0])
    for i in range(nextb + model.nrigidb + model.nvline):
        file.readline()
    
    # Faults
    for i in range(2):
        file.readline()
    for i in range(model.nfault):
        line = file.readline()
        model.faultname[i] = line.split('|')[1]
        npoint = int(line.split()[1])
        for j in range(2 * npoint):
            file.readline()
    
    file.close()