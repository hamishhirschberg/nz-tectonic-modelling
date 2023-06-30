#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct  5 12:07:23 2021

readModel.py - functions relating to reading models

@author: Hamish Hirschberg
"""

from .. import model
import os
import numpy as np

def multipleModels(prefix, nmax, nmin=0, digits=2, pad='zero',
                   readOutput=True):
    """Read multiple models.
    
    Function to read multiple models from separate folders that have a
    systematic naming convention of prefix+i for i in range(nmin, nmax).
    
    Parameters
    ----------
    prefix : string
        Prefix of folder name before numerical index.
    nmax : int
        Maximum value (+1) of numerical index in folder name. Values are
        found using range(nmin, nmax). For default of nmin=0, nmax is the
        number of models.
    nmin : int, default=0
        Minimum value of numerical index in folder name.
    digits : int, default=2
        Number of digits used in numerical index in folder name.
    pad : string, default='zero'
        Padding used to fill numerical index out to the number of digits.
        It can be 'zero' (padded with '0'), 'space' (padded with ' '),
        'nopad' (no padding, with potentially variable length names),
        'constant' (all models will be loaded from the folder named
        `prefix`).
    readOutput : bool, default=True
        Read the output of the models being loaded.
    
    Returns
    -------
    models : list of permdefmap models
        List of permdefmap models that have been read in.
    """
    
    # Template of directory names
    if pad == 'zero':
        template = prefix + '{:0' + str(digits) + 'd}'
    elif pad == 'space':
        template = prefix + '{:' + str(digits) + 'd}'
    elif pad == 'nopad':
        template = prefix + '{:d}'
    elif pad == 'constant':
        template = prefix
    else:
        raise ValueError('Padding type '+str(pad)+' not recognised.')
    
    # List of models
    models = []
    
    cwd = os.getcwd()
    # Loop through directories to find models
    for i in range(nmin, nmax):
        direc = template.format(i)
        os.chdir(direc)
        models.append(model.Model(True))
        if readOutput:
            models[i-nmin].readAllOutput()
        os.chdir(cwd)
    
    return models

def rssFiles(prefix,nmax,nmin=0,digits=2,pad='zero',file='rss.log'):
    """Read multiple RSS files.
    
    Function to read multiple residual sum of squares (RSS) files from
    separate folders that have a systematic naming convention of prefix+i
    for i in range(nmin, nmax). The files are assumed to be entirely
    numeric.
    
    Parameters
    ----------
    prefix : string
        Prefix of folder name before numerical index.
    nmax : int
        Maximum value (+ 1) of numerical index in folder name. For default
        of nmin=0, nmax is the number of models.
    nmin : int, default=0
        Minimum value of numerical index in folder name.
    digits : int, default=2
        Number of digits used in numerical index in folder name.
    pad : string, default='zero'
        Padding used to fill numerical index out to the number of digits.
        It can be 'zero' (padded with '0'), 'space' (padded with ' '),
        'nopad' (no padding, with potentially variable length names),
        'constant' (all models will be loaded from the folder named
        `prefix`).
    file : string, default='rss.log'
        Name of RSS file.
    
    Returns
    -------
    rss : list of numpy arrays
        List of numpy arrays containing the values in the RSS files.
    """
    
    # template of directory names
    if pad == 'zero':
        template = prefix + '{:0' + str(digits) + 'd}'
    elif pad == 'space':
        template = prefix + '{:' + str(digits) + 'd}'
    elif pad == 'nopad':
        template = prefix + '{:d}'
    elif pad == 'constant':
        template = prefix
    
    # List of arrays of RSS statistics
    rss = []
    
    cwd = os.getcwd()
    # Loop through directories
    for i in range(nmin, nmax):
        direc = template.format(i)
        os.chdir(direc)
        text = []
        f = open(file, 'r')
        for line in f:
            text.append(line.split())
        f.close()
        rss.append(np.array(text, dtype=np.float))
        os.chdir(cwd)
    
    return rss
    
def covarianceMatrix(file='covariance_matrix.dat'):
    """Read file containing covariance matrix of selected slip rates.
    
    Parameters
    ----------
    file : string, default='covariance_matrix.dat'
        Name of file containing covariance matrix.
    
    Returns
    -------
    covar : numpy array
        2n x 2n numpy array with variance-covariance values for n selected
        slip rates. Indices in range(0, n) are transverse components and
        indices in range(n, 2n) are normal components.
    """
    
    f = open(file)
    # Read header lines
    f.readline()
    line = f.readline.split()
    # Number of slip rate observations
    nslip = int(line[6])
    for i in range(2):
        f.readline()
    
    # Set up array
    covar = np.zeros((2*nslip, 2*nslip))
    
    # Read matrix proper
    for i in range(nslip):
        for j in range(i + 1):
            line = f.readline().split()
            (covar[i,j], covar[i,nslip+j],
             covar[nslip+i,j], covar[nslip+i,nslip+j]) = line[2:]
            (covar[j,i], covar[nslip+j,i],
             covar[j,nslip+i], covar[nslip+j,nslip+i]) = line[2:]
    
    f.close()
    return covar
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    