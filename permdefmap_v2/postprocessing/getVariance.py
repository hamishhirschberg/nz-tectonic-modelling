#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 18 15:09:22 2021

getVariance.py - collection of functions to calculate variance of different
model predictions from Monte Carlo simulations

@author: Hamish Hirschberg
"""

import numpy as np

def velocityInElements(models, soln):
    """Covariance of velocities in elements.
    
    Calculate variance-covariance matrix of model predicted velocities at
    element centres from a list of models. The models are assumed to have
    the same geometry. Indices are for all x components first, then all
    y components.
    
    Parameters
    ----------
    models : list of permdefmap models
        Calculate covariance from these models.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    
    Returns
    -------
    covar : numpy array
        Numpy array of the variance-covariance matrix. The 'all' and 'both'
        solution options return a list with the arrays for each requested
        part of the solution.
    """
    
    # Set up variables
    nmodel = len(models)
    nel = models[0].nel
    vel = np.zeros((nmodel, 2*nel))
    
    # Choose solution and load velocities
    if str(soln)=='0' or soln=='pre' or soln=='apriori':
        for i in range(nmodel):
            vel[i,:nel] = models[i].velinelx0[:nel]
            vel[i,nel:] = models[i].velinely0[:nel]
    elif str(soln)=='1' or soln=='post' or soln=='aposteriori':
        for i in range(nmodel):
            vel[i,:nel] = models[i].velinelx1[:nel]
            vel[i,nel:] = models[i].velinely1[:nel]
    elif str(soln)=='c' or soln=='con' or soln=='constraint':
        for i in range(nmodel):
            vel[i,:nel] = models[i].velinelxc[:nel]
            vel[i,nel:] = models[i].velinelyc[:nel]
    elif str(soln)=='d' or soln=='diff' or soln=='difference':
        for i in range(nmodel):
            vel[i,:nel] = models[i].velinelxd[:nel]
            vel[i,nel:] = models[i].velinelyd[:nel]
    elif soln == 'a' or soln == 'all':
        return [velocityInElements(models,'0'), velocityInElements(models,'1'),
                velocityInElements(models,'c'), velocityInElements(models,'d')]
    elif soln == 'b' or soln == 'both':
        return [velocityInElements(models,'0'), velocityInElements(models,'1')]
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
    
    # Calculate variance-covariance matrix
    covar = np.cov(vel, rowvar=False)
    
    return covar

def slipSingleSegment(models, soln, fault, seg):
    """Calaculte slip rate variance-covariance for single fault segment.
    
    Parameters
    ----------
    models : list of permdefmap models
        Calculate covariance from these models.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    fault : int
        Index of fault that segment is on.
    seg : int
        Index of segment along fault.
    
    Returns
    -------
    covar : list
        List containing the t-component variance, n-component variance and
        tn-covariance of slip rate on the segment.
    """
    
    # Set up variables
    nmodel = len(models)
    slip = np.zeros((nmodel, 2))

    # Choose solution and load velocities
    if str(soln)=='0' or soln=='pre' or soln=='apriori':
        for i in range(nmodel):
            slip[i,0] = models[i].slipt0[seg,fault]
            slip[i,1] = models[i].slipn0[seg,fault]
    elif str(soln)=='1' or soln == 'post' or soln=='aposteriori':
        for i in range(nmodel):
            slip[i,0] = models[i].slipt1[seg,fault]
            slip[i,1] = models[i].slipn1[seg,fault]
    elif str(soln)=='c' or soln=='con' or soln=='constraint':
        for i in range(nmodel):
            slip[i,0] = models[i].sliptc[seg,fault]
            slip[i,1] = models[i].slipnc[seg,fault]
    elif str(soln)=='d' or soln=='diff' or soln=='difference':
        for i in range(nmodel):
            slip[i,0] = models[i].sliptd[seg,fault]
            slip[i,1] = models[i].slipnd[seg,fault]
    elif soln == 'a' or soln == 'all':
        return [slipSingleSegment(models, '0', fault, seg),
                slipSingleSegment(models, '1', fault, seg),
                slipSingleSegment(models, 'c', fault, seg),
                slipSingleSegment(models, 'd', fault, seg)]
    elif soln == 'b' or soln == 'both':
        return [slipSingleSegment(models, '0', fault, seg),
                slipSingleSegment(models, '1', fault, seg)]
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
    
    # Calculate variance-covariance matrix
    covar = np.cov(slip, rowvar=False)
    
    return [covar[0,0], covar[1,1], covar[0,1]]

def strainAll(models, soln):
    """Covariance of strain rates in elements.
    
    Calculate variance-covariance matrix of model predicted strain rates at
    element centres from a list of models. The models are assumed to have
    the same geometry. Indices are for all xx components first, then all
    yy components, then xy components.
    
    Parameters
    ----------
    models : list of permdefmap models
        Calculate covariance from these models.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    
    Returns
    -------
    covar : numpy array
        Numpy array of the variance-covariance matrix. The 'all' and 'both'
        solution options return a list with the arrays for each requested
        part of the solution.
    """
    
    # Set up variables
    nmodel = len(models)
    nel = models[0].nel
    strain = np.zeros((nmodel, 3*nel))
    
    # Choose solution and load velocities
    if str(soln)=='0' or soln=='pre' or soln=='apriori':
        for i in range(nmodel):
            strain[i,:nel] = models[i].strainxx0[:nel]
            strain[i,nel:2*nel] = models[i].strainyy0[:nel]
            strain[i,2*nel:] = models[i].strainxy0[:nel]
    elif str(soln)=='1' or soln=='post' or soln=='aposteriori':
        for i in range(nmodel):
            strain[i,:nel] = models[i].strainxx1[:nel]
            strain[i,nel:2*nel] = models[i].strainyy1[:nel]
            strain[i,2*nel:] = models[i].strainxy1[:nel]
    elif str(soln)=='c' or soln=='con' or soln=='constraint':
        for i in range(nmodel):
            strain[i,:nel] = models[i].strainxxc[:nel]
            strain[i,nel:2*nel] = models[i].strainyyc[:nel]
            strain[i,2*nel:] = models[i].strainxyc[:nel]
    elif str(soln)=='d' or soln=='diff' or soln=='difference':
        for i in range(nmodel):
            strain[i,:nel] = models[i].strainxxd[:nel]
            strain[i,nel:2*nel] = models[i].strainyyd[:nel]
            strain[i,2*nel:] = models[i].strainxyd[:nel]
    elif soln == 'a' or soln == 'all':
        return [strainAll(models,'0'), strainAll(models,'1'),
                strainAll(models,'c'), strainAll(models,'d')]
    elif soln == 'b' or soln == 'both':
        return [strainAll(models,'0'), strainAll(models,'1')]
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
    
    # Calculate variance-covariance matrix
    covar = np.cov(strain, rowvar=False)
    
    return covar

def strainSingleElement(models, soln, el):
    """Calaculte strain rate variance-covariance for single element.
    
    Parameters
    ----------
    models : list of permdefmap models
        Calculate covariance from these models.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    el : int
        Index of element.
    
    Returns
    -------
    covar : list
        List containing the xx-component variance, yy-component variance,
        xy-component variance, xx-yy covariance, xx-xy covariance, and
        yy-xy covariance of strain rate in the element.
    """
    
    # Set up variables
    nmodel = len(models)
    strain = np.zeros((nmodel, 3))

    # Choose solution and load velocities
    if str(soln)=='0' or soln=='pre' or soln=='apriori':
        for i in range(nmodel):
            strain[i,0] = models[i].strainxx0[el]
            strain[i,1] = models[i].strainyy0[el]
            strain[i,2] = models[i].strainxy0[el]
    elif str(soln)=='1' or soln == 'post' or soln=='aposteriori':
        for i in range(nmodel):
            strain[i,0] = models[i].strainxx1[el]
            strain[i,1] = models[i].strainyy1[el]
            strain[i,2] = models[i].strainxy1[el]
    elif str(soln)=='c' or soln=='con' or soln=='constraint':
        for i in range(nmodel):
            strain[i,0] = models[i].strainxxc[el]
            strain[i,1] = models[i].strainyyc[el]
            strain[i,2] = models[i].strainxyc[el]
    elif str(soln)=='d' or soln=='diff' or soln=='difference':
        for i in range(nmodel):
            strain[i,0] = models[i].strainxxd[el]
            strain[i,1] = models[i].strainyyd[el]
            strain[i,2] = models[i].strainxyd[el]
    elif soln == 'a' or soln == 'all':
        return [strainSingleElement(models, '0', el),
                strainSingleElement(models, '1', el),
                strainSingleElement(models, 'c', el),
                strainSingleElement(models, 'd', el)]
    elif soln == 'b' or soln == 'both':
        return [strainSingleElement(models, '0', el),
                strainSingleElement(models, '1', el)]
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
    
    # Calculate variance-covariance matrix
    covar = np.cov(strain, rowvar=False)
    
    return [covar[0,0], covar[1,1], covar[2,2],
            covar[0,1], covar[0,2], covar[1,2]]
    
def var2uncert(var, ncomp, scale=1e3, conf=2.):
    """Convert variance to uncertainty.
    
    Converts variance and covariance of multiple component observarion to
    uncertainty and correlation.
    
    Parameters
    ----------
    var : 1-D numpy array
        Array of variance and covariance values. Order is variances of each
        component first then covariances in order 12, 13, 14, ..., 23, 24,
        ..., 34, ... for as many combinations as required.
    ncomp : int
        Number of components in observation.
    scale : float, default=1e3
        Scale uncertainty by this factor to convert units. Default converts
         m/yr to mm/yr.
    conf : float, default=2.
        Confidence level given in terms of standard error. Default value of
        2 corresponds to 95% confidence interval.
    
    Returns
    -------
    unc : 1-D numpy array
        Array of uncertainty and correlation values. Order is uncertainties
        of each component first then correlations values, i.e. same order
        as `var`.
    """
    
    var = np.array(var)
    unc = np.zeros_like(var)
    
    # Standard error
    std = np.sqrt(var[:ncomp])
    
    # Uncertainties
    unc[:ncomp] = std * scale * conf
    
    # Correlations
    k = ncomp + 0
    for i in range(ncomp-1):
        print([i, k, k+ncomp-i])
        unc[k:k+ncomp-i-1] = var[k:k+ncomp-i-1] / (std[i] * std[i:])
        k += ncomp - i - 1
    
    return unc



















