#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  5 15:13:27 2020

@author: hamish
"""

import numpy as np
from scipy import optimize
import warnings

def check(model):
    """Performs checks on model.
    
    This function performs a series of checks to ensure that certain
    elements of the model are consistent. Some inconsistencies are
    fixed with a warning explaining what has happened. Others raise
    an exception with an error message explaining what has happened.
    
    Parameters
    ----------
    model : 1D permdefmap model
        The model to be checked.
    """
    # Check points and segments align
    if model.npt != len(model.points):
        raise Exception('Number of points '+str(model.npt)+' does not match '
                        + 'length of points array '+str(len(model.points)))
    if model.npt != model.nseg+1:
        raise Exception('Number of points '+str(model.npt)+' is not one more '
                        +'than the number of segments '+str(model.nseg))
    # Check spacing between points is correct
    if not np.all([model.points[1:]-model.points[:-1], model.dx]):
        warnings.warn('Spacing between points does not match point locations.'
                      +' Changing spacing to match locations.')
        model.dx = model.points[1:]-model.points[:-1]
    # Check only non-zero slip rate capacities are at faults
    for i in range(model.npt):
        if model.slipcap[i] != 0 and not model.faults[i]:
            warnings.warn('Point '+str(i)+' has non-zero slip rate capacity '
                            + 'but is not a fault. Setting capacity to zero.')
            model.slipcap[i] = 0
    # Check all faults have non-zero slip rate capacity
    for i in range(model.npt):
        if model.slipcap[i] == 0 and model.faults[i]:
            raise Exception('Point '+str(i)+' is a fault but has zero slip rate'
                            + ' capacity.')
    # Check number of faults is correct
    if model.nfault != np.sum(model.faults):
        warnings.warn('Number of faults '+str(model.nfault)+' does not match '
                      + 'number of points marked as faults '
                      + str(np.sum(model.faults)) + '. Correcting number of '
                      + 'faults.')
    # Check slip rate capacities are positive
    for i in range(model.npt):
        if model.slipcap[i] < 0:
            raise Exception('Point '+str(i)+' has a slip rate capacity of '
                            + str(model.slipcap[i])+' Capacities must be '
                            + 'positive.')
    # Check strain rate capacities are positive
    for i in range(model.nseg):
        if model.straincap[i] < 0:
            raise Exception('Point '+str(i)+' has a strain rate capacity of '
                            + str(model.straincap[i])+' Capacities must be '
                            + 'positive.')
    # Check slip rate observations on faults and non-zero standard error
    for i in range(model.nslipob):
        if not model.faults[model.ptofslipob[i]]:
            raise Exception('Slip rate observation '+str(i)+' at point '
                            +str(model.ptofslipob[i])+' is not on a fault.')
        if model.slipobse[i] <= 0:
            raise Exception('Slip rate observation '+str(i)+' has std. err. '
                            + str(model.slipobse[i])+' Std. err. must be '
                            + 'positive.')
    # Check strain rate observations non-zero standard error
    for i in range(model.nstrainob):
        if model.strainobse[i] <= 0:
            raise Exception('strain rate observation '+str(i)+' has std. err. '
                            + str(model.strainobse[i])+' Std. err. must be '
                            + 'positive.')
    print('Model check complete. Model is valid.')

def analytical(model, alphaf=1., solvePost=True):
    """Analytical method of solving 1-D problem.
    
    This function uses an analytical method to solve the pure one-
    dimensional version of the permdefmap problem.
    
    Parameters
    ----------
    model : 1D permdefmap model
        The model that is being solved.
    alphaf : float, default=1.
        Weighted factor for dynamics-like (a priori) contribution
        in the a posteriori solution. Default of 1 represents expected
        equal statistical weight between dynamics and observations.
        Smaller values weight more towards observations and larger
        values weight more towards dynamics. Ignored if solvePost is
        False.
    solvePost : bool, default=True
        Solve the a posteriori model. The a priori model is always
        solved.
    """
    
    # Force potentials on segments
    potseg = (model.pot[1:]+model.pot[:-1])*0.5
    # A priori solution
    totvel = ((model.endvel-model.startvel) - np.sum(model.slipcap*model.pot)
              - np.sum(model.straincap * potseg * model.dx))
    totcap = np.sum(model.slipcap) + np.sum(model.straincap*model.dx)
    # Lagrange multiplier tau
    model.tau = totvel / totcap
    slip = model.slipcap * (model.tau+model.pot)
    strain = model.straincap * (model.tau+potseg)
    # Fill in velocities
    model.velm0[0] = model.startvel
    for i in range(model.nseg):
        model.velp0[i] = model.velm0[i] + slip[i]
        model.velm0[i+1] = model.velp0[i] + strain[i]*model.dx[i]
    model.velp0[-1] = model.endvel
    
    if solvePost:
        # Solve a posteriori
        # Value of function for a priori with no potentials
        Lpre = (np.sum(slip[model.faults]**2 / model.slipcap[model.faults])
                + np.sum(strain**2 / model.straincap * model.dx))
        # Number of unknowns
        nvar = model.nseg + np.sum(model.faults)
        # Weighting factor for a posteriori
        alpha = alphaf * np.sqrt(nvar/Lpre)
        
        # A posteriori effective capacity inverses and force potentials
        slipinv = np.zeros_like(model.slipcap)
        slipinv[model.faults] = 1 / model.slipcap[model.faults]
        straininv = 1 / model.straincap
        poteff = model.pot+0.
        # Process slip rate observations
        for i in range(model.nslipob):
            pt = model.ptofslipob[i]
            alphavar = alpha * model.slipobse[i]**2
            slipinv[pt] += 1 / alphavar
            poteff[pt] += model.slipobvalue[i] / alphavar
        # Process strain rate observations
        for i in range(model.nstrainob):
            seg = model.segofstrainob[i]
            alphavar = alpha * model.strainobse[i]**2 * model.dx[seg]
            straininv[seg] += 1 / alphavar
            potseg[seg] += model.strainobvalue[i] / alphavar
        # Effective capacities
        slipeff = np.zeros_like(slipinv)
        slipeff[model.faults] = 1 / slipinv[model.faults]
        straineff = 1 / straininv
        
        # A posteriori solution
        totvel = ((model.endvel-model.startvel) - np.sum(slipeff*poteff)
                    - np.sum(straineff * potseg * model.dx))
        totcap = np.sum(slipeff) + np.sum(straineff*model.dx)
        tau = totvel / totcap
        slip = slipeff * (tau+poteff)
        strain = straineff * (tau+potseg)
        # Fill in velocities
        model.velm1[0] = model.startvel
        for i in range(model.nseg):
            model.velp1[i] = model.velm1[i] + slip[i]
            model.velm1[i+1] = model.velp1[i] + strain[i]*model.dx[i]
        model.velp1[-1] = model.endvel

def output(model):
    """Calculate outputs from velocity solutions.
    
    Parameters
    ----------
    model : 1D permdefmap model
        Calculate outputs for this model.
    """
    
    model.velmd = model.velm1 - model.velm0
    model.velpd = model.velp1 - model.velp0
    
    model.slip0 = model.velp0 - model.velm0
    model.slip1 = model.velp1 - model.velm1
    model.slipd = model.velpd - model.velmd
    
    dx = model.points[1:] - model.points[:-1]
    model.strain0 = np.divide(model.velm0[1:] - model.velp0[:-1], dx)
    model.strain1 = np.divide(model.velm1[1:] - model.velp1[:-1], dx)
    model.straind = np.divide(model.velmd[1:] - model.velpd[:-1], dx)
    
    model.trac0 = np.divide(model.slip0, model.slipcap, where=model.faults)
    model.trac1 = np.divide(model.slip1, model.slipcap, where=model.faults)
    model.tracd = np.divide(model.slipd, model.slipcap, where=model.faults)
    
    model.stress0 = model.strain0 / model.straincap
    model.stress1 = model.strain1 / model.straincap
    model.stressd = model.straind / model.straincap
    
    model.slipobmis0 = []
    model.slipobmis1 = []
    for ob in range(model.nslipob):
        pt = model.ptofslipob[ob]
        model.slipobmis0.append((model.slip0[pt] - model.slipobvalue[ob])
                                / model.slipobse[ob])
        model.slipobmis1.append((model.slip1[pt] - model.slipobvalue[ob])
                                / model.slipobse[ob])
    
    model.strainobmis0 = []
    model.strainobmis1 = []
    for ob in range(model.nstrainob):
        s = model.segofstrainob[ob]
        model.strainobmis0.append((model.strain0[s] - model.strainobvalue[ob])
                                    / model.strainobse[ob])
        model.strainobmis1.append((model.strain1[s] - model.strainobvalue[ob])
                                    / model.strainobse[ob])
    
    model.rss0 = (np.sum(np.array(model.slipobmis0) ** 2)
                    + np.sum(np.array(model.strainobmis0) ** 2))
    model.rss1 = (np.sum(np.array(model.slipobmis1) ** 2)
                    + np.sum(np.array(model.strainobmis1) ** 2))
    
def solve(model,alphaf=1,solvePost=True):
    """
    This function solves the pure 1-D problem.
    alphaf : the alpha factor relative to normalised alpha
    solvePost : solve the aposteriori problem as well as the apriori problem
    """
    
#    nf = np.sum(model.kappa!=0)        # number of faults on line
    um = np.zeros((model.npt))    #model.um0+0.             # arrays of velocities minus side
    up = np.zeros((model.npt))    #model.up0+0.             # arrays of velocities plus side
    ufix = np.zeros((model.npt),dtype=np.bool)      # points with velocities fixed

    # calculate lengths line segments
    dx=model.points[1:]-model.points[:-1]
    
    ufix[0],ufix[-1]=True,True       # fix end velocities
    um[0]=model.uends[0]
    um[-1]=model.uends[1]
    
    # extract known and variable velocities
    faults=model.kappa != 0
    uknown=um[ufix]
    uvar=np.concatenate((um[~ufix],up[faults]))
    nunk=int(model.npt-len(uknown))        # number of points with unknown velocity
    
    # run minimisation
    # first run apriori
    alpha = 1
    upre=optimize.minimize(func1dAposteriori,uvar,tol=1e-10,method='BFGS',options={'disp':True,'maxiter':len(uvar)*2e3},\
                           args=(uknown,ufix,alpha,dx,model.eta,model.kappa,model.phi))
    model.um0[ufix]=uknown
    model.um0[~ufix]=upre.x[:nunk]
    model.up0=model.um0+0.
    model.up0[faults]=upre.x[nunk:]
    if solvePost:
        Lpre=func1dAposteriori(upre.x,uknown,ufix,alpha,dx,model.eta,model.kappa,np.zeros_like(model.phi))
        alpha = alphaf*np.sqrt(len(uvar)/Lpre)     # scale factor for aposteriori
        
        # run aposteriori minimisation
        # Methods: Nelder-Mead; CG; BFGS (default); L-BFGS-B; Powell
        uout=optimize.minimize(func1dAposteriori,uvar,tol=1e-10,method='BFGS',options={'disp':True,'maxiter':len(uvar)*2e3},\
                               args=(uknown,ufix,alpha,dx,model.eta,model.kappa,model.phi,model.isodu,model.odu,model.seodu,model.isoe,model.oe,model.seoe))
        model.um1[ufix]=uknown
        model.um1[~ufix]=uout.x[:nunk]
        model.up1=model.um1+0.
        model.up1[faults]=uout.x[nunk:]
    
def func1dAposteriori(uvar,uknown,ufix,alpha,dx,eta,kappa,phi,isodu=False,odu=False,seodu=False,\
                      isoe=False,oe=False,seoe=False):
    """
    Function to calculate the value of the 1D version of the
     aposteriori functional.
    """
    import numpy as np
    
    ns=len(eta)
    nknown=int(len(uknown))
    nunk = ns+1-nknown      # number of points with unknown velocity
    faults=kappa != 0.
    um=np.zeros((ns+1))
    um[ufix]=uknown
    um[~ufix]=uvar[:nunk]
    up=um+0.
    up[faults]=uvar[nunk:]
    
#    du=np.zeros((ns+1))
#    strain=np.zeros((ns))
    # F: contribution from faults
    du=up-um
    F=np.sum(np.divide((du-phi*kappa)**2,kappa,where=faults))
#    for p in range(ns+1):
#        if kappa[p] != 0:
#            du[p]=up[p]-um[p]    # velocity change across fault
#            F+=(du[p]-kappa[p]*phi[p])**2/kappa[p]       # contribution to functional
    
    # E: contribution from elements/segments
    strain=(um[1:]-up[:-1])/dx
    phis=(phi[:-1]+phi[1:])/2
    E=np.sum((strain-phis*eta)**2/eta*dx)
#    for s in range(ns):
#        strain[s]=(um[s+1]-up[s])/dx[s]
#        phis=(phi[s]+phi[s+1])/2
#        E+=(strain[s]-eta[s]*phis)**2/eta[s]
    
    L=alpha*(E+F)       # total functional
    
    # contribution from slip-rate observations
    L+=np.sum(np.divide((odu-du)*isodu,seodu,where=isodu)**2)
#    for p in np.arange(ns+1)[isodu]:
#        L+=((odu[p]-du[p])/seodu[p])**2
    
    # contribution from strain-rate observations
    L+=np.sum(np.divide((oe-strain)*isoe,seoe,where=isoe)**2)
#    for s in np.arange(ns)[isoe]:
#        L+=((oe[s]-strain[s])/seoe[s])**2
    
    return L
        

























