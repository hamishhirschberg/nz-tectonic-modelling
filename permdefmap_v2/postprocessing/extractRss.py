#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb  4 16:24:13 2020

@author: hamish
"""

def extractRss(soln):
    """Returns the RSS for a given solution.
    
    Function to extract the residual sum of squares (RSS) of the
    observations from the apriori and aposteriori misfit to
    observations log files.
    
    Parameters
    ----------
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'both' or 'b' (both the apriori and aposteriori)
    
    Returns
    -------
    rss : float or list
        RSS of the given solution. Returned as a single float if 
        only one of apriori or aposteriori solution chosen. Returned
        as a list if both solutions chosen.
    """
    soln = str(soln).lower()      # Convert to lowercase string
    if soln == '0' or soln == 'apriori' or soln == 'apri':
        file = 'misfit_to_observations_apriori.log'
    elif soln == '1' or soln == 'aposteriori' or soln == 'apost':
        file = 'misfit_to_observations_aposteriori.log'
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        # Both returns a priori and a posteriori solutions in a list
        rss0 = extractRss(0)
        rss1 = extractRss(1)
        return [rss0, rss1]
    elif soln == 'd' or soln == 'diff' or soln == 'difference':
        raise Exception('Difference between solutions not applicable here')
        return
    elif soln == 'c' or soln == 'con' or soln == 'constraint':
        raise Exception('Constraint part of solution not applicable here')
        return
    else:
        print('Solution '+str(soln)+' not recognised')
        return
    
    fdat = open(file, 'r')
    fdat.readline()
    fdat.readline()
    rss = fdat.readline().split()[6]
    return float(rss)

def extractObsMisfit(model,soln):
    """Loads observation misfits into model.
    
    Extracts the misfits of individual observations from the apriori and
    aposteriori misfit to observations log files. It then loads the
    misfit into the permdefmap model 'model'.
    
    Currently, only deals with regular observations but not
    observational correlations.
    
    Parameters
    ----------
    model : permdefmap model
        Load misfits into this model.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'both' or 'b' (both the apriori and aposteriori)
    """
    
    if str(soln) == '0' or soln == 'apriori' or soln == 'apri':
        file = 'misfit_to_observations_apriori.log'
        misf = open(file,'r')
        misf.readline()
        misf.readline()
        model.rss0 = float(misf.readline().split()[6])
        misf.readline()
        misf.readline()
        line = misf.readline().split()
        
        # Velocity observations
        if len(line) > 0 and line[1] == 'Velocity':
            nvelob = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nvelob):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    line = misf.readline().split()
                    model.velobxmis0[i] = float(line[3])
                    line = misf.readline().split()
                    model.velobymis0[i] = float(line[3])
            line = misf.readline().split()
        
        # Slip rate observations
        if len(line) > 0 and line[1:3] == ['Slip-rate', 'Observations']:
            nslipob = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nslipob):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    nslipobcomp = int(line[3])
                    for j in range(nslipobcomp):
                        line = misf.readline().split()
                        model.slipobmis0[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Strain rate observations
        if len(line) > 0 and line[1:3] == ['Strain-rate', 'Observations']:
            nstrainob = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nstrainob):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    nstrainobcomp = int(line[2])
                    for j in range(nstrainobcomp):
                        line = misf.readline().split()
                        model.strainobmis0[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Slip rate correlations
        if len(line) > 0 and line[1:3] == ['Slip-rate', 'Correlation']:
            nsliplink = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nsliplink):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    nsliplinkcomp = int(line[2])
                    for j in range(nsliplinkcomp):
                        line = misf.readline().split()
                        model.sliplinkmis0[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Strain rate correlations
        if len(line) > 0 and line[1:3] == ['Strain-rate', 'Correlation']:
            nstrainlink = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nstrainlink):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    nstrainlinkcomp = int(line[2])
                    for j in range(nstrainlinkcomp):
                        line = misf.readline().split()
                        model.strainlinkmis0[j,i] = float(line[3])
            line = misf.readline().split()
    
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        file = 'misfit_to_observations_aposteriori.log'
        misf = open(file,'r')
        misf.readline()
        misf.readline()
        model.rss1 = float(misf.readline().split()[6])
        misf.readline()
        misf.readline()
        line = misf.readline().split()
        
        if len(line) > 0 and line[1] == 'Velocity':
            nvelob = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nvelob):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    line = misf.readline().split()
                    model.velobxmis1[i].append(float(line[3]))
                    line = misf.readline().split()
                    model.velobymis1[i].append(float(line[3]))
            line = misf.readline().split()
        
        if len(line) > 0 and line[1] == 'Slip-rate':
            nslipob = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nslipob):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    nslipobcomp = int(line[3])
                    for j in range(nslipobcomp):
                        line = misf.readline().split()
                        model.slipobmis1[j,i] = float(line[3])
            line = misf.readline().split()
        
        if len(line) > 0 and line[1] == 'Strain-rate':
            nstrainob = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nstrainob):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    nstrainobcomp = int(line[2])
                    for j in range(nstrainobcomp):
                        line = misf.readline().split()
                        model.strainobmis1[j,i] = float(line[3])
            line = misf.readline().split()
    
        # Slip rate correlations
        if len(line) > 0 and line[1:3] == ['Slip-rate', 'Correlation']:
            nsliplink = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nsliplink):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    nsliplinkcomp = int(line[2])
                    for j in range(nsliplinkcomp):
                        line = misf.readline().split()
                        model.sliplinkmis1[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Strain rate correlations
        if len(line) > 0 and line[1:3] == ['Strain-rate', 'Correlation']:
            nstrainlink = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nstrainlink):
                line = misf.readline().split()
                index = int(line[0])
                if index == i + 1:
                    nstrainlinkcomp = int(line[2])
                    for j in range(nstrainlinkcomp):
                        line = misf.readline().split()
                        model.strainlinkmis1[j,i] = float(line[3])
            line = misf.readline().split()
    
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        extractObsMisfit(model, '0')
        extractObsMisfit(model, '1')
    elif soln == 'd' or soln == 'diff' or soln == 'difference':
        raise Exception('Difference between solutions not applicable here')
        return
    elif soln == 'c' or soln == 'con' or soln == 'constraint':
        raise Exception('Constraint part of solution not applicable here')
        return
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
        return

def strainStyleMisfit(model, soln, zerotol=1e-9, scale=1., obrange=None,
                          style=None):
    """
    Calculate the misfit to the strain rate style.
    
    Parameters
    ----------
    model : permdefmap model
        Calculate strain style misfit for this model.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'both' or 'b' (both the apriori and aposteriori)
    zerotol : float, defualt=1e-9
        Strains smaller than this are considered zero and are assigned
        a misfit of zero.
    scale : float, default=1.
        Scale misfit by inverse of this factor, equivalent to standard
        error. Defualt is no scaling (i.e. absolute, not relative value).
    obrange : iterable, default=None
        Range of observations to be considered. Default is all
        observations.
    style : array of float
        Strain rate style of all observations (including those not
        considered). Default calculates style from strain rate
        observations assuming components are xx, yy, and xy.
    """
    import numpy as np
    
    # Set range of observations to all if not set
    if obrange is None:
        obrange = range(model.nstrainob)
    
    # Calculate style if not set, assuming components are xx, yy, and xy
    if style is None:
        dil = (model.strainobvalue[0,:model.nstrainob]
               + model.strainobvalue[1,:model.nstrainob])
        mag = np.sqrt(model.strainobvalue[0,:model.nstrainob] ** 2
                      + model.strainobvalue[1,:model.nstrainob] ** 2
                      + 2 * model.strainobvalue[2,:model.nstrainob] ** 2)
        style = dil / mag            
    
    # Choose part of solution and find predicted style
    if str(soln) == '0' or soln == 'apri' or soln == 'apriori':
        dil = model.strainxx0[:model.nel] + model.strainyy0[:model.nel]
        mag = np.sqrt(model.strainxx0[:model.nel]**2
                      + model.strainyy0[:model.nel]**2
                      + 2 * model.strainxy0[:model.nel]**2)
        preds = dil / mag
        file = open('misfit_to_strain_style_apriori.dat','w')
    elif str(soln) == '1' or soln == 'apost' or soln == 'aposteriori':
        dil = model.strainxx1[:model.nel] + model.strainyy1[:model.nel]
        mag = np.sqrt(model.strainxx1[:model.nel]**2
                      + model.strainyy1[:model.nel]**2
                      + 2 * model.strainxy1[:model.nel]**2)
        preds = dil / mag
        file = open('misfit_to_strain_style_aposteriori.dat','w')
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        mis0 = strainStyleMisfit(model, '0', zerotol, scale, obrange, style)
        mis1 = strainStyleMisfit(model, '1', zerotol, scale, obrange, style)
        return [mis0, mis1]
    elif soln == 'd' or soln == 'diff' or soln == 'difference':
        raise Exception('Difference between solutions not applicable here')
        return
    elif soln == 'c' or soln == 'con' or soln == 'constraint':
        raise Exception('Constraint part of solution not applicable here')
        return
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
        return
    
    # Misfit to strain style
    mis = np.zeros((model.nstrainob))
    
    for i in obrange:
        e = model.elofstrainob[i]
        if np.isnan(style[i]):
            # If observation is not zero, perform check relative to zerotol
                mis[i] = 0
        else:
            # Find difference between observed and modelled styles
            mis[i] = (style[i]-preds[e]) / scale
        # Write to file
        file.write(str(model.ellong[e])+'\t'+str(model.ellat[e])+'\t'
                   +str(mis[i])+'\n')
    
    # Finish up
    file.close()
    return np.sum(mis**2)
    
def faultCentreRss(model, soln):
    """Calculate RSS of observations on central segments of subfaults.
    
    Parameters
    ----------
    model : permdefmap model
        Calculate RSS for this model.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'both' or 'b' (both the apriori and aposteriori)
    """
    
    import numpy as np
    from ..geometry import faultObCentres
    
    if str(soln) == '0' or soln == 'apriori' or soln == 'apri':
        # Apriori slip rate
        file = open('collapsed_sliprate_misfit_apriori.dat', 'w')
        obmis = model.slipobmis0
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        # Aposteriori slip rate
        file = open('collapsed_sliprate_misfit_aposteriori.dat', 'w')
        obmis = model.slipobmis1
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        # Calculate each solution in turn
        faultCentreRss(model, '0')
        faultCentreRss(model, '1')
        return
    # Other parts of solution not applicable
    elif soln == 'd' or soln == 'diff' or soln == 'difference':
        print('Difference between solutions not applicable here')
        return
    elif soln == 'c' or soln == 'con' or soln == 'constraint':
        print('Constraint part of solution not applicable here')
        return
    else:
        print('Solution '+str(soln)+' not recognised')
        return
    
    # Loop through observtions on subfault centres
    midobi = faultObCentres(model)
    for i in midobi:
        s = model.sideofslipob[i]
        gp1 = model.gp1onside[s]
        gp2 = model.gp2onside[s]
        
        # Write info to file
        file.write(str((model.gplong[gp1]+model.gplong[gp2])/2) + '\t')
        file.write(str((model.gplat[gp1]+model.gplat[gp2])/2) + '\t')
        file.write(str(obmis[0,i]) + '\t' + str(obmis[1,i]) + '\t')
        file.write(str(np.hypot(obmis[0,i], obmis[1,i])) + '\n')
    
    file.close()
    
def collapseSlipRss(model,soln,includeEnds=True):
    """Calculate RSS of slip rates with repeated observations collapsed
    
    Function to calculate the RSS where any observations applied to more
    than one fault segment is averaged over those segments. This function
    only considers the RSS from fault slip-rate observations. It assumes
    repeated observations will be adjacent in the model.
    
    The function takes the mean of the squared misfit for any observations that
     have the same first component value and are on the same fault.
    """
    import numpy as np
    
    if str(soln) == '0' or soln == 'apriori' or soln == 'apri':
        rss=0.
        fault=0
        val=np.array([0.,0.])
        nobs=0
        ncopy=1
        missq=0.
        for i in range(model.nslipob):
            s=model.sfo[i]
            if includeEnds or not (model.fss[s-1]==1 or model.fss[s-1]==model.nfs[fault-1]):
                if model.ffo[i]==fault and np.all(model.oduc[:,i]==val):
                    # if same as previous observation, add to that observation
                    ncopy += 1
                    missq += model.slipobmis0[0,i]**2 + model.slipobmis0[1,i]**2
                else:
                    # if different to previous observation, clear out previous
                    rss += missq/ncopy
                    # load new observation
                    fault = model.ffo[i]
                    val = model.oduc[:,i]
                    ncopy = 1
                    missq = model.slipobmis0[0,i]**2 + model.slipobmis0[1,i]**2
                    nobs += model.nslipobcomp[i]
        # deal with final observation
        rss += missq/ncopy
        return rss,nobs
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        rss=0.
        fault=0
        val=np.array([0.,0.])
        nobs=0
        ncopy=1
        missq=0.
        for i in range(model.nslipob):
            s=model.sfo[i]
            if includeEnds or not (model.fss[s-1]==1 or model.fss[s-1]==model.nfs[fault-1]):
                if model.ffo[i]==fault and np.all(model.oduc[:,i]==val):
                    # if same as previous observation, add to that observation
                    ncopy += 1
                    missq += model.slipobmis1[0,i]**2 + model.slipobmis1[1,i]**2
                else:
                    # if different to previous observation, clear out previous
                    rss += missq/ncopy
                    # load new observation
                    fault = model.ffo[i]
                    val = model.oduc[:,i]
                    ncopy = 1
                    missq = model.slipobmis1[0,i]**2 + model.slipobmis1[1,i]**2
                    nobs += model.nslipobcomp[i]
        # deal with final observation
        rss += missq/ncopy
        return rss,nobs
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        print('To get both apriori and aposteriori collapsed RSS, please run separately')
    else:
        print('Solution '+str(soln)+' not recognised')
    
def printCollapseSliprateObs(model,soln,includeEnds=True,conv=1000,ave='hypot'):
    """Calculate RSS of slip rates with repeated observations collapsed
    
    Prints the location of slip rate observations and misfits where only
    the central observation of a set of identical observations is
    considered. It can also return the index of the central observations.
    Function to print the location of slip rate observation and misfits where
     only the central observation of a set of identical observations is
     considered.
    
    includeEnds includes any observations that fall at a fault terminaion.
    conv:: conversion factor for slip rates. Default=1000 converts m/yr to mm/yr
    ave:: method of calculating average misfit. hypot takes mean of 
           sqrt(comp1**2+comp2**2). chi takes mean of comp**2.
    """
    
    import numpy as np
    
    if str(soln) == '0' or soln == 'apriori' or soln == 'apri':
        file=open('collapsed_sliprate_misfit_apriori.dat','w')
        fault=model.ffo[0]
        val=model.oduc[:,0]
        mis=[[model.slipobmis0[0,0],model.slipobmis0[1,0]]]
        fs=[model.sfo[0]]
        allmis=[]
        hassubf=model.subfname[0]!=''
        for i in range(1,model.nslipob):
            if model.ffo[i]==fault and np.all(model.oduc[:,i]==val):
                # if same as previous observation, add to that observation
                mis.append([model.slipobmis0[0,i],model.slipobmis0[1,i]])
                fs.append(model.sfo[i])
            else:
                # if different to previous observation, find previous middle segment
                mid=int(len(fs)/2)
                s=fs[mid]
                seg=model.fss[s-1]                
                if includeEnds or not (model.fss[s-1]==1 or model.fss[s-1]==model.nfs[fault-1]):
                    file.write(str((model.long[model.gp1s[s-1]-1]+model.long[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str((model.lat[model.gp1s[s-1]-1]+model.lat[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str(mis[mid][0])+'\t'+str(mis[mid][1])+'\t')
                    file.write(str(np.hypot(mis[mid][0],mis[mid][1]))+'\t')
                    file.write(str(np.sum(val[:]*model.codut[:,i-1])*conv)+'\t')
                    file.write(str(np.sum(val[:]*model.codun[:,i-1])*conv)+'\t')
                    file.write(str(model.dut0[seg-1,fault-1]*conv)+'\t')
                    file.write(str(model.dun0[seg-1,fault-1]*conv)+'\t')
                    if hassubf:
                        file.write(model.subfname[model.subfs[s-1]-1]+'\n')
                    else:
                        file.write(model.fname[fault-1]+'\n')
                    allmis.append(mis[mid])
                # load new observation
                fault = model.ffo[i]
                val = model.oduc[:,i]
                mis = [[model.slipobmis0[0,i],model.slipobmis0[1,i]]]
                fs = [model.sfo[i]]
        # deal with final observation
        mid=int(len(fs)/2)
        s=fs[mid]
        seg=model.fss[s-1]
        if includeEnds or not (model.fss[s-1]==1 or model.fss[s-1]==model.nfs[fault-1]):
            file.write(str((model.long[model.gp1s[s-1]-1]+model.long[model.gp2s[s-1]-1]))+'\t')
            file.write(str((model.lat[model.gp1s[s-1]-1]+model.lat[model.gp2s[s-1]-1]))+'\t')
            file.write(str(mis[mid][0])+'\t'+str(mis[mid][1])+'\t')
            file.write(str(np.hypot(mis[mid][0],mis[mid][1]))+'\t')
            file.write(str(np.sum(val[:]*model.codut[:,i-1])*conv)+'\t')
            file.write(str(np.sum(val[:]*model.codun[:,i-1])*conv)+'\t')
            file.write(str(model.dut0[seg-1,fault-1]*conv)+'\t')
            file.write(str(model.dun0[seg-1,fault-1]*conv)+'\t')
            if hassubf:
                file.write(model.subfname[model.subfs[s-1]-1]+'\n')
            else:
                file.write(model.fname[fault-1])
            allmis.append(mis[mid])
        file.close()
        allmisa=np.array(allmis)
        if ave=='hypot':
            return np.mean(np.hypot(allmisa[:,0],allmisa[:,1]))
        elif ave=='chi':
            return np.mean(allmisa**2)
        else:
            print('Average style not recognised. Returning all misfits.')
            return allmisa
    
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        file=open('collapsed_sliprate_misfit_aposteriori.dat','w')
        fault=model.ffo[0]
        val=model.oduc[:,0]
        mis=[[model.slipobmis1[0,0],model.slipobmis1[1,0]]]
        fs=[model.sfo[0]]
        allmis=[]
        hassubf=model.subfname[0]!=''
        for i in range(1,model.nslipob):
            if model.ffo[i]==fault and np.all(model.oduc[:,i]==val):
                # if same as previous observation, add to that observation
                mis.append([model.slipobmis1[0,i],model.slipobmis1[1,i]])
                fs.append(model.sfo[i])
            else:
                # if different to previous observation, find previous middle segment
                mid=int(len(fs)/2)
                s=fs[mid]
                seg=model.fss[s-1]
                if includeEnds or not (model.fss[s-1]==1 or model.fss[s-1]==model.nfs[fault-1]):
                    file.write(str((model.long[model.gp1s[s-1]-1]+model.long[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str((model.lat[model.gp1s[s-1]-1]+model.lat[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str(mis[mid][0])+'\t'+str(mis[mid][1])+'\t')
                    file.write(str(np.hypot(mis[mid][0],mis[mid][1]))+'\t')
                    file.write(str(np.sum(val[:]*model.codut[:,i-1])*conv)+'\t')
                    file.write(str(np.sum(val[:]*model.codun[:,i-1])*conv)+'\t')
                    file.write(str(model.dut1[seg-1,fault-1]*conv)+'\t')
                    file.write(str(model.dun1[seg-1,fault-1]*conv)+'\t')
                    if hassubf:
                        file.write(model.subfname[model.subfs[s-1]-1]+'\n')
                    else:
                        file.write(model.fname[fault-1]+'\n')
                    allmis.append(mis[mid])
                # load new observation
                fault = model.ffo[i]
                val = model.oduc[:,i]
                mis = [[model.slipobmis1[0,i],model.slipobmis1[1,i]]]
                fs = [model.sfo[i]]
        # deal with final observation
        mid=int(len(fs)/2)
        s=fs[mid]
        seg=model.fss[s-1]
        if includeEnds or not (model.fss[s-1]==1 or model.fss[s-1]==model.nfs[fault-1]):
            file.write(str((model.long[model.gp1s[s-1]-1]+model.long[model.gp2s[s-1]-1]))+'\t')
            file.write(str((model.lat[model.gp1s[s-1]-1]+model.lat[model.gp2s[s-1]-1]))+'\t')
            file.write(str(mis[mid][0])+'\t'+str(mis[mid][1])+'\t')
            file.write(str(np.hypot(mis[mid][0],mis[mid][1]))+'\t')
            file.write(str(np.sum(val[:]*model.codut[:,i-1])*conv)+'\t')
            file.write(str(np.sum(val[:]*model.codun[:,i-1])*conv)+'\t')
            file.write(str(model.dut0[seg-1,fault-1]*conv)+'\t')
            file.write(str(model.dun0[seg-1,fault-1]*conv)+'\t')
            if hassubf:
                file.write(model.subfname[model.subfs[s-1]-1]+'\n')
            else:
                file.write(model.fname[fault-1])
            allmis.append(mis[mid])
        file.close()
        allmisa=np.array(allmis)
        if ave=='hypot':
            return np.mean(np.hypot(allmisa[:,0],allmisa[:,1]))
        elif ave=='chi':
            return np.mean(allmisa**2)
        else:
            print('Average style not recognised. Returning all misfits.')
            return allmisa
    
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        mis0=printCollapseSliprateObs(model,'0',includeEnds,conv,ave)
        mis1=printCollapseSliprateObs(model,'1',includeEnds,conv,ave)
        return [mis0,mis1]
    else:
        print('Solution '+str(soln)+' not recognised')

def printCollapseSliprateObsAve(model,soln,includeEnds=True,conv=1000,ave='hypot'):
    """
    Function to print the location of slip rate observation and misfits where
     only the central observation of a set of identical observations is
     considered.
    
    includeEnds includes any observations that fall at a fault terminaion.
    conv:: conversion factor for slip rates. Default=1000 converts m/yr to mm/yr
    ave:: method of calculating average misfit. hypot takes mean of 
           sqrt(comp1**2+comp2**2). chi takes mean of comp**2.
    """
    
    import numpy as np
    
    if str(soln) == '0' or soln == 'apriori' or soln == 'apri':
        file=open('collapsed_sliprate_misfit_apriori.dat','w')
        fault=model.ffo[0]
        val=model.oduc[:,0]
        mis=[[model.slipobmis0[0,0],model.slipobmis0[1,0]]]
        fs=[model.sfo[0]]
        allmis=[]
        hassubf=model.subfname[0]!=''
        for i in range(1,model.nslipob):
            if model.ffo[i]==fault and np.all(model.oduc[:,i]==val):
                # if same as previous observation, add to that observation
                mis.append([model.slipobmis0[0,i],model.slipobmis0[1,i]])
                fs.append(model.sfo[i])
            else:
                # if different to previous observation, find previous middle segment
                if len(fs)>2:
                    mid=int(len(fs)/2)
                    s=fs[mid]
                    seg=model.fss[np.array(fs[1:-1])-1]
                    avemis=np.mean(mis,axis=0)
                    
                    file.write(str((model.long[model.gp1s[s-1]-1]+model.long[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str((model.lat[model.gp1s[s-1]-1]+model.lat[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str(avemis[0])+'\t'+str(avemis[1])+'\t')
                    file.write(str(np.hypot(avemis[0],avemis[1]))+'\t')
                    file.write(str(np.sum(val[:]*model.codut[:,i-1])*conv)+'\t')
                    file.write(str(np.sum(val[:]*model.codun[:,i-1])*conv)+'\t')
                    file.write(str(np.mean(model.dut0[seg-1,fault-1])*conv)+'\t')
                    file.write(str(np.mean(model.dun0[seg-1,fault-1])*conv)+'\t')
                    if hassubf:
                        file.write(model.subfname[model.subfs[s-1]-1]+'\n')
                    else:
                        file.write(model.fname[fault-1]+'\n')
                    allmis.append(mis[mid])
                # load new observation
                fault = model.ffo[i]
                val = model.oduc[:,i]
                mis = [[model.slipobmis0[0,i],model.slipobmis0[1,i]]]
                fs = [model.sfo[i]]
        # deal with final observation
        if len(fs)>2:
            mid=int(len(fs)/2)
            s=fs[mid]
            seg=model.fss[np.array(fs[1:-1])-1]
            avemis=np.mean(mis,axis=0)
            
            file.write(str((model.long[model.gp1s[s-1]-1]+model.long[model.gp2s[s-1]-1])/2)+'\t')
            file.write(str((model.lat[model.gp1s[s-1]-1]+model.lat[model.gp2s[s-1]-1])/2)+'\t')
            file.write(str(avemis[0])+'\t'+str(avemis[1])+'\t')
            file.write(str(np.hypot(avemis[0],avemis[1]))+'\t')
            file.write(str(np.sum(val[:]*model.codut[:,i-1])*conv)+'\t')
            file.write(str(np.sum(val[:]*model.codun[:,i-1])*conv)+'\t')
            file.write(str(np.mean(model.dut0[seg-1,fault-1])*conv)+'\t')
            file.write(str(np.mean(model.dun0[seg-1,fault-1])*conv)+'\t')
            if hassubf:
                file.write(model.subfname[model.subfs[s-1]-1]+'\n')
            else:
                file.write(model.fname[fault-1])
            allmis.append(mis[mid])
        file.close()
        allmisa=np.array(allmis)
        if ave=='hypot':
            return np.mean(np.hypot(allmisa[:,0],allmisa[:,1]))
        elif ave=='chi':
            return np.mean(allmisa**2)
        else:
            print('Average style not recognised. Returning all misfits.')
            return allmisa
    
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        file=open('collapsed_sliprate_misfit_aposteriori.dat','w')
        fault=model.ffo[0]
        val=model.oduc[:,0]
        mis=[[model.slipobmis1[0,0],model.slipobmis1[1,0]]]
        fs=[model.sfo[0]]
        allmis=[]
        hassubf=model.subfname[0]!=''
        for i in range(1,model.nslipob):
            if model.ffo[i]==fault and np.all(model.oduc[:,i]==val):
                # if same as previous observation, add to that observation
                mis.append([model.slipobmis1[0,i],model.slipobmis1[1,i]])
                fs.append(model.sfo[i])
            else:
                # if different to previous observation, find previous middle segment
                if len(fs)>2:
                    mid=int(len(fs)/2)
                    s=fs[mid]
                    seg=model.fss[np.array(fs[1:-1])-1]
                    avemis=np.mean(mis,axis=0)
                    
                    file.write(str((model.long[model.gp1s[s-1]-1]+model.long[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str((model.lat[model.gp1s[s-1]-1]+model.lat[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str(avemis[0])+'\t'+str(avemis[1])+'\t')
                    file.write(str(np.hypot(avemis[0],avemis[1]))+'\t')
                    file.write(str(np.sum(val[:]*model.codut[:,i-1])*conv)+'\t')
                    file.write(str(np.sum(val[:]*model.codun[:,i-1])*conv)+'\t')
                    file.write(str(np.mean(model.dut1[seg-1,fault-1])*conv)+'\t')
                    file.write(str(np.mean(model.dun1[seg-1,fault-1])*conv)+'\t')
                    if hassubf:
                        file.write(model.subfname[model.subfs[s-1]-1]+'\n')
                    else:
                        file.write(model.fname[fault-1]+'\n')
                    allmis.append(mis[mid])
                # load new observation
                fault = model.ffo[i]
                val = model.oduc[:,i]
                mis = [[model.slipobmis1[0,i],model.slipobmis1[1,i]]]
                fs = [model.sfo[i]]
        # deal with final observation
        if len(fs)>2:
            mid=int(len(fs)/2)
            s=fs[mid]
            seg=model.fss[np.array(fs[1:-1])-1]
            avemis=np.mean(mis,axis=0)
            
            file.write(str((model.long[model.gp1s[s-1]-1]+model.long[model.gp2s[s-1]-1])/2)+'\t')
            file.write(str((model.lat[model.gp1s[s-1]-1]+model.lat[model.gp2s[s-1]-1])/2)+'\t')
            file.write(str(avemis[0])+'\t'+str(avemis[1])+'\t')
            file.write(str(np.hypot(avemis[0],avemis[1]))+'\t')
            file.write(str(np.sum(val[:]*model.codut[:,i-1])*conv)+'\t')
            file.write(str(np.sum(val[:]*model.codun[:,i-1])*conv)+'\t')
            file.write(str(np.mean(model.dut1[seg-1,fault-1])*conv)+'\t')
            file.write(str(np.mean(model.dun1[seg-1,fault-1])*conv)+'\t')
            if hassubf:
                file.write(model.subfname[model.subfs[s-1]-1]+'\n')
            else:
                file.write(model.fname[fault-1])
            allmis.append(mis[mid])
        file.close()
        allmisa=np.array(allmis)
        if ave=='hypot':
            return np.mean(np.hypot(allmisa[:,0],allmisa[:,1]))
        elif ave=='chi':
            return np.mean(allmisa**2)
        else:
            print('Average style not recognised. Returning all misfits.')
            return allmisa
    
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        mis0=printCollapseSliprateObs(model,'0',includeEnds,conv,ave)
        mis1=printCollapseSliprateObs(model,'1',includeEnds,conv,ave)
        return [mis0,mis1]
    else:
        print('Solution '+str(soln)+' not recognised')
    
def collapseSliprateObs(model,soln):
    """
    Function to extract the slip rate observations and misfits where
     only the central observation of a set of identical observations is
     considered.
    """
    
    import numpy as np
    
    if str(soln) == '0' or soln == 'apriori' or soln == 'apri':
        fault=model.ffo[0]
        val=model.oduc[:,0]
        fs=[model.sfo[0]]
        mis=[[fault,model.sfo[0],val,model.seoduc[:,0],model.slipobmis0[:,0]]]
        allmis=[]
        for i in range(1,model.nslipob):
            if model.ffo[i]==fault and np.all(model.oduc[:,i]==val):
                # if same as previous observation, add to that observation
                fs.append(model.sfo[i])
                mis.append([fault,model.sfo[i],val,model.seoduc[:,i],model.slipobmis0[:,i]])
            else:
                # if different to previous observation, find previous middle segment
                allmis.append(mis[int(len(fs)/2)])
                # load new observation
                fault = model.ffo[i]
                val = model.oduc[:,i]
                fs = [model.sfo[i]]
                mis = [[fault,model.sfo[i],val,model.seoduc[:,i],model.slipobmis0[:,i]]]
        # deal with final observation
        allmis.append(mis[int(len(fs)/2)])
        return allmis
    
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        fault=model.ffo[0]
        val=model.oduc[:,0]
        fs=[model.sfo[0]]
        mis=[[fault,model.sfo[0],val,model.seoduc[:,0],model.slipobmis1[:,0]]]
        allmis=[]
        for i in range(1,model.nslipob):
            if model.ffo[i]==fault and np.all(model.oduc[:,i]==val):
                # if same as previous observation, add to that observation
                fs.append(model.sfo[i])
                mis.append([fault,model.sfo[i],val,model.seoduc[:,i],model.slipobmis1[:,i]])
            else:
                # if different to previous observation, find previous middle segment
                allmis.append(mis[int(len(fs)/2)])
                # load new observation
                fault = model.ffo[i]
                val = model.oduc[:,i]
                fs = [model.sfo[i]]
                mis = [[fault,model.sfo[i],val,model.seoduc[:,i],model.slipobmis1[:,i]]]
        # deal with final observation
        allmis.append(mis[int(len(fs)/2)])
        return allmis
    
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        allmis0=collapseSliprateObs(model,'0')
        allmis1=collapseSliprateObs(model,'1')
        return [allmis0,allmis1]
    else:
        print('Solution '+str(soln)+' not recognised')
        return
    
    
def calcStrainStyleSmoothMisfit(model,soln,width,space=None,zerotol=1e-9,scale=1,obrange=None):
    """
    Function to calculate the misfit to the strain rate style.
    
    zerotol:: strains smaller than this are considered zero
    scale:: scale misfit by inverse of this factor, equivalent to standard error
    obrange:: range of observations to be considered
    """
    import numpy as np
    from .readElements import readElements
    from ..misc import smoothStrainStyle
    
    if obrange is None:
        obrange=range(model.nstrainob)
    
    if str(soln)=='0' or soln=='apri' or soln=='apriori':
        if model.e110[0]==0.:
            readElements(model,'0')
        preds=smoothStrainStyle(model,'0',width,space)
        file=open('misfit_to_strain_style_smooth_apriori.dat','w')
    elif str(soln)=='1' or soln=='apost' or soln=='aposteriori':
        if model.e111[0]==0.:
            readElements(model,'1')
        preds=smoothStrainStyle(model,'1',width,space)
        file=open('misfit_to_strain_style_smooth_aposteriori.dat','w')
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        calcStrainStyleSmoothMisfit(model,'0',width,space,zerotol,scale,obrange)
        calcStrainStyleSmoothMisfit(model,'1',width,space,zerotol,scale,obrange)
        return
    else:
        print('Solution not recognised.')
        return
        
    mis=np.zeros((len(obrange)))
    
    for i in obrange:
        e=model.eeo[i]
        # assume observations are perpendicular components ii, jj, and ij
        # ignore observations of zero
        if model.oec[0,i]+model.oec[1,i]==0:
            mis[i]=0
        else:
            # find difference between observed and modelled styles
            style=(model.oec[0,i]+model.oec[1,i])/np.sqrt(model.oec[0,i]**2+model.oec[1,i]**2+2*model.oec[2,i]**2)
            mis[i]=(style-preds[e-1])/scale
        file.write(str(model.elong[e-1])+'\t'+str(model.elat[e-1])+'\t'+str(mis[i])+'\n')
    
    
    
    
    
    
    
    
    
    