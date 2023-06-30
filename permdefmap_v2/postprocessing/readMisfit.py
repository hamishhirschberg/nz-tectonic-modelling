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
        print('Difference between solutions not applicable here')
        return
    elif soln == 'c' or soln == 'con' or soln == 'constraint':
        print('Constraint part of solution not applicable here')
        return
    else:
        print('Solution '+str(soln)+' not recognised')
        return
    
    fdat = open(file, 'r')
    fdat.readline()
    fdat.readline()
    rss = fdat.readline().split()[6]
    return float(rss)

def readMisfit(model,soln):
    """Reads in observation misfits.
    
    Function to extract the misfits of individual observations from the
    apriori and aposteriori misfit to observations log files. It then loads the
    misfit into the permdefmap model 'model'.
    
    Parameters
    ----------
    model : permdefmap model
        Model into which the results are stored.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'both' or 'b' (both the apriori and aposteriori)
    """
    
    # A priori solution
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
            nvo = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nvo):
                line = misf.readline().split()
                index = int(line[0])
                if index == i+1:
                    line = misf.readline().split()
                    model.velobxmis0[i].append(float(line[3]))
                    line = misf.readline().split()
                    model.velobymis0[i].append(float(line[3]))
            line = misf.readline().split()
        
        # Slip rate observations
        if len(line) == 3 and line[1] == 'Slip-rate':
            nslipob = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nslipob):
                line = misf.readline().split()
                index = int(line[0])
                if index == i+1:
                    nslipobc = int(line[3])
                    for j in range(nslipobc):
                        line = misf.readline().split()
                        model.slipobmis0[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Strain rate observations
        if len(line) == 3 and line[1] == 'Strain-rate':
            neo = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(neo):
                line = misf.readline().split()
                index = int(line[0])
                if index == i+1:
                    neoc = int(line[2])
                    for j in range(neoc):
                        line = misf.readline().split()
                        model.strainobmis0[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Slip rate constraints
        if len(line) == 4 and line[1] == 'Slip-rate':
            nduc = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nduc):
                line = misf.readline().split()
                ncduc = int(line[2])
                for j in range(ncduc):
                    line = misf.readline().split()
                    model.sliplinkmis0[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Strain rate constraints
        if len(line) == 4 and line[1] == 'Strain-rate':
            nec = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nec):
                line = misf.readline().split()
                ncec = int(line[2])
                for j in range(ncec):
                    line = misf.readline().split()
                    model.strainlinkmis0[j,i] = float(line[3])
            line = misf.readline().split()
    
    # A posteriori solution
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        file='misfit_to_observations_aposteriori.log'
        misf = open(file,'r')
        misf.readline()
        misf.readline()
        model.rss1=float(misf.readline().split()[6])
        misf.readline()
        misf.readline()
        line = misf.readline().split()
        
        # Velocity observations
        if len(line) > 0 and line[1] == 'Velocity':
            nvo = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nvo):
                line = misf.readline().split()
                index = int(line[0])
                if index == i+1:
                    line = misf.readline().split()
                    model.velobxmis1[i].append(float(line[3]))
                    line = misf.readline().split()
                    model.velobymis1[i].append(float(line[3]))
            line = misf.readline().split()
        
        # Slip rate observations
        if len(line) > 0 and line[1] == 'Slip-rate':
            nslipob = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nslipob):
                line = misf.readline().split()
                index = int(line[0])
                if index == i+1:
                    nslipobc = int(line[3])
                    for j in range(nslipobc):
                        line = misf.readline().split()
                        model.slipobmis1[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Strain rate observations
        if len(line) > 0 and line[1] == 'Strain-rate':
            neo = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(neo):
                line = misf.readline().split()
                index = int(line[0])
                if index == i+1:
                    neoc = int(line[2])
                    for j in range(neoc):
                        line = misf.readline().split()
                        model.strainobmis1[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Slip rate constraints
        if len(line) == 4 and line[1] == 'Slip-rate':
            nduc = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nduc):
                line = misf.readline().split()
                ncduc = int(line[2])
                for j in range(ncduc):
                    line = misf.readline().split()
                    model.sliplinkmis1[j,i] = float(line[3])
            line = misf.readline().split()
        
        # Strain rate constraints
        if len(line) == 4 and line[1] == 'Strain-rate':
            nec = int(line[0])
            misf.readline()
            misf.readline()
            
            for i in range(nec):
                line = misf.readline().split()
                ncec = int(line[2])
                for j in range(ncec):
                    line = misf.readline().split()
                    model.strainlinkmis1[j,i] = float(line[3])
            line = misf.readline().split()
    
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        readMisfit(model, '0')
        readMisfit(model, '1')
    else:
        print('Solution '+str(soln)+' not recognised')
    
def collapseSliprateRss(model, soln, includeEnds=True):
    """
    Function to calculate the RSS where any observations applied to more than
    one fault segment have been removed. This function only considers the RSS
    from fault slip-rate observations. It assumes repeated observations will be
    next to each other in the model.
    
    The function takes the mean of the squared misfit for any observations that
     have the same first component value and are on the same fault.
    """
    import numpy as np
    
    if str(soln) == '0' or soln == 'apriori' or soln == 'apri':
        rss = 0.
        fault = 0
        val = np.array([0., 0.])
        nobs = 0
        ncopy = 1
        missq = 0.
        for i in range(model.nslipob):
            s = model.sideofslipob[i]
            if includeEnds or not (model.segonside[s] == 1
                                   or model.segonside[s] == model.nfs[fault]):
                if (model.faultofslipob[i] == fault
                    and np.all(model.slipobvalue[:,i] == val)):
                    # if same as previous observation, add to that observation
                    ncopy += 1
                    missq += model.slipobmis0[0,i]**2 + model.slipobmis0[1,i]**2
                else:
                    # if different to previous observation, clear out previous
                    rss += missq / ncopy
                    # load new observation
                    fault = model.faultofslipob[i]
                    val = model.slipobvalue[:,i]
                    ncopy = 1
                    missq = model.slipobmis0[0,i]**2 + model.slipobmis0[1,i]**2
                    nobs += model.nslipobc[i]
        # deal with final observation
        rss += missq / ncopy
        return rss, nobs
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        rss = 0.
        fault = 0
        val = np.array([0., 0.])
        nobs = 0
        ncopy = 1
        missq = 0.
        for i in range(model.nslipob):
            s = model.sideofslipob[i]
            if includeEnds or not (model.segonside[s] == 1
                                   or model.segonside[s] == model.nfs[fault]):
                if (model.faultofslipob[i] == fault
                    and np.all(model.slipobvalue[:,i] == val)):
                    # if same as previous observation, add to that observation
                    ncopy += 1
                    missq += model.slipobmis1[0,i]**2 + model.slipobmis1[1,i]**2
                else:
                    # if different to previous observation, clear out previous
                    rss += missq / ncopy
                    # load new observation
                    fault = model.faultofslipob[i]
                    val = model.slipobvalue[:,i]
                    ncopy = 1
                    missq = model.slipobmis1[0,i]**2 + model.slipobmis1[1,i]**2
                    nobs += model.nslipobc[i]
        # deal with final observation
        rss += missq/ncopy
        return rss, nobs
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        print('To get both apriori and aposteriori collapsed RSS, please run separately')
    else:
        print('Solution '+str(soln)+' not recognised')
    
def printCollapseSliprateObs(model,soln,includeEnds=True,conv=1000,ave='hypot'):
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
        fault=model.faultofslipob[0]
        val=model.slipobvalue[:,0]
        mis=[[model.slipobmis0[0,0],model.slipobmis0[1,0]]]
        fs=[model.sideofslipob[0]]
        allmis=[]
        hassubf=model.subfname[0]!=''
        for i in range(1,model.nslipob):
            if model.faultofslipob[i]==fault and np.all(model.slipobvalue[:,i]==val):
                # if same as previous observation, add to that observation
                mis.append([model.slipobmis0[0,i],model.slipobmis0[1,i]])
                fs.append(model.sideofslipob[i])
            else:
                # if different to previous observation, find previous middle segment
                mid=int(len(fs)/2)
                s=fs[mid]
                seg=model.segonside[s-1]                
                if includeEnds or not (model.segonside[s-1]==1 or model.segonside[s-1]==model.nfs[fault-1]):
                    file.write(str((model.gplong[model.gp1onside[s-1]-1]+model.gplong[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str((model.gplat[model.gp1onside[s-1]-1]+model.gplat[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str(mis[mid][0])+'\t'+str(mis[mid][1])+'\t')
                    file.write(str(np.hypot(mis[mid][0],mis[mid][1]))+'\t')
                    file.write(str(np.sum(val[:]*model.slipobcoefft[:,i-1])*conv)+'\t')
                    file.write(str(np.sum(val[:]*model.slipobcoeffn[:,i-1])*conv)+'\t')
                    file.write(str(model.slipt0[seg-1,fault-1]*conv)+'\t')
                    file.write(str(model.slipn0[seg-1,fault-1]*conv)+'\t')
                    if hassubf:
                        file.write(model.subfname[model.subfs[s-1]-1]+'\n')
                    else:
                        file.write(model.fname[fault-1]+'\n')
                    allmis.append(mis[mid])
                # load new observation
                fault = model.faultofslipob[i]
                val = model.slipobvalue[:,i]
                mis = [[model.slipobmis0[0,i],model.slipobmis0[1,i]]]
                fs = [model.sideofslipob[i]]
        # deal with final observation
        mid=int(len(fs)/2)
        s=fs[mid]
        seg=model.segonside[s-1]
        if includeEnds or not (model.segonside[s-1]==1 or model.segonside[s-1]==model.nfs[fault-1]):
            file.write(str((model.gplong[model.gp1onside[s-1]-1]+model.gplong[model.gp2s[s-1]-1]))+'\t')
            file.write(str((model.gplat[model.gp1onside[s-1]-1]+model.gplat[model.gp2s[s-1]-1]))+'\t')
            file.write(str(mis[mid][0])+'\t'+str(mis[mid][1])+'\t')
            file.write(str(np.hypot(mis[mid][0],mis[mid][1]))+'\t')
            file.write(str(np.sum(val[:]*model.slipobcoefft[:,i-1])*conv)+'\t')
            file.write(str(np.sum(val[:]*model.slipobcoeffn[:,i-1])*conv)+'\t')
            file.write(str(model.slipt0[seg-1,fault-1]*conv)+'\t')
            file.write(str(model.slipn0[seg-1,fault-1]*conv)+'\t')
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
        fault=model.faultofslipob[0]
        val=model.slipobvalue[:,0]
        mis=[[model.slipobmis1[0,0],model.slipobmis1[1,0]]]
        fs=[model.sideofslipob[0]]
        allmis=[]
        hassubf=model.subfname[0]!=''
        for i in range(1,model.nslipob):
            if model.faultofslipob[i]==fault and np.all(model.slipobvalue[:,i]==val):
                # if same as previous observation, add to that observation
                mis.append([model.slipobmis1[0,i],model.slipobmis1[1,i]])
                fs.append(model.sideofslipob[i])
            else:
                # if different to previous observation, find previous middle segment
                mid=int(len(fs)/2)
                s=fs[mid]
                seg=model.segonside[s-1]
                if includeEnds or not (model.segonside[s-1]==1 or model.segonside[s-1]==model.nfs[fault-1]):
                    file.write(str((model.gplong[model.gp1onside[s-1]-1]+model.gplong[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str((model.gplat[model.gp1onside[s-1]-1]+model.gplat[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str(mis[mid][0])+'\t'+str(mis[mid][1])+'\t')
                    file.write(str(np.hypot(mis[mid][0],mis[mid][1]))+'\t')
                    file.write(str(np.sum(val[:]*model.slipobcoefft[:,i-1])*conv)+'\t')
                    file.write(str(np.sum(val[:]*model.slipobcoeffn[:,i-1])*conv)+'\t')
                    file.write(str(model.slipt1[seg-1,fault-1]*conv)+'\t')
                    file.write(str(model.slipn1[seg-1,fault-1]*conv)+'\t')
                    if hassubf:
                        file.write(model.subfname[model.subfs[s-1]-1]+'\n')
                    else:
                        file.write(model.fname[fault-1]+'\n')
                    allmis.append(mis[mid])
                # load new observation
                fault = model.faultofslipob[i]
                val = model.slipobvalue[:,i]
                mis = [[model.slipobmis1[0,i],model.slipobmis1[1,i]]]
                fs = [model.sideofslipob[i]]
        # deal with final observation
        mid=int(len(fs)/2)
        s=fs[mid]
        seg=model.segonside[s-1]
        if includeEnds or not (model.segonside[s-1]==1 or model.segonside[s-1]==model.nfs[fault-1]):
            file.write(str((model.gplong[model.gp1onside[s-1]-1]+model.gplong[model.gp2s[s-1]-1]))+'\t')
            file.write(str((model.gplat[model.gp1onside[s-1]-1]+model.gplat[model.gp2onside[s-1]-1]))+'\t')
            file.write(str(mis[mid][0])+'\t'+str(mis[mid][1])+'\t')
            file.write(str(np.hypot(mis[mid][0],mis[mid][1]))+'\t')
            file.write(str(np.sum(val[:]*model.slipobcoefft[:,i-1])*conv)+'\t')
            file.write(str(np.sum(val[:]*model.slipobcoeffn[:,i-1])*conv)+'\t')
            file.write(str(model.slipt0[seg-1,fault-1]*conv)+'\t')
            file.write(str(model.slipn0[seg-1,fault-1]*conv)+'\t')
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
        fault=model.faultofslipob[0]
        val=model.slipobvalue[:,0]
        mis=[[model.slipobmis0[0,0],model.slipobmis0[1,0]]]
        fs=[model.sideofslipob[0]]
        allmis=[]
        hassubf=model.subfname[0]!=''
        for i in range(1,model.nslipob):
            if model.faultofslipob[i]==fault and np.all(model.slipobvalue[:,i]==val):
                # if same as previous observation, add to that observation
                mis.append([model.slipobmis0[0,i],model.slipobmis0[1,i]])
                fs.append(model.sideofslipob[i])
            else:
                # if different to previous observation, find previous middle segment
                if len(fs)>2:
                    mid=int(len(fs)/2)
                    s=fs[mid]
                    seg=model.segonside[np.array(fs[1:-1])-1]
                    avemis=np.mean(mis,axis=0)
                    
                    file.write(str((model.gplong[model.gp1onside[s-1]-1]+model.gplong[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str((model.gplat[model.gp1onside[s-1]-1]+model.gplat[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str(avemis[0])+'\t'+str(avemis[1])+'\t')
                    file.write(str(np.hypot(avemis[0],avemis[1]))+'\t')
                    file.write(str(np.sum(val[:]*model.slipobcoefft[:,i-1])*conv)+'\t')
                    file.write(str(np.sum(val[:]*model.slipobcoeffn[:,i-1])*conv)+'\t')
                    file.write(str(np.mean(model.slipt0[seg-1,fault-1])*conv)+'\t')
                    file.write(str(np.mean(model.slipn0[seg-1,fault-1])*conv)+'\t')
                    if hassubf:
                        file.write(model.subfname[model.subfs[s-1]-1]+'\n')
                    else:
                        file.write(model.fname[fault-1]+'\n')
                    allmis.append(mis[mid])
                # load new observation
                fault = model.faultofslipob[i]
                val = model.slipobvalue[:,i]
                mis = [[model.slipobmis0[0,i],model.slipobmis0[1,i]]]
                fs = [model.sideofslipob[i]]
        # deal with final observation
        if len(fs)>2:
            mid=int(len(fs)/2)
            s=fs[mid]
            seg=model.segonside[np.array(fs[1:-1])-1]
            avemis=np.mean(mis,axis=0)
            
            file.write(str((model.gplong[model.gp1onside[s-1]-1]+model.gplong[model.gp2s[s-1]-1])/2)+'\t')
            file.write(str((model.gplat[model.gp1onside[s-1]-1]+model.gplat[model.gp2s[s-1]-1])/2)+'\t')
            file.write(str(avemis[0])+'\t'+str(avemis[1])+'\t')
            file.write(str(np.hypot(avemis[0],avemis[1]))+'\t')
            file.write(str(np.sum(val[:]*model.slipobcoefft[:,i-1])*conv)+'\t')
            file.write(str(np.sum(val[:]*model.slipobcoeffn[:,i-1])*conv)+'\t')
            file.write(str(np.mean(model.slipt0[seg-1,fault-1])*conv)+'\t')
            file.write(str(np.mean(model.slipn0[seg-1,fault-1])*conv)+'\t')
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
        fault=model.faultofslipob[0]
        val=model.slipobvalue[:,0]
        mis=[[model.slipobmis1[0,0],model.slipobmis1[1,0]]]
        fs=[model.sideofslipob[0]]
        allmis=[]
        hassubf=model.subfname[0]!=''
        for i in range(1,model.nslipob):
            if model.faultofslipob[i]==fault and np.all(model.slipobvalue[:,i]==val):
                # if same as previous observation, add to that observation
                mis.append([model.slipobmis1[0,i],model.slipobmis1[1,i]])
                fs.append(model.sideofslipob[i])
            else:
                # if different to previous observation, find previous middle segment
                if len(fs)>2:
                    mid=int(len(fs)/2)
                    s=fs[mid]
                    seg=model.segonside[np.array(fs[1:-1])-1]
                    avemis=np.mean(mis,axis=0)
                    
                    file.write(str((model.gplong[model.gp1onside[s-1]-1]+model.gplong[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str((model.gplat[model.gp1onside[s-1]-1]+model.gplat[model.gp2s[s-1]-1])/2)+'\t')
                    file.write(str(avemis[0])+'\t'+str(avemis[1])+'\t')
                    file.write(str(np.hypot(avemis[0],avemis[1]))+'\t')
                    file.write(str(np.sum(val[:]*model.slipobcoefft[:,i-1])*conv)+'\t')
                    file.write(str(np.sum(val[:]*model.slipobcoeffn[:,i-1])*conv)+'\t')
                    file.write(str(np.mean(model.slipt1[seg-1,fault-1])*conv)+'\t')
                    file.write(str(np.mean(model.slipn1[seg-1,fault-1])*conv)+'\t')
                    if hassubf:
                        file.write(model.subfname[model.subfs[s-1]-1]+'\n')
                    else:
                        file.write(model.fname[fault-1]+'\n')
                    allmis.append(mis[mid])
                # load new observation
                fault = model.faultofslipob[i]
                val = model.slipobvalue[:,i]
                mis = [[model.slipobmis1[0,i],model.slipobmis1[1,i]]]
                fs = [model.sideofslipob[i]]
        # deal with final observation
        if len(fs)>2:
            mid=int(len(fs)/2)
            s=fs[mid]
            seg=model.segonside[np.array(fs[1:-1])-1]
            avemis=np.mean(mis,axis=0)
            
            file.write(str((model.gplong[model.gp1onside[s-1]-1]+model.gplong[model.gp2s[s-1]-1])/2)+'\t')
            file.write(str((model.gplat[model.gp1onside[s-1]-1]+model.gplat[model.gp2s[s-1]-1])/2)+'\t')
            file.write(str(avemis[0])+'\t'+str(avemis[1])+'\t')
            file.write(str(np.hypot(avemis[0],avemis[1]))+'\t')
            file.write(str(np.sum(val[:]*model.slipobcoefft[:,i-1])*conv)+'\t')
            file.write(str(np.sum(val[:]*model.slipobcoeffn[:,i-1])*conv)+'\t')
            file.write(str(np.mean(model.slipt1[seg-1,fault-1])*conv)+'\t')
            file.write(str(np.mean(model.slipn1[seg-1,fault-1])*conv)+'\t')
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
        fault=model.faultofslipob[0]
        val=model.slipobvalue[:,0]
        fs=[model.sideofslipob[0]]
        mis=[[fault,model.sideofslipob[0],val,model.seslipobvalue[:,0],model.slipobmis0[:,0]]]
        allmis=[]
        for i in range(1,model.nslipob):
            if model.faultofslipob[i]==fault and np.all(model.slipobvalue[:,i]==val):
                # if same as previous observation, add to that observation
                fs.append(model.sideofslipob[i])
                mis.append([fault,model.sideofslipob[i],val,model.seslipobvalue[:,i],model.slipobmis0[:,i]])
            else:
                # if different to previous observation, find previous middle segment
                allmis.append(mis[int(len(fs)/2)])
                # load new observation
                fault = model.faultofslipob[i]
                val = model.slipobvalue[:,i]
                fs = [model.sideofslipob[i]]
                mis = [[fault,model.sideofslipob[i],val,model.seslipobvalue[:,i],model.slipobmis0[:,i]]]
        # deal with final observation
        allmis.append(mis[int(len(fs)/2)])
        return allmis
    
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        fault=model.faultofslipob[0]
        val=model.slipobvalue[:,0]
        fs=[model.sideofslipob[0]]
        mis=[[fault,model.sideofslipob[0],val,model.seslipobvalue[:,0],model.slipobmis1[:,0]]]
        allmis=[]
        for i in range(1,model.nslipob):
            if model.faultofslipob[i]==fault and np.all(model.slipobvalue[:,i]==val):
                # if same as previous observation, add to that observation
                fs.append(model.sideofslipob[i])
                mis.append([fault,model.sideofslipob[i],val,model.seslipobvalue[:,i],model.slipobmis1[:,i]])
            else:
                # if different to previous observation, find previous middle segment
                allmis.append(mis[int(len(fs)/2)])
                # load new observation
                fault = model.faultofslipob[i]
                val = model.slipobvalue[:,i]
                fs = [model.sideofslipob[i]]
                mis = [[fault,model.sideofslipob[i],val,model.seslipobvalue[:,i],model.slipobmis1[:,i]]]
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
    
def calcStrainStyleMisfit(model,soln,zerotol=1e-9,scale=1,obrange=None,style=None):
    """
    Function to calculate the misfit to the strain rate style.
    
    zerotol:: strains smaller than this are considered zero
    scale:: scale misfit by inverse of this factor, equivalent to standard error
    obrange:: range of observations to be considered
    style:: strain rate style of all observations (including those not considered)
    """
    import numpy as np
    from .readElements import readElements
    
    if obrange is None:
        obrange=range(model.neo)
    
    if style is None:
        style=(model.oec[0,:model.neo]+model.oec[1,:model.neo])/\
            np.sqrt(model.oec[0,:model.neo]**2+model.oec[1,:model.neo]**2+2*model.oec[2,:model.neo]**2)
    
    if str(soln)=='0' or soln=='apri' or soln=='apriori':
        if model.e110[0]==0.:
            readElements(model,'0')
        dil=model.e110[:model.ne]+model.e220[:model.ne]
        preds=dil/np.sqrt(model.e110[:model.ne]**2+model.e220[:model.ne]**2+2*model.e120[:model.ne]**2)
        file=open('misfit_to_strain_style_apriori.dat','w')
    elif str(soln)=='1' or soln=='apost' or soln=='aposteriori':
        if model.e111[0]==0.:
            readElements(model,'1')
        dil=model.e111[:model.ne]+model.e221[:model.ne]
        preds=dil/np.sqrt(model.e111[:model.ne]**2+model.e221[:model.ne]**2+2*model.e121[:model.ne]**2)
        file=open('misfit_to_strain_style_aposteriori.dat','w')
    elif soln == 'b' or soln == 'both' or soln == 'a' or soln == 'all':
        mis0=calcStrainStyleMisfit(model,'0',zerotol,scale,obrange)
        mis1=calcStrainStyleMisfit(model,'1',zerotol,scale,obrange)
        return [mis0,mis1]
    else:
        print('Solution not recognised.')
        return
        
    mis=np.zeros((model.neo))
    
    for i in obrange:
        e=model.eeo[i]
        # assume observations are perpendicular components ii, jj, and ij
        if np.isnan(style[i]):
            # if observation is not zero, perform check relative to zerotol
                mis[i]=0
        else:
            # find difference between observed and modelled styles
            mis[i]=(style[i]-preds[e-1])/scale
        file.write(str(model.elong[e-1])+'\t'+str(model.elat[e-1])+'\t'+str(mis[i])+'\n')
    
    return np.sum(mis**2)
    
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
        obrange=range(model.neo)
    
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
    
    
    
    
    
    
    
    
    
    