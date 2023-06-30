#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 14:44:07 2019

readResults.py   

@author: Hamish Hirschberg
"""

def readElements(model, soln):
    """Reads in element results.
    
    This function reads the input file '*_solution_in_elements.log'
    for the chosen part(s) of the solution and saves the variables into
    the given permdefmap model.
    
    Parameters
    ----------
    model : permdefmap model
        Model into which the results are stored.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    """
    # Identify which solution(s)/part of solution is desired
    soln = str(soln).lower()      # convert to lowercase string
    if soln == '0' or soln == 'apriori' or soln == 'apri':
        soln='0'
    elif soln == '1' or soln == 'aposteriori' or soln == 'apost':
        soln='1'
    elif soln == 'd' or soln == 'difference' or soln == 'diff':
        soln='d'
    elif soln == 'c' or soln == 'constraint' or soln == 'con':
        soln='c'
    elif soln == 'a' or soln == 'all':
        soln='a'
    elif soln == 'b' or soln == 'both':
        soln='b'
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
    
    # Read the apriori solution
    if soln == '0' or soln == 'a' or soln == 'b':
        file = 'apriori_solution_in_elements.log'
        fdat = open(file, 'r')
        for i in range(11):
            fdat.readline()     # ignore comment lines
        for i in range(model.nel):
            # for each element
            j, model.ellong[i], model.ellat[i] = fdat.readline().split()
            (model.nvelobinel[i], model.nstrainobinel[i],
                model.nstrainlinkinel[i],
                model.elarea[i]) = fdat.readline().split()
            model.velinelx0[i], model.velinely0[i] = fdat.readline().split()
            (model.strainxx0[i], model.strainyy0[i],
                model.strainxy0[i]) = fdat.readline().split()
            (model.stressxx0[i], model.stressyy0[i],
                model.stressxy0[i]) = fdat.readline().split()
            model.velinelaz0[i], model.velinelmag0[i] = fdat.readline().split()
            (model.strainaz0[i], model.strainmax0[i],
                model.strainmin0[i]) = fdat.readline().split()
            (model.stressaz0[i], model.stressmax0[i],
                model.stressmin0[i]) = fdat.readline().split()
        fdat.close()
    
    # Read the aposteriori solution
    if soln == '1' or soln == 'a' or soln == 'b':
        file = 'aposteriori_solution_in_elements.log'
        fdat = open(file,'r')
        for i in range(11):
            fdat.readline()     # ignore comment lines
        for i in range(model.nel):
            j, model.ellong[i], model.ellat[i] = fdat.readline().split()
            (model.nvelobinel[i], model.nstrainobinel[i],
                model.nstrainlinkinel[i],
                model.elarea[i]) = fdat.readline().split()
            model.velinelx1[i], model.velinely1[i] = fdat.readline().split()
            (model.strainxx1[i], model.strainyy1[i],
                model.strainxy1[i]) = fdat.readline().split()
            (model.stressxx1[i], model.stressyy1[i],
                model.stressxy1[i]) = fdat.readline().split()
            model.velinelaz1[i], model.velinelmag1[i] = fdat.readline().split()
            (model.strainaz1[i], model.strainmax1[i],
                model.strainmin1[i]) = fdat.readline().split()
            (model.stressaz1[i], model.stressmax1[i],
                model.stressmin1[i]) = fdat.readline().split()
        fdat.close()
    
    # Read the difference (aposteriori minus apriori solution)
    if soln == 'd' or soln == 'a':
        file = 'aposteriori_minus_apriori_in_elements.log'
        fdat = open(file,'r')
        for i in range(11):
            fdat.readline()     # ignore comment lines
        for i in range(model.nel):
            j, model.ellong[i], model.ellat[i] = fdat.readline().split()
            (model.nvelobinel[i], model.nstrainobinel[i],
                model.nstrainlinkinel[i],
                model.elarea[i]) = fdat.readline().split()
            model.velinelxd[i], model.velinelyd[i] = fdat.readline().split()
            (model.strainxxd[i], model.strainyyd[i],
                model.strainxyd[i]) = fdat.readline().split()
            (model.stressxxd[i], model.stressyyd[i],
                model.stressxyd[i]) = fdat.readline().split()
            model.velinelazd[i], model.velinelmagd[i] = fdat.readline().split()
            (model.strainazd[i], model.strainmaxd[i],
                model.strainmind[i]) = fdat.readline().split()
            (model.stressazd[i], model.stressmaxd[i],
                model.stressmind[i]) = fdat.readline().split()
        fdat.close()
    
    # Read the constraint part of the solution
    if soln == 'c' or soln == 'a':
        file = 'constraint_part_of_solution_in_elements.log'
        fdat = open(file,'r')
        for i in range(11):
            fdat.readline()     # ignore comment lines
        for i in range(model.nel):
            j, model.ellong[i], model.ellat[i] = fdat.readline().split()
            (model.nvelobinel[i], model.nstrainobinel[i],
                model.nstrainlinkinel[i],
                model.elarea[i]) = fdat.readline().split()
            model.velinelxc[i], model.velinelyc[i] = fdat.readline().split()
            (model.strainxxc[i], model.strainyyc[i],
                model.strainxyc[i]) = fdat.readline().split()
            (model.stressxxc[i], model.stressyyc[i],
                model.stressxyc[i]) = fdat.readline().split()
            model.velinelazc[i], model.velinelmagc[i] = fdat.readline().split()
            (model.strainazc[i], model.strainmaxc[i],
                model.strainminc[i]) = fdat.readline().split()
            (model.stressazc[i], model.stressmaxc[i],
                model.stressminc[i]) = fdat.readline().split()
        fdat.close()

def readElementsExtra(model, soln):
    """Reads in extra element results.
    
    This function reads the input file '*_solution_in_elements_extra.log'
    for the chosen part(s) of the solution and saves the variables into
    the given permdefmap model.
    
    Parameters
    ----------
    model : permdefmap model
        Model into which the results are stored.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    """
    # Identify which solution(s)/part of solution is desired
    soln = str(soln).lower()      # convert to lowercase string
    if soln == '0' or soln == 'apriori' or soln == 'apri':
        soln='0'
    elif soln == '1' or soln == 'aposteriori' or soln == 'apost':
        soln='1'
    elif soln == 'd' or soln == 'difference' or soln == 'diff':
        soln='d'
    elif soln == 'c' or soln == 'constraint' or soln == 'con':
        soln='c'
    elif soln == 'a' or soln == 'all':
        soln='a'
    elif soln == 'b' or soln == 'both':
        soln='b'
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
    
    # Read the apriori solution
    if soln == '0' or soln == 'a' or soln == 'b':
        file='apriori_solution_in_elements_extra.log'
        fdat=open(file,'r')
        for i in range(5):
            fdat.readline()     # Ignore comment lines
        for i in range(model.nel):
            j=fdat.readline().split()
            model.velatvertx0[:,i] = fdat.readline().split()
            model.velatverty0[:,i] = fdat.readline().split()
            model.rotationxy0[i] = fdat.readline()
        fdat.close()
        
    # Read the aposteriori solution
    if soln == '1' or soln == 'a' or soln == 'b':
        file='aposteriori_solution_in_elements_extra.log'
        fdat=open(file,'r')
        for i in range(5):
            fdat.readline()     # Ignore comment lines
        for i in range(model.nel):
            j=fdat.readline().split()
            model.velatvertx1[:,i] = fdat.readline().split()
            model.velatverty1[:,i] = fdat.readline().split()
            model.rotationxy1[i] = fdat.readline()
        fdat.close()
        
    # Read the difference (aposteriori minus apriori solution)
    if soln == 'd' or soln == 'a':
        file='aposteriori_minus_apriori_in_elements_extra.log'
        fdat=open(file,'r')
        for i in range(5):
            fdat.readline()     # Ignore comment lines
        for i in range(model.nel):
            j=fdat.readline().split()
            model.velatvertxd[:,i] = fdat.readline().split()
            model.velatvertyd[:,i] = fdat.readline().split()
            model.rotationxyd[i] = fdat.readline()
        fdat.close()
        
    # Read the constraint part of the solution
    if soln == 'c' or soln == 'a':
        file='constraint_part_of_solution_in_elements_extra.log'
        fdat=open(file,'r')
        for i in range(5):
            fdat.readline()     # Ignore comment lines
        for i in range(model.nel):
            j=fdat.readline().split()
            model.velatvertxc[:,i] = fdat.readline().split()
            model.velatvertyc[:,i] = fdat.readline().split()
            model.rotationxyc[i] = fdat.readline()
        fdat.close()

def readFaults(model, soln):
    """Reads in fault results.
    
    This function reads the input file '*_solution_on_faults.log'
    for the chosen part(s) of the solution and saves the variables into
    the given permdefmap model.
    
    Parameters
    ----------
    model : permdefmap model
        Model into which the results are stored.
    soln : string
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    """
    # If there are no faults, there is no file to read in
    if model.nfault == 0:
        print('No faults in model. No fault data read in.')
        return
    
    # Identify which solution/part of solution is desired
    if str(soln) == '0' or soln == 'apriori' or soln == 'apri':
        soln='0'
    elif str(soln) == '1' or soln == 'aposteriori' or soln == 'apost':
        soln='1'
    elif soln == 'd' or soln == 'difference' or soln == 'diff':
        soln='d'
    elif soln == 'c' or soln == 'constraint' or soln == 'con':
        soln='c'
    elif soln == 'a' or soln == 'all':
        soln='a'
    elif soln == 'b' or soln == 'both':
        soln='b'
    else:
        raise Exception('Solution '+str(soln)+' not recognised')
    
    if soln == '0' or soln == 'a' or soln == 'b':
        file = 'apriori_solution_on_faults.log'
        fdat = open(file,'r')
        for i in range(12):
            fdat.readline()         # ignore comment lines
        # for each fault
        for i in range(model.nfault):
            fdat.readline()
            # for each fault segment
            for j in range(model.nfaultseg[i]):
                (l, l, model.seglong[j,i],
                    model.seglat[j,i]) = fdat.readline().split()
                (model.nslipobonseg[j,i], model.nsliplinkonseg[j,i],
                    model.seglength[j,i],
                    model.segtaz[j,i]) = fdat.readline().split()
                (model.segtx[j,i], model.segty[j,i],
                    l) = fdat.readline().split()
                (model.segnx[j,i], model.segny[j,i],
                    model.segnaz[j,i]) = fdat.readline().split()
                model.slipt0[j,i], model.slipn0[j,i] = fdat.readline().split()
                model.tract0[j,i], model.tracn0[j,i] = fdat.readline().split()
                model.slipx0[j,i], model.slipy0[j,i] = fdat.readline().split()
                model.tracx0[j,i], model.tracy0[j,i] = fdat.readline().split()
                (model.slipaz0[j,i],
                    model.slipmag0[j,i]) = fdat.readline().split()
                (model.tracaz0[j,i],
                    model.tracmag0[j,i]) = fdat.readline().split()
        fdat.close()
        
    if soln == '1' or soln == 'a' or soln == 'b':
        file = 'aposteriori_solution_on_faults.log'
        fdat = open(file,'r')
        for i in range(12):
            fdat.readline()         # ignore comment lines
        for i in range(model.nfault):
            fdat.readline()
            for j in range(model.nfaultseg[i]):
                (l, l, model.seglong[j,i],
                    model.seglat[j,i]) = fdat.readline().split()
                (model.nslipobonseg[j,i], model.nsliplinkonseg[j,i],
                    model.seglength[j,i],
                    model.segtaz[j,i]) = fdat.readline().split()
                (model.segtx[j,i], model.segty[j,i],
                    l) = fdat.readline().split()
                (model.segnx[j,i], model.segny[j,i],
                    model.segnaz[j,i]) = fdat.readline().split()
                model.slipt1[j,i], model.slipn1[j,i] = fdat.readline().split()
                model.tract1[j,i], model.tracn1[j,i] = fdat.readline().split()
                model.slipx1[j,i], model.slipy1[j,i] = fdat.readline().split()
                model.tracx1[j,i], model.tracy1[j,i] = fdat.readline().split()
                (model.slipaz1[j,i],
                    model.slipmag1[j,i]) = fdat.readline().split()
                (model.tracaz1[j,i],
                    model.tracmag1[j,i]) = fdat.readline().split()
        fdat.close()
        
    if soln == 'd' or soln == 'a':
        file = 'aposteriori_minus_apriori_on_faults.log'
        fdat = open(file,'r')
        for i in range(12):
            fdat.readline()         # ignore comment lines
        for i in range(model.nfault):
            fdat.readline()
            for j in range(model.nfaultseg[i]):
                (l, l, model.seglong[j,i],
                    model.seglat[j,i]) = fdat.readline().split()
                (model.nslipobonseg[j,i], model.nsliplinkonseg[j,i],
                    model.seglength[j,i],
                    model.segtaz[j,i]) = fdat.readline().split()
                (model.segtx[j,i], model.segty[j,i],
                    l) = fdat.readline().split()
                (model.segnx[j,i], model.segny[j,i],
                    model.segnaz[j,i]) = fdat.readline().split()
                model.sliptd[j,i], model.slipnd[j,i] = fdat.readline().split()
                model.tractd[j,i], model.tracnd[j,i] = fdat.readline().split()
                model.slipxd[j,i], model.slipyd[j,i] = fdat.readline().split()
                model.tracxd[j,i], model.tracyd[j,i] = fdat.readline().split()
                (model.slipazd[j,i],
                    model.slipmagd[j,i]) = fdat.readline().split()
                (model.tracazd[j,i],
                    model.tracmagd[j,i]) = fdat.readline().split()
        fdat.close()
        
    if soln == 'c' or soln == 'a':
        file = 'constraint_part_of_solution_on_faults.log'
        fdat = open(file,'r')
        for i in range(12):
            fdat.readline()         # ignore comment lines
        for i in range(model.nfault):
            fdat.readline()
            for j in range(model.nfaultseg[i]):
                (l, l, model.seglong[j,i],
                    model.seglat[j,i]) = fdat.readline().split()
                (model.nslipobonseg[j,i], model.nsliplinkonseg[j,i],
                    model.seglength[j,i],
                    model.segtaz[j,i]) = fdat.readline().split()
                (model.segtx[j,i], model.segty[j,i],
                    l) = fdat.readline().split()
                (model.segnx[j,i], model.segny[j,i],
                    model.segnaz[j,i]) = fdat.readline().split()
                model.sliptc[j,i], model.slipnc[j,i] = fdat.readline().split()
                model.tractc[j,i], model.tracnc[j,i] = fdat.readline().split()
                model.slipxc[j,i], model.slipyc[j,i] = fdat.readline().split()
                model.tracxc[j,i], model.tracyc[j,i] = fdat.readline().split()
                (model.slipazc[j,i],
                    model.slipmagc[j,i]) = fdat.readline().split()
                (model.tracazc[j,i],
                    model.tracmagc[j,i]) = fdat.readline().split()
        fdat.close()













