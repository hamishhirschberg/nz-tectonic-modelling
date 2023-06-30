#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep  7 14:00:53 2017

@author: hirschhami (conversion to Python)
This function is the same as the Matlab gmsh_geometry_for_setup.m script
written by John Haines and converted into the Perl .pl script by E. Klein.

gmshGeom.py
This is the function version of the script prepares the GIS export file for use
in gmsh.
"""
def gmsh_geom(infile='gmsh_control.dat', outfile='gmsh_geometry_for_setup.geo',
              algorithm=1):
    """Converts the output of preGmsh into the format required by GMSH.
    
    Parameters
    ----------
    infile : str, default='gmsh_control.dat'
        Name of input file that came from preGmsh.
    outfile : str, default='gmsh_geometry_for_setup.geo'
        Name of output file for GMSH.
    algorithm : int, default=1
        Specifies which algorithm will be used by GMSH.
    """
    # open file and initialise variables
    fdat = open(infile,'r')       # open the file
    ncp = int(fdat.readline())                 # read number of control points
    lon = []
    lat = []
    dmax = []
    nlcp = [0]*ncp                     # number of lines at control point
    
    # read first section of file - control points
    for i in range(ncp):
        line = fdat.readline().split()              # read each line
        index = int(line[0])                   # index of line - will be i+1
        if index == i + 1:
            lon.append(line[1])         # longitude of point
            lat.append(line[2])         # latitude of point
            dmax.append(line[3])        # maximum element size
        else:
            print('A control point has the wrong index ' + str(index) + ' ' + str(i+1))
            exit('Stopping')        # exit if point has wrong index
    
    # read second section of file - lines
    np = ncp            # initially set number of points to no. of control points
    i = []
    while not i:
        i = fdat.readline().strip()       # ignoring any blank lines
    nl = int(i)         # number of lines
    cp0 = [0]*nl
    cp1 = [0]*nl
    nip = [0]*nl
    ltype = [0]*nl
    nls = [0]*nl
    p0 = [[] for _ in range(nl)]
    #p1 = [[] for _ in range(nl)]
    nrl = [0]*nl
    
    # loop through each line
    for i in range(nl):
        line = fdat.readline().split()      # read each line
        index = int(line[0])
        if index == i + 1:
            cp0[i] = int(line[1])       # index of start control point
            cp1[i] = int(line[2])       # index of end control point
            nip[i] = int(line[3])       # number of intermediate points
        else:
            print('A line has the wrong index ' + str(index) + ' ' + str(i+1))
            exit('Stopping')        # exit if line has wrong index
        
        # read in 1-character code for line type
        # 'E' = exterior line
        # 'B' = internal boundary line (separating two regions)
        # 'I' = interior line (with at most one end at a control point)
        t = fdat.readline().split()[0]
        if t == 'E':
            ltype[i] = 1
        elif t == 'B':
            ltype[i] = 2
        elif t == 'I':
            ltype[i] = 3
        else:
            print('A line has unknown type ' + t + ' ' + str(index))
            exit('Stopping')        # exit if line has unknown type
        
        # read in longitude and latitude for all points
        line = fdat.readline().split()      # read first control point
        if line[0] == lon[cp0[i]-1] and line[1] == lat[cp0[i]-1]:
            nlcp[cp0[i]-1] += 1      # increment count of control point usage
            # lcp - appears not to be used
        else:
            print('A line has a wrong position for its first control point '\
                + str(line) + ' ' + str(index))
            exit('Stopping')    # exit if wrong position for control point
        # record number of line segments and set first point
        nls[i] = nip[i] + 1     # number of line segments
        p0[i].append(cp0[i])    # index of points along lines
            # ip0 - appears not to be used
            # ipn - appears not to be used
        # cycle through intermediate points; will skip if there are none
        for j in range(nip[i]):    
            np += 1         # increment count of points
            line = fdat.readline().split()      # read intermediate point
            lon.append(line[0])         # add longitude of point
            lat.append(line[1])         # add latitude of point
            dmax.append(line[2])        # add dmax of point
    #        p1[i].append(np)            # index of point at end of segment
            p0[i].append(np)            # add index of point to this line
        
        line = fdat.readline().split()      # read last control point
        if line[0] == lon[cp1[i]-1] and line[1] == lat[cp1[i]-1]:
            nlcp[cp1[i]-1] += 1      # increment count of control point usage
            # lcp - appears not to be used
        else:
            print('A line has a wrong position for its last control point '\
                + str(line) + ' ' + str(index))
            exit('Stopping')    # exit if wrong position for control point
        p0[i].append(cp1[i])        # add last control point to line
    
    # check each control point has at least one line
    for i in range(ncp):
        if nlcp[i] == 0:
            print('A control point is not on at least one line ' + str(i+1))
            exit('Stopping')
    
    # read in number of regions
    i = []
    while not i:
        i = fdat.readline().strip()       # ignoring any blank lines
    nr = int(i)         # number of regions
    nlr = [0]*nr
    rtype = [0]*nr
    lr = [[] for _ in range(nr)]
    nrir = [0]*nr
    nirr = [0]*nr
    irr = [[] for _ in range(nr)]
    
    for i in range(nr):
        line = fdat.readline().split()     # read line
        index = int(line[0])             # index of region
        if index == i+1:
            nlr[i] = int(line[1])        # number of lines in region
        else:
            print('A region has the wrong index ' + str(index) + ' ' + str(i+1))
            exit('Stopping')
        
        # read in 1-character code for the type of region
        # "R" = region
        # "H" = hole within a region
        # "L" = interior line region
        t = fdat.readline().split()[0]
        if t == 'R':
            rtype[i] = 1
        elif t == 'H':
            rtype[i] = 2
        elif t == 'L':
            rtype[i] = 3
        else:
            print('A region has unknown type ' + t + ' ' + str(index))
            exit('Stopping')        # exit if region has unknown type
        
        # for each line defining the region read in the "index" with a minus sign
        # in front of the index if the line is reversed, as the first control point
        # must match the second control point of the previous line
        for j in range(nlr[i]):
            line = int(fdat.readline().split()[0])      # read line: index of line
            # check whether line is of the correct type
            if rtype[i] == 1 or rtype[i] == 2:      # no I-type lines
                if ltype[abs(line)-1] == 3:
                    print('A region has a line of the wrong type ' + str(i+1)\
                        + ' ' + str(line))
                    exit('Stopping')
            elif rtype[i] == 3 and ltype[abs(line)-1] != 3:     # only I-type lines
                print('A region has a line of the wrong type ' + str(i+1)\
                    + ' ' + str(line))
                exit('Stopping')
            # check sequencing of lines
            if j > 0:       # excluding first line
                if lr[i][j-1] > 0:      # positive numbers indicate line forward
                    cpl1 = cp1[lr[i][j-1]-1]      # last point of previous line
                else:       # negative numbers indicate line reversed
                    cpl1 = cp0[abs(lr[i][j-1])-1]
                if line > 0:        # forward line
                    cpl0 = cp0[line-1]
                else:               # reversed line
                    cpl0 = cp1[abs(line)-1]
                # check endpoints are the same
                if cpl0 == cpl1:
                    lr[i].append(line)     # store line if they match
                else:
                    print('A region has lines wrongly sequenced ' + str(i+1) + ' '\
                        + str(line))
                    exit('Stopping')
            else:
                lr[i].append(line)         # store first line
            nrl[abs(line)-1] += 1      # increment number of regions with this line
            # rl - appears not to be used
        
        # check whether a loop has been formed
        if lr[i][nlr[i]-1] > 0:
            cpl1 = cp1[lr[i][nlr[i]-1]-1]       # last point of region
        else:
            cpl1 = cp0[abs(lr[i][nlr[i]-1])-1]       # last point of region
        if lr[i][0] > 0:
            cpl0 = cp0[lr[i][0]-1]              # first point of region
        else:
            cpl0 = cp1[abs(lr[i][0])-1]         # first point of region
        if cpl1 != cpl0:            # if the endpoints don't match
            if rtype[i] == 1 or rtype[i] == 2:
                print('The lines for a region do not form a loop ' + str(i+1))
                exit('Stopping')
        else:                   # if the endpoints match
            if rtype[i] == 3:
                print('The lines for a line region form a loop ' + str(i+1))
                exit('Stopping')
        
    # check that the following applies to each line:
    # (1) every E line has 1 region with it as a boundary
    # (2) every B line has 1 or 2 regions with it as a boundary
    # (3) every I line has 1 region with it as a boundary
    for i in range(nl):
        if ltype[i] == 1:
            if nrl[i] != 1:
                print('A line bounds a wrong number of regions ' + str(i+1))
                exit('Stopping')
        elif ltype[i] == 2:
            if nrl[i] != 1 and nrl[i] != 2:
                print('A line bounds a wrong number of regions ' + str(i+1))
                exit('Stopping')
        elif ltype[i] == 3:
            if nrl[i] != 1:
                print('A line bounds a wrong number of regions ' + str(i+1))
                exit('Stopping')
    
    i = []
    while not i:
        i = fdat.readline().strip()       # ignoring any blank lines
    nir = int(i)        # number of regions interior to other regions
    ir = [0]*nir        # indices of surrounded region
    sr = [0]*nir        # indices of surrounding region
    
    # for each interior region
    for i in range(nir):
        line = fdat.readline().split()      # read line
        index = int(line[0])            # index of region
        if index == i + 1:
            sr[i] = int(line[2])           # index of surrounding region
            if rtype[sr[i]-1] != 1:
                print('A surrounding region is of the wrong type ' + str(index))
                exit('Stopping')
            ir[i] = int(line[1])        # index of internal region
            nrir[ir[i]-1] += 1          # increment no. times region internal
            nirr[sr[i]-1] += 1          # increment no. times region surrounding
            irr[sr[i]-1].append(ir[i])     # surrounding's internal region
        else:
            print('An interior region has the wrong index ' + str(index) + ' '\
                + str(i+1))
            exit('Stopping')
    
    # check that each region is interior to the correct number of regions
    for i in range(nr):
        if nrir[i] > 1:
            print('A region is interior to more than one region ' + str(i+1))
            exit('Stopping')
    
    fdat.close()        # close input file
    fgeo = open(outfile,'w')      # open output file to write
    
    # write out information on points
    for i in range(np):
        # print index, longitude, latitude, 0, dmax
        fgeo.write('Point(' + str(i+1) + ') = {' + lon[i] + ',' + lat[i] + ',0,'\
            + dmax[i] + '};\n')
            
    # write out information on lines (including line segments)
    ls = 0     # number of line segments
    lsl =[[] for _ in range(nl)]
    for i in range(nl):
        for j in range(nls[i]):
            ls += 1             # increment number of lines
            lsl[i].append(ls)      # line number
            # print line number, start point, end point
            fgeo.write('Line(' + str(ls) + ') = {' + str(p0[i][j]) + ','\
                + str(p0[i][j+1]) + '};\n')
    
    # write out information on regions
    for i in range(nr):
        if rtype[i] != 3:
            # if 
           if lr[i][0] > 0:
               # write out lines in loop
               fgeo.write('Line Loop(' + str(i+1) + ') = {'\
                   + str(lsl[lr[i][0]-1][0]))
               for j in range(1,nls[lr[i][0]-1]):
                   fgeo.write(',' + str(lsl[lr[i][0]-1][j]))
           else:
               # write out lines in loop
               jm = nls[abs(lr[i][0])-1]-1
               fgeo.write('Line Loop(' + str(i+1) + ') = {'\
                   + str(-lsl[abs(lr[i][0])-1][0]))
               for j in range(nls[abs(lr[i][0])-1]):
                   jm -= 1
                   fgeo.write(',' + str(-lsl[abs(lr[i][0])-1][jm]))
           # write out rest of the loop
           for k in range(1,nlr[i]):
               if lr[i][k] > 0:
                   for j in range(nls[lr[i][k]-1]):
                       fgeo.write(',' + str(lsl[lr[i][k]-1][j]))
               else:
                   jm = nls[abs(lr[i][k])-1]
                   for j in range(nls[abs(lr[i][k])-1]):
                       jm -= 1
                       fgeo.write(',' + str(-lsl[abs(lr[i][k])-1][jm]))
           fgeo.write('};\n')
    
    # loop through regions for surfaces
    for i in range(nr):
        if rtype[i] == 1:
            fgeo.write('Plane Surface(' + str(i+1) + ') = {' + str(i+1))
            if nirr[i] > 0:
                for j in range(nirr[i]):
                    if rtype[irr[i][j]-1] != 3:
                        fgeo.write(',' + str(irr[i][j]))
            fgeo.write('};\n')
    
    # Lines in surfaces
    for i in range(nr):
        if rtype[i] == 1:
            if nirr[i] > 0:
                irr3 = 0
                for j in range(nirr[i]):
                    if rtype[irr[i][j]-1] == 3:
                        irr3 = irr[i][j]
                if irr3 > 0:
                    fgeo.write('Line {')
                    for j in range(nirr[i]):
                        if rtype[irr[i][j]-1] == 3:
                            for k in range(nlr[irr[i][j]-1]):
                                for l in range(nls[abs(lr[irr[i][j]-1][k])-1]):
                                    lsl3 = abs(lsl[abs(lr[irr[i][j]-1][k])-1][l])
                                    if irr[i][j] == irr3 and nlr[irr[i][j]-1] == k+1\
                                        and nls[abs(lr[irr[i][j]-1][k])-1] == l+1:
                                        fgeo.write(str(lsl3))
                                    else:
                                        fgeo.write(str(lsl3) + ',')
                    fgeo.write('} In Surface {' + str(i+1) + '};\n')
    
    # write out the control points
    for i in range(ncp):
        fgeo.write('Physical Point("P' + str(i+1) + '") = {' + str(i+1) + '};\n')
    
    # write out lines
    for i in range(nl):
        if ltype[i] == 1:
            fgeo.write('Physical Line("E' + str(i+1) + '") = {' + str(lsl[i][0]))
        elif ltype[i] == 2:
            fgeo.write('Physical Line("B' + str(i+1) + '") = {' + str(lsl[i][0]))
        elif ltype[i] == 3:
            fgeo.write('Physical Line("I' + str(i+1) + '") = {' + str(lsl[i][0]))
        for j in range(1,nls[i]):
            fgeo.write(',' + str(lsl[i][j]))
        fgeo.write('};\n')
    
    # write out regions
    for i in range(nr):
        if rtype[i] == 1:
            fgeo.write('Physical Surface("R' + str(i+1) + '") = {' + str(i+1) +'};\n')
            
    fgeo.write('Mesh.Algorithm = '+str(algorithm)+' ;\n')
    fgeo.close()          # close file
