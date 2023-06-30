#!/usr/bin/env python3

import numpy as np

def maskStrain():
    """Converts strain-rate capacities from xyz format to two line format
    used by postGmsh.    
    """
    # open input and output files
    infile  = open('mask_strain.xyz','r')
    outfile = open('strain-rate_capacities.dat','w')
    
    # read text file into arrays
    x,y,Lc,Lcc,Lcs,Ls,Lsc,Lss = np.loadtxt(infile,unpack=True)
    
    xs=np.sort(x)
    ys=np.sort(y)
    xss=[]
    yss=[]
    vs=-999
    for i in xs:
        if i>vs:
            xss.append(i)
            vs=i
    vs=-999
    for i in ys:
        if i>vs:
            yss.append(i)
            vs=i
    
    n_x=len(xss)
    outfile.write(str(n_x)+'   Number of longitudes\n')
    for i in range(n_x):
        outfile.write(str(i+1)+' '+str(xss[i])+'  index, longitude\n')
    
    n_y=len(yss)
    outfile.write('\n'+str(n_y)+'   Number of latitudes\n')
    for i in range(n_y):
        outfile.write(str(i+1)+' '+str(yss[i])+'  index, latitude\n')
    
    n_p=len(x)
    outfile.write('\n'+str(n_p)+'   Number of points with values\n')
    for i in range(n_p):
        outfile.write(str(i+1)+' '+str(x[i])+' '+str(y[i])+'  index, longitude, latitude\n')
        outfile.write(str(Lc[i])+' '+str(Lcc[i])+' '+str(Lcs[i])+' '+str(Ls[i])+' '+str(Lsc[i])+' '+str(Lss[i])+'  Lc, Lcc, Lcs, Ls, Lsc, Lss\n')
    
    
    infile.close()
    outfile.close()
    
    print('\nCreated strain-rate_capacities.dat\n')
