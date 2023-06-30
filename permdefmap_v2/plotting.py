#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar  8 11:35:51 2021

plotting.py - a collection of functions that organise and print attributes from
permdefmap models.

@author: hamish
"""

from .postprocessing.readResults import readElements, readFaults, readElementsExtra
from .geometry import eulerVel
import numpy as np
import matplotlib.pyplot as plt

def rotatedVels(model, plate, soln, fileName=None):
    """Prints velocities relative to specified plate.
    
    This functions prints out the velocities from a solution but rotated so
    that they are relative to the specified plate. Velocities are printed
    in the form required by GMT's psvelo.
    
    Parameters
    ----------
    model : permdefmap model
        Print velocities from this model.
    plate : int
        Print velocities relative to plate with this index.
    soln : str
        Part or parts of the solution to be read in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    fileName : str, optional
        File name of output file. Default is of the form
        'plot_velocity_' + soln + '_rotated_in_elements.dat'.
        Custom file names for 'all' or 'both' solutions should be given as
        a list of file names in the order apri, apost, diff, con.
    """
    
    readElements(model, soln)
    
    if str(soln) == '0' or soln == 'apri' or soln == 'apriori':
        vels = np.array([model.velinelx0, model.velinely0])
        if fileName is None:
            fileName = 'plot_velocity_apriori_rotated_in_elements.dat'
    elif str(soln) == '1' or soln == 'apost' or soln == 'aposteriori':
        vels = np.array([model.velinelx1, model.velinely1])
        if fileName is None:
            fileName = 'plot_velocity_aposteriori_rotated_in_elements.dat'
    elif soln == 'c' or soln == 'con' or soln == 'constraint':
        vels = np.array([model.velinelxc, model.velinelyc])
        if fileName is None:
            fileName = 'plot_velocity_constraint_rotated_in_elements.dat'
    elif soln == 'd' or soln == 'diff' or soln == 'difference':
        vels = np.array([model.velinelxd, model.velinelyd])
        if fileName is None:
            fileName = 'plot_velocity_difference_rotated_in_elements.dat'
    elif soln == 'b' or soln == 'both':
        if fileName is None:
            rotatedVels(model, plate, '0')
            rotatedVels(model, plate, '1')
        else:
            rotatedVels(model, plate, '0', fileName[0])
            rotatedVels(model, plate, '1', fileName[1])
        return
    elif soln == 'a' or soln == 'all':
        if fileName is None:
            rotatedVels(model, plate, '0')
            rotatedVels(model, plate, '1')
            rotatedVels(model, plate, 'd')
            rotatedVels(model, plate, 'c')
        else:
            rotatedVels(model, plate, '0', fileName[0])
            rotatedVels(model, plate, '1', fileName[1])
            rotatedVels(model, plate, 'd', fileName[2])
            rotatedVels(model, plate, 'c', fileName[3])
        return
    else:
        print('Solution not recognised. No output produced.')
        return
    
    euler = eulerVel(model.eulerlong[plate], model.eulerlat[plate],
                     model.eulerrate[plate], model.ellong, model.ellat,
                     model.earthradius)
    vels = vels - euler
    
    file = open(fileName, 'w')
    for i in range(model.nel):
        file.write(str(model.ellong[i])+'\t' + str(model.ellat[i])+'\t'
                   + str(vels[0,i])+'\t' + str(vels[1,i])+'\n')
    file.close()

def fullStrainObs(model, fileName='plot_strain_rate_observations.dat',
                  obrange=None):
    """Prints strain rate observations to be plotted.
    
    This function prints the strain observations in a form that can be
    plotted. The printed values are:
        long, lat of element centre
        exx, eyy, exy
    The function requires all three components to perform the calculation
    so it only prints observations where all three are present.
    
    Parameters
    ----------
    model : permdefmap model
        Print strain rate observations from this model.
    fileName : str, default='plot_strain_rate_observations.dat'
        Name of file where observations are printed.
    obrange : iterable
        The indices of observations to be printed. Default is all
        observations.
    """
    
    import numpy as np
    
    if obrange is None:
        obrange=range(model.nstrainob)
    
    outf=open(fileName, 'w')
    if model.gp1ofel[1] == -1:
        print('Element gridpoints not loaded. Loading gridpoints now.')
        model.getElementPoints()
    
    for i in obrange:
        if model.nstrainobcomp[i] == 3:
            # observation location
            e = model.elofstrainob[i]
            gp1 = model.gp1ofel[e]
            gp2 = model.gp2ofel[e]
            gp3 = model.gp3ofel[e]
            lon = (model.gplong[gp1] + model.gplong[gp2]
                    + model.gplong[gp3]) / 3
            lat = (model.gplat[gp1] + model.gplat[gp2] + model.gplat[gp3]) / 3
            
            # convert observation to xy-coordinates
            A = [[model.strainobcoeffxx[0,i], model.strainobcoeffyy[0,i],
                  model.strainobcoeffxy[0,i]],
                 [model.strainobcoeffxx[1,i], model.strainobcoeffyy[1,i],
                  model.strainobcoeffxy[1,i]],
                 [model.strainobcoeffxx[2,i], model.strainobcoeffyy[2,i],
                  model.strainobcoeffxy[2,i]]]
            b = [model.strainobvalue[0,i], model.strainobvalue[1,i],
                 model.strainobvalue[2,i]]
            Ainvb = np.linalg.solve(A, b)
            exx = Ainvb[0]
            eyy = Ainvb[1]
            exy = Ainvb[2]
            
            # print to file
            outf.write(str(lon)+' ' + str(lat)+' ')
            outf.write(str(exx)+' ' + str(eyy)+' ' + str(exy)+'\n')
    
    outf.close()

def strainMisfitElements(model, soln, fileName=None):
    """Prints misfit to strain rates to be plotted as elements.
    
    This function prints the misfit to strain rate observations and the
    vertices of the associated elements. This is intended for plotting
    in GMT. For observations with multiple components, the misfit is the
    square-root of the sum of the squares of the individual component
    misfits.
    
    Parameters
    ----------
    model : permdefmap model
        Print misfits from this model.
    soln : str
        Part or parts of solution to plot misfits from. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'both' or 'b' (both the apriori and aposteriori)
    fileName : str, default=None
        Name of output file. Default for apriori is
        'plot_misfit_to_strain_rate_observations_in_elements_apriori.dat'
        and default for aposteriori is
        'plot_misfit_to_strain_rate_observations_in_elements_aposteriori.dat'.
        If soln=='both', then fileName should be a list of the apriori
        and aposteriori file names.
    """
    
    soln = str(soln).lower()
    if soln=='0' or soln=='apri' or soln=='apriori':
        f = 'plot_misfit_to_strain_rate_observations_in_elements_apriori.dat'
        misfit = np.sqrt(np.sum(model.strainobmis0**2, axis=0))
    elif soln=='1' or soln=='apost' or soln=='aposteriori':
        f = 'plot_misfit_to_strain_rate_observations_in_elements_aposteriori.dat'
        misfit = np.sqrt(np.sum(model.strainobmis0**2, axis=0))
    elif soln=='b' or soln=='both':
        if fileName is None:
            strainMisfitElements(model, '0')
            strainMisfitElements(model, '1')
        else:
            strainMisfitElements(model, '0', fileName[0])
            strainMisfitElements(model, '1', fileName[1])
        return
    else:
        print('Solution '+str(soln)+' not recognised. No ouput produced.')
    
    if fileName is None:
        file = open(f, 'w')
    else:
        file = open(fileName, 'w')
        
    for i in range(model.nstrainob):
        file.write('>-Z'+str(misfit[i])+'\n')
        el = model.elofstrainob[i]
        file.write(str(model.gplong[model.gp1ofel[el]])+' '
                   + str(model.gplat[model.gp1ofel[el]])+'\n')
        file.write(str(model.gplong[model.gp2ofel[el]])+' '
                   + str(model.gplat[model.gp2ofel[el]])+'\n')
        file.write(str(model.gplong[model.gp3ofel[el]])+' '
                   + str(model.gplat[model.gp3ofel[el]])+'\n')
        file.write(str(model.gplong[model.gp1ofel[el]])+' '
                   + str(model.gplat[model.gp1ofel[el]])+'\n')
        

def faultData(model, data, fileName=None, includeEnds=True):
    """Prints fault data (parameters or results).
    
    This function prints faults with data (usually parameters or results)
    for plotting with GMT's psxy. data must be specified as either a string
    (for a pre-specified set of data) or a numpy array (for a custom set of
    data).
    
    Parameters
    ----------
    model : permdefmap model
        Print fault data from this model.
    data : str or array
        Data to be printed. It can be either a string (for a pre-specified
        set of data) or a numpy array (for a custom set of data).
        The pre-specified data sets and the strings to call them are:
            'dip' or 'd' - fault segment dip from slip-rate capacity (in
                degrees).
            'slip0' or 'sr0' - fault segment slip rate magnitude apriori
                (multiplied by 1000).
            'slip1' or 'sr1' - fault segment slip rate magnitude
                aposteriori (multiplied by 1000).
            'rake0' or 'r0' - fault segment rake apriori
            'rake1' or 'r1' - fault segment rake aposteriori
            'trac0' or 't0' - fault segment traction magnitude apriori
            'trac1' or 't1' - fault segment traction magnitude aposteriori
            'tracdir0' of 'td0' - fault segment traction direction apriori
                (in degrees)
            'tracdir1' or 'td1' - fault segment traction direction
                aposteriori (in degrees)
            'slipcap' or 'src' - sum of fault segment slip-rate capacity
                components.
    fileName : str
        Name of file where data is printed. Required when using a custom
        set of data. Each pre-specified set of data has a default file name
        that is used when fileName=None and is overridden when another file
        name is given.
    includeEnds : bool, default=True
        If True, data on fault end segments are printed. If False, data on
        end segments is replaced with nan.
    """
    
    if model.nfault > 0:
        # Check if fault gridpoints loaded into the model.
        if model.gponfault[1,0] == -1:
            print('Fault gridpoints not loaded. Loading gridpoints now.')
            model.getFaultPoints()
        if model.nelatgp[0]==0:
            print('Gridpoint segments not loaded. Loading now.')
            model.getPointSegments()
        
        # Process pre-specified data sets.
        if type(data) == str:
            # Select data set and file name based on string
            if data == 'dip' or data == 'd':
                f = 'fault_dips.dat'
                cap = np.sqrt(model.slipcaps / model.slipcapc)
                data = np.degrees(np.arctan(cap))
            elif data == 'slip0' or data == 'sr0':
                f = 'fault_sliprate_apriori.dat'
                readFaults(model, 'apriori')
                data = model.slipmag0 * 1e3
            elif data == 'slip1' or data == 'sr1':
                f = 'fault_sliprate_aposteriori.dat'
                readFaults(model, 'aposteriori')
                data = model.slipmag1 * 1e3
            elif data == 'rake0' or data == 'r0':
                f = 'fault_rake_apriori.dat'
                readFaults(model, 'apriori')
                data = np.degrees(np.arctan2(-model.slipn0,-model.slipt0))
            elif data == 'rake1' or data == 'r1':
                f = 'fault_rake_aposteriori.dat'
                readFaults(model, 'aposteriori')
                data = np.degrees(np.arctan2(-model.slipn1,-model.slipt1))
            elif data == 'trac0' or data == 't0':
                f = 'fault_tractions_apriori.dat'
                readFaults(model, 'apriori')
                data = model.tracmag0
            elif data == 'trac1' or data == 't1':
                f = 'fault_tractions_aposteriori.dat'
                readFaults(model, 'aposteriori')
                data = model.tracmag1
            elif data == 'tracdir0' or data == 'td0':
                f = 'fault_traction_direction_apriori.dat'
                readFaults(model, 'apriori')
                data = np.degrees(np.arctan2(-model.tracn0,-model.tract0))
            elif data == 'tracdir1' or data == 'td1':
                f = 'fault_traction_direction_aposteriori.dat'
                readFaults(model, 'aposteriori')
                data = np.degrees(np.arctan2(-model.tracn1,-model.tract1))
            elif data == 'slipcap' or data == 'src':
                f = 'fault_sliprate_capacity.dat'
                data = model.slipcaps + model.slipcapc
            else:
                print('Pre-specified data type not recognised. No output '
                      + 'produced.')
                return
            
            # Override default file name if necessary
            if fileName is None:
                outf = open(f, 'w')
            else:
                outf = open(fileName, 'w')
        
        # Process custom data sets
        else:
            outf = open(fileName, 'w')
                
        # Print information for each segment of each fault
        for i in range(model.nfault):
            for j in range(model.nfaultseg[i]):
                # Identify gridpoints of segment
                gp1 = model.gp1onside[model.sideonfault[j,i]]
                gp2 = model.gp2onside[model.sideonfault[j,i]]
                if not includeEnds and (model.nsegatgp[gp1]==1
                                        or model.nsegatgp[gp2]==1):
                    # Set end segments to nan if desired
                    outf.write('>-Znan\n')
                else:
                    # Write value, long, lat out
                    outf.write('>-Z'+str(data[j,i])+'\n')
                outf.write(str(model.gplong[gp1])+' '
                           + str(model.gplat[gp1])+'\n')
                outf.write(str(model.gplong[gp2])+' '
                           + str(model.gplat[gp2])+'\n')
    else:
        print('No faults in model. No fault data file written.')

def elementData(model, data, fileName=None):
    """Prints element data (parameters or results)
    
    This function prints elements with data (usually parameters or results)
    for plotting with GMT's psxy. data must be specified as either a string
    (for a pre-specified set of data) or a numpy array (for a custom set of
    data).
    
    Parameters
    ----------
    model : permdefmap model
        Print element data from this model.
    data : str or array
        Data to be printed. It can be either a string (for a pre-specified
        set of data) or a numpy array (for a custom set of data).
        The pre-specified data sets and the strings to call them are:
            'strain0' or 'sr0' - element strain rate magnitude apriori
            'strain1' or 'sr1' - element strain rate magnitude aposteriori
            'straincap' or 'src' - element strain rate capacity
    fileName : str
        Name of file where data is printed. Required when using a custom
        set of data. Each pre-specified set of data has a default file name
        that is used when fileName=None and is overridden when another file
        name is given.
    """
    
    readElements(model,0)       # get element long, lat
    # Check if element gridpoints loaded into model
    if model.gp1ofel[1] == -1:
        print('Element gridpoints not loaded. Loading gridpoints now.')
        model.getElementPoints()
    
    # Process pre-specified data sets
    if type(data) == str:
        # Select data set and file name based on string
        if data == 'straincap' or data == 'src':
            f = 'element_strainrate_capacity.dat'
            data = model.straincapc
        elif data == 'strain0' or data == 'sr0':
            f = 'element_strainrate_magnitude_apriori.dat'
            readElements(model, 'apriori')
            data = model.strainmag0
        elif data == 'strain1' or data == 'sr1':
            f = 'element_strainrate_magnitude_aposteriori.dat'
            readElements(model, 'aposteriori')
            data = model.strainmag1
        else:
            print('Pre-specifieid data type not recognised. No output '
                  + 'produced.')
        
        if fileName is None:
            outf = open(f, 'w')
        else:
            outf = open(fileName, 'w')
    else:
        outf = open(fileName, 'w')
    
    # print information for each element
    for i in range(model.nel):
        outf.write('>-Z'+str(data[i])+'\n')
        outf.write(str(model.gplong[model.gp1ofel[i]])+' '
                   + str(model.gplat[model.gp1ofel[i]])+'\n')
        outf.write(str(model.gplong[model.gp2ofel[i]])+' '
                   + str(model.gplat[model.gp2ofel[i]])+'\n')
        outf.write(str(model.gplong[model.gp3ofel[i]])+' '
                   + str(model.gplat[model.gp3ofel[i]])+'\n')
        outf.write(str(model.gplong[model.gp1ofel[i]])+' '
                   + str(model.gplat[model.gp1ofel[i]])+'\n')
    outf.close()

def rotationRate(model, soln, plate=None):
    """Print rotation rate.
    
    Parameters
    ----------
    model : permdefmap model
        Print rotation rate for this model.
    soln : string
        Part or parts of the solution to be printed in. The solution can
        be specified as:
            'apriori' or 'apri' or 0
            'aposteriori' or 'apost' or 1
            'difference' or 'diff' or 'd' (aposteriori minus apriori)
            'constraint' or 'con' or 'c'
            'all' or 'a' (all of the above)
            'both' or 'b' (both the apriori and aposteriori)
    plate : int, optional
        Print rotation rate relative to this plate. Default is relative to
        the model's zero rotation frame.
    """
    
    if model.ellong[0] == 0:
        readElements(model, 0)
    
    soln = str(soln).lower()
    if soln=='0' or soln=='apri' or soln=='apriori':
        if model.rotationxy0[0] == 0:
            readElementsExtra(model, 0)
        val = model.rotationxy0[:model.ne]
        fileName = 'plot_rotation_rate_apriori_in_elements.dat'
    elif soln=='1' or soln=='apost' or soln=='aposteriori':
        if model.rotationxy1[0] == 0:
            readElementsExtra(model, 1)
        val=model.rotationxy1[:model.ne]
        fileName = 'plot_rotation_rate_aposteriori_in_elements.dat'
    elif soln=='d' or soln=='diff' or soln=='difference':
        if model.rotationxyd[0] == 0:
            readElementsExtra(model, 'd')
        val = model.rotationxyd[:model.ne]
        fileName = 'plot_rotation_rate_difference_in_elements.dat'
    elif soln=='c' or soln=='con' or soln=='constraint':
        if model.rotationxyc[0] == 0:
            readElementsExtra(model, 'c')
        val = model.rotationxyc[:model.ne]
        fileName = 'plot_rotation_rate_constraint_in_elements.dat'
    elif soln=='a' or soln=='all':
        rotationRate(model, '0', plate)
        rotationRate(model, '1', plate)
        rotationRate(model, 'd', plate)
        rotationRate(model, 'c', plate)
        return
    elif soln=='b' or soln=='both':
        rotationRate(model, '0', plate)
        rotationRate(model, '1', plate)
        return
    else:
        print('Solution '+str(soln)+' not recognised. No output produced.')
    
    if not plate is None:
        val += model.eulerrate[plate] * 0.5
    
    outf = open(fileName, 'w')
    for i in range(model.nel):
        outf.write(str(model.ellong[i])+' ' + str(model.ellat[i])+' '
                   + str(val[i])+'\n')
    outf.close()

def forcePotentials(model, fileName='force_potentials_on_points.dat'):
    """Prints force potentials at points for plotting.
    
    Parameters
    ----------
    model : permdefmap model
        Print force potentials from this model.
    fileName : str, default='force_potentials_on_points.dat'
        Name of output file.
    """
    outf=open(fileName,'w')
    for i in range(model.ngp):
        outf.write(str(model.gplong[i])+' ' + str(model.gplat[i])+' ')
        outf.write(str(model.potxx[i])+' ' + str(model.potyy[i])+' '
                   + str(model.potxy[i])+'\n')
    outf.close()

def forcesFromArealStress(model,
                          fileName='forces_from_areal_stress_in_elements.dat'):
    """Prints forces in elements from areal stress for plotting.
    
    Parameters
    ----------
    model : permdefmap model
        Print values from this model.
    fileName : str, default='forces_from_areal_stress_in_elements.dat'
        Name of output file.
    """
    
    from .geometry import pointAverageFromElements, forcesFromPots
    
    # Calculate forces from areal stress
    arealel = model.stressmax0 + model.stressmin0
    arealgp = pointAverageFromElements(model, arealel)
    fareal = forcesFromPots(model, -arealgp, -arealgp, np.zeros((model.ngp)))
    fxareal, fyareal = fareal
    
    # Print to file
    file = open(fileName, 'w')
    for i in range(model.nel):
        file.write(str(model.ellong[i])+' '+str(model.ellat[i])+' ')
        file.write(str(fxareal[i])+' '+str(fyareal[i])+'\n')

def forcesIsoDev(model, fileName='forces_iso_and_dev_in_elements.dat'):
    """Prints forces from isotropic and deviatoric parts for plotting.
    
    Force potentials are re-expressed in terms of an isotropic component
    (xx+yy)/2 and a deviatoric component (the remainder). Forces due to
    each component are calculated and printed to file.
    
    The format of the output file is
    long, lat, iso-force-x, iso-force-y, dev-force-x, dev-force-y
    
    Parameters
    ----------
    model : permdefmap model
        Print forces from this model.
    fileName : str, default='forces_iso_and_dev_in_elements.dat'
        Name of output file.
    """
    
    from .geometry import forcesFromPots
    
    # Find isotropic force potential
    potiso = (model.potxx+model.potyy) * 0.5
    # Find forces from isotropic potential
    fxiso, fyiso = forcesFromPots(model, potiso, potiso, 0)
    # Find forces from deviatoric potentials
    fxdev, fydev = forcesFromPots(model, model.potxx-potiso,
                                  model.potyy-potiso, model.potxy)
    
    # Print out forces
    file = open(fileName, 'w')
    for i in range(model.ngp):
        file.write(str(model.ellong[i])+' '+str(model.ellat[i])+' ')
        file.write(str(fxiso[i])+' '+str(fyiso[i])+' ')
        file.write(str(fxdev[i])+' '+str(fydev[i])+'\n')
    
def plotRss(columns=['Iteration','$A~priori$','$A~posteriori$'],
            rssFile='rss.log', plotFile='rss.eps', iterCol=True, logAxis=True):
    """Plots RSS against iterations.
    
    Parameters
    ----------
    columns : list of str
        List of column names. This should be as long as the number of
        columns present in the RSS file. To skip a column, set the name
        to ''. Default is ['Iteration','$A~priori$','$A~posteriori$']
    rssFile : str, default='rss.log'
        Name of file with RSS information.
    plotFile : str, default='rss.eps'
        Name of output file for plot. Suffix determines output file type.
    iterCol : bool, default=True
        Indicates that first column is the iteration number.
    logAxis : bool, default=True
        Plots RSS on a log axis if True.
    """
    
    # read file and remove empty lines
    file = open(rssFile, 'r')
    text = file.read().split('\n')
    
    for i in range(len(text)):
        if text[i] == '':
            text.pop(i)
        else:
            text[i] = text[i].split()
    
    # establish column names, adding/removing where necessary
    for i in range(len(columns), len(text[0])):
        columns.append('Column ' + str(i+1))
    for i in range(len(text[0]), len(columns)):
        columns.pop(i)
    
    # convert into array
    rsstext = np.array(text)
    if iterCol:
        iters = np.array(rsstext[:,0], dtype=np.int)
        rss = np.array(rsstext[:,1:], dtype=np.float)
        ittext = columns.pop(0)
    else:
        iters = np.arange(len(text))
        rss = np.array(rsstext, dtype=np.float)
        ittext = 'Iteration'
    
    # create plot
    fig, ax = plt.subplots()
    for i in range(len(columns)):
        if columns[i] != '':
            ax.plot(iters, rss[:,i], label=columns[i])
    ax.set_title('RSS with Iteration')
    ax.set_xlabel(ittext)
    ax.set_ylabel('Residual Sum of Squares')
    ax.set_xlim(0)
    ax.grid(True)
    ax.legend(loc='upper right')
    plt.tight_layout()
    if logAxis:
        ax.set_yscale('log')
    else:
        ax.set_ylim(0)
    fig.savefig(plotFile)
    fig.show()
    
def plotVline(model, vline, against='long', out=False, filePrefix='v-line',
              legend='best'):
    """Plot velocities along a velocity line
    
    Parameters
    ----------
    model : permdefmap model
        Plot velocity line from this model.
    vline : int
        Index of velocity line to be plotted.
    against : str, default='long'
        Plot the velocities against this variable. Options are:
            'longitude' or 'long' or 'x'
            'latitude' or 'lat' or 'y'
            'index' or 'i'
    out : bool, default=False
        If False, only plots the input velocities. If True, also plots the
        output velocities interpolared between the input velocities.
    filePrefix : str, default='v-line'
        Prefix of the file name.
    legend : str, default='best'
        Location of legend to be passed to matplotlib
    """
    
    import matplotlib.pyplot as plt
    
    against = against.lower()
    if against=='long' or against=='longitude' or against=='x':
        dist = model.gplong
        against = 'Longitude'
    elif against=='lat' or against=='latitude' or against=='y':
        dist = model.gplat
        against = 'Latitude'
    elif against=='index' or against=='i':
        dist = np.arange(model.nvlinegp[vline])
        against = 'Index'
    else:
        print('against not recognised. No output produced.')
        return
    
    gp = []
    ux = []
    uy = []
    fig, ax = plt.subplots()
    
    for i in range(2*model.nvlineside[vline] + 1):
        # Only plot points with value already
        if not model.interpvline[i,vline]:
            # Grid points
            if i%2 == 0:
                j = int(i/2)
                p = dist[model.gponvline[j,vline]]
                # Velcoities on faults
                if model.faultonvline[j,vline] > 0:
                    # Add point to lists
                    gp.extend([p,p])
                    if model.faultsignonvline[j,vline] > 0:
                        ux.extend([model.vlineminusx[j,vline],
                                   model.vlineplusx[j,vline]])
                        uy.extend([model.vlineminusy[j,vline],
                                   model.vlineplusy[j,vline]])
                    elif model.faultsignonvline[j,vline] < 0:
                        ux.extend([model.vlineplusx[j,vline],
                                   model.vlineminusx[j,vline]])
                        uy.extend([model.vlineplusy[j,vline],
                                   model.vlineminusy[j,vline]])
                    else:
                        print('Fault at point '+str(j)+' has no sign. No output produced.')
                # Velocities off faults
                else:
                    gp.append(p)
                    ux.append(model.vlinex[i,vline])
                    uy.append(model.vliney[i,vline])
            # Sections (plotted at average of two end points)
            else:
                j = int((i-1) / 2)
                p = (dist[model.gponvline[j,vline]]
                     + dist[model.gponvline[j+1,vline]]) * 0.5
                gp.append(p)
                ux.append(model.vlinex[i,vline])
                uy.append(model.vliney[i,vline])
    
    if out:
        # Plot input velocities as circles
        ax.plot(gp, ux, label='x-velocity in', marker='o', linestyle='')
        ax.plot(gp, uy, label='y-velocity in', marker='o', linestyle='')
        # Find all velocities, both input and interpolated output
        gp=[]
        ux=[]
        uy=[]
        for i in range(2*model.nvlineside[vline] + 1):
            # Grid points
            if i%2 == 0:
                j = int(i/2)
                p = dist[model.gponvline[j,vline]]
                # Velcoities on faults
                if model.faultonvline[j,vline] >= 0:
                    # Add point to lists
                    gp.extend([p,p])
                    if model.faultsignonvline[j,vline] > 0:
                        ux.extend([model.vlineoutmx[j,vline],
                                   model.vlineoutpx[j,vline]])
                        uy.extend([model.vlineoutmy[j,vline],
                                   model.vlineoutpy[j,vline]])
                    elif model.faultsignonvline[j,vline] < 0:
                        ux.extend([model.vlineoutpx[j,vline],
                                   model.vlineoutmx[j,vline]])
                        uy.extend([model.vlineoutpy[j,vline],
                                   model.vlineoutmy[j,vline]])
                    else:
                        print('Fault at point '+str(j)+' has no sign. No output produced.')
                # Velocities off faults
                else:
                    gp.append(p)
                    ux.append(model.vlineoutx[i,vline])
                    uy.append(model.vlineouty[i,vline])
            # Sections (plotted at average of two end points)
            else:
                j = int((i-1) / 2)
                p = (dist[model.gponvline[j,vline]]
                     + dist[model.gponvline[j+1,vline]]) * 0.5
                gp.append(p)
                ux.append(model.vlineoutx[i,vline])
                uy.append(model.vlineouty[i,vline])
        # Plot all velocities as line
        ax.set_prop_cycle(None)
        ax.plot(gp, ux, label='x-velocity out')
        ax.plot(gp, uy, label='y-velocity out')
    else:
        # Plot velocities
        ax.plot(gp, ux, label='x-velocity', marker='o')
        ax.plot(gp, uy, label='y-velocity', marker='o')
    # Finish plot
    ax.set_title('Velocity along Velocity Line '+str(vline))
    ax.set_xlabel(against)
    ax.set_ylabel('Velocity')
    plt.legend(loc=legend)
    fig.savefig(filePrefix+'_'+str(vline)+'.eps')
    fig.show()
            
def plotRheology(model, subr=False, fitPower=False):
    """Plot stress against strain rate
    
    Parameters
    ----------
    model : permdefmap model
        Plot rheology for this model
    subr : bool, default=False
        Group elements by subregion
    fitPower : bool, default=False
        Fit least squares power law.
    """
    
    import matplotlib.pyplot as plt
    
    # Determine stress and strain rate magnitudes
    stressmag = np.hypot(model.stressmax0[:model.nel],
                         model.stressmin0[:model.nel])
    strainmag = np.hypot(model.strainmax0[:model.nel],
                         model.strainmin0[:model.nel])
    stresslog = np.log(stressmag)
    strainlog = np.log(strainmag)
    # Initialise plot
    fig, ax = plt.subplots()
    
    # Plot by subregions if desired
    if subr:
        if model.nsubr == 0:
            print('No subregions loaded into model. Unable to plot by subregion')
        for r in range(model.nsubr):
            insubr = model.subrofel[:model.nel]==r
            ax.plot(strainmag[insubr], stressmag[insubr],
                    label=model.subrname[r], marker=',', linestyle='')
        if fitPower:
            plt.gca().set_prop_cycle(None)
            for r in range(model.nsubr):
                insubr = model.subrofel[:model.nel]==r
                coeff = np.polyfit(strainlog[insubr], stresslog[insubr], 1)
                fit = np.poly1d(coeff)
                print(model.subrname[r]+': n = '+str(1/coeff[0])+'; '+
                      'B = '+str(np.exp(coeff[1])))
                ax.plot(strainmag[insubr], np.exp(fit(strainlog[insubr])))
        plt.legend(ncol=2)
    # Plot without subregions if desired
    else:
        ax.plot(strainmag, stressmag, marker=',', linestyle='')
        if fitPower:
            coeff = np.polyfit(strainlog, stresslog, 1)
            fit = np.poly1d(coeff)
            print('n = '+str(1/coeff[0]))
            print('B = '+str(np.exp(coeff[1])))
            plt.gca().set_prop_cycle(None)
            ax.plot(strainmag, np.exp(fit(strainlog)))
    
    # Finish plot
    ax.set_title('Stress Magnitude against Strain Rate Magnitude')
    ax.set_xlabel('Strain Rate Magnitude')
    ax.set_ylabel('Stress Magnitude')
    ax.set_xscale('log')
    ax.set_yscale('log')
    fig.savefig('rheology.eps')
    fig.show()
    
def plotSingleFaultSlip(model, fault, pre=True, post=True, ob=True):
    """Plot fault slip rate along single fault
    
    Parameters
    ----------
    model : permdefmap model
        Plot fault from this model.
    fault : int
        Index of fault to plot.
    pre, post, ob : bool, default=True
        Plot apriori soluntion, aposteriori solution and slip rate
        observations, respectively.
    """
    
    import matplotlib.pyplot as plt
    
    # Number of fault segments
    nseg = model.nfaultseg[fault]
    
    # Set up array of distances of segment midpoints along fault
    length = np.cumsum(model.seglength[:nseg, fault])
    length2 = np.zeros_like(length)
    length2[1:] = length[:-1]
    dist = (length+length2) / 2 * 1e-3
    
    # Initialise plot
    fig, ax = plt.subplots()
    ax.axhline(0, color='k', linewidth=0.5)
    
    # Get apriori slip rates
    if pre:
        slipt0 = model.slipt0[:nseg, fault] * 1e3
        slipn0 = model.slipn0[:nseg, fault] * 1e3
        ax.plot(dist, slipt0)
        ax.plot(dist, slipn0)
    
    # Get aposteriori slip rates
    if post:
        slipt1 = model.slipt1[:nseg, fault] * 1e3
        slipn1 = model.slipn1[:nseg, fault] * 1e3
        plt.gca().set_prop_cycle(None)
        ax.plot(dist, slipt1, linestyle='--')
        ax.plot(dist, slipn1, linestyle='--')
    
    # Get slip rate observations
    if ob:
        # Slip rate observation components
        sliptob = np.zeros_like(dist) * np.nan
        slipnob = np.zeros_like(dist) * np.nan
        # Slip rate uncertainties
        sliptunc = np.zeros_like(dist) * np.nan
        slipnunc = np.zeros_like(dist) * np.nan
        
        # Observations on fault
        obs = model.getSlipObsOnFault(fault)
        for i in obs:
            # Only use observations with two components
            if model.nslipobcomp[i] == 2:
                side = model.sideofslipob[i]
                seg = model.segonside[side]
                sliptob[seg] = np.sum(model.slipobvalue[:,i]
                                      * model.slipobcoefft[:,i]) * 1e3
                slipnob[seg] = np.sum(model.slipobvalue[:,i]
                                      * model.slipobcoeffn[:,i]) * 1e3
                sliptunc[seg] = np.sum(model.slipobse[:,i]
                                      * np.abs(model.slipobcoefft[:,i])) * 2e3
                slipnunc[seg] = np.sum(model.slipobse[:,i]
                                      * np.abs(model.slipobcoeffn[:,i])) * 2e3
        
        # Plot observations
        plt.gca().set_prop_cycle(None)
        ax.errorbar(dist, sliptob, sliptunc, marker='.', linestyle='')
        ax.errorbar(dist, slipnob, slipnunc, marker='.', linestyle='')
    
    # Finish plot
    if model.faultname[fault]:
        name = model.faultname[fault]
    else:
        name = str(fault)
    ax.set_title('Slip Rate Along Fault '+name)
    ax.set_xlabel('Distance (km)')
    ax.set_ylabel('Slip rate (mm/yr)')
    fig.savefig('slip_fault_'+str(fault)+'.eps')
    fig.show()
    
def shape2xy(file, xyfile, attr=None):
    """Convert Shapefile to xy(z) file for plotting in GMT.
    
    Parameters
    ----------
    file : str
        Name of Shapefile to convert.
    xyfile : str
        Name of output xy(z) file.
    attr : str, default=None
        Name of attribute to use as z-value. Will present an error if the
        attribute values cannot be interpreted as numbers. Default is no
        z-value
    """
    
    import shapefile
    
    xy = open(xyfile,'w')
    
    sf = shapefile.Reader(file)
    # Shape data
    shape = sf.shapes()
    # Number of geometries with shape data
    ngeom = len(shape)
    
    # Select attribute to use as z-value if desired
    if attr is None:
        attrhas = False
    else:
        # Field definitions (attribute definitions) as a list
        fields = sf.fields
        # Records (attribute values) as a list
        records = sf.records()
        # Dictionary linking attribute name to index - start using a dummy, 1-index is first
        fieldind = {'':0}
        for i in range(1, len(fields)):
            fieldind[fields[i][0]] = (i-1)
        
        # Store index of desired attribute
        attrind = fieldind[attr]
        attrhas = True
    
    # Loop through the geometries to extract points and attributes
    for i in range(ngeom):
        points = shape[i].points
        
        # Start writing out geometry
        xy.write('>')
        if attrhas:
            xy.write('Z'+str(records[i][attrind]))
        xy.write('\n')
        
        # Write out points
        for p in points:
            xy.write(str(p[0])+' '+str(p[1])+'\n')














