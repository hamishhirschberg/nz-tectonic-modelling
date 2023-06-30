#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 17 11:10:02 2022

@author: Hamish Hirschberg
"""

def addSubregions(model, regions, subregionName='Reg_Name',
                  subregionNumber='ind', out=False):
    """Adds subregion identifiers to elements.
    
    A 'subregion' is a region containing one or more model elements that
    have similar properties or some other reason for being considered in a
    collective manner. Subregions do not affect the model inversion but can
    be used for analysis or adjustment. Subregions are extracted from a
    Shapefile that covers the model domain and elements are assigned based
    on which subregion their centre fits into. To avoid the jagged
    boundaries that may result from this, subregion boundaries can be
    included as dummy lines when setting up the model geometry.
    
    Parameters
    ----------
    model : permdefmap model
        Add subfaults to this model.
    regions : str
        Name of shapefile with subregion information.
    subregionName : str, default='Reg_Name'
        Name of Shapefile attribute containing subregion name.
    subregionNumber : str, default='ind'
        Name of Shapefile attribute containing subregion index number.
    out : bool, default=False
        If True, print xy file suitable for plotting in GMT.
    """
    
    import shapefile
    import numpy as np
    from ..fortran.postGmsh import pinr
    
    if out:
        file = open('subregions.xy', 'w')
    
    sf = shapefile.Reader(regions)
    # Shape data
    shape = sf.shapes()
    # Number of regions with shape data
    nreg = len(shape)
    # Field definitions (attribute definitions) as a list
    fields = sf.fields
    # Records (attribute values) as a list
    records = sf.records()
    # Dictionary linking attribute name to index - start using a dummy, 1-index is first
    fieldind = {'':0}
    for i in range(1, len(fields)):
        fieldind[fields[i][0]] = (i-1)
    
    # Loop through subregions to extract points and attributes.
    points = []
    for i in range(nreg):
        model.subrname[i] = records[i][fieldind[subregionName]]
        points.append(shape[i].points)
        
        # Write out polygons if desired.
        if out:
            file.write('>\n')
            for p in points[i]:
                file.write(str(round(p[0],6))+' '+str(round(p[1],6))+'\n')
        
        # Convert points to array
        points[i] = np.array(points[i]).transpose()
        
    if out:
        file.close()
    
    model.nsubr = nreg
    
    # Loop through elements to establish which subregion they are in.
    # Calculate centre of elements.
    ellong = (model.gplong[model.gp1ofel[:model.nel]] 
              + model.gplong[model.gp2ofel[:model.nel]]
              + model.gplong[model.gp3ofel[:model.nel]]) / 3
    ellat = (model.gplat[model.gp1ofel[:model.nel]] 
             + model.gplat[model.gp2ofel[:model.nel]]
             + model.gplat[model.gp3ofel[:model.nel]]) / 3
    for i in range(model.nel):
        # Loop through regions to see if it is in the region
        for r in range(nreg):
            inreg = pinr(points[r][0,:], points[r][1,:], ellong[i], ellat[i])
            if inreg:
                model.subrofel[i] = r
                break
        else:
            print('Element '+str(i)+' is not in a subregion.')

def obSubregions(model):
    """Determine the subregions of element strain rate observations
    
    Parameters
    ----------
    model : permdefmap model
        Model with the observations and subregions.
    
    Returns
    -------
    subrofstrainob : array of int
        Indices of the subregion each strain rate observation is in.
    """
    
    import numpy as np
    
    if model.nsubr == 0:
        print('No subregions loaded.')
        return
    
    subrofstrainob = -np.ones((model.maxeo), dtype=np.int)
    
    for i in range(model.nstrainob):
        subrofstrainob[i] = model.subrofel[model.elofstrainob[i]]
    
    return subrofstrainob

def boundary(file='subregions.xy'):
    """Return the subregion boundaries as lists
    
    Parameters
    ----------
    file : str, default='subregions.xy'
        Name of xy file that contains the list of points that define the
        boundaries of the subregions.
    
    Returns
    -------
    subr : list of list of tuples
        List of points that define the boundaries of the subregions.
    """
    
    







