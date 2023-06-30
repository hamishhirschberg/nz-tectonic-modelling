#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 13 15:42:02 2023

@author: Hamish Hirschberg
"""

def profileAve(model, points, forces=None, rotate=False, prec=1e-12):
    """Calculate mean of forces along profile.
    
    Parameters
    ----------
    model : permdefmap model
        Calculate mean from this model.
    points : list of float
        Coordinates of end points of profile given as
        [long0, lat0, long1, lat1]. Profile is calculated in Cartesian
        coordinates.
    forces : array of float, default=None
        Calculate mean of these forces. Input is in the format
        [forcex, forcey] where forcex and forcey are the list of x- and
        y-components of forces. Default uses forces from the given model.
    rotate : bool, default=False
        Present forces in rotated coordinates of profile-normal and
        profile-transverse.
    prec : float, default=1e-12
        Precision for accounting for numerical errors when determing
        intersections of lines.
    
    Returns
    -------
    ave : list of float
        Mean forces in the format [forcex, forcey].
    """
    
    import numpy as np
    
    # Get input forces into desired format
    if forces is None:
        forcex = model.forcex[:model.nel]
        forcey = model.forcey[:model.nel]
    else:
        forcex = forces[0][:model.nel]
        forcey = forces[1][:model.nel]
    
    # Get points into desired format
    lo0 = points[0]
    la0 = points[1]
    lo1 = points[2]
    la1 = points[3]
    
    # Force averages
    avex = 0.
    avey = 0.
    
    # Find element just after first point (to avoid coincident grid points)
    lo = lo0*(1-1e-5) + lo1*1e-5
    la = la0*(1-1e-5) + la1*1e-5
    out = model.findElement(lo, la, weights=True, flag=True, el=0)
    if out[4] == 2:
        raise Exception('Unable to find element for start of profile.')
        return
    el = out[0]
    
    # Length of profile accounted for so far
    prevdist = 0.
    
    # Loop through elements on profile (limited to number of elements)
    for e in range(model.nel):
        # Start with element with start of profile
        # Find intersect of the line with the sides of element
        lenfrac = [0, 0, 0]
        # Grid points of element (start point also end point)
        gps = np.array([model.gp1ofel[el], model.gp2ofel[el],
                        model.gp3ofel[el], model.gp1ofel[el]])
        for side in range(3):
            # Find relative intersect of side with segment
            x0, x1 = model.gplong[gps[side:side+2]]
            y0, y1 = model.gplat[gps[side:side+2]]
            det = (lo1-lo0) * (y1-y0) - (la1-la0) * (x1-x0)
            dist = (x1-lo0) * (y1-y0) - (y1-la0) * (x1-x0)
            # Distance along side where profile intersects
            # Distances are fractions of length of profile
            lenfrac[side] = dist / det
        # Identify order in which profile intersects sides
        # Mid is middle side and max is last side intersected
        midf = np.median(lenfrac)
        maxf = np.max(lenfrac)
        midi = lenfrac.index(midf)
        maxi = lenfrac.index(maxf)
        if midf <= prevdist + prec:
            # Here profile enters element by intersecting mid
            # Portion of profile in element starts where previous one ended
            start = prevdist + 0
            # Portion ends when segment exits element or profile ends
            end = min(maxf, 1)
            # Side through which profile exits element
            nextside = maxi + 0
        else:
            # Here profile exits element by intersecting mid
            start = prevdist + 0
            end = min(midf, 1)
            nextside = midi + 0
        
        # Length fraction of profile in this element
        # Whole length of profile is normalised to 1.
        length = end - start
        # Calculate contribution to average
        avex += forcex[el] * length
        avey += forcey[el] * length
        
        # Set where on profile this element ended
        prevdist = end + 0
        if end >= 1 - prec:
            # End of profile
            break
        
        if nextside == 0:
            # Next side is side 3
            side = model.side3ofel[el]
        elif nextside == 1:
            # Next side is side 1
            side = model.side1ofel[el]
        else:
            # Next side is side 2
            side = model.side2ofel[el]
        # Find next element
        if model.el1onside[side] == el:
            el = model.el2onside[side] + 0
        else:
            el = model.el1onside[side] + 0
        if el == 0:
            # Boundary reached
            break
    else:
        raise Exception('Maximum number of elements reached on profile.')
        return
    
    # Check whole profile was used
    if 1-end > prec:
        print('Only '+str(round(end,3))+' proportion of profile found. '
              + 'Force average calculated based on this portion.')
        avex /= end
        avey /= end
    
    if rotate:
        # Rotate forces to along profile
        # Calculate direction vector
        dy = la1 - la0
        dx = ((lo1 - lo0)
              * np.cos(np.radians((la0+la1) * 0.5)))
        dl = np.hypot(dx, dy)
        tx = dx / dl        # = +ny
        ty = dy / dl        # = -nx
        avet = avex*tx + avey*ty
        aven = -avex*ty + avey*tx
        # Return rotated forces
        return [avet, aven]
        
    else:
        # Return unrotated forces
        return [avex, avey]



















