This code, (R Studio code) developed during my internship, analyzes roadless areas in Italy.
For the first part, which aim to identify the roadless area i used: road data from Open Street Map, administrative data from GADM, and water and wetness layers from Copernicus.
The additional layers depend on the variables you wish to analyze.
Here’s a summary of the steps taken:

    Road Data from OpenStreetMap:
        Selected roads of interest
        Exclusion of tunnels
        Applied a 1 km buffer to each roadside to create a road mask

    Administrative Data from GADM:
        Combined with the road mask to define preliminary roadless areas

    Preliminary roadless areas    
        Reduced edge effects by applying internal and external buffers to each geometry
        Excluded areas smaller than 1 km²

    Copernicus Water and Wetness Layer:
        Excluded geometries covered by more than 70% water

    Final Geometries

    
    Creation of a Raster Stack:
        Integrated roadless areas, the area of Italy, and layers of interest (e.g., elevation, water availability)
        Converted the stack into a data frame

    Some analysis examples:
        Calculated area sizes
        Calculated mean values for the variables
        Performed Mann-Whitney tests to assess significance between road and roadless areas (e.g., roadless elevation vs. road elevation)
        Created visualizations:
            Correlation graphs
            Variable plots
            Comparisons of roadless and protected areas
