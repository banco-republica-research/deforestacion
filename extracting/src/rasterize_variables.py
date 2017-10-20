import os
import geopandas as gpd
import rasterio
from rasterio import features

def rasterize(name, shape, variable, template_meta):
    '''
    This function takes a shapefile data and rasterize its data to create
    new arrays with template_meta specifications. The function will print out a
    new GeoTIFF under the name defined in name variable. If the all_touched
    option is uncommented, the function will update the values of the raster
    using the painting algorithm.
    '''
    file_name = str(name + '_' + variable + '.tif')
    with rasterio.open(os.path.join(os.environ['DATA_FOLDER'], 'SIMCI' ,file_name), 'w', **template_meta) as out:
        out_arr = out.read(1)
        if(shape[variable].dtypes == 'float64'):
            print(variable)
            shapes = ((geom, value) for geom, value in zip(shape.geometry,
                                                          shape[variable]))
            burned = features.rasterize(shapes = shapes, fill = 0, out = out_arr,
                                                       transform = out.transform,
                                                       #all_touched = True 
                                       )
            out.write_band(1, burned)

        else:
            print('Variable is not numeric!')
