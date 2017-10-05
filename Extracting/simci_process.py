import os 
import geopandas as gpd
import rasterio
from rasterio import features

from src import rasterize_variables as rs

'''
This code will open both coca and illegal mining shapefiles to convert them
to raster format. To do this, we will open, reproject, and use nightlight
raster data as a template.
'''

# Open shapefiles and store them in a list

simci_files = ["Coca_01_16_grillas1k.shp", "EVOA_MTPL_1k_18N.shp"]
simci_shapes = [gpd.read_file(os.path.join(os.environ['DATA_FOLDER'], 'SIMCI', x)) for x in
                simci_files]
simci_shapes_proj = [x.to_crs({'init': 'epsg:4326'}) for x in simci_shapes] 

# Open raster template (nightlights - same as deforestation)

nightlights = rasterio.open(os.path.join(os.environ['DATA_FOLDER'], 'NOAA2',
                                         'light_brick_colombia.tif'))

# Extract metadata from raster to copy to shapefiles
meta = nightlights.meta.copy()
meta.update(compress='lzw') #Does this affect the GeoTIFF?


#Rasterize!
for column in simci_shapes_proj[0]:
    rs.rasterize('coca', simci_shapes_proj[0], column, meta)

for column in simci_shapes_proj[1]:
    rs.rasterize('illegal_mining', simci_shapes_proj[1], column, meta)


