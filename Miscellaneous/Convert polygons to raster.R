library(terra)

# Load your shapefile (replace "your_shapefile.shp" with your actual file path)
vector_data <- vect("C:/Users/Ebenbeck_J/Desktop/quadrat50_gk6orig.shp")
vector_data


# Calculate the resolution from the polygons
# Assume polygons are rectangular and take the width and height from one of them
bbox <- ext(vector_data) # Bounding box of the entire shapefile
single_poly <- vector_data[1] # Extract one polygon (assuming all are the same size)

# Compute the width and height of a single polygon
poly_bbox <- ext(single_poly)
cell_width <- poly_bbox[2] - poly_bbox[1]  # xmax - xmin
cell_height <- poly_bbox[4] - poly_bbox[3] # ymax - ymin

# Create a raster with this resolution
raster_template <- rast(ext=bbox, resolution=c(cell_width, cell_height))
crs(raster_template) <- crs(vector_data)


# Rasterize the polygons
# Replace "field" with the name of the attribute column in your shapefile
rasterized <- rasterize(vector_data, raster_template, field="Id")
plot(rasterized)

# Save the raster (optional)
writeRaster(rasterized, "C:/Users/Ebenbeck_J/Desktop/quadrat50_gk6orig.tif", overwrite=TRUE)

# Plot the raster
plot(rasterized)