library(FedData)
library(sf)
library(elevatr)
library(NatParksPalettes)
library(rayshader)
library(colorspace)
library(NatParksPalettes)
library(MetBrewer)
library(scico)
library(glue)
height_shade2 <- function (heightmap, 
                           heightmap2 = NULL,
                           texture1, 
                           texture2, 
                           split, 
                           keep_user_par = TRUE) 
{
  
  t1 <- texture1
  t2 <- texture2
  
  if (!is.null(heightmap2)) {
    # Sea level and above
    
    tempfilename = tempfile()
    
    grDevices::png(tempfilename, width = nrow(heightmap), height = ncol(heightmap))
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t1, 
                    useRaster = TRUE)
    graphics::image(rayshader:::fliplr(heightmap2), axes = FALSE, col = t2, 
                    useRaster = TRUE, add = TRUE)
    grDevices::dev.off()
    tempmap = png::readPNG(tempfilename)
  } else {
    range1 <- c(min(heightmap, na.rm = TRUE), split)
    range2 <- c(split, max(heightmap, na.rm = TRUE))
    
    # Sea level and above
    
    tempfilename = tempfile()
    
    grDevices::png(tempfilename, width = nrow(heightmap), height = ncol(heightmap))
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t1, 
                    useRaster = TRUE, zlim = range1)
    graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t2, 
                    useRaster = TRUE, zlim = range2, add = TRUE)
    grDevices::dev.off()
    tempmap = png::readPNG(tempfilename)
  }
  
  return(tempmap)
}


map <- "fchurch"
protected_areas <- get_padus(template = "Frank Church-River Of No Return Wilderness",
                             label = "FCWA") 

pa_buff <- protected_areas[[1]] %>% 
  st_buffer(., dist = 1)
z <- 11
zelev <- get_elev_raster(pa_buff, z = z, clip = "location")
mat <- raster_to_matrix(zelev)

pal <- "gray_arches2"

c1 <- scico(palette = "grayC", n = 5)
c2 <- natparks.pals(name = "Volcanoes", n = 7)
colors <- c(c1[2:4], rev(c2[3:7]))
# Calculate the aspect ratio of the plot so you can translate the dimensions

w <- nrow(mat)
h <- ncol(mat)

# Scale so longer side is 1

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))
rgl::rgl.close()

# Create the initial 3D object
shadow_depth <- min(mat, na.rm = TRUE)

# setting resolution to about 5x for height
res <- mean(round(terra::res(zelev))) / 8

try(rgl::rgl.close())

# Create the initial 3D object

mat %>%
  # This adds the coloring, we're passing in our `colors` object
  height_shade(texture = grDevices::colorRampPalette(c("white", "grey90", colors), bias = .5)(256))  %>%
  plot_3d(heightmap = mat,
          # This is my preference, I don't love the `solid` in most cases
          solid = FALSE,
          # You might need to hone this in depending on the data resolution;
          # lower values exaggerate the height
          z = res,
          # Set the location of the shadow, i.e. where the floor is.
          # This is on the same scale as your data, so call `zelev` to see the
          # min/max, and set it however far below min as you like.
          shadowdepth = shadow_depth,
          # Set the window size relatively small with the dimensions of our data.
          # Don't make this too big because it will just take longer to build,
          # and we're going to resize with `render_highquality()` below.
          windowsize = c(1200,1200), 
          # This is the azimuth, like the angle of the sun.
          # 90 degrees is directly above, 0 degrees is a profile view.
          phi = 90, 
          zoom = 1, 
          # `theta` is the rotations of the map. Keeping it at 0 will preserve
          # the standard (i.e. north is up) orientation of a plot
          theta = 0, 
          background = "white") 

# Use this to adjust the view after building the window object
render_camera(phi = 36, zoom = 0.65, theta = -20)

###############################
# Create High Quality Graphic #
###############################

# You should only move on if you have the object set up
# as you want it, including colors, resolution, viewing position, etc.
library(tidyverse)
# Ensure dir exists for these graphics
if (!dir.exists(glue("images/{map}"))) {
  dir.create(glue("images/{map}"))
}

# Set up outfile where graphic will be saved.
# Note that I am not tracking the `images` directory, and this
# is because these files are big enough to make tracking them on
# GitHub difficult. 
outfile <- str_to_lower(glue("images/{map}/{map}_{pal}_z{z}.png"))

# Now that everything is assigned, save these objects so we
# can use then in our markup script
saveRDS(list(
  map = map,
  pal = pal,
  z = z,
  colors = colors,
  outfile = outfile
), "images/fchurch/fchurch.rds")

# Wrap this in brackets so it runs as chunk
{
  # Test write a PNG to ensure the file path is good.
  # You don't want `render_highquality()` to fail after it's 
  # taken hours to render.
  png::writePNG(matrix(1), outfile)
  # I like to track when I start the render
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  render_highquality(
    # We test-wrote to this file above, so we know it's good
    outfile, 
    # See rayrender::render_scene for more info, but best
    # sample method ('sobol') works best with values over 256
    samples = 300, 
    # Turn light off because we're using environment_light
    light = FALSE, 
    # All it takes is accidentally interacting with a render that takes
    # hours in total to decide you NEVER want it interactive
    interactive = FALSE,
    # HDR lighting used to light the scene
    #environment_light = "../bathybase/env/phalzer_forest_01_4k.hdr",
    # Adjust this value to brighten or darken lighting
    intensity_env = 1.75,
    # Rotate the light -- positive values move it counter-clockwise
    rotate_env = 90,
    # This effectively sets the resolution of the final graphic,
    # because you increase the number of pixels here.
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}
