#Emily L. Bierbaum
#October 20, 2022
#Lesson on GIS and ARC-GIS


# install.packages(c("sp","rgdal","raster","rgeos","geosphere","dismo"))
# Once those packages are installed, download those libraries

library(sp) # classes for vector data (polygons, points, lines)
library(rgdal) # basic operations for spatial data
library(raster) # handles rasters
library(rgeos) # methods for vector files
library(geosphere) # more methods for vector files
library(dismo) # species distribution modeling tools


#Made folder GIS_Lessons
## Within it are Country_Shapefiles, My_Climate_Space, My_locations, and WORLDCLIM_rasters

# Went to 'WorldClim.org' and examined the historical climate data
# Downloaded Bioclimatic variables at 10min = 'bio 10m' 
# Copied those 19 files from downloads to the 'WORLDCLIM-rasters' folder
# Unzip the files


# Load a raster to examine it
Biol1<-raster("WORLDCLIM_rasters/wc2.1_10m_bio_1.tif")
plot(Biol1)
# Make sure you are in the correct working directory!
# Biol1 shows the mean temperature of the earth from 1970-2000 in Celsius



# Next we can convert Celsius to Fahrenheit
Bio1_f <- Biol1 * (9/5)+32
plot(Bio1_f)


Biol1
Bio1_f

# Create a stack with multiple rasters
# Need to have the same extent and resolution 

clim_stack <- stack(list.files("WORLDCLIM_Rasters", full.names = TRUE, pattern = ".tif"))
# Output a list of files that are within that folder 
# Output the name of all the files within the folder use (list.files)


#Let’s observe the stack

plot(clim_stack, nc = 5) 
  #nc plots five columns of the 19 rasters
  #nc= number of columns 



plot(clim_stack[[17]])
# Look at only image 17

clim_stack


# Select a subset (three) of the climate variables 
# We will make our own raster stack
my_clim_stack <- stack(
  raster('WORLDCLIM_Rasters/wc2.1_10m_bio_2.tif'),
  raster('WORLDCLIM_Rasters/wc2.1_10m_bio_4.tif'),
  raster('WORLDCLIM_Rasters/wc2.1_10m_bio_13.tif')
)

# To examine the name of the variable on visit this website: https://www.worldclim.org/data/bioclim.html 
# Rename the variable to match the variable name 
names(my_clim_stack) <- c("mean_diurnal_range", "temp_seasonality", "precip_wettest_mo")


plot(my_clim_stack)


 
# Plots bivariate relationships among climate variables 
pairs(my_clim_stack)

# Visit Natural Earth and under "Admin 0-Countries" section download countries
# Unzip these and load country shape files
# Place in "Country_Shapefiles" folder
countries<-shapefile("Country_Shapefiles/ne_10m_admin_0_countries.shp")

countries

head(countries)

# Create the outline dark blue and the color of the countries
plot(countries, col="goldenrod", border="darkblue")

# Open a new plot device
dev.new()

plot(my_clim_stack[[3]]) # Plot the mean annual temperature
plot(countries, add=TRUE) # add the countries shapefile

# Pick ten points to model our climatic niche
my_sites<-as.data.frame(click(n=10))

names(my_sites) <- c("longitude", "latitude")
# We can examine "my_sites" which has the latitude and longitude for each point
my_sites
# List the longitude and latitude


# Extract the climate variable values at the ten points
env<-as.data.frame(extract(my_clim_stack, my_sites))
env


# Join the environmental data with the site data 
my_sites<-cbind(my_sites, env)

my_sites


# Create a shape file of my sites, then can export and use on ARC-GIS
# Assign points to a coordinate system


# We will turn our points into a shapefile by specifying a coordinate system
# We will acquire the coordinate system from our raster file

#Get the projection from the raster file
myCRS<- projection(my_clim_stack)
myCRS



# Apply that projection to our points to create a shapefile
# Make into a points file
my_sites_shape <- SpatialPointsDataFrame(coords = my_sites, data = my_sites, proj4string = CRS(myCRS))


# Plot the shapefile along with other elements
plot(my_clim_stack[[2]]) 
points(my_sites_shape, pch=16) # show sites on the map
plot(countries, add=TRUE) #adds lines around each country 



# Generate a set of random points for comparison to our selected locations
bg<- as.data.frame(randomPoints(my_clim_stack, n=1000))
head(bg)

names(bg) <- c("longitude", "latitude")
# Label longitude and latitude
head(bg)

plot(my_clim_stack[[2]])
points(bg, pch = ".")
# Plot on the map


# Extract environmental variables for the 1000 random points generated
# Could not include 10,000 random points because R would crash 
bgEnv <- as.data.frame(extract(my_clim_stack, bg))
head(bgEnv)


# Bind the random points and environmental data
bg<- cbind(bg, bgEnv)
head(bg)


# Combine site and background data and create a variable called "pres_bg"
# Point will = 1 and background = 0
pres_bg<- c(rep(1,nrow(my_sites)), rep(0,nrow(bg)))

pres_bg

train_data <- data.frame(pres_bg = pres_bg, 
                         rbind(my_sites, bg))

head(train_data)



# The model is ready to train
# We will implement a generalized linear model (glm)
# `pres_bg` is the dependent variable and climate as the independent variable

my_model <- glm(
  pres_bg ~ mean_diurnal_range*temp_seasonality*precip_wettest_mo
  + I(mean_diurnal_range^2) + I(temp_seasonality^2) + I(precip_wettest_mo^2),
  data=train_data,
  family = "binomial",
  weights=c(rep(1, nrow(my_sites)), rep(nrow(my_sites)/nrow(bg), nrow(bg))
            ))
  
  
summary(my_model)


# Ignore the warning message
# Use model to predict climate niche

# Examine "My World" to see what it looks like
my_world <- predict(
  my_clim_stack,
  my_model,
  type='response'
)


my_world


# Plot my world
plot(my_world)
plot(countries, add=TRUE)
points(my_sites_shape, pch=16)

# Save R graphics (png or JPEG) and add as an image!!! R cannot handle all of the code



# Save your WORLD
writeRaster(my_world, 'My_Climate_Space/my_world', format='GTiff', overwrite=TRUE, progress='text')
# The closer a point is to 1, the better match it is to the points already selected


# The climate preference is defined 
# Threshold your "preferred" climate regions
my_world_thresh <- my_world >= quantile(my_world, 0.75)
plot(my_world_thresh)
# Everything above 75 gets a one (green) everything else is lost
# Convert all values not equal to 1 to NA...
# Sets a threshold to select the top 25% most preferred regions and comparing those to the globe

# Compare my climate space to the world
# Convert values not equal to 1 to NA
# Use the "calc" function to apply a custom function
my_world_thresh <- calc(my_world_thresh, fun=function(x) ifelse(x==0 | is.na(x), NA, 1))


# Gather the random sites
my_best_sites <- randomPoints(my_world_thresh, 1000)
my_best_env <- as.data.frame(extract(my_clim_stack, my_best_sites))


# Plot values of climate variables for my world vs whole globe
# Of the globe
smoothScatter(x=bgEnv$mean_diurnal_range, 
              y=bgEnv$precip_wettest_mo, 
              col='lightblue')

# Add in the points of value of my WORLD
points(my_best_env$mean_diurnal_range,
       my_best_env$precip_wettest_mo, col='red', pch=16, cex=0.2)



points(my_sites$mean_diurnal_range, 
       my_sites$precip_wettest_mo, pch=16)
# Adds in the clicked location observations



# convert all values not equal to 1 to NA...
# using "calc" function to implement a custom function
my_world_thresh <- calc(my_world_thresh, fun=function(x) ifelse(x==0 | is.na(x), NA, 1))

# get random sites
my_best_sites <- randomPoints(my_world_thresh, 10000)
my_best_env <- as.data.frame(extract(my_clim_stack, my_best_sites))

# plot world's climate
smoothScatter(x=bgEnv$temperature_seasonality, y=bgEnv$precip_driest_quarter, col='lightblue')
points(my_best_env$temperature_seasonality, my_best_env$precip_driest_quarter, col='red', pch=16, cex=0.2)
points(my_sites$temperature_seasonality, my_sites$precip_driest_quarter, pch=16)
legend(
  'bottomright',
  inset=0.01,
  legend=c('world', 'my niche', 'my locations'),
  pch=16,
  col=c('lightblue', 'red', 'black'),
  pt.cex=c(1, 0.4, 1)
  
)

# Now there is a legend
