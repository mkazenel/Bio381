# Spatial Analyses
# Samanthan Alger

# File formats for spatial data
# Most common format: shape file -- a collection of multiple files that make up the geospatial dataset
# Three files necessary to make up a shape file:
# 1) "shapefile" -- .shp --> shape format (e.g., defines polygons for countries on a map)
# 2) .dbf -- attribute table --> contains the information associated with each polygon (e.g., population size, gdp for each country)
# 3) .shx -- shape index formate --> machine code version of shape file so that computer can read it

# How R reads in these data: 
# readOGR(dsn=directory/folder name, layer=name of shape file)

# After you read in your data, R will coerce your data into slots; you can access slots using the @ symbol, as you would use $ in a data frame
# These slots are: 
# .data (data from attribute table)
# .polygons (the shapes and coordinates of each polygon)
# .bbox (bounding box -- identifies coordinates for spatial extent of map)
# .proj4string (coordinate reference system)

# Structure of spatial data

#Read in the data:

lnd <- readOGR("data", "london_sport") # Read in the data

head(lnd@data) # use the @ symbol to refer to the data slot of the lnd object

mean(lnd$Partic_Per) # use the $ symbol to refer to an attribute in the 'data' slot

sapply(lnd@data, class) # To check the classes of all the variabes in a spatial dataset, you can use this command

lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001)) # Unexpectedly, Pop_2001 is a factor, coerce the variable into the correct, numeric format...

  # type "lnd@" and then tab to see which slots are in lnd...

#check number of rows and columns...
nrow(lnd)
ncol(lnd)

# Basic plotting
plot(lnd)

# DATA QUERYING AND PLOTTING

# select rows of lnd@data where sports participation is less than 15
lnd@data[lnd$Partic_Per < 15, ] # This is interrogating the attribute data slot only of the lnd object..

#Using the same logic to subset and plot spatial objects (polygon slot)

# Select zones where sports participation is between 20 and 25%
sel <- lnd$Partic_Per > 20 & lnd$Partic_Per < 25
head(sel) # test output of previous selection

plot(lnd[sel, ]) # plotting all TRUE Cases

# To see these areas in context with other areas of the maps... use add= TRUE..

plot(lnd, col = "lightgrey") # plot the london_sport object
sel <- lnd$Partic_Per > 25
plot(lnd[ sel, ], col = "turquoise", add = TRUE) # add selected zones to map


# USING tmap()

library(tmap) # load tmap package

tmap_mode="plot"

# Check out it's capabilities!
vignette("tmap-nutshell")

# Quick Thematic Maps (qtm):

# load spatial data included in the tmap package
data("World", "metro")

# Try typing head(World$...) or head(metro@...) to see the type of data...
head(World@data)

head(metro@coords)

#  quick plot example: 
qtm(World, fill = "income_grp", text = "iso_a3", text.size = "AREA") # use "World$" to see the two attributes: income_grp and iso_a3, text.size= area: text is sized increasingly with coutry area size.


# MORE COMPLEX MAPPING

# calculate annual growth rate
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

# plot
tm_shape(World) +
  tm_polygons("income_grp", palette = "-Blues", # using income level to color code map
              title = "Income class", contrast = 0.7, border.col = "grey30", id = "name") + # specifies legend
  
  tm_text("iso_a3", size = "AREA", col = "grey30", root=3) +
  
  tm_shape(metro) +
  tm_bubbles("pop2010", col = "growth", border.col = "black", # adding bubbles, colored and sized based on specified factors
             border.alpha = 0.5,
             breaks = c(-Inf, 0, 2, 4, 6, Inf) ,
             palette = "-RdYlGn",
             title.size = "Metro population (2010)", 
             title.col = "Annual growth rate (%)",
             id = "name",
             popup.vars=c("pop2010", "pop2020", "growth")) + 
  tm_format_World() + 
  tm_style_gray()


# INTERACTIVE VIEW MODE

tmap_mode("view") # sets the tmap mode to interactive viewing

last_map() # returns the last map to be modified or created

