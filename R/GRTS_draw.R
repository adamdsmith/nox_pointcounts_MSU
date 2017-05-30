if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")

pacman::p_load(rgdal, sp, raster, rgeos, dplyr, spsurvey)

# Load shapefiles
stands <- readOGR("./Data", "used_stands", verbose = FALSE, stringsAsFactors = FALSE)
stands <- raster::aggregate(stands, by = "Stand_Id")

### FILTER TO STAND 17-6
stands <- stands[grepl("17-6", stands$Stand_Id), ]
stands_df <- stands@data

# Simplify stands attributes
stands@data <- stands@data %>%
  mutate(ac = round(sapply(stands@polygons, function(x) x@area / 4046.8564224), 2),
         ha = round(sapply(stands@polygons, function(x) x@area / 10000), 2))

## Buffer area of interest by 200 m so no areas are excluded a priori
stands_lg <- gBuffer(stands, width = 200)

# Create hexagon grid with inradius of 100 m (area = 34,641 m2 [3.464 ha])
hex_pts <- spsample(stands_lg, type="hexagonal", cellsize = 100 * 2)
hex_ply <- HexPoints2SpatialPolygons(hex_pts)
hex_ply <- hex_ply[stands,] 
hex_ply <- SpatialPolygonsDataFrame(hex_ply, 
                                    data = data.frame(hex = 1:length(hex_ply)),
                                    match.ID = FALSE)

# Calculate area of interest in each hexagon
hex_int <- raster::intersect(hex_ply, stands)
hex_int@data <- hex_int@data %>%
  select(-ac, -ha) %>%
  mutate(area_ha = gArea(hex_int, byid = TRUE) / 10000)

hex_stand <- hex_int@data %>%
  group_by(hex) %>%
  summarise(area_ha = sum(as.numeric(area_ha)))

hex_ply@data <- left_join(hex_ply@data, hex_stand, by = "hex")
hex_ply <- hex_ply[hex_ply$area_ha >= pi/2, ]

# Get points in retained hex cells
hex_pts <- SpatialPointsDataFrame(hex_pts,
                                  data = over(hex_pts, hex_ply),
                                  match.ID = FALSE)
hex_pts <- hex_pts[!is.na(hex_pts$hex), ]

# GRTS sample calculation
dsgn <- list(None = list(panel = c(Base = 100), seltype = "Continuous", over = 75))
sites <- grts(design = dsgn,
              DesignID = "Point",
              type.frame = "finite",
              src.frame = "sp.object",
              sp.object = hex_pts,
              mdcaty = "area_ha",
              shapefile = FALSE)
bird_pts <- as(sites, "SpatialPointsDataFrame")

# Load custom function to neatly order GRTS sample locations
source("./R/order_label.R")
bird_pts@data <- order_label(bird_pts, "panel")
proj4string(bird_pts) <- proj4string(stands)
writeOGR(bird_pts, "./Output", "point_counts_17-6", driver = "ESRI Shapefile", overwrite_layer = TRUE)
