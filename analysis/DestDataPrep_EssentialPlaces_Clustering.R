# GOAL ####
# Identify clusters of Essential Destinations which can become essential places
# The idea of essential places is informed by pandemic era essential worker locations/ key in-person activity settings
# but we are not strictly sticking to the pandemic framework.

# PACKAGES ####
library(dbscan) # Density Based scanning package has an assortment of point clustering options
library(sf)
library(mapview)

# INPUTS ####
essential_places<- st_read( "output/DestinationData.gpkg", "essential_destinations_PT" )

# SETUP ####
ep <- essential_places # copy to ep to modify
# pull coordinates from geometry to run as matrix
coordinates_dest <- ep %>%
  st_coordinates()

# OPTION 1: Density Based scan (DBSCAN)
# Determine inputs for the dbscan process, https://medium.com/@tarammullin/dbscan-parameter-estimation-ff8330e3a3bd
# minPts: the fewest number of points required to form a cluster
# eps: "epsilon" is the maximum distance two points can be from one another while still belonging to the same cluster
# b/c data is projected with a unit of meters: 
# set epsilon to the euclidean distance of a x min walk shed using 3mi/hr walk speed

min <- 2 # "maximum minutes to get to next destination in the cluster as the crow flies"
eps <- 3*1609/60*min # (3mi/hr) * (1609m/mi) * (1hr/60min) * min 
minPts <- 3 # minimum of three destinations to make a cluster

# With the above parameters a cluster is defined as 
# at least three destinations that are at most .1 miles apart as the crow flies
ep$cluster_dbscan <- dbscan(coordinates_dest, eps = eps, minPts = minPts) %>% 
  pluck('cluster') %>% as.character()
mapview(ep, zcol= 'cluster_dbscan')

# OPTION 2: Use a hierarchical density based scan (HDBSCAN) 
# https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html
# where the only input is min points
clusters <- hdbscan(coordinates_dest, minPts = minPts)
ep$cluster_hdbscan <- clusters %>% 
  pluck('cluster') %>% as.character()
mapview(ep, zcol= 'cluster_hdbscan')

# core distance is the distance from a particular feature that must be traveled 
# to create a cluster with a minimum of x features including itself
core <- ep 
core$core_dist_hd <- clusters %>% pluck('coredist')
buffered <- st_buffer(core, core$core_dist_hd)
mapview(buffered, zcol= "cluster_hdbscan") + mapview(ep)

dissolved <- buffered %>% 
  group_by(cluster_hdbscan) %>% 
  summarize(geometry= st_union(geom))
mapview(filter(dissolved, cluster_hdbscan!= "0"), zcol='cluster_hdbscan')


# OPTION 3: OPTICS
# OPTICS takes the upper limit epsilon size for the neighborhood and minPts
clusters <- optics(coordinates_dest, eps= eps, minPts = minPts)
# eps cl is the threshold to identify clusters
clusters_extract <- extractDBSCAN(clusters, eps_cl=150)
ep$cluster_optics <- clusters_extract %>% 
  pluck('cluster') %>% as.character()

mapview(ep, zcol= 'cluster_optics')

# st_write(ep, "GIS/cluster_testing.gpkg", "essentialPlaces_clusters")

# summarize number of clusters found in each option
cluster_summary <- ep %>% 
  st_drop_geometry() %>% 
  summarize(optics= max(as.numeric(cluster_optics)),
           hbscan = max(as.numeric(cluster_hdbscan)),
           dbscan = max(as.numeric(cluster_dbscan)))


# Create compromise clusters region-wide ###
# the HDBSCAN does a better job a clustering in the suburban areas of the mpo region
# while the DBSCAN does a better job of clustering within the urban/inner core region

# Pull in subregion shapes
subregions <- st_read("output/AggregationAreas.gpkg", 'CommunityTypes')
icc<- subregions %>%
  filter(grepl("(ICC)", subregion))

# mapview(ep,)+ icc
# Divide clusters between inner core and non-innercore
ep_alt <- ep
# identify point closest to cluster centroid
centriod_dbscan <- ep_alt %>% 
  filter(cluster_dbscan>0) %>%
  arrange(cluster_dbscan) %>% 
  ungroup() %>% 
  group_by(cluster_dbscan) %>%
  summarize(
    count = n(),
    geometry = st_centroid(st_combine(geom)))

mapview(filter(ep_alt, cluster_dbscan!= "0"), zcol='cluster_dbscan')+ 
  mapview(centriod_dbscan, color= "red")

test <- ep_alt %>% 
  arrange(cluster_dbscan) %>% 
  filter(cluster_dbscan>0)


test$centroid_geom<-rep(centriod_dbscan$geometry,centriod_dbscan$count)
test<- test %>% 
  mutate(dist = map2_dbl(.x= geom, .y= centroid_geom, ~unclass(st_distance(.x,.y))))
  
# check boundaries and create a compromise cluster dataset

# TODO: identify the most central destination within a cluster, use this destination as the point to route to


# References and Background ####
# https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html
# https://02522-cua.github.io/lecturenotes/spatial-clustering.html
# https://02522-cua.github.io/lecturenotes/spatial-clustering.html4
# https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/how-density-based-clustering-works.htm
# https://hdbscan.readthedocs.io/en/latest/parameter_selection.html
