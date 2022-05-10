# GOAL ####
# Identify clusters of Essential Destinations which can become essential places
# The idea of essential places is informed by pandemic era essential worker locations/ key in-person activity destinations
# but we are not strictly sticking to the pandemic framework.

# PACKAGES ####
library(tidyverse)
library(dbscan) # Density Based scanning package has an assortment of point clustering options
library(sf)
library(mapview)

`%notin%` <- Negate(`%in%`)

# INPUTS ####
essential_destinations<- st_read( "output/DestinationData.gpkg", "essential_destinations_PT" )

# SETUP ####
ed <- essential_destinations # copy to ed to modify
# pull coordinates from geometry to run as matrix
coordinates_dest <- ed %>%
  st_coordinates()

# OPTION 1: Density Based scan (DBSCAN)
# Determine inputs for the dbscan process, https://medium.com/@tarammullin/dbscan-parameter-estimation-ff8330e3a3bd
# minPts: the fewest number of points required to form a cluster
# eds: "epsilon" is the maximum distance two points can be from one another while still belonging to the same cluster
# b/c data is projected with a unit of meters: 
# set epsilon to the euclidean distance of a x min walk shed using 3mi/hr walk speed

min <- 2 # "maximum minutes to get to next destination in the cluster as the crow flies"
eps <- 3*1609/60*min # (3mi/hr) * (1609m/mi) * (1hr/60min) * min 
minPts <- 4 # minimum of three destinations to make a cluster

# With the above parameters a cluster is defined as 
# at least three destinations that are at most .1 miles apart as the crow flies
ed$cluster_dbscan <- dbscan(coordinates_dest, eps = eps, minPts = minPts) %>% 
  pluck('cluster') %>% as.character()
mapview(ed, zcol= 'cluster_dbscan')

# OPTION 2: Use a hierarchical density based scan (HDBSCAN) 
# https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html
# where the only input is min points
clusters <- hdbscan(coordinates_dest, minPts = minPts)
ed$cluster_hdbscan <- clusters %>% 
  pluck('cluster') %>% as.character()
mapview(ed, zcol= 'cluster_hdbscan')

# # core distance is the distance from a particular feature that must be traveled 
# # to create a cluster with a minimum of x features including itself
# core <- ed 
# core$core_dist_hd <- clusters %>% pluck('coredist')
# buffered <- st_buffer(core, core$core_dist_hd)
# mapview(buffered, zcol= "cluster_hdbscan") + mapview(ed)
# 
# dissolved <- buffered %>% 
#   group_by(cluster_hdbscan) %>% 
#   summarize(geometry= st_union(geom))
# mapview(filter(dissolved, cluster_hdbscan!= "0"), zcol='cluster_hdbscan')


# OPTION 3: OPTICS
# OPTICS takes the upper limit epsilon size for the neighborhood and minPts
clusters <- optics(coordinates_dest, eps= eps, minPts = minPts)
# eps cl is the threshold to identify clusters
clusters_extract <- extractDBSCAN(clusters, eps_cl=150)
ed$cluster_optics <- clusters_extract %>% 
  pluck('cluster') %>% as.character()

mapview(ed, zcol= 'cluster_optics')

# st_write(ed, "GIS/cluster_testing.gpkg", "essentialPlaces_clusters")

# summarize number of clusters found in each option
cluster_summary <- ed %>% 
  st_drop_geometry() %>% 
  summarize(optics= max(as.numeric(cluster_optics)),
           hbscan = max(as.numeric(cluster_hdbscan)),
           dbscan = max(as.numeric(cluster_dbscan)))


# Create compromise clusters region-wide ###
# the HDBSCAN does a better job a clustering in the suburban areas of the mpo region
# while the DBSCAN does a better job of clustering within the urban/inner core region

# Pull in subregion shapes
subregions <- st_read("output/AggregationAreas.gpkg", 'MPO_SubRegions')
icc<- subregions %>%
  filter(grepl("(ICC)", subregion))

# mapview(ed,)+ icc

# Divide clusters between inner core and non-innercore ####
# Problem: we need to consider clusters that span the ICC boarder, without double counting destinations
# Solution: All points belonging to DBSCAN clusters which have a centroid within the ICC boundary will be clustered included as DBSCAN clusters
# Then, for all remaining points, run through the hdbscan clustering again and bind the two cluster results together

# find the centroid for all DBSCAN clusters
centriod_dbscan <- ed %>% 
  filter(cluster_dbscan > 0) %>%
  arrange(cluster_dbscan) %>% 
  group_by(cluster_dbscan) %>%
  summarize(
    count = n(),
    geometry = st_centroid(st_combine(geom))) %>% 
  st_as_sf()

# find DBSCAN clusters within the icc boundary
icc_clusters <- centriod_dbscan %>% 
  st_filter(icc, .predicate = st_within)

ed_clustered_icc <- ed %>% 
  filter(cluster_dbscan %in% icc_clusters$cluster_dbscan) %>% 
  select(name, address, type, type_detail, id, cluster= cluster_dbscan)
# mapview(ed_clustered)+icc

icc_clustered_poly <- ed_clustered_icc %>%
  # buffer points to smooth polygon shape
  st_buffer(50) %>% 
  group_by(cluster) %>% 
  summarize() %>% 
  st_convex_hull() %>% 
  st_as_sf()
mapview(ed_clustered_icc)+icc + icc_clustered_poly

# find max number of clusters so that we can make unique cluster ids for non-icc-clusters
if (max(as.numeric((icc_clustered_poly$cluster))) < 1000 ) { 
  addAmt <- 1000
} else {
  print("Check cluster add amt for ids.")
  }

ed_clustered_nonicc <- ed %>% 
  filter(cluster_dbscan %notin% icc_clusters$cluster_dbscan) %>% 
  # filter out unclustered within icc
  filter(id %notin% st_filter(ed, icc, .predicate = st_within)$id) %>% 
  select(name, address, type, type_detail, id)

min2 <- 6 # "maximum minutes to get to next destination in the cluster as the crow flies"
eps2 <- 3*1609/60*min2 # (3mi/hr) * (1609m/mi) * (1hr/60min) * min 


clusters <- dbscan(st_coordinates(ed_clustered_nonicc), eps=eps2, minPts = minPts)
ed_clustered_nonicc$cluster <- clusters %>% 
  pluck('cluster') %>% 
  lapply(function(x){x+addAmt}) %>% 
  as.character() 

ed_clustered_nonicc <- ed_clustered_nonicc %>% 
  filter(cluster> addAmt)

nonicc_clusters_poly <- ed_clustered_nonicc%>% 
  st_buffer(50) %>% 
  group_by(cluster) %>% 
  summarize() %>% 
  st_convex_hull() %>% 
  st_as_sf()

# mapview(ed2, zcol= 'cluster2')+icc +nonicc_clusters

mapview(icc_clustered_poly)+ nonicc_clusters_poly


# Bring together icc and nonicc clusters ####

# check for unique cluster ids across icc and nonicc clusters
if(anyDuplicated(c(icc_clustered_poly$cluster, nonicc_clusters_poly$cluster))==0){ 
  print("Clutsters okay to bind together.")
} else {
    print("Make sure cluster ids are unique before binding.")
}

ed_clustered <- bind_rows(ed_clustered_icc, ed_clustered_nonicc) %>% 
  st_as_sf()
ed_clustered_poly <- bind_rows(icc_clustered_poly, nonicc_clusters_poly) %>% 
  st_as_sf()

# Central destination for each cluster ####
# Problem: Need to identify which point to route to using conveyal
# Solution: Identity the 'most central' destination within a cluster, use this destination as the point to route to
# 'most central point' will be the point closest to the cluster centroid.
# By picking the destination closest to the center of the destination clusters, 
# we route to a real place rather than an average point which could be removed from the road network 
# (ie, the park next to a could of strip malls)
cluster_centriods <-ed_clustered %>% 
  ungroup() %>% 
  arrange(cluster, id) %>% 
  group_by(cluster) %>%
  summarize(
    count = n(),
    geometry = st_centroid(st_combine(geom)))

mapview(ed_clustered, zcol='cluster')+ 
  mapview(cluster_centriods, color= "red")

central_dest <- ed_clustered %>% 
  ungroup() %>% 
  arrange(cluster,id)
central_dest$centroid_geom<-rep(cluster_centriods$geometry,cluster_centriods$count)
central_dest<- central_dest %>% 
  mutate(dist = map2_dbl(.x= geom, .y= centroid_geom, ~unclass(st_distance(.x,.y))))%>% 
  group_by(cluster) %>% 
  arrange(dist) %>% 
  slice(1) %>% 
  select(cluster, central_id = id)

# Identify place options by cluster diversity ####
# Summarize types within clusters
# Need to develop rules to find what clusters should be considered essential places
ed_summary <- ed_clustered %>% 
  st_drop_geometry() %>% 
  group_by(cluster) %>% 
  summarize(
    n = n(),
    types = paste0(type, collapse= ', ')
    )
ed_summary_byType <- ed_clustered %>% 
  st_drop_geometry() %>% 
  group_by(cluster, type) %>% 
  count() %>%
  mutate(
    type_ab= case_when(
      type == "Grocery"~ 'gr',
      type == "Hospital" ~ 'hp',
      type == "Licensed Clinic" ~ 'lc',
      type == "Retail Pharmacy" ~ 'ph',
      type == "Community Health Center" ~ 'hc',
      type == "Post Office" ~ 'po',
      type == "Library" ~ "li",
      type == "Townhall" ~ "th",
      type == "Farmers' Markets" ~ "fm",
      TRUE ~ 'other'),
    type_general= case_when(
      type_ab %in% c('th', 'li', 'po')  ~ "civic",
      type_ab %in% c('hp', 'ph', 'hc', 'lc') ~ "health",
      type_ab %in% c('fm', 'gr') ~ "food")) %>% 
  pivot_wider(id_cols = cluster, values_from = n, names_from = c(type_general,type_ab)) %>% 
  left_join(select(ed_summary, cluster, n))

ed_summary_byTypeGen <- ed_clustered %>% 
  st_drop_geometry() %>%
  mutate(
    type_ab= case_when(
      type == "Grocery"~ 'gr',
      type == "Hospital" ~ 'hp',
      type == "Licensed Clinic" ~ 'lc',
      type == "Retail Pharmacy" ~ 'ph',
      type == "Community Health Center" ~ 'hc',
      type == "Post Office" ~ 'po',
      type == "Library" ~ "li",
      type == "Townhall" ~ "th",
      type == "Farmers' Markets" ~ "fm",
      TRUE ~ 'other'),
    type_general= case_when(
      type_ab %in% c('th', 'li', 'po')  ~ "civic",
      type_ab %in% c('hp', 'ph', 'hc', 'lc') ~ "health",
      type_ab %in% c('fm', 'gr') ~ "food")) %>%
  group_by(cluster, type_general) %>% 
  count() %>%
  pivot_wider(id_cols = cluster, values_from = n, names_from = c(type_general)) %>% 
  left_join(select(ed_summary, cluster, n))

# Option 1: all clusters considered places
ep1 <- ed_summary_byType %>% 
  left_join(ed_clustered_poly) %>% 
  st_as_sf()
ep1_pt <- ep1 %>% 
  st_drop_geometry() %>% 
  left_join(central_dest) %>% 
  st_as_sf()

# Option 2: only considered a place if at least two different general types included
ep2 <- ed_summary_byTypeGen %>% 
  rowwise() %>% 
  filter(sum(is.na(c(food, health, civic)))<2) %>% 
  left_join(ed_clustered_poly) %>% 
  st_as_sf()
ep2_pt <- ep2 %>% 
  st_drop_geometry() %>% 
  left_join(central_dest) %>% 
  st_as_sf()

ep3 <- ed_summary_byTypeGen %>% 
  rowwise() %>% 
  filter(sum(is.na(c(food, health, civic)))<1) %>% 
  left_join(ed_clustered_poly) %>% 
  st_as_sf()
ep3_pt <- ep3 %>% 
  st_drop_geometry() %>% 
  left_join(central_dest) %>% 
  st_as_sf()

mapview(ep1)+ ep2 + ep3

mapview(ep3)+ ep3_pt



# Save Data ####
gpkg ="DestinationData.gpkg"
st_write(ep1, gpkg, 'essentailPlace_Option1_POLY', append = T)
st_write(ep1_pt, gpkg,'essentailPlace_Option1_PT', append = T)
st_write(ep2, gpkg, 'essentailPlace_Option2_POLY', append = T)
st_write(ep2_pt, gpkg,'essentailPlace_Option2_PT', append = T)
st_write(ep3, gpkg, 'essentailPlace_Option3_POLY', append = T)
st_write(ep3_pt, gpkg,'essentailPlace_Option3_PT', append = T)

# Save Data for Conveyal ####
ep_conveyal <- st_read("DestinationData.gpkg",'essentailPlace_Option3_PT') %>% 
  rename(id= cluster, weight = n) %>% 
  mutate(type = 'essentialPlace') %>% 
  prep_pt_to_csv_keepID_weight()

ep_csv <- ep_conveyal %>% 
  pt_to_csv("output/essentailPlaces.csv")

# References and Background ####
# https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html
# https://02522-cua.github.io/lecturenotes/spatial-clustering.html
# https://02522-cua.github.io/lecturenotes/spatial-clustering.html4
# https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/how-density-based-clustering-works.htm
# https://hdbscan.readthedocs.io/en/latest/parameter_selection.html
