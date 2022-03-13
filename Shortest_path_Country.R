#Country Guinea Bissau
#An an R-cript to:
#1. Extract network data from OSM and process it topologically
#2. Compute the shortest path between the city to each destination
#3. Produce a webmap(using tmap) showing this short path for all cities
#loading the required libraries
library(sf)
library(tidygraph)
library(igraph)
library(dplyr)
library(tibble)
library(ggplot2)
library(units)
library(tmap)
library(osmdata)
library(rgrass7)
library(link2GI)
library(nabor)
library(rgdal)
##Extracting network data(highways) from OSM using guinea bissau coordinates
Guinea_Bissau <- opq(bbox =  c(-16.243,11.275,-15.117,12.259)) %>% 
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf() %>% 
  osm_poly2line()

GBissau_center <- Guinea_Bissau$osm_lines %>% 
  select(highway)
GBissau_center
ggplot(data = GBissau_center) + geom_sf()
#Preparing the data and creating  a network from it
#Step1: Clean the data
writeVECT(
  SDF = GBissau_center, 
  vname = 'GBissau_center', 
  v.in.ogr_flags = 'overwrite'
)

# Execute the v.clean tool
execGRASS("g.proj", flags = c("c", "quiet"), proj4 = proj4)
execGRASS(
  cmd = 'v.clean', 
  input = 'GBissau_center', 
  output = 'GBissau_cleaned',        
  tool = 'break', 
  flags = c('overwrite', 'c')
)

# Read back into R
use_sf()
GBissau_center <- readVECT('GBissau_cleaned') %>%
  rename(geometry = geom) %>%
  select(-cat)

#Step 2:Assigning a Unique index to each edge
edges <- GBissau_center %>%
  mutate(edgeID = c(1:n()))

edges
#Step 3: Create nodes at the start and end point of each edge
nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))
nodes
#Step 4: Give each node a unique index
nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

nodes
#Step 5: Combine the node indices with the edges
source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)

edges
#Step 6: Remove all duplicate nodes
nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

nodes
#Step 7: Convert to tbl_graph
graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph
#Step 8: Putting it together
sf_to_tidygraph = function(x, directed = TRUE) {
  
  edges <- x %>%
    mutate(edgeID = c(1:n()))
  
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    select(-xy)
  
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  
}

sf_to_tidygraph(GBissau_center, directed = FALSE)
######
graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

graph

graph %>%
  activate(edges) %>%
  as_tibble() %>%
  st_as_sf() %>%
  group_by(highway) %>%
  summarise(length = sum(length))
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()) + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), size = 0.5)

#Getting the Cities data and plotting it 
#Guinea_Bissau 15 Cities
Bissau<- st_point(c(-15.587391,11.864822)) %>% 
  st_sfc(crs = 2095)

City1 <- st_point(c(-14.594618,11.953727)) %>% 
  st_sfc(crs = 2095)
Tite <- st_point(c(-15.391059,11.783326)) %>% 
  st_sfc(crs = 2095)

Buba <- st_point(c(-14.998395,11.598107)) %>% 
  st_sfc(crs = 2095)

Canchungo <- st_point(c(-16.039325,12.075971)) %>% 
  st_sfc(crs = 2095)

Quinhámel<- st_point(c(-15.854107,11.901866)) %>% 
  st_sfc(crs = 2095)
Mansaba<- st_point(c(-15.172501,12.298234)) %>% 
  st_sfc(crs = 2095)

Bula<- st_point(c(-15.713340,12.109311)) %>% 
  st_sfc(crs = 2095)

Bigene<- st_point(c(-15.539234,12.450113)) %>% 
  st_sfc(crs = 2095)

Binar <- st_point(c(-15.628140,12.090789 )) %>% 
  st_sfc(crs = 2095)

Cufar <- st_point(c(-15.168797,11.283235)) %>% 
  st_sfc(crs = 2095)

Bambadinca <- st_point(c(-14.861333,12.024110)) %>% 
  st_sfc(crs = 2095)

Deba <- st_point(c(-14.553870,12.042632)) %>% 
  st_sfc(crs = 2095)

Gabu<- st_point(c(-14.224181,12.283416)) %>% 
  st_sfc(crs =2095)

Pirada<- st_point(c(-14.153797,12.664967)) %>% 
  st_sfc(crs = 2095)


#plotting cities
library(ggplot2)
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 2.5) +
  geom_sf(data = Bissau, size = 2, col = 'firebrick') +
  geom_sf(data = City1, size = 2, col = 'firebrick') +
  geom_sf(data = Tite, size = 2, col = 'firebrick') +
  geom_sf(data =Buba , size = 2, col = 'firebrick') +
  geom_sf(data = Canchungo, size = 2, col = 'firebrick') +
  geom_sf(data = Quinhámel, size = 2, col = 'firebrick') +
  geom_sf(data = Mansaba, size = 2, col = 'firebrick') +
  geom_sf(data = Bula , size = 2, col = 'firebrick') +
  geom_sf(data = Bigene, size = 2, col = 'firebrick') +
  geom_sf(data = Binar, size = 2, col = 'firebrick') +
  geom_sf(data = Cufar, size = 2, col = 'firebrick') +
  geom_sf(data = Bambadinca, size = 2, col = 'firebrick') +
  geom_sf(data = Deba, size = 2, col = 'firebrick') +
  geom_sf(data = Gabu, size = 2, col = 'firebrick') +
  geom_sf(data = Pirada, size = 2, col = 'firebrick') +
  
  geom_sf_label(data = Bissau, aes(label = ' Bissau'), nudge_x = 0.004) +
  geom_sf_label(data = City1, aes(label = 'City1'), nudge_x = 0.005)
geom_sf_label(data = Tite, aes(label = 'Tite'), nudge_x = 0.004) +
  geom_sf_label(data = Buba, aes(label = 'Buba'), nudge_x = 0.005)+
  geom_sf_label(data = Canchungo, aes(label = 'Canchungo'), nudge_x = 0.004) +
  geom_sf_label(data = Quinhámel, aes(label = 'Quinhámel'), nudge_x = 0.005)+
  geom_sf_label(data = Mansaba, aes(label = 'Mansaba'), nudge_x = 0.004) +
  geom_sf_label(data =Bula , aes(label = 'Bula'), nudge_x = 0.005)+
  geom_sf_label(data = Bigene, aes(label = ' Bigene'), nudge_x = 0.004) +
  geom_sf_label(data = Binar, aes(label = 'Binar'), nudge_x = 0.005)+
  geom_sf_label(data = Bambadinca, aes(label = ' Bambadinca'), nudge_x = 0.005)+
  geom_sf_label(data = Wiltz, aes(label = 'Deba'), nudge_x = 0.004) +
  geom_sf_label(data = Gabu, aes(label = 'Gabu'), nudge_x = 0.005) +
  geom_sf_label(data = Pirada, aes(label = ' Pirada'), nudge_x = 0.004)+
  #coordinates of the cities
  
  city1 <- Bissau %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city2 <- City1 %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city3 <-Tite %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city4 <- Buba %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city5 <- Canchungo %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city6 <- Quinhámel %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city7 <- Mansaba %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city8 <- Bula %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city9 <- Bigene %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city10 <- Binar %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city11 <- Cufar %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city12 <- Bambadinca %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city13 <- Deba %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city14 <- Gabu %>%
  st_coordinates() %>%
  matrix(ncol = 2)
city15 <- Pirada %>%
  st_coordinates() %>%
  matrix(ncol = 2)

#Coordinates of all nodes 
nodes <- graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf()

coords <- nodes %>%
  st_coordinates()
#calculating nearest points 
node_index_1 <- knn(data = coords, query = city1, k = 1)
node_index_2 <- knn(data = coords, query = city2, k = 1)
node_index_3 <- knn(data = coords, query = city3, k = 1)
node_index_4 <- knn(data = coords, query = city4, k = 1)
node_index_5 <- knn(data = coords, query = city5, k = 1)
node_index_6 <- knn(data = coords, query = city6, k = 1)
node_index_7 <- knn(data = coords, query = city7, k = 1)
node_index_8 <- knn(data = coords, query = city8, k = 1)
node_index_9 <- knn(data = coords, query = city9, k = 1)
node_index_10 <- knn(data = coords, query = city10, k = 1)
node_index_11 <- knn(data = coords, query = city11, k = 1)
node_index_12 <- knn(data = coords, query = city12, k = 1)
node_index_13 <- knn(data = coords, query = city13, k = 1)
node_index_14 <- knn(data = coords, query = city14, k = 1)
node_index_15 <- knn(data = coords, query = city15, k = 1)
node_index_16 <- knn(data = coords, query = city16, k = 1)
node_1 <- nodes[node_index_1$nn.idx, ]
node_2 <- nodes[node_index_2$nn.idx, ]
node_3 <- nodes[node_index_3$nn.idx, ]
node_4 <- nodes[node_index_4$nn.idx, ]
node_5 <- nodes[node_index_5$nn.idx, ]
node_6 <- nodes[node_index_6$nn.idx, ]
node_7 <- nodes[node_index_7$nn.idx, ]
node_8 <- nodes[node_index_8$nn.idx, ]
node_9 <- nodes[node_index_9$nn.idx, ]
node_10 <- nodes[node_index_10$nn.idx, ]
node_11 <- nodes[node_index_11$nn.idx, ]
node_12 <- nodes[node_index_12$nn.idx, ]
node_13 <- nodes[node_index_13$nn.idx, ]
node_14 <- nodes[node_index_14$nn.idx, ]
node_15 <- nodes[node_index_15$nn.idx, ]
node_16 <- nodes[node_index_16$nn.idx, ]
#calculating the shortest path from the created network(Cities and highways connecting them)
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_2$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph1 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_3$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph2 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_4$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph3 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_5$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph4 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_6$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph5 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_7$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph6 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_8$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph7 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_9$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph8 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_10$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph9 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_11$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph10 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_12$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph11 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_13$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph12 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_14$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph13 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_15$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)
path_graph14 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
path <- shortest_paths(
  graph = graph,
  from = node_1$nodeID, 
  to = node_16$nodeID,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path_graph15 <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 0.5) +
  geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph1 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph2 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph3 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph4 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph5 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph6 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph7 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph8 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph9 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph10 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph11 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph12 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph13 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph14 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph15 %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = node_1, size = 2) +
  geom_sf(data = node_2, size = 2)  +
  geom_sf(data = node_3, size = 2)  +
  geom_sf(data = node_4, size = 2)  +
  geom_sf(data = node_5, size = 2)  +
  geom_sf(data = node_6, size = 2)  +
  geom_sf(data = node_7, size = 2)  +
  geom_sf(data = node_8, size = 2)  +
  geom_sf(data = node_9, size = 2)  +
  geom_sf(data = node_10, size = 2)  +
  geom_sf(data = node_11, size = 2)  +
  geom_sf(data = node_12, size = 2)  +
  geom_sf(data = node_13, size = 2)  +
  geom_sf(data = node_14, size = 2)  +
  geom_sf(data = node_15, size = 2)  +
  geom_sf(data = node_16, size = 2)  +
  geom_sf_label(data =  node_1, aes(label = 'Bissau'), nudge_x = 0.004) +
  geom_sf_label(data =  node_2, aes(label = 'City1 '), nudge_x = 0.005)+
  geom_sf_label(data =  node_3, aes(label = 'Tite'), nudge_x = 0.004)+
  geom_sf_label(data =  node_4, aes(label = 'Buba'), nudge_x = 0.005)+
  geom_sf_label(data =  node_5, aes(label = 'Canchungo'), nudge_x = 0.004)+
  geom_sf_label(data =  node_6, aes(label = 'Quinhámel'), nudge_x = 0.005)+
  geom_sf_label(data =  node_7, aes(label = 'Mansaba'), nudge_x = 0.004)+
  geom_sf_label(data =  node_8, aes(label = 'Bula'), nudge_x = 0.005)+
  geom_sf_label(data =  node_9, aes(label = 'Bigene'), nudge_x = 0.004)+
  geom_sf_label(data =  node_10, aes(label = 'Binar'), nudge_x = 0.005)+
  geom_sf_label(data =  node_11, aes(label = 'Cufar'), nudge_x = 0.004)+
  geom_sf_label(data =  node_12, aes(label = 'Bambadinca'), nudge_x = 0.005)+
  geom_sf_label(data =  node_13, aes(label = 'Deba'), nudge_x = 0.004)+
  geom_sf_label(data =  node_14, aes(label = 'Gabu'), nudge_x = 0.005)+
  geom_sf_label(data =  node_15, aes(label = 'Pirada'), nudge_x = 0.004)
