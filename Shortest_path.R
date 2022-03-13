##R code to get the shortest path in a graph(Network) with 5 nodes
library("igraph")
## Nodes and cost (km):
df2 = rbind(c(234,235,21.6),
            c(234,326,11.0),
            c(235,241,14.5),
            c(326,241,8.2),
            c(241,245,15.3),
            c(234,245,38.46))
names(df2) = c("start_id","end_id","newcost")

#Create Graph
g2 <-graph.data.frame(df2, directed=FALSE)

## calculate shortest path between vertex 234 and 245 considering the newcost(distances) as the weights
(tmp2 = get.shortest.paths(g2, from='234', to='245',weights=E(g2)$newcost))

# compute the min distances from '234' to all other vertices
tmp3 <-shortest.paths(g2,v='234',weights=E(g2)$newcost)

# print min distance from '234' to '245'
tmp3[1, which(V(g2)$name == '245')]