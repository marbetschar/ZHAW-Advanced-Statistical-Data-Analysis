library("gRbase")
gapple <- dag(c("Sun"),
              c("Water","Sun"),
              c("Height","Sun","Water"),
              c("Crop","Height","Parasites"),
              c("Parasites"))
set.seed(23)
plot(gapple)

nodes(gapple)
edgeList(gapple)
edges(gapple)

parents("Height", gapple)
children("Height", gapple)

ancestralSet("Height", gapple)

ga <- as(gapple, "matrix")
ga


g1 <- as(ga, "igraph")
plot(g1)
g2 <- addEdge("Water", "Parasites", g1)
plot(g2)
g3 <- removeEdge("Sun","Water",g2)
plot(g3)

is.complete(g3)
