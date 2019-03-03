library(sp)
library(rgeos)

coord <- matrix(c(0,0, 0, 20, 16, 20, 16, 0, 0, 0), ncol=2, byrow=T)

pg <- Polygon(coord)
ps <- Polygons(list(pg),1)
sps <- SpatialPolygons(list(ps), proj4string = CRS(as.character(NA)))
plot(sps)

s <- 1
hexpoints <- spsample(sps, n=100, type="hexagonal", cellsize=s)
hexgrid <- HexPoints2SpatialPolygons(hexpoints, dx = s)
plot(hexgrid)

hexgrid_df <- fortify(hexgrid, region="ID")

hexcenter <- as.data.frame(coordinates(hexgrid)) %>%
  mutate(id=rownames(.)) %>%
  rename(long=V1,
         lat=V2)

maps <- list(grid=hexgrid,
             grid_df=hexgrid_df,
             grid_ct=hexcenter)

save(maps, file="data/maps.RData")

dist_matrix <- round(gDistance(maps$grid, maps$grid, byid = T, hausdorff = T))
dist_matrix_exact <- round(gDistance(maps$grid, maps$grid, byid = T, hausdorff = T),4)

save(dist_matrix, file = "data/dist_matrix.RData")
save(dist_matrix_exact, file = "data/dist_matrix_exact.RData")