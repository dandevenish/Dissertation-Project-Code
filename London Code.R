packages <- c("sf", "tidyverse","tmap", "devtools", "gtools")

not_installed <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(not_installed) > 1) {
  install.packages(not_installed, repos = "https://cran.rstudio.com/", dep = T)
}

library(sf)         
library(tidyverse)  
library(tmap)       
library(devtools)
library(gtools)

install_version("tbart", version = "1.0", dep = T)

library(tbart)

msoa = st_read("NHS London MSOAs.gpkg", quiet = T) %>% `st_crs<-`(27700)

Hospitals = read.csv("NHS hospital locations.csv")

Hospitals$X_eastings = as.numeric(as.character((Hospitals$X_eastings)))

Hospitals$Y_Northings = as.numeric(as.character((Hospitals$Y_Northings)))


index = !is.na(Hospitals$Y_Northings)

Hospitals_sf = st_as_sf(Hospitals[index,], coords = c("X_eastings", "Y_Northings"), crs = 27700)
Hospitals_sf$Organisation.Name = "existing"

msoa_buffer = st_buffer(st_union(msoa), 2000)

Hospitals_sf = Hospitals_sf[msoa_buffer,]

colnames(msoa)

names(msoa)[9] <- 'Demand'

colnames(msoa)


tm_shape(msoa) + tm_polygons("Demand", palette = "Reds", n = 6, type = "kmeans") +
  tm_layout(title = "Hospital Admission Demand",
            legend.position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"))


grid = st_make_grid(msoa, 8000, what = "centers", crs = 27700, square = F)

sup_pt = data.frame(type = rep("potential",length(grid)))

st_geometry(sup_pt) = grid


colnames(Hospitals_sf)

names(Hospitals_sf)[1] <- 'type'

colnames(Hospitals_sf)

supply = rbind(Hospitals_sf,sup_pt)

supply = supply[msoa_buffer,]

p1 = tm_shape(msoa) + tm_borders() +
  tm_shape(supply %>% filter(type == "existing") ) + tm_dots(col = "red", size = 0.3) +
  tm_layout(title = "Existing")

p2 = tm_shape(msoa) + tm_borders() +
  tm_shape(supply %>% filter(type == "potential") ) + tm_dots(col = "blue", size = 0.05) +
  tm_layout(title = "Potential")
tmap_arrange(p1,p2)

d.mat <- (st_distance(msoa, supply)/1000)

dim(d.mat)

dim(msoa)

dim(supply)

d.mat[1:6, 1:6]

w <- msoa$Demand

head(w)

d.mat <- d.mat * w

d.mat[1:6, 1:6]

keep.index = which(supply$type == "existing")

head(keep.index)

length(keep.index)

p = length(keep.index) + 5

allocations.list2 = allocate(swdf1 = as(msoa, "Spatial"),
                             swdf2 = as(supply, "Spatial"), 
                             force = keep.index, 
                             p=p, 
                             metric = d.mat, 
                             verbose = F)

new_hospital_index = setdiff(allocations.list2, keep.index)




tm_shape(supply) + tm_dots(col = "white") + 
  tm_shape(msoa) + tm_polygons(col = "lightyellow") + 
  tm_shape(supply[c(allocations.list2, keep.index),]) + 
  tm_dots("type", title = "Hospital Type", size = 0.5, palette = "Set1") +
  tm_layout(title = "New Hospital Sites",
            frame = T, legend.outside = F, 
            legend.hist.width = 0.8,
            legend.format = list(digits = 1), 
            legend.position = c("left", "bottom"),
            legend.text.size = 1,
            legend.title.size = 1.5) +
  tm_compass(type = "arrow", position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"))


msoa_allocs <- allocations(as(msoa, "Spatial"), as(supply, "Spatial"), 
                         p=73,force=unique(allocations.list2))

msoa_allocs = st_as_sf(msoa_allocs)



create_star_lines = function (sf1,sf2, alloc) {
  if (missing(sf2)) 
    sf2 <- sf1
  if (missing(alloc)) {
    alloc <- sf1$allocation
  }
  co1 <- st_coordinates(st_centroid(sf1))
  co2 <- st_coordinates(st_centroid(sf2))
  result <- matrix(nrow = 0, ncol = 5)
  for (i in 1:nrow(co1)) {
    result <- rbind(result,  c(i, c(co1[i, 1], co2[alloc[i], 1]), c(co1[i, 2], co2[alloc[i], 2]) ) )
  }
  result = as.data.frame(result)
  ls <- apply(result, 1, function(x) 
  {
    v <- as.numeric(x[c(2,3,4,5)])
    m <- matrix(v, nrow = 2)
    return(st_sfc(st_linestring(m), crs = st_crs(sf1)))
  })
  ls = Reduce(c, ls)
  star_lines = data.frame(ID = result$V1)
  st_geometry(star_lines) = ls
  return(star_lines)
}

star_lines <- create_star_lines(msoa_allocs, supply)

p1 = tm_shape(msoa) + tm_borders(col = "lightgrey") + 
  tm_layout(title = "Star Lines") +
  tm_shape(star_lines)+tm_lines(col = "darkred", lwd = 2) +
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"))

p2 = tm_shape(msoa_allocs) + 
  tm_layout(title = "Distance from Hospital") +
  tm_polygons(col = "allocdist", 
              title = "Distance (metres)",
              palette = "Reds", n = 5, style = "kmeans") +
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"))
tmap_arrange(p1, p2)

view(msoa_allocs)

msoa$allocation = allocations.list2
msoa %>% st_drop_geometry() %>% group_by(allocation) %>% 
  summarise(Demand = mean(Demand)) %>% data.frame() -> hospital_demand
summary(hospital_demand$Demand)

msoa$allocation = match(allocations.list2, new_hospital_index)
msoa %>% st_drop_geometry() %>% group_by(allocation) %>% 
  summarise(Demand = mean(Demand)) %>% na.omit() %>% data.frame() -> new_hospital_demand
new_hospital_demand