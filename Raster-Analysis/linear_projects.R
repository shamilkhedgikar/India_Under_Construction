packages <- c("sf","osc","dplyr","mapview","lubridate","raster","fasterize","gdalUtils","rgdal","esri2sf")
lapply(packages,require,character.only=T)

setwd("D:/Hemanth/Sustainability")
linear <- st_as_sf(readRDS("Linear_Projects.RDS"))
linear <- readRDS("Linear_Projects.RDS")
nonlinear <- readRDS("Non_Linear_Projects.RDS")

linear = st_set_crs(linear,as.character(crs(esa2)))
mapview(linear)

table(year(linear$Completion.Date))

files <- list.files(path=".",pattern=".tif",full.names = T)

dist <- list()
for (i in 1:length(files)) {
  dist[[i]] <- data.frame(table(getValues(raster(files[i]))))
  print(i)
}

esa1 <- raster("D:/ESA-CCI-1992-2015/India Extent/esa_1992.tif") 
esa2 <- raster("D:/ESA-CCI-1992-2015/India Extent/esa_2014.tif")
crosstab(esa1,esa2,long=F)

linear_comp <- linear %>% filter

mask_ras <- raster(extent(linear[1,]),crs=crs(linear))
rasterize(linear@polygons,esa1,1)
plot(mask(esa2,mask_ras))


############################### Rasters for projects #####################

linear_raster <- list()
for (i in 1:nrow(linear)){
  IT = linear[i,]
  x_crop <- crop(esa2, IT)
  writeOGR(as(IT,"Spatial"), tempdir(), f <- basename(tempfile()), 'ESRI Shapefile')
  gdal_rasterize(sprintf('%s/%s.shp', tempdir(), f), 
                 f2 <- tempfile(fileext='.tif'), at=T,
                 tr=res(x_crop), te=c(bbox(x_crop)), burn=1, 
                 init=0, a_nodata=0, ot='Byte')
  
  linear_raster[[i]] <- x_crop*raster(f2)
  names(linear_raster[[i]]) <- IT$X1.x
  print(i)

#######################BUffers ####################

buffers <- list.files(pattern=".shp",recursive = T)
buffers <- lapply(buffers, read_sf)  

buffer_lulc <- vector("list",length(buffers))
for (i in 1:length(buffers)) {
  buffer_lulc[[i]] <- vector("list",nrow(buffers[[i]]))
  for (j in 1:nrow(buffers[[i]])) {
    IT = buffers[[i]][j,]
    x_crop <- crop(esa2, IT)
    
    writeOGR(as(IT,"Spatial"), tempdir(), f <- basename(tempfile()), 'ESRI Shapefile')
    
    gdal_rasterize(sprintf('%s/%s.shp', tempdir(), f), 
                   f2 <- tempfile(fileext='.tif'), at=T,
                   tr=res(x_crop), te=c(bbox(x_crop)), burn=1, 
                   init=0, a_nodata=0, ot='Byte')
    
    buffer_lulc[[i]][j] <- x_crop*raster(f2)
    names(buffer_lulc[[i]][j]) <- IT$X1.x
    print(j)
  }
  print(paste("Shapefile done.",i))
}

###### raster crosstab from 1992 to 2014####
linear_crosstab <- vector("list",length(buffers))
for(i in 1:length(buffer_lulc)){
  linear_crosstab[[i]] <- lapply(buffer_lulc[[i]], function(x){
    y  <- crop(esa1,extent(x))
    y = mask(y,x)
    crosstab(y,x)
  })
  print(i)
}

for (i in 1:length(raster_crosstab)) {
  lapply(raster_crosstab[[i]],function(x){
    urban_92 <- data.frame(x) %>% filter(esa_1992=="190") %>% summarise(`1992_190` = sum(Freq))
    urban_14 <- data.frame(x) %>% filter(layer=="190") %>% summarise(`2014_190` = sum(Freq))
    cbind(urban_14,urban_92)
   })
  print(i)
}
