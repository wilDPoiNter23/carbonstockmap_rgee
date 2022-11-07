library(pacman)
p_load(rgee,remotes,reticulate,stars,tmap,tidyverse,hrbrthemes,ggplot2,
       dplyr,viridis,rgdal,XML,maptools,sf,data.table,rlist,mice,wesanderson,raster,
       phenofit,rTIMESAT,patchwork,ggthemr,ggpubr,caret,googleCloudStorageR,reshape,readxl,corrplot,
       outliers)
# Initialize GEE in the R
ee_Initialize()
# Slect target imagecollection filter by date, aoi and cloud cover
s2Sr <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED") 
AOI <- - ee$FeatureCollection('projects/ee-kby/assets/coastal')$geomerty()
collection <- s2Sr$
  filterDate(start, end)$
  filter(
    ee$Filter$listContains(
      'system:band_names','constant')$Not()
  )$filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20))$
  filterBounds(AOI)$sort("system:time_start")
# mask cloud and cloud shadow 
maskCloudAndShadowsSR <- function(image) {
  cloudProb <- image$select('MSK_CLDPRB')
  snowProb <- image$select('MSK_SNWPRB')
  cloud <- cloudProb$lt(20)
  snow <- snowProb$lt(10)
  scl <- image$select('SCL')
  shadow <- scl$eq(3)   #3 = cloud shadow
  cirrus <- scl$eq(10)    # 10 = cirrus
  # Cloud probability less than 20% or cloud shadow classification
  mask <- (cloud$And(snow))$And(cirrus$neq(1))$And(shadow$neq(1))
  return(image$updateMask(mask)$divide(10000)$
           select("B.*")
         $copyProperties(image, list("system:time_start"))
  )
}
collection_clean<- collection$map(maskCloudAndShadowsSR)
# imagecollection infromation
images_date <- ee_get_date_ic(collection)%>%
  as_tibble() %>%
  separate(time_start, into = c("year", "month", "day"), sep = "-") %>%
  .[,c("year","month","day")]

date <- ee_get_date_ic(collection)%>%
  as_tibble() %>% .$time_start 
cppList <- collection$aggregate_array("CLOUDY_PIXEL_PERCENTAGE")$getInfo() 
sentinel_name <- collection$aggregate_array("SPACECRAFT_NAME")$getInfo() 
index <- collection$aggregate_array("system:index")$getInfo() 
snow_ice <- collection$aggregate_array("SNOW_ICE_PERCENTAGE")$getInfo() %>% as.numeric()
hpc <- collection$aggregate_array('HIGH_PROBA_CLOUDS_PERCENTAGE')$getInfo() %>% as.numeric()
system_id<-collection$aggregate_array("system:id")$getInfo() 
data_describ <- tibble(
  daysort= c(1:length(date)),
  cpp=cppList,
  sn=sentinel_name,
  id=index,
  si= snow_ice,
  date = date,
  hpc = hpc
) %>%
  bind_cols(.,images_date)

####use ggplot2 to describe metadata
th <- theme_grey()+theme(aspect.ratio = 1)+
  theme(axis.text.x= element_text(family = 'Times New Roman',colour="black", size = 13))+
  theme(axis.text.y= element_text(family = 'Times New Roman',colour="black", size = 13))+
  theme(axis.title.y= element_text(family = 'Times New Roman',colour="black", size = 15))+
  theme(axis.title.x= element_text(family = 'Times New Roman',colour="black", size = 15)) 

hist_cpp <- data_describ %>%
  ggplot(aes(x= month, y= cpp, fill= sn)) +
  geom_col( 
    position = 'dodge')  +
  scale_fill_manual(values=c("#69b3a2", "#FFC0CB"))+ scale_color_viridis(discrete = TRUE)+ylab('Cloud cover (%)')+xlab('Month')+
  labs(fill="Spacecraft tpye")+th
 
line_cpp <- data_describ %>%
  ggplot(aes(x= date, y = cpp)) +ylab('Cloud cover (%)')+xlab('Date')+
  geom_line(color = '#404080') +th 

hist_hpc <- data_describ %>%
  ggplot(aes(x= hpc, fill= sn)) +
  geom_histogram( bins = 40,
                  position = 'dodge')  +
  scale_fill_manual(values=c("#69b3a2", "#FFC0CB"))+ scale_color_viridis(discrete = TRUE)+ylab('Counts')+xlab('High probability of cloud (%)')+
  labs(fill="Spacecraft type")+th