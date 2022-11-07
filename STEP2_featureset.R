# Calculate vegetation indexes
addforclassify <- function(image) {
  NDVI <- image$normalizedDifference(c('B8', 'B4'))$rename('NDVI')
  NDVI_A <- image$normalizedDifference(c('B8A', 'B4'))$rename('NDVI_A')
  NDBI <- image$normalizedDifference(c('B11', 'B8'))$rename('NDBI')
  NDWI <- image$normalizedDifference(c('B3', 'B8'))$rename('NDWI')
  MNDWI <- image$normalizedDifference(c('B3', 'B11'))$rename('MNDWI')
  LSWI <- image$normalizedDifference(c('B8', 'B11'))$rename('LSWI')
  EVI <- image$expression(
    '(2.5 * (
    (NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE))
    ) ',list(
      'NIR'=image$select('B8'),
      'RED'=image$select('B4'),
      'BLUE'=image$select('B2')
    ))$rename('EVI')
  DVI <- image$expression(
    '(NIR-RED) ',list(
      'NIR'=image$select('B8'),
      'RED'=image$select('B4')
    ))$rename('DVI')
  RDVI <- image$expression(
    '(sqrt(
    pow((B8-B4),2)/(B8+B4)
    )) ',list(
      'B8'=image$select('B8'),
      'B4'=image$select('B4')
    ))$rename('RDVI')
  MASVI <-  image$expression(
    '(
    1/2*(2*NIR+1-sqrt(
    pow((2*NIR+1),2)-8*(NIR-RED)
    ))
    )',list( 
      'NIR'=image$select('B8'),
      'RED'=image$select('B4')
    )
  )$rename('MASVI')
  RVI <-  image$expression(
    '(NIR/RED) ',list(
      'NIR'=image$select('B8'),
      'RED'=image$select('B4')
    ))$rename('RVI')
  BSI <- image$expression(
    '
    ((B11+B4)-(B8+B2))/((B11+B4)+(B8+B2))
    ', list(
      'B11'= image$select('B11'), 
      'B4'= image$select('B4'), 
      'B8'= image$select('B8'),
      'B2'= image$select('B2')
    ))$rename('BSI')
  OSAVI <- image$expression('(NIR-RED) / (NIR+RED+0.16)',
                            list('NIR'=image$select('B8'),
                                 'RED'=image$select('B4'))
  )$rename('OSAVI')
  SAVI <- image$expression('(1.5*
                           (NIR-RED) / (NIR+RED+0.5))',
                           list('NIR'=image$select('B8'),
                                'RED'=image$select('B4'))
  )$rename('SAVI')
  RNDVI <- image$normalizedDifference(c('B5', 'B4'))$rename('RNDVI')
  NDVIre1 <- image$normalizedDifference(c('B8', 'B5'))$rename('NDVIre1')
  NDVIre2 <- image$normalizedDifference(c('B8', 'B6'))$rename('NDVIre2')
  NDVIre3 <- image$normalizedDifference(c('B8', 'B7'))$rename('NDVIre3')
  RRI1 <- image$expression(
    '(B8/B5)',list(
      'B8' = image$select('B8'),
      'B5' = image$select('B5')
    )
  )$rename('RRI1')
  RRI2 <- image$expression(
    '(B5/B4)',list(
      'B5' = image$select('B5'),
      'B4' = image$select('B4')
    )
  )$rename('RRI2')
  CIire1 <-  image$expression(
    '((B8/B5)-1)',list(
      'B8' = image$select('B8'),
      'B5' = image$select('B5')
    )
  )$rename('CIire1')
  CIire2 <-  image$expression(
    '((B7/B5)-1)',list(
      'B7' = image$select('B7'),
      'B5' = image$select('B5')
    )
  )$rename('CIire2')
  NDRE1 <-  image$expression(
    '((B6-B5)/(B5+B6))',list(
      'B6' = image$select('B6'),
      'B5' = image$select('B5')
    )
  )$rename('NDRE1')
  MSR <- image$normalizedDifference(c('B6', 'B1'))$rename('MSR')
  MTCI <- image$expression(
    '((B6-B5)/(B5-B4))',list(
      'B6' = image$select('B6'),
      'B5' = image$select('B5'),
      'B4' = image$select('B4')
    )
  )$rename('MTCI')
  REP <- image$expression(
    '(
    705+35*((0.5*(B4+B7)-B5)/(B6-B5))
    )',list(
      'B6' = image$select('B6'),
      'B5' = image$select('B5'),
      'B4' = image$select('B4'),
      'B7' = image$select('B7')
    )
  )$rename('REP')
  IREcl <- image$expression(
    '((B7-B4)/(B5/B6))',list(
      'B6' = image$select('B6'),
      'B5' = image$select('B5'),
      'B4' = image$select('B4'),
      'B7' = image$select('B7')
    )
  )$rename('IREcl')
  MCARI2 <-  image$expression(
    '(
    (B6/B5)*((B6-B5)-0.2*(B6-B3))
    )',list(
      'B6' = image$select('B6'),
      'B5' = image$select('B5'),
      'B3' = image$select('B3')
    )
  )$rename('MCARI2')
  PSRI <-  image$expression(
    '(
    (B4-B2)/B6
    )',list(
      'B6' = image$select('B6'),
      'B4' = image$select('B4'),
      'B2' = image$select('B2')
    )
  )$rename('PSRI')
  NIRv <-  image$expression(
    '(
    ((B8-B4)*B8)/(B8+B4)
    )',list(
      'B8' = image$select('B8'),
      'B4' = image$select('B4')
    )
  )$rename('NIRv')
  return (image$addBands(NDVI)$addBands(NDVI_A)$
            addBands(NDBI)$addBands(NDWI)$addBands(LSWI)$addBands(MNDWI)$
            addBands(DVI)$addBands(RDVI)$addBands(MASVI)$addBands(SAVI)$
            addBands(RVI)$addBands(BSI)$addBands(EVI)$addBands(OSAVI)$
            addBands(RRI1)$addBands(RRI2)$addBands(CIire1)$addBands(CIire2)$
            addBands(NDRE1)$addBands(MSR)$addBands(MTCI)$
            addBands(REP)$addBands(IREcl)$addBands(MCARI2)$addBands(NDVIre1)$
            addBands(NDVIre2)$addBands(NDVIre3)$addBands(PSRI)$addBands(NIRv))
}
collection_clean <- collection_clean$map(addforclassify)
# Extract phenoligical features
ee_user_info()
# Obtain your asset home name
ee_manage_assetlist()
pheno <- ee$Image('users/kby/twobands_fullaoi')
addNDWI <- function(img,sample){ #Calculating mean NDWI of tidal flat samples
  img2 <- img$select("NDWI")
  value <- img2$reduceRegion( reducer = ee$Reducer$mean(),
                              geometry = sample,
                              maxPixels = 1e16)
  return (img$set('MeanNDWI', ee$Number(value$get("NDWI"))))
}
addNDVI <- function(img,sample){ #Calculating mean NDVI of tidal flat samples
  img2 <- img$select("NDVI")
  value <- img2$reduceRegion( reducer = ee$Reducer$mean(),
                              geometry = sample,
                              maxPixels = 1e16)
  return (img$set('MeanNDVI', ee$Number(value$get("NDVI"))))
}
addNIRv <- function(img,sample){ #Calculating mean NDRv of tidal flat samples
  img2 <- img$select("NIRv")
  value <- img2$reduceRegion( reducer = ee$Reducer$mean(),
                              geometry = sample,
                              maxPixels = 1e16)
  return (img$set('MeanNIRv', ee$Number(value$get("NIRv"))))
}
addPSRI <- function(img,sample){ #Calculating mean PSRI of tidal flat samples
  img2 <- img$select("PSRI")
  value <- img2$reduceRegion( reducer = ee$Reducer$mean(),
                              geometry = sample,
                              maxPixels = 1e16)
  return (img$set('MeanPSRI', ee$Number(value$get("PSRI"))))
}
tidel_mean  <-  img_col$map(function(image){
  return (addNDVI(image,hhmc_poi))
})$map(function(image){
  return (addNDWI(image,hhmc_poi))
})

count <-  tidel_mean$size()$divide(5)$round()   #Calculating the amount of 20% images;
LETC <-  tidel_mean$sort("MeanNDVI",FALSE)$limit(count)$qualityMosaic("NDVI")  #Sorting NDVI in descending order and composite to lowest images
HETC <-  tidel_mean$sort("MeanNDWI",FALSE)$limit(count)$qualityMosaic("NDWI") #Sorting NDwI in descending order and composite to highest images

Map$addLayer(LETC,list( min = 0, max = 3000, bands = c('B8','B4','B3')),"Lowest tide image")+
  Map$addLayer(HETC,list( min = 0, max = 3000, bands = c('B8','B4','B3')),"Highest tide image")

#Median composites represent low- and high-tide images with less noise
MLTC <-  tidel_mean$sort("MeanNDVI",FALSE)$limit(count)$median()  #Sorting NDVI in descending order and composite to lowest images
MHTC <-  tidel_mean$sort("MeanNDWI",FALSE)$limit(count)$median()

phen_marsh <-  img_col$map(function(image){
  return (addNIRv(image,veg_poi))
})$map(function(image){
  return (addPSRI(image,veg_poi))
})
CFGC <-  phen_marsh$sort("MeanNIRv",FALSE)$limit(count)$median()
CFSC <-  phen_marsh$sort("MeanPSRI",FALSE)$limit(count)$median()
# Bulid phenological-spatial feature space
phase <- pheno$select('phase')
amplitude <- pheno$select('amplitude')
mltc <- MLTC$select('NDVI')$rename('mltc')
mhtc <- MHTC$select('NDWI')$rename('mhtc')
cfgc <- CFGC$select('NIRv')$rename('cfgc')
cfsc <- CFSC$select('PSRI')$rename('cfsc')

FullImage <- collection_clean$select('B.*','NDVI','NDVI_A','NDBI','NDWI','LSWI','MNDWI',
                                     'DVI','RDVI','MASVI','SAVI','RVI','EVI','OSAVI','RRI1','RRI2','CIire1','CIire2',
                                     'MSR','IREcl','MCARI2','NDVIre1','NDVIre2','NDVIre3','REP','NDRE1')$reduce(
                                       ee$Reducer$percentile(
                                         percentiles = percentiles
                                       )
                                     )$addBands(phase)$addBands(amplitude)$addBands(cfgc)$addBands(mltc)$addBands(mhtc)$
  addBands(cfsc)$clip(AOI)
# Bulid temporal space
grow_start <- ee$Date('2020-07-01')
grow_end<- ee$Date('2020-09-01')
grow_clean<- collection$filterDate(grow_start,grow_end)$
  map(maskCloudAndShadowsSR)$map(addforclassify)
percentiles <- c(10,25,50,75,90)
growimage <- grow_clean$select('B.*','NDVI','NDVI_A','NDBI','NDWI','LSWI','MNDWI',
                               'DVI','RDVI','MASVI','SAVI','RVI','EVI','OSAVI','RRI1','RRI2','CIire1','CIire2',
                               'MSR','IREcl','MCARI2','NDVIre1','NDVIre2','NDVIre3','REP','NDRE1')$reduce(
                                 ee$Reducer$percentile(
                                   percentiles = percentiles
                                 ))$clip(AOI)
# Temporal-Phenological-Spatial Feature set
rfimage <- ee$Image$cat(FullImage,renameimage)