bio_bandlist <- c(
  'ab_pa','ab_sa','ab_ss','un_pa','un_sa','un_ss',
  'c_ab_pa','c_ab_sa','c_ab_ss',
  'c_un_pa','c_un_sa','c_un_ss',
  'total_pa','total_sa','total_ss',
  'tc_pa','tc_sa','tc_ss',
  'bioc_pa','bioc_sa','bioc_ss'
)
img_2020 <- collection_clean$mean()
ab_account <- function(image) {
  ab_pa <- image$expression(
    '-1.183 -9.725*NDWI -11.346*NDVI_A-3.907*SAVI + 1.162*RVI ',list(
      'NDWI'=image$select('NDWI'),
      'NDVI_A'=image$select('NDVI_A'),
      'SAVI'=image$select('SAVI'),
      'RVI'=image$select('RVI')
    ))$rename('ab_pa')
  c_ab_pa <- image$expression(
    '0.542*(-1.183 -9.725*NDWI -11.346*NDVI_A-3.907*SAVI + 1.162*RVI) ',list(
      'NDWI'=image$select('NDWI'),
      'NDVI_A'=image$select('NDVI_A'),
      'SAVI'=image$select('SAVI'),
      'RVI'=image$select('RVI')
    ))$rename('c_ab_pa')
  ab_sa <- image$expression(
    '-7.241-85.068*NDWI -277.716*NDVI +11.551*EVI + 802.351*SAVI -1.17*RVI-858.544*DVI',list(
      'NDWI'=image$select('NDWI'),
      'NDVI'=image$select('NDVI'),
      'SAVI'=image$select('SAVI'),
      'DVI'=image$select('DVI'),
      'RVI'=image$select('RVI'),
      'EVI'= image$select('EVI')
    ))$rename('ab_sa')
  c_ab_sa <- image$expression(
    '0.6887*(-7.241-85.068*NDWI -277.716*NDVI +11.551*EVI + 802.351*SAVI -1.17*RVI-858.544*DVI)',list(
      'NDWI'=image$select('NDWI'),
      'NDVI'=image$select('NDVI'),
      'SAVI'=image$select('SAVI'),
      'DVI'=image$select('DVI'),
      'RVI'=image$select('RVI'),
      'EVI'= image$select('EVI')
    ))$rename('c_ab_sa')
  ab_ss <- image$expression(
    '0.13+4.825*NDWI -10.160*NDVI+92.838*SAVI-61.299*MASVI +0.636*RVI-32.037*DVI',list(
      'NDWI'=image$select('NDWI'),
      'NDVI'=image$select('NDVI'),
      'SAVI'=image$select('SAVI'),
      'MASVI'=image$select('MASVI'),
      'DVI'=image$select('DVI'),
      'RVI'=image$select('RVI')
    ))$rename('ab_ss')
  c_ab_ss <- image$expression(
    '0.3953*(0.13+4.825*NDWI -10.160*NDVI+92.838*SAVI-61.299*MASVI +0.636*RVI-32.037*DVI)',list(
      'NDWI'=image$select('NDWI'),
      'NDVI'=image$select('NDVI'),
      'SAVI'=image$select('SAVI'),
      'MASVI'=image$select('MASVI'),
      'DVI'=image$select('DVI'),
      'RVI'=image$select('RVI')
    ))$rename('c_ab_ss')
  return (image$addBands(ab_pa)$addBands(c_ab_pa)
          $addBands(ab_sa)$addBands(c_ab_sa)
          $addBands(ab_ss)$addBands(c_ab_ss))
}
ab_img <- ab_account(img_2020sep)

un_account <- function(image) {
  un_pa <- image$expression(
    '10**(-0.617)*ab_pa**1.152 ',list(
      'ab_pa'=image$select('ab_pa')
    ))$rename('un_pa')
  c_un_pa <- image$expression(
    '(10**(-0.617)*ab_pa**1.152)*0.6153 ',list(
      'ab_pa'=image$select('ab_pa')
    ))$rename('c_un_pa')
  un_sa <- image$expression(
    '10**(-0.186)*ab_sa**0.935 ',list(
      'ab_sa'=image$select('ab_sa')
    ))$rename('un_sa')
  c_un_sa <- image$expression(
    '(10**(-0.186)*ab_sa**0.935)*0.7620 ',list(
      'ab_sa'=image$select('ab_sa')
    ))$rename('c_un_sa')
  un_ss <- image$expression(
    '10**(-0.719)*ab_ss**1.013 ',list(
      'ab_ss'=image$select('ab_ss')
    ))$rename('un_ss')
  c_un_ss <- image$expression(
    '(10**(-0.719)*ab_ss**1.013)*0.4686 ',list(
      'ab_ss'=image$select('ab_ss')
    ))$rename('c_un_ss')
  return (image$addBands(un_pa)$addBands(c_un_pa)
          $addBands(un_sa)$addBands(c_un_sa)
          $addBands(un_ss)$addBands(c_un_ss))
}

un_img <- un_account(ab_img)
un_img$bandNames()$getInfo()
total_account <- function(image) {
  total_pa <- image$expression(
    'ab_pa+un_pa ',list(
      'ab_pa'=image$select('ab_pa'),
      'un_pa'=image$select('un_pa')
    ))$rename('total_pa')
  total_sa <- image$expression(
    'ab_sa+un_sa ',list(
      'ab_sa'=image$select('ab_sa'),
      'un_sa'=image$select('un_sa')
    ))$rename('total_sa')
  total_ss <- image$expression(
    'ab_ss+un_ss ',list(
      'ab_ss'=image$select('ab_ss'),
      'un_ss'=image$select('un_ss')
    ))$rename('total_ss')
  bioc_pa <- image$expression(
    'c_ab_pa+c_un_pa ',list(
      'c_ab_pa'=image$select('c_ab_pa'),
      'c_un_pa'=image$select('c_un_pa')
    ))$rename('bioc_pa')
  bioc_sa <- image$expression(
    'c_ab_sa+c_un_sa ',list(
      'c_ab_sa'=image$select('c_ab_sa'),
      'c_un_sa'=image$select('c_un_sa')
    ))$rename('bioc_sa')
  bioc_ss <- image$expression(
    'c_ab_ss+c_un_ss ',list(
      'c_ab_ss'=image$select('c_ab_ss'),
      'c_un_ss'=image$select('c_un_ss')
    ))$rename('bioc_ss')
  
  tc_pa <- image$expression(
    'c_ab_pa+c_un_pa+4.36 ',list(
      'c_ab_pa'=image$select('c_ab_pa'),
      'c_un_pa'=image$select('c_un_pa')
    ))$rename('tc_pa')
  tc_sa <- image$expression(
    'c_ab_sa+c_un_sa+2.428 ',list(
      'c_ab_sa'=image$select('c_ab_sa'),
      'c_un_sa'=image$select('c_un_sa')
    ))$rename('tc_sa')
  tc_ss <- image$expression(
    'c_ab_ss+c_un_ss+3.88 ',list(
      'c_ab_ss'=image$select('c_ab_ss'),
      'c_un_ss'=image$select('c_un_ss')
    ))$rename('tc_ss')
  return (image$addBands(total_pa)$addBands(bioc_pa)$addBands(tc_pa)
          $addBands(total_sa)$addBands(bioc_sa)$addBands(tc_sa)
          $addBands(total_ss)$addBands(bioc_ss)$addBands(tc_ss))
}

total_img <- total_account(un_img)$select(bio_bandlist)
total_img$bandNames()$getInfo()

rfresult <- ee$Image('projects/ee-kby/assets/c9_21b_3to301')
ss_mask <- rfresult$select('b1')$eq(1)
ss_class <- total_img$updateMask(ss_mask)

sa_mask <- rfresult$select('b1')$eq(2)
sa_class <- total_img$updateMask(sa_mask)
pa_mask <- rfresult$select('b1')$eq(3)
pa_class <- total_img$updateMask(pa_mask)


tc_pa <- pa_class$select('tc_pa')
c_ab_pa <- pa_class$select('c_ab_pa')
c_un_pa <- pa_class$select('c_un_pa')
ab_pa <- pa_class$select('ab_pa')
un_pa <- pa_class$select('un_pa')
total_pa <- pa_class$select('total_pa')
bioc_pa <- pa_class$select('bioc_pa')

tc_sa <- sa_class$select('tc_sa')
c_ab_sa <- sa_class$select('c_ab_sa')
c_un_sa <- sa_class$select('c_un_sa')
ab_sa <- sa_class$select('ab_sa')
un_sa <- sa_class$select('un_sa')
total_sa <- sa_class$select('total_sa')
bioc_sa <- sa_class$select('bioc_sa')


tc_ss <- ss_class$select('tc_ss')
c_ab_ss <- ss_class$select('c_ab_ss')
c_un_ss <- ss_class$select('c_un_ss')
ab_ss <- ss_class$select('ab_ss')
un_ss <- ss_class$select('un_ss')
total_ss <- ss_class$select('total_ss')
bioc_ss <- ss_class$select('bioc_ss')




downConfig <- list(
  scale =10,
  maxPixels = 1.0E13,
  driveFolder = "image"
)

task <- ee$batch$Export$image(tc_ss , 'tc_ss_3', downConfig)

task$start()
ee_monitoring(task) 
