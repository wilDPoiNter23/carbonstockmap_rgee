#This being a simpler classification, we take 20% points
#for validation. 
sampleData <- plotsfeature$randomColumn('random')
sample_training <- sampleData$filter(ee$Filter$lte("random", 0.8))
sample_validate  <- sampleData$filter(ee$Filter$gt("random", 0.8))
bandlist <- rfimage$bandNames()$getInfo()
# rfe to filter features
fc_total <- read_csv('/Users/kby/Downloads/bc_data/output/finaltable0602.csv')
fc_total <- na.omit(fc_total)
set.seed(7)
ctrl <- rfeControl(
  functions = rfFuncs,
  method = "cv",
  number = 10,
  verbose = TRUE,
  allowParallel = TRUE
)

y = as.factor(unlist(fc_total[,387]))
rfe_results_1 <- caret::rfe(fc_total[,-387], y,
                            sizes = seq(1,386,20),
                            rfeControl = ctrl)

print(rfe_results_1)
# obtain the featureset after rfe approach
prebands_more <- predictors(rfe_results_1)
#training set
training<- rfimage$select(prebands_more)$sampleRegions(
  collection = sample_training,
  properties = list('Name'),
  scale = 10,
  tileScale = 16
)
###### rf#####
classifier_importance <- ee$Classifier$smileRandomForest(numberOfTrees = 310,
                                                         minLeafPopulation = 1,
                                                         bagFraction = 0.8)$
  train(
    features = training,
    classProperty = 'Name',
    inputProperties = prebands_more
  )
classified <- FullImage$classify(classifier_importance)

downConfig <- list(
  scale =10,
  maxPixels = 1.0E13,
  driveFolder = "image"
)

task <- ee$batch$Export$image(classified, 'rfclassified01', downConfig)

task$start()
ee_monitoring(task) 

########export accuracy 

improtance_dict <- classifier_importance$explain()

validated <-test$classify(classifier_importance)$errorMatrix("Name", "classification")

acc <- ee$Feature(NULL, list(accuracy =validated$accuracy(),
                             kappa = validated$kappa(),
                             ua= validated$consumersAccuracy(),
                             pa = validated$producersAccuracy()
))

resultFc <- ee$FeatureCollection(acc)
task <- ee$batch$Export$table$toDrive(
  collection = resultFc,
  description = 'rfacc03',
  fileFormat = "CSV"
)
task$start()
ee_monitoring(task) 



########cart #######
classifier_importance <- ee$Classifier$smileCart(100)$train(
  features = training,
  classProperty = 'Name',
  inputProperties = prebands_more
)
classified <- FullImage$classify(classifier_importance)

downConfig <- list(
  scale =10,
  maxPixels = 1.0E13,
  driveFolder = "image"
)

task <- ee$batch$Export$image(classified, 'cartclassified01', downConfig)

task$start()
ee_monitoring(task) 

########export accuracy 

improtance_dict <- classifier_importance$explain()

validated <-test$classify(classifier_importance)$errorMatrix("Name", "classification")

acc <- ee$Feature(NULL, list(accuracy =validated$accuracy(),
                             kappa = validated$kappa(),
                             ua= validated$consumersAccuracy(),
                             pa = validated$producersAccuracy()
))

resultFc <- ee$FeatureCollection(acc)
task <- ee$batch$Export$table$toDrive(
  collection = resultFc,
  description = '0920cartacc03',
  fileFormat = "CSV"
)
task$start()
ee_monitoring(task) 


########libsvm #######
classifier_importance <- ee$Classifier$libsvm()$train(
  features = training,
  classProperty = 'Name',
  inputProperties = prebands_more
)
classified <- FullImage$classify(classifier_importance)

downConfig <- list(
  scale =10,
  maxPixels = 1.0E13,
  driveFolder = "image"
)

task <- ee$batch$Export$image(classified, 'libclassified', downConfig)

task$start()
ee_monitoring(task) 

########export accuracy 

improtance_dict <- classifier_importance$explain()
validated <-test$classify(classifier_importance)$errorMatrix("Name", "classification")

acc <- ee$Feature(NULL, list(accuracy =validated$accuracy(),
                             kappa = validated$kappa(),
                             ua= validated$consumersAccuracy(),
                             pa = validated$producersAccuracy()
))

resultFc <- ee$FeatureCollection(acc)
task <- ee$batch$Export$table$toDrive(
  collection = resultFc,
  description = '0920libacc03',
  fileFormat = "CSV"
)
task$start()
ee_monitoring(task) 
########sgt#######
classifier_importance <- ee$Classifier$smileGradientTreeBoost(10)$train(
  features = training,
  classProperty = 'Name',
  inputProperties = prebands_more
)
classified <- FullImage$classify(classifier_importance)

downConfig <- list(
  scale =10,
  maxPixels = 1.0E13,
  driveFolder = "image"
)

task <- ee$batch$Export$image(classified, 'sgtclassified', downConfig)

task$start()
ee_monitoring(task) 

########export accuracy 

improtance_dict <- classifier_importance$explain()
validated <-test$classify(classifier_importance)$errorMatrix("Name", "classification")

acc <- ee$Feature(NULL, list(accuracy =validated$accuracy(),
                             kappa = validated$kappa(),
                             ua= validated$consumersAccuracy(),
                             pa = validated$producersAccuracy()
))

resultFc <- ee$FeatureCollection(acc)
task <- ee$batch$Export$table$toDrive(
  collection = resultFc,
  description = '0920sgtacc03',
  fileFormat = "CSV"
)
task$start()
ee_monitoring(task) 