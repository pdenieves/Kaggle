# load packages
# install.packages("")
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(corrplot)
library(glmnet)
library(e1071)
library(randomForest)
#
library(stringr)



# load data
setwd("C:\\Users\\ecomped\\Documents\\r_corr_families")
passenger <- read.csv("passenger.csv")

# change data types
passenger$HasAge <- 1
passenger$SameTicketId <- NA

passenger$Name <- as.character(passenger$Name)
passenger$Ticket <- as.character(passenger$Ticket)
passenger$Cabin <- as.character(passenger$Cabin)
passenger$Surname <- as.character(passenger$Surname)
passenger$MaidenName <- as.character(passenger$MaidenName)
passenger$Firstname <- as.character(passenger$Firstname)
passenger$SiblingId <- as.character(passenger$SiblingId)
passenger$ChildrenId <- as.character(passenger$ChildrenId)
passenger$ParentsId <- as.character(passenger$ParentsId)
passenger$HusbandFirstname <- as.character(passenger$HusbandFirstname)
passenger$SiblingId <- as.list(passenger$SiblingId)
passenger$ChildrenId <- as.list(passenger$ChildrenId)
passenger$ParentsId <- as.list(passenger$ParentsId)
passenger$SameTicketId <- as.list(passenger$SameTicketId)

# remove "X" column
passenger <- select(passenger, -X)

# convert passenger ids of relatives from character to lists of integers
for (i in 1:dim(passenger)[1]){
  passengerSelect <- passenger[i, ]
  
  # convert sibling ids
  if (!is.na(passengerSelect$SiblingId)){
    passengerSelect$SiblingId <- list(as.numeric(unlist(strsplit(passengerSelect$SiblingId[[1]], ";"))))
  } else {
    passengerSelect$SiblingId <- NA
  }
  # convert children ids
  if (!is.na(passengerSelect$ChildrenId)){
    passengerSelect$ChildrenId <- list(as.numeric(unlist(strsplit(passengerSelect$ChildrenId[[1]], ";"))))
  } else {
    passengerSelect$ChildrenId <- NA
  }
  # convert parents ids
  if (!is.na(passengerSelect$ParentsId)){
    passengerSelect$ParentsId <- list(as.numeric(unlist(strsplit(passengerSelect$ParentsId[[1]], ";"))))
  } else {
    passengerSelect$ParentsId <- NA
  } 
  # convert same ticket id
  if (!is.na(passengerSelect$SameTicketId)){
    passengerSelect$SameTicketId <- list(as.numeric(unlist(strsplit(as.character(passengerSelect$SameTicketId[[1]]), ";"))))
  } else {
    passengerSelect$SameTicketId <- NA
  } 
  
  passenger[i, ] <- passengerSelect
}

# add feature about survival of passengers with same ticket
passenger$SameTicketSurvivedDiff <- NA




# import prediction
prediction <- read.csv("solutionLogReg.csv")

# number of prediction
nPred <- nrow(prediction)

# insert predicted survival to data set
for (i in 1:nPred){
  passenger$Survived[passenger$PassengerId == prediction$PassengerId[i]] <- prediction$Survived[i]
}

# next, I update the survival inforamtion of all relatives
# function to update survival features of all passengers
updateSurvivalFeatures <- function(passenger){
  # number of passengers
  nPassenger <- dim(passenger)[1]
  # transform survived feature to -1 or 1
  survived <- passenger$Survived * 2 - 1
  # set survival differences to NA
  passenger$SameTicketSurvivedDiff <- NA
  passenger$SpouseSurvivedDiff <- NA
  passenger$SiblingSurvivedDiff <- NA
  passenger$ChildrenSurvivedDiff <- NA
  passenger$ParentsSurvivedDiff <- NA
  passenger$FatherSurvivedDiff <- NA
  passenger$MotherSurvivedDiff <- NA
  
  for (iPassenger in 1:nPassenger){
    passengerSelect <- passenger[iPassenger, ]
    
    # survival of spouse
    if (passengerSelect$SpouseNumber == 1){
      passengerSelect$SpouseSurvivedDiff <- survived[passengerSelect$SpouseId]
    } else {
      passengerSelect$SpouseSurvivedDiff <- 0
    }
    
    # survival of siblings
    passengerSelect$SiblingSurvivedDiff <- 
      sum(survived[passenger$PassengerId %in% unlist(passengerSelect$SiblingId)])
    
    # survival of children
    passengerSelect$ChildrenSurvivedDiff <- 
      sum(survived[passenger$PassengerId %in% unlist(passengerSelect$ChildrenId)])    
    
    # survival of parents
    passengerSelect$ParentsSurvivedDiff <- 
      sum(survived[passenger$PassengerId %in% unlist(passengerSelect$ParentsId)])    
    
    # survival of passengers with same ticket
    passengerSelect$SameTicketSurvivedDiff <- 
      sum(survived[passenger$PassengerId %in% unlist(passengerSelect$SameTicketId)])        
    
    # survival of mother and father
    if (passengerSelect$ParentsNumber > 0){
      # get parents
      parents <- passenger[passenger$PassengerId %in% unlist(passengerSelect$ParentsId), ]
      # survival of father
      if (passengerSelect$FatherNumber == 1){
        father <- parents[parents$Sex == 'male', ]
        passengerSelect$FatherSurvivedDiff <- father$Survived * 2 - 1
      } else {
        passengerSelect$FatherSurvivedDiff <- 0 
      }
      # survival of mother
      if (passengerSelect$MotherNumber == 1){
        mother <- parents[parents$Sex == 'female', ]
        passengerSelect$MotherSurvivedDiff <- mother$Survived * 2 - 1
      } else {
        passengerSelect$MotherSurvivedDiff <- 0 
      }
    } else {
      passengerSelect$ParentsSurvivedDiff <- 0
      passengerSelect$FatherSurvivedDiff <- 0
      passengerSelect$MotherSurvivedDiff <- 0
    }
    
    # insert selected passenger into passenger set
    passenger[iPassenger, ] <- passengerSelect
  }
  
  # survival of relatives
  passenger$RelativesSurvivedDiff <- 
    passenger$SpouseSurvivedDiff + 
    passenger$SiblingSurvivedDiff + 
    passenger$ChildrenSurvivedDiff + 
    passenger$ParentsSurvivedDiff
  
  passenger
}

passenger <- updateSurvivalFeatures(passenger)

# now I reset the survival inforatmion of all passengers of the test set to NA
passenger$Survived[passenger$DataSet == "Test"] <- NA

passenger$FamilySize <- passenger$SibSp + passenger$Parch + 1

# passenger travled alone
passenger$IsAlone <- passenger$FamilySize == 1

# passenger is a child
passenger$IsChild <- passenger$Age <= 14

# passenger's wife or husband survived (0 if passenger didn't have a wife or husband)
passenger$HusbandSurvivedDiff <- 0
passenger$HusbandSurvivedDiff[passenger$Sex == "female"] <- passenger$SpouseSurvivedDiff[passenger$Sex == "female"]
passenger$WifeSurvivedDiff <- 0
passenger$WifeSurvivedDiff[passenger$Sex == "male"] <- passenger$SpouseSurvivedDiff[passenger$Sex == "male"]

# children of mother or father survived
passenger$MothersChildrenSurvivedDiff <- 0
passenger$MothersChildrenSurvivedDiff[passenger$Sex == "female"] <- passenger$ChildrenSurvivedDiff[passenger$Sex == "female"]
passenger$FathersChildrenSurvivedDiff <- 0
passenger$FathersChildrenSurvivedDiff[passenger$Sex == "male"] <- passenger$ChildrenSurvivedDiff[passenger$Sex == "male"]





# exclude features which are not relevant for prediction
featureRemove <- c("Name", "Ticket", "Fare", "Cabin", "Embarked", "Surname", "MaidenName", "Firstname", "HusbandFirstname", "SiblingAgeMin", "SiblingAgeMax", "SiblingAgeMean", "ParentAgeMin", "ParentAgeMax", "ParentAgeMean", "ChildrenAgeMin", "ChildrenAgeMax", "ChildrenAgeMean", "SameTicketAgeMin", "SameTicketAgeMax", "SameTicketAgeMean", "SpouseAge", "CabinNr")
names(passenger)[!(names(passenger) %in% featureRemove)]


passenger$Deck <- factor(sapply(passenger$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
#passenger$Deck <- factor(passenger$Deck)

# starboard and port side. Odd numbered cabins are starboard side, and even numbers are port side.
passenger$CabinSide <- "Unknown"
passenger$CabinSide[str_sub(passenger$Cabin, -1, -1) %in% c("1", "3", "5", "7", "9")] <- "Starboard"
passenger$CabinSide[str_sub(passenger$Cabin, -1, -1) %in% c("2", "4", "6", "8", "0")] <- "Port"



# map Deck feature to numerical feature
passenger$DeckNumber <- NA
letter <- c("A", "B", "C", "D", "E", "F", "G")
for (i in 1:length(letter)){
  passenger$DeckNumber[passenger$Deck == letter[i]] <- i
}

# features to be encoded
featureEncode <- c("Pclass", "Sex", "Title", "Deck", "CabinSide")

# one-hot encoding
for (feature in featureEncode){
  # get values of feature
  values <- as.character(unique(passenger[, feature]))
  for (value in values){
    # create feature name
    newFeatureName <- paste0(feature, "_", value)
    # populate feature
    passenger[, newFeatureName] <- passenger[, feature] == value
  }
}
names(passenger)
# remove original feature from data set
passenger <- passenger[, !(names(passenger) %in% c("Sex", "Title", "Deck", "CabinSide"))]

# copy passenger data set
passengerOrg <- passenger

# add relatives' id to feature remove
featureRemove <- c(featureRemove, "SpouseId", "SiblingId", "ChildrenId", "ParentsId", "SameTicketId")

# remove columns which are not used for prediction
passenger <- passenger[, !(names(passenger) %in% featureRemove)]




names(passenger)




training <- passenger[passenger$DataSet == "Training", ]

survived <- training$Survived

# remove features which are not used for correlation analysis
training <- select(training, -c(PassengerId, Survived, DataSet))

# calculate correlation coefficient of each feature with survival
feature <- names(training)

corrSurvived <- data.frame(feature = feature, coef = rep(NA, length(feature)))
for (iFeature in 1:length(feature)){
  corrSurvived$coef[iFeature] <- cor(training[, iFeature], survived)
}

# sort by correlation coefficient
corrSurvivedOrder <- corrSurvived[order(corrSurvived$coef, decreasing = FALSE), ]

ggplot(corrSurvivedOrder, aes(x = factor(feature, levels = feature), y = coef)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  xlab("Feature") + 
  ylab("Correlation Coefficient")


summary(passenger)


##
passenger$Deck_A <- as.integer(passenger$Deck_A)
passenger$Deck_B <- as.integer(passenger$Deck_B)
passenger$Deck_C <- as.integer(passenger$Deck_C)
passenger$Deck_D <- as.integer(passenger$Deck_D)
passenger$Deck_E <- as.integer(passenger$Deck_E)
passenger$Deck_F <- as.integer(passenger$Deck_F)
passenger$Deck_G <- as.integer(passenger$Deck_G)
passenger$Deck_T <- as.integer(passenger$Deck_T)




# function to calculate plot feature correlation matrix
getCorrMatrix <- function(featureList, showPlot = TRUE){
  # remove Survived from training set and order feature with respect to correlation coefficient to survived
  passengerCorr <- passenger[, as.character(featureList)]
  # calculate correlation matrix
  corrMatrix <- cor(passengerCorr)
  # plot matrix
  if (showPlot) {corrplot(corrMatrix, method = "color", type = "upper")}
  corrMatrix
}

corrMatrix <- getCorrMatrix(rev(corrSurvivedOrder$feature))




# function to get data frame with pairwise correlation of features
getPairCorrelation <- function(corrMatrix){
  featureName <- colnames(corrMatrix)
  nFeature <- length(featureName)
  
  # set lower triangle of matrix to NA (these values are all redundant)
  corrMatrix[lower.tri(corrMatrix, diag = TRUE)] <- NA
  
  # convert matrix to data frame
  featurePair <- data.frame(feature1 = rep(featureName, nFeature), feature2 = rep(featureName, each = nFeature), coef = as.vector(corrMatrix))
  # remove NAs
  featurePair <- featurePair[!is.na(featurePair$coef), ]
  # calculate absolute value of correlation coefficient
  featurePair$coefAbs <- abs(featurePair$coef)
  # order by coefficient
  featurePair <- featurePair[order(featurePair$coefAbs, decreasing = TRUE), ]
  
  featurePair
}    

featureCorr <- getPairCorrelation(corrMatrix)    



kable(featureCorr[1:10, ]) %>% kable_styling(full_width = FALSE)
#kable(corrMatrix[15:24, ]) %>% kable_styling(full_width = FALSE)




# plot histogram of correlation factors
ggplot(featureCorr, aes(coef)) + geom_histogram(binwidth = 0.1) + xlab("Correlation Coefficient")




# features to be eliminated
nFeature <- length(corrSurvivedOrder$feature)
featureEliminate <- character(nFeature)

# data frame with all features 
featureRest <- featureCorr
featureRest$feature1 <- as.character(featureRest$feature1)
featureRest$feature2 <- as.character(featureRest$feature2)

# calculate absolute value of correlation coefficient with survival
corrSurvived$coefAbs <- abs(corrSurvived$coef)

for (iFeature in 1:(nFeature - 1)){
  # get correlation coefficient to survival
  coefAbs1 <- corrSurvived$coefAbs[corrSurvived$feature == featureRest$feature1[1]]
  coefAbs2 <- corrSurvived$coefAbs[corrSurvived$feature == featureRest$feature2[1]]
  
  # choose which feature has lower absolute correlation coefficient to survival
  if (coefAbs1 <= coefAbs2) {
    # eliminate feature 1
    featureRemove <- featureRest$feature1[1]
    featureKeep <- featureRest$feature2[1]
  } else {
    # eliminate feature 2
    featureRemove <- featureRest$feature2[1]
    featureKeep <- featureRest$feature1[1]
  }
  
  # add selected feature to elimination list
  featureEliminate[iFeature] <- featureRemove
  
  # remove feature from featureRest
  featureRest <- featureRest[featureRest$feature1 != featureRemove & featureRest$feature2 != featureRemove, ]
}

# add last remaining feature to elimination list
featureEliminate[nFeature] <- featureKeep

# reverse elimination list
featureGE <- rev(featureEliminate)

featureGE


corrMatrixGE <- getCorrMatrix(featureGE)





# plot histograms for 5, 10, 20, and 40 features

# get pairwise correlations
featurePairGE5 <- getPairCorrelation(corrMatrixGE[1:5, 1:5])
featurePairGE10 <- getPairCorrelation(corrMatrixGE[1:10, 1:10])
featurePairGE20 <- getPairCorrelation(corrMatrixGE[1:20, 1:20])
featurePairGE40 <- getPairCorrelation(corrMatrixGE[1:40, 1:40])

# add column with number of features
featurePairGE5$nFeature <- 5
featurePairGE10$nFeature <- 10
featurePairGE20$nFeature <- 20
featurePairGE40$nFeature <- 40

# combine all data frames
featurePairGE <- do.call("rbind", list(featurePairGE5, featurePairGE10, featurePairGE20, featurePairGE40))
featurePairGE$nFeature <- factor(featurePairGE$nFeature, levels = c(40, 20, 10, 5))

# plot histograms
ggplot(featurePairGE) + 
  geom_histogram(aes(x = coef, fill = nFeature), binwidth = 0.1) + 
  xlab("Correlation Coefficient") +
  lab


# number of features to be eliminated after each fit
nEl <- 5

# prepare data for fit
XTrain <- select(passenger[passenger$DataSet == "Training", ], -c(PassengerId, Survived, DataSet))
yTrain <- passenger$Survived[passenger$DataSet == "Training"]
dataFit <- XTrain
for (i in 1:ncol(dataFit)){
  dataFit[, i] <- as.numeric(dataFit[, i])
}
dataFit$Survived <- yTrain

orderFeatureByPValue <- function(dataFit){
  
  # fit logistic regression model
  LRFit <- glm(Survived ~ ., family = binomial(), data = dataFit)
  
  # get features which were not fitted
  featureNotFit <- names(LRFit$coefficients[is.na(LRFit$coefficients)])
  nFeatureNotFit <- length(featureNotFit)
  
  # check if there are features which were not fitted
  if (nFeatureNotFit > 1){
    # get order of these features by fitting model with these features only
    featureOrderNF <- orderFeatureByPValue(dataFit[, names(dataFit) %in% c(featureNotFit, "Survived")])
  } else {
    featureOrderNF <- featureNotFit
  }
  
  # get coefficients of fitted features
  coefStat <- coef(summary(LRFit))
  # remove intercept
  coefStat <- coefStat[-1, ]
  
  # order fitted features by their p-value
  featureOrderF <- rownames(coefStat)[order(coefStat[, 4])]
  
  # combine features
  c(featureOrderF, featureOrderNF)
}

# number of remaining features
nR <- ncol(dataFit) - 1

featureRFE <- character(0)

# remove nEl features per iteration
while(nR > 0){
  # get data of remaining features
  dataFit <- dataFit[, !(names(dataFit) %in% featureRFE)]
  
  # get order of remaining features
  featureOrder <- orderFeatureByPValue(dataFit)
  
  # eliminate features
  featureRFE <- c(rev(rev(featureOrder)[1:min(nEl, nR)]), featureRFE)
  
  nR <- nR - nEl
}

featureRFEs(fill = "Number of Features") +
  xlim(-1, 1)





lambda <- 10 ^ seq(from = 0, to = -7, by = -0.01)

XTrain <- as.matrix(XTrain)

# get minimum and maximum of each column
getColMinMax <- function(data){
  nCol <- ncol(data)
  colMin <- rep(NA, nCol)
  colMax <- rep(NA, nCol)
  for (iCol in 1:nCol){
    colMin[iCol] <- min(data[, iCol])
    colMax[iCol] <- max(data[, iCol])
  }
  list("min" = colMin, "max" = colMax)
}
# function to normalise data
normData <- function(data, colMinMax){
  for (iCol in 1:ncol(data)){
    data[, iCol] <- (data[, iCol] - colMinMax$min[iCol]) / (colMinMax$max[iCol] - colMinMax$min[iCol])
  }
  data
}

# normalise data
colMinMax <- getColMinMax(XTrain)
XTrain <- normData(XTrain, colMinMax)

logRegModel <- glmnet(XTrain, yTrain, family = "binomial", alpha = 1, lambda = lambda)

# get weights
weights <- as.matrix(coef(logRegModel))
featureSelect <- weights[2:(nFeature + 1), ] != 0 # remove first row as this row contains the intercept
featureName <- rownames(featureSelect)

# get number of selected features
nFeatureSelect <- colSums(featureSelect)

# plot number of selected features
plotData <- data.frame(lambda = lambda, nFeature = nFeatureSelect)
ggplot(plotData, aes(x = lambda, y = nFeature))+ geom_line() + scale_x_log10() + xlab("Lambda") + ylab("Number of Features")







