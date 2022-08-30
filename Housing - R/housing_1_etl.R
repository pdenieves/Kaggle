library('ggplot2')
library("stringr")



# ------------------------------------------------------------------------------
# Step 01 - Importing data
# ------------------------------------------------------------------------------

setwd("C:/Users/ecomped/Documents/R/kaggle_housing/input")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)




# ------------------------------------------------------------------------------
# Step 02 - Validate for correctness
# ------------------------------------------------------------------------------

## Number of records anf dimensions
dim(train)
dim(test)

## Structure
str(train)
str(test)

## Values
summary(train)
summary(test)

## First inspections
head(train)
tail(train)
head(test)
tail(test)




# ------------------------------------------------------------------------------
# Step 03 - Data preparation
# ------------------------------------------------------------------------------
# Cleaning data, removing features without information, imputing missing values, ..

## Get a unique set
test$SalePrice <- NA
train <- data.frame(train)
test <- data.frame(test)
global <- rbind(train, test)

str(global)


## Let's check for any missing values in the data and drop features without information
colSums(is.na(global))
colnames(global)[sapply(global, function(x)all(any(is.na(x))))]


colSums(global=='')
colnames(global)[sapply(global, function(x)all(any(!is.na(x)&(str_trim(x)==""))))]

# Note: A lot of Garage information is missing (159 obs.)



# Feature MSSubClass analysis
-----------------------------
# Desc: Identifies the type of dwelling involved in the sale.	

## Information analysis
table(global$MSSubClass)
prop.table(table(global$MSSubClass))*100

global$MSSubClass <- as.factor(global$MSSubClass)

#ggplot(global, aes(MSSubClass, fill=MSSubClass)) + geom_bar()

## Missing values
global[which(is.na(global$MSSubClass)), ]

## No actions



# Feature MSZoning analysis
-----------------------------
# Desc: Identifies the general zoning classification of the sale.
  
## Information analysis
table(global$MSZoning)
prop.table(table(global$MSZoning))*100

global$MSZoning <- as.factor(global$MSZoning)
#ggplot(global, aes(MSZoning, fill=MSZoning)) + geom_bar()

## Missing values
global[which(is.na(global$MSZoning)), ]

## Values imputation
# I impute the most frequent value: RL
global$MSZoning[which(is.na(global$MSZoning))] <- 'RL'



# Feature LotFrontage analysis
-----------------------------
# Desc: Linear feet of street connected to property

## Information analysis
table(global$LotFrontage)
prop.table(table(global$LotFrontage))*100

#ggplot(global, aes(LotFrontage, fill=LotConfig)) + geom_bar()

## Missing values
global[which(is.na(global$LotFrontage)), ]
global[which(global$LotFrontage==''), ]

## Value imputation
#I get the ration between area & LotConfig/frontage
#ggplot(data=subset(global, !is.na(LotFrontage) & LotArea < 70000), aes(x=LotFrontage, y=LotArea)) +
  geom_point() + geom_smooth(method = "lm") 

#ggplot(data=subset(global, !is.na(LotFrontage) & LotArea < 70000), aes(x=LotFrontage, y=LotArea)) +
  geom_point(aes(color = LotConfig, shape = LotConfig))+
  geom_smooth(aes(color = LotConfig, fill = LotConfig), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~LotConfig) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#E7DDDD", "#E70000"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#E7DDDD", "#E70000")) +
  theme_bw()

for (i in levels(as.factor(global$LotConfig))) {
  media <- mean(global$LotFrontage[which(!is.na(global$LotFrontage) & i=='Inside')])/mean(global$LotArea[which(!is.na(global$LotArea) & i=='Inside')])
  global$LotFrontage[which(is.na(global$LotFrontage) & i=='Inside')] <- round(global$LotArea[which(is.na(global$LotFrontage) & i=='Inside')] * media)
  }

  

# Feature LotArea: analysis
-----------------------------
# Desc: Lot size in square feet
  
## Information analysis
table(global$LotArea)
prop.table(table(global$LotArea))*100

#ggplot(global, aes(LotArea, fill=LotArea)) + geom_bar()

## Missing values
global[which(is.na(global$LotArea)), ]
global[which(global$LotArea==''), ]

## No actions


    
# Feature Street analysis
-----------------------------
# Desc: Type of road access to property

## Information analysis
table(global$Street)
prop.table(table(global$Street))*100

#ggplot(global, aes(Street, fill=Street)) + geom_bar()

## Missing values
global[which(is.na(global$Street)), ]
global[which(global$Street==''), ]

## No actions



# Feature Alley analysis
-----------------------------
# Desc: Type of alley access to property

## Information analysis
table(global$Alley)
prop.table(table(global$Alley))*100

#ggplot(global, aes(Alley, fill=Alley)) + geom_bar()

## Missing values
global[which(is.na(global$Alley)), ]
global[which(global$Alley==''), ]

## Value imputation 
# No value means no alley
global$Alley[which(is.na(global$Alley))] <- 'NA'



# Feature LotShape analysis
-----------------------------
# Desc: General shape of property
  
## Information analysis
table(global$LotShape)
prop.table(table(global$LotShape))*100

#ggplot(global, aes(LotShape, fill=LotShape)) + geom_bar()

## Missing values
global[which(is.na(global$LotShape)), ]
global[which(global$LotShape==''), ]

## No actions



# Feature LandContour analysis
-----------------------------
# Desc: Flatness of the property

## Information analysis
table(global$LandContour)
prop.table(table(global$LandContour))*100

#ggplot(global, aes(LandContour, fill=LandContour)) + geom_bar()

## Missing values
global[which(is.na(global$LandContour)), ]
global[which(global$LandContour==''), ]


  
# Feature Utilities analysis
-----------------------------
# Desc: Type of utilities available.

## Information analysis
table(global$Utilities)
prop.table(table(global$Utilities))*100

## Missing values
global[which(is.na(global$Utilities)), ]

# Value imputation
global$Utilities[which(is.na(global$Utilities))] <- 'AllPub'



# Feature LotConfig analysis
-----------------------------
# Desc: Lot configuration

## Information analysis
table(global$LotConfig)
prop.table(table(global$LotConfig))*100

#ggplot(global, aes(LotConfig, fill=LotConfig)) + geom_bar()

## Missing values
global[which(is.na(global$LotConfig)), ]
global[which(global$LotConfig==''), ]

## No actions


  
# Feature LandSlope analysis
-----------------------------
# Desc: Slope of property
  
## Information analysis
table(global$LandSlope)
prop.table(table(global$LandSlope))*100

#ggplot(global, aes(LandSlope, fill=LandSlope)) + geom_bar()

## Missing values
global[which(is.na(global$LandSlope)), ]
global[which(global$LandSlope==''), ]

## No actions

  

# Feature Neighborhood analysis
-----------------------------
# Desc: Physical locations within Ames city limits

## Information analysis
table(global$Neighborhood)
prop.table(table(global$Neighborhood))*100

#ggplot(global, aes(Neighborhood, fill=Neighborhood)) + geom_bar()

## Missing values
global[which(is.na(global$Neighborhood)), ]
global[which(global$Neighborhood==''), ]

## No actions
  
  

# Feature Condition1 analysis
-----------------------------
# Desc: Proximity to various conditions
## Information analysis
table(global$Condition1)
prop.table(table(global$Condition1))*100

#ggplot(global, aes(Condition1, fill=Condition1)) + geom_bar()

## Missing values
global[which(is.na(global$Condition1)), ]
global[which(global$Condition1==''), ]

## No actions

  
  
# Feature Condition2 analysis
-----------------------------
# Desc: Proximity to various conditions (if more than one is present)

## Information analysis
table(global$Condition2)
prop.table(table(global$Condition2))*100

#ggplot(global, aes(Condition2, fill=Condition2)) + geom_bar()

## Missing values
global[which(is.na(global$Condition2)), ]
global[which(global$Condition2==''), ]

## No actions

  
  
# Feature BldgType analysis
-----------------------------
# Desc: Type of dwelling

## Information analysis
table(global$BldgType)
prop.table(table(global$BldgType))*100

#ggplot(global, aes(BldgType, fill=BldgType)) + geom_bar()

## Missing values
global[which(is.na(global$BldgType)), ]
global[which(global$BldgType==''), ]

## No actions

  

# Feature HouseStyle analysis
-----------------------------
# Desc: Style of dwelling

## Information analysis
table(global$HouseStyle)
prop.table(table(global$HouseStyle))*100

#ggplot(global, aes(HouseStyle, fill=HouseStyle)) + geom_bar()

## Missing values
global[which(is.na(global$HouseStyle)), ]
global[which(global$HouseStyle==''), ]

## No actions

  
# Feature OverallQual analysis
-----------------------------
# Desc: Rates the overall material and finish of the house

## Information analysis
table(global$OverallQual)
prop.table(table(global$OverallQual))*100

#ggplot(global, aes(OverallQual, fill=OverallQual)) + geom_bar()

## Missing values
global[which(is.na(global$OverallQual)), ]
global[which(global$OverallQual==''), ]

## No actions

  
  
# Feature OverallCond analysis
-----------------------------
# Desc: Rates the overall condition of the house

## Information analysis
table(global$OverallCond)
prop.table(table(global$OverallCond))*100

#ggplot(global, aes(OverallCond, fill=OverallCond)) + geom_bar()

## Missing values
global[which(is.na(global$OverallCond)), ]
global[which(global$OverallCond==''), ]

## No actions


  
# Feature YearBuilt analysis
-----------------------------
# Desc: Original construction date
  
## Information analysis
table(global$YearBuilt)
prop.table(table(global$YearBuilt))*100

#ggplot(global, aes(YearBuilt, fill=YearBuilt)) + geom_bar()

## Missing and non-valid values
global[which(is.na(global$YearBuilt)), ]
global[which(global$YearBuilt==''), ]
global[which(global$YearBuilt=='0'), ]

## No actions

  
  
# Feature YearRemodAdd analysis
-----------------------------
# Desc: Remodel date (same as construction date if no remodeling or additions)

## Information analysis
table(global$YearRemodAdd)
prop.table(table(global$YearRemodAdd))*100

#ggplot(global, aes(YearRemodAdd, fill=YearRemodAdd)) + geom_bar()

## Missing values
global[which(is.na(global$YearRemodAdd)), ]
global[which(global$YearRemodAdd==''), ]
global[which(global$YearRemodAdd=='0'), ]

## No actions


  
# Feature RoofStyle analysis
-----------------------------
# Desc: Type of roof

## Information analysis
table(global$RoofStyle)
prop.table(table(global$RoofStyle))*100

#ggplot(global, aes(RoofStyle, fill=RoofStyle)) + geom_bar()

## Missing values
global[which(is.na(global$RoofStyle)), ]
global[which(global$RoofStyle==''), ]

## No actions


  
# Feature RoofMatl analysis
-----------------------------
# Desc: Roof material

## Information analysis
table(global$RoofMatl)
prop.table(table(global$RoofMatl))*100

#ggplot(global, aes(RoofStyle, fill=RoofMatl)) + geom_bar()

## Missing values
global[which(is.na(global$RoofMatl)), ]
global[which(global$RoofMatl==''), ]

## No actions



# Feature Exterior1st analysis
-----------------------------
# Desc: Exterior covering on house.

## Information analysis
global$Exterior1st <- as.factor(global$Exterior1st)
table(global$Exterior1st)
prop.table(table(global$Exterior1st))*100

#ggplot(global, aes(Exterior1st, fill=Exterior1st)) + geom_bar()

## Missing values
global[which(is.na(global$Exterior1st)), ]

# Value imputation
# Several missing data. We should remove the observation.
global <- global[!is.na(global$Exterior1st), ]



# Feature Exterior2nd analysis
-----------------------------
  # Desc: Exterior covering on house (if more than one material).
  
  ## Information analysis
  global$Exterior2nd <- as.factor(global$Exterior2nd)
table(global$Exterior2nd)
prop.table(table(global$Exterior2nd))*100

#ggplot(global, aes(Exterior2nd, fill=Exterior2nd)) + geom_bar()

## Missing values
global[which(is.na(global$Exterior2nd)), ]

# Value imputation
# Observation removed with Exterior1st



# Feature MasVnrArea analysis
-----------------------------
  # Desc: Masonry veneer area in square feet
  
  ## Information analysis
  table(global$MasVnrArea)
prop.table(table(global$MasVnrArea))*100

#ggplot(global, aes(MasVnrArea, fill=MasVnrArea)) + geom_bar()

## Missing values
global[which(is.na(global$MasVnrArea)), ]
global[which(global$MasVnrArea==''), ]

## Value imputation
# I assume that, if the value is 'NA', there is no surface (and, specially, if MasVnrType is also NA)
global$MasVnrArea[which(is.na(global$MasVnrArea))] <- 0



# Feature MasVnrType analysis
-----------------------------
# Desc: Masonry veneer type

## Information analysis
table(global$MasVnrType)
prop.table(table(global$MasVnrType))*100

#ggplot(global, aes(MasVnrType, fill=MasVnrType)) + geom_bar()

## Missing values
global[which(is.na(global$MasVnrType)), ]
global[which(global$MasVnrType==''), ]

## Value imputation

# I assume that, if the value is 'NA', there is no masonry surface
global$MasVnrType[which(is.na(global$MasVnrType) & global$MasVnrArea==0)] <- 'None'

# The is 1 case with 198 of masonry. I give ir the most probable type: BrkFace
global$MasVnrType[which(is.na(global$MasVnrType) & global$MasVnrArea!=0)] <- 'BrkFace'


  
# Feature ExterQual analysis
-----------------------------
# Desc: Evaluates the quality of the material on the exterior 

## Information analysis
table(global$ExterQual)
prop.table(table(global$ExterQual))*100

#ggplot(global, aes(ExterQual, fill=ExterQual)) + geom_bar()

## Missing values
global[which(is.na(global$ExterQual)), ]
global[which(global$ExterQual==''), ]

## No actions


  
# Feature ExterCond analysis
-----------------------------
# Desc: Evaluates the present condition of the material on the exterior

## Information analysis
table(global$ExterCond)
prop.table(table(global$ExterCond))*100

#ggplot(global, aes(ExterCond, fill=ExterCond)) + geom_bar()

## Missing values
global[which(is.na(global$ExterCond)), ]
global[which(global$ExterCond==''), ]

## No actions


# Feature Foundation analysis
-----------------------------
# Desc: Type of foundation

## Information analysis
table(global$Foundation)
prop.table(table(global$Foundation))*100

#ggplot(global, aes(Foundation, fill=Foundation)) + geom_bar()

## Missing values
global[which(is.na(global$Foundation)), ]
global[which(global$Foundation==''), ]

## No actions

  
  
# Feature BsmtQual analysis
-----------------------------
# Desc: Evaluates the height of the basement

## Information analysis
table(global$BsmtQual)
prop.table(table(global$BsmtQual))*100

#ggplot(global, aes(BsmtQual, fill=BsmtQual)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtQual)), ]
global[which(global$BsmtQual==''), ]

## Value imputation
# Let's assume that 'NA' means there is no basement
global$BsmtQual[which(is.na(global$BsmtQual))] <- 'NA'



# Feature BsmtCond analysis
-----------------------------
# Desc: Evaluates the general condition of the basement

## Information analysis
table(global$BsmtCond)
prop.table(table(global$BsmtCond))*100

#ggplot(global, aes(BsmtCond, fill=BsmtCond)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtCond)), ]
global[which(global$BsmtCond==''), ]

## Value imputation
# Most of the NA values correspond to the feature BsmtQual imputation
global$BsmtCond[which(is.na(global$BsmtCond) & global$BsmtQual=='NA')] <- 'NA'
# But there are few cases different. I assign the most frequent value: TA
global$BsmtCond[which(is.na(global$BsmtCond) & global$BsmtQual!='NA')] <- 'TA'

  
  
# Feature BsmtExposure analysis
-----------------------------
# Desc: Refers to walkout or garden level walls

## Information analysis
table(global$BsmtExposure)
prop.table(table(global$BsmtExposure))*100

#ggplot(global, aes(BsmtExposure, fill=BsmtExposure)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtExposure)), ]
global[which(global$BsmtExposure==''), ]

## Value imputation
# Most of the NA values correspond to the feature BsmtQual imputation
global$BsmtExposure[which(is.na(global$BsmtExposure) & global$BsmtQual=='NA')] <- 'NA'
# But there are few cases different. I assign the most frequent value: No
global$BsmtExposure[which(is.na(global$BsmtExposure) & global$BsmtQual!='NA')] <- 'No'


  
# Feature BsmtFinType1 analysis
-----------------------------
# Desc: Rating of basement finished area

## Information analysis
table(global$BsmtFinType1)
prop.table(table(global$BsmtFinType1))*100

#ggplot(global, aes(BsmtFinType1, fill=BsmtFinType1)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtFinType1)), ]
global[which(global$BsmtFinType1==''), ] 

## Value imputation
# Most of the NA values correspond to the feature BsmtQual imputation
global$BsmtFinType1[which(is.na(global$BsmtFinType1) & global$BsmtQual=='NA')] <- 'NA'
  

  
# Feature BsmtFinSF1 analysis
-----------------------------
# Desc: Type 1 finished square feet

## Information analysis
table(global$BsmtFinSF1)
prop.table(table(global$BsmtFinSF1))*100

#ggplot(global, aes(BsmtFinSF1, fill=BsmtFinSF1)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtFinSF1)), ]
global[which(global$BsmtFinSF1==''), ] 

## Value imputation
# Most of the NA values correspond to the feature BsmtQual imputation
global$BsmtFinSF1[which(is.na(global$BsmtFinSF1) & global$BsmtQual=='NA')] <- 0


  
# Feature BsmtFinType2 analysis
-----------------------------
# Desc: Rating of basement finished area (if multiple types)

## Information analysis
table(global$BsmtFinType2)
prop.table(table(global$BsmtFinType2))*100

#ggplot(global, aes(BsmtFinType2, fill=BsmtFinType2)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtFinType2)), ]
global[which(global$BsmtFinType2==''), ] 

## Value imputation
# Most of the NA values correspond to the feature BsmtQual imputation
global$BsmtFinType2[which(is.na(global$BsmtFinType2) & global$BsmtQual=='NA')] <- 'NA'
# But there are few cases different. I assign the most frequent value: Unf
global$BsmtFinType2[which(is.na(global$BsmtFinType2) & global$BsmtQual!='NA')] <- 'Unf'

  
  
# Feature BsmtFinSF2 analysis
-----------------------------
# Desc: Type 2 finished square feet

## Information analysis
table(global$BsmtFinSF2)
prop.table(table(global$BsmtFinSF2))*100

#ggplot(global, aes(BsmtFinSF2, fill=BsmtFinSF2)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtFinSF2)), ]
global[which(global$BsmtFinSF2==''), ] 

## Value imputation
# Most of the NA values correspond to the feature BsmtQual imputation
global$BsmtFinSF2[which(is.na(global$BsmtFinSF2) & global$BsmtQual=='NA')] <- 0


  
# Feature BsmtUnfSF analysis
-----------------------------
# Desc: Unfinished square feet of basement area

## Information analysis
table(global$BsmtUnfSF)
prop.table(table(global$BsmtUnfSF))*100

#ggplot(global, aes(BsmtUnfSF, fill=BsmtUnfSF)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtUnfSF)), ]
global[which(global$BsmtUnfSF==''), ] 

## Value imputation
# Most of the NA values correspond to the feature BsmtQual imputation
global$BsmtUnfSF[which(is.na(global$BsmtUnfSF) & global$BsmtQual=='NA')] <- 0

  
  
# Feature TotalBsmtSF analysis
-----------------------------
# Desc: Total square feet of basement area

## Information analysis
table(global$TotalBsmtSF)
prop.table(table(global$TotalBsmtSF))*100

#ggplot(global, aes(TotalBsmtSF, fill=TotalBsmtSF)) + geom_bar()

## Missing values
global[which(is.na(global$TotalBsmtSF)), ]
global[which(global$TotalBsmtSF==''), ] 

## Value imputation
# Most of the NA values correspond to the feature BsmtQual imputation
global$TotalBsmtSF[which(is.na(global$TotalBsmtSF) & global$BsmtQual=='NA')] <- 0


  
# Feature Heating analysis
-----------------------------
# Desc: Type of heating

## Information analysis
table(global$Heating)
prop.table(table(global$Heating))*100

#ggplot(global, aes(Heating, fill=Heating)) + geom_bar()

## Missing values
global[which(is.na(global$Heating)), ]
global[which(global$Heating==''), ] 

## No actions
  

  
# Feature HeatingQC analysis
-----------------------------
# Desc: Heating quality and condition

## Information analysis
table(global$HeatingQC)
prop.table(table(global$HeatingQC))*100

#ggplot(global, aes(HeatingQC, fill=HeatingQC)) + geom_bar()

## Missing values
global[which(is.na(global$HeatingQC)), ]
global[which(global$HeatingQC==''), ] 

## No actions

  
  
# Feature CentralAir analysis
-----------------------------
# Desc: Central air conditioning

## Information analysis
table(global$CentralAir)
prop.table(table(global$CentralAir))*100

#ggplot(global, aes(CentralAir, fill=CentralAir)) + geom_bar()

## Missing values
global[which(is.na(global$CentralAir)), ]
global[which(global$CentralAir==''), ] 

## No actions

  
  
# Feature Electrical analysis
-----------------------------
# Desc: Electrical system

## Information analysis
table(global$Electrical)
prop.table(table(global$Electrical))*100

#ggplot(global, aes(Electrical, fill=Electrical)) + geom_bar()

## Missing values
global[which(is.na(global$Electrical)), ]
global[which(global$Electrical==''), ] 

## Value imputation
# I impute the most frequent value: SBrkr
global$Electrical[which(is.na(global$Electrical))] <- 'SBrkr'


  
# Feature 1stFlrSF analysis
-----------------------------
# Desc: First Floor square feet

## Information analysis (X1stFlrSF)
table(global$X1stFlrSF)
prop.table(table(global$X1stFlrSF))*100

#ggplot(global, aes(X1stFlrSF, fill=X1stFlrSF)) + geom_bar()

## Missing values
global[which(is.na(global$X1stFlrSF)), ]
global[which(global$X1stFlrSF==''), ] 

## No actions



# Feature 2ndFlrSF: analysis
-----------------------------
# Desc: Second floor square feet

## Information analysis (X2ndFlrSF)
table(global$X2ndFlrSF)
prop.table(table(global$X2ndFlrSF))*100

#ggplot(global, aes(X2ndFlrSF, fill=X2ndFlrSF)) + geom_bar()

## Missing values
global[which(is.na(global$X2ndFlrSF)), ]
global[which(global$X2ndFlrSF==''), ] 

## No actions



# Feature LowQualFinSF analysis
-----------------------------
# Desc: Low quality finished square feet (all floors)

## Information analysis
table(global$LowQualFinSF)
prop.table(table(global$LowQualFinSF))*100

#ggplot(global, aes(LowQualFinSF, fill=LowQualFinSF)) + geom_bar()

## Missing values
global[which(is.na(global$LowQualFinSF)), ]
global[which(global$LowQualFinSF==''), ] 

## No actions

  
  
# Feature GrLivArea analysis
-----------------------------
# Desc: Above grade (ground) living area square feet

## Information analysis
table(global$GrLivArea)
prop.table(table(global$GrLivArea))*100

#ggplot(global, aes(GrLivArea, fill=GrLivArea)) + geom_bar()

## Missing values
global[which(is.na(global$GrLivArea)), ]
global[which(global$GrLivArea==''), ] 

## No actions

  
  
# Feature BsmtFullBath analysis
-----------------------------
# Desc: Basement full bathrooms

## Information analysis
table(global$BsmtFullBath)
prop.table(table(global$BsmtFullBath))*100

#ggplot(global, aes(BsmtFullBath, fill=BsmtFullBath)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtFullBath)), ]
global[which(global$BsmtFullBath==''), ] 

## Value imputation
# The NA values correspond to the feature BsmtQual imputation
global$BsmtFullBath[which(is.na(global$BsmtFullBath) & global$BsmtQual=='NA')] <- 0
  

  
# Feature BsmtHalfBath analysis
-----------------------------
# Desc: Basement half bathrooms

## Information analysis
table(global$BsmtHalfBath)
prop.table(table(global$BsmtHalfBath))*100

#ggplot(global, aes(BsmtHalfBath, fill=BsmtHalfBath)) + geom_bar()

## Missing values
global[which(is.na(global$BsmtHalfBath)), ]
global[which(global$BsmtHalfBath==''), ] 

## Value imputation
# The NA values correspond to the feature BsmtQual imputation
global$BsmtHalfBath[which(is.na(global$BsmtHalfBath) & global$BsmtQual=='NA')] <- 0


  
# Feature FullBath analysis
-----------------------------
# Desc: Full bathrooms above grade

## Information analysis
table(global$FullBath)
prop.table(table(global$FullBath))*100

#ggplot(global, aes(FullBath, fill=FullBath)) + geom_bar()

## Missing values
global[which(is.na(global$FullBath)), ]
global[which(global$FullBath==''), ] 

## No Actions
  
  
# Feature HalfBath analysis
-----------------------------
# Desc: Half baths above grade

## Information analysis
table(global$HalfBath)
prop.table(table(global$HalfBath))*100

#ggplot(global, aes(HalfBath, fill=HalfBath)) + geom_bar()

## Missing values
global[which(is.na(global$HalfBath)), ]
global[which(global$HalfBath==''), ] 

## No Actions


  
# Feature Bedroom analysis
-----------------------------
# Desc: Bedrooms above grade (does NOT include basement bedrooms)

## Information analysis
table(global$BedroomAbvGr)
prop.table(table(global$BedroomAbvGr))*100

#ggplot(global, aes(BedroomAbvGr, fill=BedroomAbvGr)) + geom_bar()

## Missing values
global[which(is.na(global$BedroomAbvGr)), ]
global[which(global$BedroomAbvGr==''), ] 

## No Actions



# Feature KitchenAbvGr analysis
-----------------------------
# Desc: Kitchens above grade

## Information analysis
table(global$KitchenAbvGr)
prop.table(table(global$KitchenAbvGr))*100

#ggplot(global, aes(KitchenAbvGr, fill=KitchenAbvGr)) + geom_bar()

## Missing values
global[which(is.na(global$KitchenAbvGr)), ]
global[which(global$KitchenAbvGr==''), ] 

## No Actions

  
  
# Feature KitchenQual analysis
-----------------------------
# Desc: Kitchen quality

## Information analysis
table(global$KitchenQual)
prop.table(table(global$KitchenQual))*100

#ggplot(global, aes(KitchenQual, fill=KitchenQual)) + geom_bar()

## Missing values
global[which(is.na(global$KitchenQual)), ]
global[which(global$KitchenQual==''), ] 

## Value imputation
global$OverallQual[which(is.na(global$KitchenQual))]
#ggplot(global, aes(OverallQual, fill=KitchenQual)) + geom_bar()
# The most frequent value for OverallQual= 5 is 'TA'
global$KitchenQual[which(is.na(global$KitchenQual))] <- 'TA'


  
# Feature TotRmsAbvGrd analysis
-----------------------------
# Desc: Total rooms above grade (does not include bathrooms)

## Information analysis
table(global$TotRmsAbvGrd)
prop.table(table(global$TotRmsAbvGrd))*100

#ggplot(global, aes(TotRmsAbvGrd, fill=TotRmsAbvGrd)) + geom_bar()

## Missing values
global[which(is.na(global$TotRmsAbvGrd)), ]
global[which(global$TotRmsAbvGrd==''), ]   

## No actions



# Feature Functional analysis
-----------------------------
# Desc: Home functionality (Assume typical unless deductions are warranted)

## Information analysis
table(global$Functional)
prop.table(table(global$Functional))*100

#ggplot(global, aes(Functional, fill=Functional)) + geom_bar()

## Missing values
global[which(is.na(global$Functional)), ]
global[which(global$Functional==''), ]   

## Value imputation
# The most frequent value is 'Typ'
global$Functional[which(is.na(global$Functional))] <- 'Typ'

  
  
# Feature Fireplaces analysis
-----------------------------
# Desc: Number of fireplaces

## Information analysis
table(global$Fireplaces)
prop.table(table(global$Fireplaces))*100

#ggplot(global, aes(Fireplaces, fill=Fireplaces)) + geom_bar()

## Missing values
global[which(is.na(global$Fireplaces)), ]
global[which(global$Fireplaces==''), ]   

## No actions


  
# Feature FireplaceQu analysis
-----------------------------
# Desc: Fireplace quality

## Information analysis
table(global$FireplaceQu)
prop.table(table(global$FireplaceQu))*100

#ggplot(global, aes(FireplaceQu, fill=FireplaceQu)) + geom_bar()

## Missing values
global[which(is.na(global$FireplaceQu)), ]
global[which(global$FireplaceQu==''), ]   

## Value imputation
global$FireplaceQu[which(is.na(global$FireplaceQu) & global$Fireplaces==0)] <- 'NA'

  
  
# Feature GarageType analysis
-----------------------------
# Desc: Garage location

## Information analysis
table(global$GarageType)
prop.table(table(global$GarageType))*100

#ggplot(global, aes(GarageType, fill=GarageType)) + geom_bar()

## Missing values
global[which(is.na(global$GarageType)), ]
global[which(global$GarageType==''), ]   

## Value imputation 
# No value means no garage
global$GarageType[which(is.na(global$GarageType))] <- 'NA'


  
# Feature GarageYrBlt analysis
-----------------------------
# Desc: Year garage was built

## Information analysis
table(global$GarageYrBlt)
prop.table(table(global$GarageYrBlt))*100

#ggplot(global, aes(GarageYrBlt, fill=GarageYrBlt)) + geom_bar()

## Missing values
global[which(is.na(global$GarageYrBlt)), ]
global[which(global$GarageYrBlt==''), ]   

## Value imputation 
# No value means no garage
global$GarageYrBlt[which(is.na(global$GarageYrBlt))] <- 'NA'

  
  
# Feature GarageFinish analysis
-----------------------------
# Desc: Interior finish of the garage

## Information analysis
table(global$GarageFinish)
prop.table(table(global$GarageFinish))*100

#ggplot(global, aes(GarageFinish, fill=GarageFinish)) + geom_bar()

## Missing values
global[which(is.na(global$GarageFinish)), ]
global[which(global$GarageFinish==''), ] 

## Value imputation 
# No value means no garage
global$GarageFinish[which(is.na(global$GarageFinish))] <- 'NA'
  
  

# Feature GarageCars analysis
-----------------------------
# Desc: Size of garage in car capacity

## Information analysis
table(global$GarageCars)
prop.table(table(global$GarageCars))*100

#ggplot(global, aes(GarageCars, fill=GarageCars)) + geom_bar()

## Missing values
global[which(is.na(global$GarageCars)), ]
global[which(global$GarageCars==''), ] 

## Value imputation 
# No value means no garage
global$GarageCars[which(is.na(global$GarageCars))] <- 'NA'

  
  
# Feature GarageArea analysis
-----------------------------
# Desc: Size of garage in square feet

## Information analysis
table(global$GarageArea)
prop.table(table(global$GarageArea))*100

#ggplot(global, aes(GarageArea, fill=GarageArea)) + geom_bar()

## Missing values
global[which(is.na(global$GarageArea)), ]
global[which(global$GarageArea==''), ] 

## Value imputation 
# No value means no garage
global$GarageArea[which(is.na(global$GarageArea))] <- 'NA'


  
# Feature GarageQual analysis
-----------------------------
# Desc: Garage quality

## Information analysis
table(global$GarageQual)
prop.table(table(global$GarageQual))*100

#ggplot(global, aes(GarageQual, fill=GarageQual)) + geom_bar()

## Missing values
global[which(is.na(global$GarageQual)), ]
global[which(global$GarageQual==''), ] 

## Value imputation 
# No value means no garage
global$GarageQual[which(is.na(global$GarageQual))] <- 'NA'

  
  
# Feature GarageCond analysis
-----------------------------
# Desc: Garage condition

## Information analysis
table(global$GarageCond)
prop.table(table(global$GarageCond))*100

#ggplot(global, aes(GarageCond, fill=GarageCond)) + geom_bar()

## Missing values
global[which(is.na(global$GarageCond)), ]
global[which(global$GarageCond==''), ] 

## Value imputation 
# No value means no garage
global$GarageCond[which(is.na(global$GarageCond))] <- 'NA'


  
  
# Feature PavedDrive analysis
-----------------------------
# Desc: Paved driveway

## Information analysis
table(global$PavedDrive)
prop.table(table(global$PavedDrive))*100

#ggplot(global, aes(PavedDrive, fill=PavedDrive)) + geom_bar()

## Missing values
global[which(is.na(global$PavedDrive)), ]
global[which(global$PavedDrive==''), ] 

## No action



# Feature WoodDeckSF analysis
-----------------------------
# Desc: Wood deck area in square feet

## Information analysis
table(global$WoodDeckSF)
prop.table(table(global$WoodDeckSF))*100

#ggplot(global, aes(WoodDeckSF, fill=WoodDeckSF)) + geom_bar()

## Missing values
global[which(is.na(global$WoodDeckSF)), ]
global[which(global$WoodDeckSF==''), ] 

## No action  


  
# Feature OpenPorchSF analysis
-----------------------------
# Desc: Open porch area in square feet

## Information analysis
table(global$OpenPorchSF)
prop.table(table(global$OpenPorchSF))*100

#ggplot(global, aes(OpenPorchSF, fill=OpenPorchSF)) + geom_bar()

## Missing values
global[which(is.na(global$OpenPorchSF)), ]
global[which(global$OpenPorchSF==''), ] 

## No action  

  
  
# Feature EnclosedPorch analysis
-----------------------------
# Desc: Enclosed porch area in square feet

## Information analysis
table(global$EnclosedPorch)
prop.table(table(global$EnclosedPorch))*100

#ggplot(global, aes(EnclosedPorch, fill=EnclosedPorch)) + geom_bar()

## Missing values
global[which(is.na(global$EnclosedPorch)), ]
global[which(global$EnclosedPorch==''), ] 

## No action  


  
# Feature 3SsnPorch analysis
-----------------------------
# Desc: Three season porch area in square feet

## Information analysis (X3SsnPorch)
table(global$X3SsnPorch)
prop.table(table(global$X3SsnPorch))*100

#ggplot(global, aes(X3SsnPorch, fill=X3SsnPorch)) + geom_bar()

## Missing values
global[which(is.na(global$X3SsnPorch)), ]
global[which(global$X3SsnPorch==''), ] 

## No action  


  
# Feature ScreenPorch analysis
-----------------------------
# Desc: Screen porch area in square feet

## Information analysis
table(global$ScreenPorch)
prop.table(table(global$ScreenPorch))*100

#ggplot(global, aes(ScreenPorch, fill=ScreenPorch)) + geom_bar()

## Missing values
global[which(is.na(global$ScreenPorch)), ]
global[which(global$ScreenPorch==''), ] 

## No action  

  
  
# Feature PoolArea analysis
-----------------------------
# Desc: Pool area in square feet

## Information analysis
table(global$PoolArea)
prop.table(table(global$PoolArea))*100

#ggplot(global, aes(PoolArea, fill=PoolArea)) + geom_bar()

## Missing values
global[which(is.na(global$PoolArea)), ]
global[which(global$PoolArea==''), ] 

## No action  


  
# Feature PoolQC analysis
-----------------------------
# Desc: Pool quality

## Information analysis
table(global$PoolQC)
prop.table(table(global$PoolQC))*100

#ggplot(global, aes(PoolQC, fill=PoolQC)) + geom_bar()

## Missing values
global[which(is.na(global$PoolQC)), ]
global[which(global$PoolQC==''), ] 

## Value imputation 
# No value means no pool
global$PoolQC[which(is.na(global$PoolQC))] <- 'NA'


  
# Feature Fence analysis
-----------------------------
# Desc: Fence quality

## Information analysis
table(global$Fence)
prop.table(table(global$Fence))*100

#ggplot(global, aes(Fence, fill=Fence)) + geom_bar()

## Missing values
global[which(is.na(global$Fence)), ]
global[which(global$Fence==''), ] 

## Value imputation 
# No value means no fence
global$Fence[which(is.na(global$Fence))] <- 'NA'


  
# Feature MiscFeature analysis
-----------------------------
# Desc: Miscellaneous feature not covered in other categories

## Information analysis
table(global$MiscFeature)
prop.table(table(MiscFeature))*100

#ggplot(global, aes(MiscFeature, fill=MiscFeature)) + geom_bar()

## Missing values
global[which(is.na(global$MiscFeature)), ]
global[which(global$MiscFeature==''), ] 

## Value imputation 
# No value means no exgtra feature
global$MiscFeature[which(is.na(global$MiscFeature))] <- 'NA'


  
# Feature MiscVal analysis
-----------------------------
# Desc: $Value of miscellaneous feature

## Information analysis
table(global$MiscVal)
prop.table(table(MiscVal))*100

#ggplot(global, aes(MiscVal, fill=MiscVal)) + geom_bar()

## Missing values
global[which(is.na(global$MiscVal)), ]
global[which(global$MiscVal==''), ] 

## No actions


  
# Feature MoSold analysis
-----------------------------
# Desc: Month Sold (MM)

## Information analysis
table(global$MoSold)
prop.table(table(MoSold))*100

#ggplot(global, aes(MoSold, fill=MoSold)) + geom_bar()

## Missing values
global[which(is.na(global$MoSold)), ]
global[which(global$MoSold==''), ] 

## No actions

  
  
# Feature YrSold analysis
-----------------------------
# Desc: Year Sold (YYYY)

## Information analysis
table(global$YrSold)
prop.table(table(YrSold))*100

#ggplot(global, aes(YrSold, fill=YrSold)) + geom_bar()

## Missing values
global[which(is.na(global$YrSold)), ]
global[which(global$YrSold==''), ] 

## No actions

  
  
# Feature SaleType analysis
-----------------------------
# Desc: Type of sale

## Information analysis
table(global$SaleType)
prop.table(table(SaleType))*100

#ggplot(global, aes(SaleType, fill=SaleType)) + geom_bar()

## Missing values
global[which(is.na(global$SaleType)), ]
global[which(global$SaleType==''), ] 

## Value imputation
# The most frequent value is 'WD' (1 NA case)
global$SaleType[which(is.na(global$SaleType))] <- 'WD'

  
  
# Feature SaleCondition analysis
-----------------------------
# Desc: Condition of sale

## Information analysis
table(global$SaleCondition)
prop.table(table(SaleCondition))*100

#ggplot(global, aes(SaleCondition, fill=SaleCondition)) + geom_bar()

## Missing values
global[which(is.na(global$SaleCondition)), ]
global[which(global$SaleCondition==''), ]   
  
## No actions



