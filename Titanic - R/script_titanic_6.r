#
# Carga de los datos originales
# 
setwd("C:\\Users\\ecomped\\Documents\\r_2")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)


#
# Juntamos los 2 conjuntos de datos para limpiar y añadir campos
#
test$Survived <- NA
combi <- rbind(train, test)
combi <- data.frame(combi)


#
# Revisamos los NA
#
library("Amelia")
## missmap(combi)


#
# Tratamiento del campo Survived
#
combi$Survived <- factor(combi$Survived)


#
# Tratamiento del campo Sex
#
combi$Sex <- factor(combi$Sex)


#
# Tratamiento del campo Pclass
#
combi$Pclass <- factor(combi$Pclass)


#
# Tratamiento del campo Embarked
#
combi$Embarked[which(combi$Embarked == '')] = "S"
combi$Embarked <- factor(combi$Embarked)


#
# Tratamiento del campo Fare
#
combi$Fare[1044] <- median(combi[combi$Pclass == '3' & combi$Embarked == 'S', ]$Fare, na.rm = TRUE)


#
# Creamos el campo Title
#
library('dplyr')
combi$Title2 <- gsub('(.*, )|(\\..*)', '', combi$Name)
combi$Title <- gsub('(.*, )|(\\..*)', '', combi$Name)
combi$Title[combi$Title %in% c('Ms', 'Mlle')]  <- 'Miss'
combi$Title[combi$Title %in% c('Mme', 'Dona')]  <- 'Mrs'
combi$Title[combi$Title %in% c('the Countess', 'Lady')]  <- 'Lady'
combi$Title[combi$Title %in% c('Jonkheer', 'Sir', 'Col')]  <- 'Sir'
combi$Title[combi$Title %in% c('Don')]  <- 'Mr'
combi$Title[combi$Title %in% c('Capt', 'Dr', 'Major', 'Rev')]  <- 'Officer'
combi$Title[combi$Title == 'Officer' & combi$Sex == 'female']  <- 'Lady'
combi$Title <- factor(combi$Title)
## table(combi$Sex, combi$Title) # Validación
## table(combi$Title)

#
# Tratamiento de la familia
#
combi$Fsize <- combi$SibSp + combi$Parch + 1
table(combi$Fsize)
table(combi$Fsize, combi$Survived)


combi$FsizeD[combi$Fsize == 1] <- 'singleton'
combi$FsizeD[combi$Fsize == 2] <- 'couple'
combi$FsizeD[combi$Fsize > 2 & combi$Fsize < 5] <- 'small'
combi$FsizeD[combi$Fsize >= 5] <- 'large'
combi$FsizeD <- factor(combi$FsizeD)


#
# Creamos el campo Surname
#
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$Surname <- factor(combi$Surname)


#
# Creamos el campo FamilyID
#
combi$FamilyID <- paste(as.character(combi$Fsize), combi$Surname, sep="-")
combi$FamilyID <- factor(combi$FamilyID)


#
# Completamos datos del campo Age
#
library('mice')
set.seed(129)
mice_mod <- mice(combi[, !names(combi) %in% c('Deck', 'PassengerId', 'Name','Ticket','Cabin','Survived','Surname','FamilyID')], method='rf')
mice_output <- complete(mice_mod)
combi$Age <- mice_output$Age


#
# Creamos el campo AgePclass
#
combi$Age <- combi$Age
combi$Pclass <- combi$Pclass
combi$AgePclass = as.numeric(combi$Age) * as.numeric(combi$Age)


#
# Creamos el campo IsAlone
#
combi$IsAlone <- 0
combi$IsAlone[combi$Fsize == 1] <- 1



#
# Creamos el campo Deck
#
combi$Deck <- sapply(combi$Cabin, function(x) strsplit(x, NULL)[[1]][1])
combi$Deck[which(is.na(combi$Deck))] <- "U"
combi$Deck <- factor(combi$Deck)
# str(combi$Deck)



#
# Creamos el campo HasCabin
#
combi$HasCabin <- 1
combi$HasCabin[combi$Deck == "U"] <- 0


#
# Creamos el campo Child
#
combi$Child = "Infant"
combi$Child[combi$Age >= 2] <- "Child"
combi$Child[combi$Age >= 12] <- "Teen"
combi$Child[combi$Age >= 18] <- "Young"
combi$Child[combi$Age >= 24] <- "Adult"
combi$Child[combi$Age >= 55] <- "Senior"
combi$Child[combi$Age >= 70] <- "Elderly"
combi$Child <- factor(combi$Child)


#
# Creamos campo Priority
#
combi$Priority <- 0 
combi$Priority[combi$Sex == 'female']  <- 1
combi$Priority[combi$Title2 == 'Master']  <- 1
combi$Priority <- factor(combi$Priority)
# Mujeres y niños primero
# table(combi$Survived, combi$Priority) # Validación
# prop.table(table(combi$Survived == 1, combi$Priority))


#
# Creamos el campo Social
#
combi$Social.1 <- 0
combi$Social.1[combi$Pclass == 2] <- 1
combi$Social.1[combi$Pclass == 1] <- 2
combi$Social.2 <- 0
combi$Social.2[gsub('(.*, )|(\\..*)', '', combi$Name) %in% c('Capt', 'Rev', 'Dr', 'Major', 'Lady')]  <- 1
combi$Social.2[gsub('(.*, )|(\\..*)', '', combi$Name) %in% c('Col', 'Jonkheer', 'the Countess', 'Sir')]  <- 2
combi$Social.3 <- 0
#combi$Social.3 [combi$Child %in% c('Teen', 'Young')] <- 1
combi$Social.4 <- 0
combi$Social.4 [combi$UniFare >= 20 & combi$UniFare < 46] <- 1
combi$Social.4 [combi$UniFare >= 46 ] <- 2

combi$Social <- combi$Social.1 + combi$Social.2 + combi$Social.3 + combi$Social.4
combi <- subset(combi, select = -c(Social.1, Social.2, Social.3, Social.4))
combi$Social <- factor(combi$Social)


#
# Creamos el campo UniFare
#
combi$UniFare <- combi$Fare / combi$Fsize


#
# Creamos el campo FareBand
#
combi$FareBand <- 'Medium'
combi$FareBand [combi$UniFare < 27] <- 'Cheap'
combi$FareBand [combi$UniFare > 70] <- 'Expensive'
combi$FareBand <- factor(combi$FareBand)


#
# Quitamos los campos no útiles
#
combi <- subset(combi, select = -c(Ticket, Cabin, Name, Deck))  #Deck, 
combi <- subset(combi, select = -c(Surname, FamilyID, Title2))

str(combi)
summary(combi)
missmap(combi)


# Dividimos los conjuntos reestructurados
##names(combi)
train <- combi[1:891,]
test <- combi[892:1309,]



