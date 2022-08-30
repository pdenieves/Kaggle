# install.packages('corrplot')
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(GGally)

#
# Búsqueda de relación entre Deck y otros campos para completarlo
#
ggplot(data=combi, aes(x=Age, fill=Sex)) + 
  geom_density(alpha=0.3)

ggplot(data=subset(combi, Fare < 200), aes(x=Fare, fill=Deck)) + 
  geom_density(alpha=0.3)

ggplot(data=combi, aes(x=Fare, fill=Deck)) + 
  geom_density(alpha=0.3)

ggplot(data=subset(combi, Pclass == 1), aes(x=Fare, fill=Deck)) + 
  geom_density(alpha=0.3)

ggplot(data=subset(combi, Fare < 30 & Pclass == 2), aes(x=Fare, fill=Deck)) + 
  geom_density(alpha=0.3)
# Demasiado redondeo en la gráfica. Mejor histograma


ggplot(subset(combi, Pclass == 1), aes(x=Fare, y=Title)) + 
  geom_point() + 
  theme_bw() + 
  xlim(0,65)

# Pclass == 1 - OK
ggplot(subset(combi, Pclass == 1 & Social %in% c(2)), aes(x = Deck, y = Fare)) +
  geom_violin(aes(fill = Deck) ) 
# Pclass == 1 & Social == 2 -> Deck = 'E'
# combi$Deck[which (combi$Pclass == 1 & combi$Social == 2)]

# Pclass == 2 - OK
ggplot(subset(combi, Fare >= 15.25 & Fare < 400 & Pclass == 2), aes(x = Deck, y = Fare)) +
  geom_violin(aes(fill = Deck) ) 
# combi$Deck[which(combi$Fare > 10.50 & combi$Fare < 12.75 & combi$Pclass == 2 & is.na(combi$Deck))] #<- 'E'
# combi$Deck[which(combi$Fare <= 10.50 & combi$Pclass == 2 & is.na(combi$Deck))] #<- 'F'
# combi$Deck[which(combi$Fare >= 12.75 & combi$Fare < 15.25 & !is.na(combi$Deck))] #<- 'D'
# combi$Deck[which(combi$Pclass == 2 & combi$Fare >= 15.25  & !is.na(combi$Deck))] #<- 'F'

# Pclass == 3
ggplot(subset(combi, Fare >= 10 & Fare < 20 & Pclass == 3), aes(x = Deck, y = Fare)) +
  geom_violin(aes(fill = Deck) ) 
# combi$Deck[which(combi$Pclass == 3 & combi$Fare >= 8 & combi$Fare < 200  & !is.na(combi$Deck))] #<- 'E'

summary(combi)

# ggplot(subset(combi, Pclass == 2 & combi$Social != 2 & combi$Fsize %in% c(1, 2)), 
ggplot(combi, 
       aes(x=Fare, y=Deck2, color=Deck2)) + 
  geom_point() + 
  theme_bw() +
  xlim(0,50)

combi$Embarked[which(combi$Deck == 'E' & combi$Pclass == 3 & combi$Fare > 10 & !is.na(combi$Deck))]



#
# Correlación entre diferentes campos
#
combi$Survived <- as.integer(combi$Survived)
combi$Sex <- as.integer(combi$Sex)
combi$Mother <- as.integer(combi$Mother)
combi$Child <- as.integer(combi$Child)
combi$Social <- as.integer(combi$Social)
combi$Pclass <- as.integer(combi$Pclass)

ggcorr(combi,   
       nbreaks = 9,
       label = TRUE,
       label_size = 3,
       palette = "PuOr")


#
# Ticket
#
install.packages('survPen')
library(survPen)
combi[which (combi$Surname == "Sage"), c("Ticket", "Surname", "Fare", "Fsize")]
combi[with(combi, order(combi$FamilyID)), c("Ticket", "FamilyID", "Fare", "Fsize")]
combi[with(combi, order(Surname, decreasing=FALSE)), c("Ticket", "Surname", "Fare", "Fsize", "Age")]
combi[which (grepl("Kink", combi$Surname) > 0), c("Ticket", "Surname", "Fare", "SibSp", "Parch", "Sex", "Age")]


#
# Fare
#
ggplot(data=subset(combi, Fare > 170 & Deck %in% c('A', 'B', 'C', 'D', 'E', 'NA')), aes(x=Fare, fill=Deck)) + 
  geom_density(alpha=0.3)

ggplot(data=subset(combi, Fare > 0 & !Title %in% c('Mr', 'Mrs', 'Miss', 'Master')), aes(x=Fare, fill=Title)) + 
  geom_density(alpha=0.3)



#
# Ver más detalle con los hombres
#
ggplot(data=subset(train, Priority == 1), aes(x=Age, fill=Survived)) + 
  geom_density(alpha=0.3)

combi$Title0 <- gsub('(.*, )|(\\..*)', '', combi$Name)




#subconj <- subset(train, train$Title == 'Mr' & train$Fare < 26)
#subconj <- subset(train, train$Title == 'Mr')
#subconj <- subset(train, train$Fare > 0)
subconj <- train
table(subconj$Priority, subconj$Survived)

scontrol <- rpart.control(minsplit = 10,
                         #minbucket = round(5 / 3),
                         maxdepth = 5,
                         cp = 0)
#stree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Fare2 + Title + Fsize + FsizeD + AgePclass + Child + Priority + Social,
stree <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Embarked + Fare2 + Title + FsizeD + AgePclass + Child + Priority + Social,
              data=subconj,
              method="class",
              control=scontrol); fancyRpartPlot(stree)

Prediction <- predict(stree, newdata = test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "prior-b.csv", row.names = FALSE)
#subconj = 0.76076
#subconj-Deck2 = 0.75598 
#Overfitting con Age y Fare
#subconj-senior = 0.74641
#subconj-age = 0.76555




