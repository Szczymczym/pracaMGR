setwd("C:/Users/Dawid/Dropbox/MAGISTER SGH/Projekt - voice")
getwd()

dane <- read.csv("voice.csv")
dane2 <- read.csv("C:/Users/Dawid/Dropbox/eSGieHa/Prezentacja i Wizualizacja Danych/Drugi semestr/voice2.csv")

summary(dane)

#Bilbioteki
library(moments)
library(ggplot2)

###Exploracja Danych
##Zmienna meanfreq
summary(dane$meanfreq)
mean(dane$meanfreq)
skewness(dane$meanfreq) #gdy dodatnie to skos pozytywna, czyli ogon rozkladu po prawej od sredniej
kurtosis(dane$meanfreq) #gdy >3 to dane skoncentrowane wokól sredniej


average <- tapply(
  dane$meanfreq, 
  dane$label, 
  mean)
#histogram
hist(dane$meanfreq, xlab = "ael")
plot(density(dane$meanfreq))

d1 <- density(dane[dane$label == "male",]$meanfreq)
d2 <- density(dane[dane$label == "female",]$meanfreq)

plot(d1, col = "blue")
lines(d2, col = "red")
#ogarnac wersje wizualizacji z zajec WIZ na mailu

#barplot
ggplot(
  data = dane, 
  aes(x = label, y = meanfreq)) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = meanfreq)) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = meanfreq)) +
  geom_violin()

##Zmienna sd

summary(dane$sd)
mean(dane$sd)

skewness(dane$sd)
kurtosis(dane$sd)
plot(density(dane$sd))

#histogram i gestosc
hist(dane$sd)

d1 <- density(dane[dane$label == "male",]$sd)
d2 <- density(dane[dane$label == "female",]$sd)

plot(d1, col = "blue")
lines(d2, col = "red")

#barplot
ggplot(
  data = dane, 
  aes(x = label, y = meanfreq)) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = meanfreq)) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = meanfreq)) +
  geom_violin()

##Zmienna median


summary(dane$median)
mean(dane$median)
skewness(dane$median)
kurtosis(dane$median)

#histogra i gestosc

hist(dane$median)
plot(density(dane$median))

d1 <- density(dane[dane$label == "male",]$median)
d2 <- density(dane[dane$label == "female",]$median)

plot(d1, col = "blue")
lines(d2, col = "red")

#barplot
ggplot(
  data = dane, 
  aes(x = label, y = median)) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = median)) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = median)) +
  geom_violin()

##Zmienna mode

summary(dane$mode)
mean(dane$mode)
skewness(dane$mode)
kurtosis(dane$mode)

#histogra i gestosc

hist(dane$mode)
plot(density(dane$mode))

d1 <- density(dane[dane$label == "male",]$mode)
d2 <- density(dane[dane$label == "female",]$mode)

plot(d1, col = "blue")
lines(d2, col = "red")

#barplot
ggplot(
  data = dane, 
  aes(x = label, y = mode)) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = mode)) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = mode)) +
  geom_violin()

##Zmienna IQR

summary(dane$IQR)
mean(dane$IQR)
skewness(dane$IQR)
kurtosis(dane$IQR)

#histogra i gestosc

hist(dane$IQR)
plot(density(dane$IQR))

d1 <- density(dane[dane$label == "male",]$IQR)
d2 <- density(dane[dane$label == "female",]$IQR)

plot(d1, col = "blue")
lines(d2, col = "red")

#barplot
ggplot(
  data = dane, 
  aes(x = label, y = IQR)) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = IQR)) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = IQR)) +
  geom_violin()

##Zmienna centroid

summary(dane$centroid)
mean(dane$centroid)
skewness(dane$centroid)
kurtosis(dane$centroid)

#histogra i gestosc

hist(dane$centroid)
plot(density(dane$centroid))

d1 <- density(dane[dane$label == "male",]$centroid)
d2 <- density(dane[dane$label == "female",]$centroid)

plot(d1, col = "blue")
lines(d2, col = "red")

#barplot
ggplot(
  data = dane, 
  aes(x = label, y = centroid)) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = centroid)) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = centroid)) +
  geom_violin()

##Zmienna meanfun

summary(dane$meanfun)
mean(dane$meanfun)
skewness(dane$meanfun)
kurtosis(dane$meanfun)

#histogram i gestosc
par(mfrow=c(1,1))
hist(dane$meanfun, prob =TRUE)
lines(density(dane$meanfun))

par(mfrow=c(1,2))
d1 <- density(dane[dane$label == "male",]$meanfun)
d2 <- density(dane[dane$label == "female",]$meanfun)
hist(dane$meanfun[dane$label=="male"], prob = TRUE)
lines(d1, col = "blue")
hist(dane$meanfun[dane$label=="female"], prob = TRUE)
lines(d2, col = "red")

par(mfrow=c(1,1)) #uwaga na zakres wykresów
plot(d2, col = "blue")
lines(d1, col = "red")

#barplot
ggplot(
  data = dane, 
  aes(x = label, y = meanfun)) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = meanfun)) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = meanfun)) +
  geom_violin()

##Zmienna minfun

summary(dane$minfun)
mean(dane$minfun)
skewness(dane$minfun)
kurtosis(dane$minfun)

#histogram i gestosc
par(mfrow=c(1,1))
hist(dane$minfun, prob =TRUE)
lines(density(dane$minfun))

par(mfrow=c(1,2))
d1 <- density(dane[dane$label == "male",]$minfun)
d2 <- density(dane[dane$label == "female",]$minfun)
hist(dane$minfun[dane$label=="male"], prob = TRUE)
lines(d1, col = "blue")
hist(dane$minfun[dane$label=="female"], prob = TRUE)
lines(d2, col = "red")

par(mfrow=c(1,1)) #uwaga na zakres wykresów
plot(d2, col = "blue")
lines(d1, col = "red")

#barplot
ggplot(
  data = dane, 
  aes(x = label, y = minfun)) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = minfun)) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = minfun)) +
  geom_violin()

##Zmienna x

x=18
dane[,x]
names(dane[x])

summary(dane[,x])
mean(dane[,x])
skewness(dane[,x])
kurtosis(dane[,x])

#histogram i gestosc
par(mfrow=c(1,1))
hist(dane[,x], prob = FALSE, breaks = 20, xlim = c(0,50))
lines(density(dane[,x]))

#tip: zwraca to samo
#dane[dane$label == "male",]$minfun
#dane$minfun[dane$label=="male"]
#dane[dane$label == "male",x]

par(mfrow=c(1,2))
d1 <- density(dane[dane$label == "male",x])
d2 <- density(dane[dane$label == "female",x])
hist(dane[dane$label == "male",x], prob = TRUE)
lines(d1, col = "blue")
hist(dane[dane$label == "female",x], prob = TRUE)
lines(d2, col = "red")

par(mfrow=c(1,1)) #uwaga na zakres wykresów
plot(d2, col = "blue")
lines(d1, col = "red")

plot(d1, col = "blue")
lines(d2, col = "red")


#barplot
ggplot(
  data = dane, 
  aes(x = label, y = dane[,x])) +
  geom_bar(stat = "identity")

#boxplot
ggplot(
  data = dane, 
  aes(x = label, y = dane[,x])) +
  geom_boxplot()

#violin plot
ggplot(
  data = dane, 
  aes(x = label, y = dane[,x])) +
  geom_violin()

##Dzielenie skewness
x=7
hist(dane[,x], prob = FALSE, breaks = 20, xlim = c(0,50))
lines(density(dane[,x]))

sum(dane[,x]>0)
sum(dane[,x]>3.5)
mean(dane[,x]>3.5) #positive
mean(dane[,x]<2.5) #negative
mean(dane[,x]>2.5 & dane[,x]<3.5) #neutral

##Dzielenie kurtosis
x=8
hist(dane[,x], prob = TRUE)
lines(density(dane[,x]))
sum(dane[,x]>0)

mean(dane[,x]>3) #skoncentrowane wokól sredniej
mean(dane[,x]<3) #rozrzucone

##HIstogramy wszystkich zmiennych
for(i in 1:23)
{
  hist(dane2[,i], main = paste("Wykres ",names(dane[i])))
}
############################################################

##Regresja logistyczna


#tworzymy zmienna kategorialna 0/1 dla label: 0 - kobieta, female; 1 - mezczyzna, male

log_vec <- dane2$label == 'male'
males_ind <- which(log_vec)

dane2$label_cat[males_ind] <- 1
dane2$label_cat[-males_ind] <- 0

#Budowanie modelu

  #model 1 - tylko 1 zmienna objasniajaca
model.log <- glm(data = dane2.train, label_cat ~ meanfreq, family = 'binomial')
model.log
summary(model.log)

  #model 2 - wszystkie zmienne objasniane ciagle

model.log2 <- glm(data = dane2, label ~ meanfreq + sd_cat + median + Q25 + Q75 + IQR + 
                    skew_cat + kurt_cat + sp.ent + sfm + mode + centroid + meanfun + minfun + maxfun_cat + 
                    meandom + mindom + maxdom + dfrange + modindx, family = 'binomial')
model.log2
summary(model.log2)

  #model 3 - tylko zmienne objasniane statystycznie istotne na poziomie *** z modelu 2

model.log3 <- glm(data = dane2.train, label_cat ~ sp.ent + sfm + meanfun + minfun , 
                  family = 'binomial')
model.log3
summary(model.log3)

  #model 4 - wybrane przeze mnie zmienne ciagle i stworzone zmienne kategorialnle

model.log4 <- glm(data = dane2.train, label_cat ~  meanfreq + median + IQR + meanfun
                  + minfun + skew_cat_num + sd_cat_num
                  ,family = 'binomial')
#wyrzucam zmienne: mode, maxdom, kurt_cat_num, maxfun_cat2
model.log4
summary(model.log4)

#model 5 - zmienne z regresja stepwise

model.log4 <- glm(data = dane2, label ~ IQR + meanfun + minfun  + modindx + sd_cat +  sfm + sp.ent, family = 'binomial')


#Predykcje modelu regresji logistycznej

predict.model.log.train <- predict(model.log, newdata=dane2.train, type = "response") 
predict.model.log.test <- predict(model.log, newdata=dane2.test, type = "response")

predict.model.log2.train <- predict(model.log2, newdata=dane2.train, type = "response") 
predict.model.log2.test <- predict(model.log2, newdata=dane2.test, type = "response")

predict.model.log3.train <- predict(model.log3, newdata=dane2.train, type = "response") 
predict.model.log3.test <- predict(model.log3, newdata=dane2.test, type = "response")

predict.model.log4.train <- predict(model.log4, newdata=dane2.train, type = "response") 
predict.model.log4.test <- predict(model.log4, newdata=dane2.test, type = "response")

#optymalny punkt odciecia

library(InformationValue)
optCutOff.train <- optimalCutoff(dane2.train$label_cat, predict.model.log.train)[1] 
optCutOff.test <- optimalCutoff(dane2.test$label_cat, predict.model.log.test)[1] 

optCutOff2.train <- optimalCutoff(dane2.train$label_cat, predict.model.log2.train)[1] 
optCutOff2.test <- optimalCutoff(dane2.test$label_cat, predict.model.log2.test)[1] 

optCutOff3.train <- optimalCutoff(dane2.train$label_cat, predict.model.log3.train)[1] 
optCutOff3.test <- optimalCutoff(dane2.test$label_cat, predict.model.log3.test)[1] 

optCutOff4.train <- optimalCutoff(dane2.train$label_cat, predict.model.log4.train)[1] 
optCutOff4.test <- optimalCutoff(dane2.test$label_cat, predict.model.log4.test)[1]


#kategoryzacja predykcji 

  #model 1
predict.model.log.train_cat <- c(1:length(predict.model.log.train))

for(i in 1:length(predict.model.log.train))
{
  if (predict.model.log.train[i]>optCutOff.train)
  {
    predict.model.log.train_cat[i] <- 1
  } else {
    predict.model.log.train_cat[i] <- 0
  }
}

predict.model.log.test_cat <- c(1:length(predict.model.log.test))

for(i in 1:length(predict.model.log.test))
{
  if (predict.model.log.test[i]>optCutOff.test)
  {
    predict.model.log.test_cat[i] <- 1
  } else {
    predict.model.log.test_cat[i] <- 0
  }
}

  #model 2

predict.model.log2.train_cat <- c(1:length(predict.model.log2.train))

for(i in 1:length(predict.model.log2.train))
{
  if (predict.model.log2.train[i]>optCutOff2.train)
  {
    predict.model.log2.train_cat[i] <- 1
  } else {
    predict.model.log2.train_cat[i] <- 0
  }
}

predict.model.log2.test_cat <- c(1:length(predict.model.log2.test))

for(i in 1:length(predict.model.log2.test))
{
  if (predict.model.log2.test[i]>optCutOff2.test)
  {
    predict.model.log2.test_cat[i] <- 1
  } else {
    predict.model.log2.test_cat[i] <- 0
  }
}

  #model 3
predict.model.log3.train_cat <- c(1:length(predict.model.log3.train))

for(i in 1:length(predict.model.log3.train))
{
  if (predict.model.log3.train[i]>optCutOff3.train)
  {
    predict.model.log3.train_cat[i] <- 1
  } else {
    predict.model.log3.train_cat[i] <- 0
  }
}

predict.model.log3.test_cat <- c(1:length(predict.model.log3.test))

for(i in 1:length(predict.model.log3.test))
{
  if (predict.model.log3.test[i]>optCutOff3.test)
  {
    predict.model.log3.test_cat[i] <- 1
  } else {
    predict.model.log3.test_cat[i] <- 0
  }
}

#model 4
predict.model.log4.train_cat <- c(1:length(predict.model.log4.train))

for(i in 1:length(predict.model.log4.train))
{
  if (predict.model.log4.train[i]>optCutOff4.train)
  {
    predict.model.log4.train_cat[i] <- 1
  } else {
    predict.model.log4.train_cat[i] <- 0
  }
}

predict.model.log4.test_cat <- c(1:length(predict.model.log4.test))

for(i in 1:length(predict.model.log4.test))
{
  if (predict.model.log4.test[i]>optCutOff4.test)
  {
    predict.model.log4.test_cat[i] <- 1
  } else {
    predict.model.log4.test_cat[i] <- 0
  }
}


#Diagnostyka modelu

#reszty
reszty = residuals(model.log)
hist(reszty, prob=TRUE, ylim=c(0,1.8))
lines(density(reszty), col="red", lwd=2)

reszty2 = residuals(model.log2)
hist(reszty2, prob=TRUE, ylim=c(0,3))
lines(density(reszty2), col="red", lwd=2)

reszty3 = residuals(model.log3)
hist(reszty3, prob=TRUE, ylim=c(0,3))
lines(density(reszty3), col="red", lwd=2)

reszty4 = residuals(model.log4)
hist(reszty4, prob=TRUE, ylim=c(0,3))
lines(density(reszty4), col="red", lwd=2)

#test Shapiro-Wilk
shapiro.test(reszty)
shapiro.test(reszty2)
shapiro.test(reszty3)
shapiro.test(reszty4)

#confusion matrix
conf.matrix.log.train <- confusionMatrix(predict.model.log.train_cat, as.numeric(dane2.train$label_cat))
conf.matrix.log.test <- confusionMatrix(predict.model.log.test_cat, as.numeric(dane2.test$label_cat))

conf.matrix.log.train.prc <- matrix(1:4, nrow = 2, ncol = 2)

conf.matrix.log.train.prc[1,1] <- conf.matrix.log.train[1,1] / (conf.matrix.log.train[1,1] + conf.matrix.log.train[1,2])
conf.matrix.log.train.prc[1,2] <- conf.matrix.log.train[1,2] / (conf.matrix.log.train[1,1] + conf.matrix.log.train[1,2])
conf.matrix.log.train.prc[2,1] <- conf.matrix.log.train[2,1] / (conf.matrix.log.train[2,1] + conf.matrix.log.train[2,2])
conf.matrix.log.train.prc[2,2] <- conf.matrix.log.train[2,2] / (conf.matrix.log.train[2,1] + conf.matrix.log.train[2,2])

conf.matrix.log.train.prc <- round(conf.matrix.log.train.prc,2)

conf.matrix.log.train
conf.matrix.log.train.prc
conf.matrix.log.test

summary(dane2.train$label)

conf.matrix.log2.train <- confusionMatrix(predict.model.log2.train_cat, as.numeric(dane2.train$label_cat))
conf.matrix.log2.test <- confusionMatrix(predict.model.log2.test_cat, as.numeric(dane2.test$label_cat))

conf.matrix.log2.train
conf.matrix.log2.test

conf.matrix.log3.train <- confusionMatrix(predict.model.log3.train_cat, as.numeric(dane2.train$label_cat))
conf.matrix.log3.test <- confusionMatrix(predict.model.log3.test_cat, as.numeric(dane2.test$label_cat))

conf.matrix.log3.train
conf.matrix.log3.test

conf.matrix.log4.train <- confusionMatrix(predict.model.log4.train_cat, as.numeric(dane2.train$label_cat))
conf.matrix.log4.test <- confusionMatrix(predict.model.log4.test_cat, as.numeric(dane2.test$label_cat))

conf.matrix.log4.train
conf.matrix.log4.test

#wykres krzywej ROC

roc.model.log <- roc(as.numeric(dane2.train$label_cat), predict.model.log.train_cat)
roc.predict.model.log <- roc(as.numeric(dane2.test$label_cat),predict.model.log.test_cat)

plot(roc.model.log, col = 'blue')
plot(roc.predict.model.log, add=TRUE, col = 'red')

roc.model.log2 <- roc(as.numeric(dane2.train$label_cat), predict.model.log2.train_cat)
roc.predict.model.log2 <- roc(as.numeric(dane2.test$label_cat),predict.model.log2.test_cat)

plot(roc.model.log2, col = 'blue')
plot(roc.predict.model.log2, add=TRUE, col = 'red')

roc.model.log3 <- roc(as.numeric(dane2.train$label_cat), predict.model.log3.train_cat)
roc.predict.model.log3 <- roc(as.numeric(dane2.test$label_cat),predict.model.log3.test_cat)

plot(roc.model.log3, col = 'blue')
plot(roc.predict.model.log3, add=TRUE, col = 'red')

roc.model.log4 <- roc(as.numeric(dane2.train$label_cat), predict.model.log4.train_cat)
roc.predict.model.log4 <- roc(as.numeric(dane2.test$label_cat),predict.model.log4.test_cat)

plot(roc.model.log4, col = 'blue')
plot(roc.predict.model.log4, add=TRUE, col = 'red')

#czulosc i specyficznosc

sens.train <- sensitivity(dane2.train$label_cat,predict.model.log.train_cat )
sens.test <- sensitivity(dane2.test$label_cat, predict.model.log.test_cat)

spec.train <- specificity(dane2.train$label_cat,predict.model.log.train_cat )
spec.test <- specificity(dane2.test$label_cat, predict.model.log.test_cat)

sens.train
sens.test

spec.train
spec.test

sens.model.log2.train <- sensitivity(dane2.train$label_cat,predict.model.log2.train_cat )
sens.model.log2.test <- sensitivity(dane2.test$label_cat, predict.model.log2.test_cat)

spec.model.log2.train <- specificity(dane2.train$label_cat,predict.model.log2.train_cat )
spec.model.log2.test <- specificity(dane2.test$label_cat, predict.model.log2.test_cat)

sens.model.log2.train
sens.model.log2.test

spec.model.log2.train
spec.model.log2.test

sens.model.log3.train <- sensitivity(dane2.train$label_cat,predict.model.log3.train_cat )
sens.model.log3.test <- sensitivity(dane2.test$label_cat, predict.model.log3.test_cat)

spec.model.log3.train <- specificity(dane2.train$label_cat,predict.model.log3.train_cat )
spec.model.log3.test <- specificity(dane2.test$label_cat, predict.model.log3.test_cat)

sens.model.log3.train
sens.model.log3.test

spec.model.log3.train
spec.model.log3.test

sens.model.log4.train <- sensitivity(dane2.train$label_cat,predict.model.log4.train_cat )
sens.model.log4.test <- sensitivity(dane2.test$label_cat, predict.model.log4.test_cat)

spec.model.log4.train <- specificity(dane2.train$label_cat,predict.model.log4.train_cat )
spec.model.log4.test <- specificity(dane2.test$label_cat, predict.model.log4.test_cat)

sens.model.log4.train
sens.model.log4.test

spec.model.log4.train
spec.model.log4.test

#zgodnosc

Concordance(dane2.train$label_cat, predict.model.log.train)
Concordance(dane2.test$label_cat, predict.model.log.test)

Concordance(dane2.train$label_cat, predict.model.log2.train)
Concordance(dane2.test$label_cat, predict.model.log2.test)

Concordance(dane2.train$label_cat, predict.model.log3.train)
Concordance(dane2.test$label_cat, predict.model.log3.test)

Concordance(dane2.train$label_cat, predict.model.log4.train)
Concordance(dane2.test$label_cat, predict.model.log4.test)

############################################################

#Drzewo decyzyjne

library(rpart)
library(rpart.plot)
library(tree)

  #model 1 - wszystkie zmienne objasniane ciagle i kategorialne

model.tree <- rpart(data = dane2, label ~ meanfreq + sd_cat + median + Q25 + Q75 + IQR + 
                      skew_cat + kurt_cat + sp.ent + sfm + mode + centroid + meanfun + minfun + maxfun_cat + 
                      meandom + mindom + maxdom + dfrange + modindx, method = "class" )

printcp(model.tree)
plotcp(model.tree)
prp(model.tree)
summary(model.tree)

# plot tree
plot(model.tree, uniform=TRUE, main="Classification Tree for Chemicals")
text(model.tree, use.n=TRUE, all=TRUE, cex=.6)

  #model 2 - tylko zmienne objasniane statystycznie istotne na poziomie *** z modelu 2 log

model.tree2 <- rpart(data = dane2.train, label ~ sp.ent + sfm + meanfun + minfun,
                     method = "class" )

printcp(model.tree2)
plotcp(model.tree2)
prp(model.tree2)
summary(model.tree2)

# plot tree
plot(model.tree2, uniform=TRUE, main="Classification Tree for Chemicals")
text(model.tree2, use.n=TRUE, all=TRUE, cex=.6)

  #model 3 - wybrane przeze mnie zmienne ciagle i stworzone zmienne kategorialnle

model.tree3 <- rpart(data = dane2.train, label_cat ~  meanfreq + median + IQR + meanfun
                  + minfun + skew_cat_num + sd_cat_num
                  ,method = "anova")

printcp(model.tree3)
plotcp(model.tree3)
prp(model.tree3)
summary(model.tree3)

# plot tree
plot(model.tree3, uniform=TRUE, main="Classification Tree for Chemicals")
text(model.tree3, use.n=TRUE, all=TRUE, cex=.6)

#krzywa ROC

predict.model.tree3 <- predict(model.tree3, newdata = dane2.test, type = "vector")
roc.model.tree3 <- roc(dane2.test$label, predict.model.tree3)

plot(roc.model.tree3)


#model 4 - wybrane przeze mnie zmienne ciagle i stworzone zmienne kategorialnle

model.tree4 <- tree(data = dane2.train, label_cat ~  meanfreq + median + IQR + meanfun
                     + minfun + skew_cat_num + sd_cat_num)

plot(model.tree4)
text(model.tree4)
summary(model.tree4)

# plot tree
plot(model.tree3, uniform=TRUE, main="Classification Tree for Chemicals")
text(model.tree3, use.n=TRUE, all=TRUE, cex=.6)

#krzywa ROC

predict.model.tree3 <- predict(model.tree3, newdata = dane2.test, type = "vector")
roc.model.tree3 <- roc(dane2.test$label, predict.model.tree3)

plot(roc.model.tree3)

  #model 5 Random Forest

## randomForest
library(randomForest)
model.rf <- randomForest(data = dane2.train, label_cat ~  meanfreq + median + IQR + meanfun
                        + minfun + skew_cat_num + sd_cat_num)
print(model.rf)
importance(model.rf)
plot(model.rf)
plot( importance(model.rf), lty=2, pch=16)
lines(importance(model.rf))
imp = importance(model.rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))
for (i in seq_along(impvar)) {
  partialPlot(model.rf, dane2.train, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}

############################################################
#Zapiski

hist(dane2$sd.procent.mean.)
summary(dane2$sd.procent.mean.)

plot(dane2$sd_cat)
summary(dane2$sd_cat)
class(dane2$sd_cat)
dane2$sd_cat <- factor(dane2$sd_cat, levels = c("low", "medium", "high"))

write.csv(dane2, file = "C:/Users/Dawid/Dropbox/eSGieHa/Prezentacja i Wizualizacja Danych/Drugi semestr/voice2.csv")

