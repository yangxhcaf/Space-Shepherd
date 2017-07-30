

# =========================================
# GovHack 2017
# 
# Project:
# Space Shepherd: Satellite remote sensing of sheep distributions in 2020 and 2030
# 
# Team Name:
# The Next 100 Years
#
#==========================================



#==========================================
# code for predicting future distributions of
# sheep populations and number of agricultural business
#==========================================



#==========================================
# read sheep data in 2010
# read temperature and precipitation data in 2010, 2020 and 2030
#==========================================


setwd("F:\\GovHack2017\\Sheep\\R")

dt1 <- read.table("sheep rcp3 2010.txt")
dt2 <- read.table("sheep rcp3 2020.txt")
dt3 <- read.table("sheep rcp3 2030.txt")


# data cleaning and pre-processing
hist(dt1$nosheep)
dt1$lognosheep <- log(dt1$nosheep+1)
hist(dt1$lognosheep)


# correlation analysis for 
# the relationship between sheep population and climate senarios
library(PerformanceAnalytics)
chart.Correlation(dt1[,c(10,4:9)])


commonTheme = list(labs(color="Density",fill="Density",
                        x="variable 1",
                        y="variable 2"),
                   theme_bw(),
                   theme(legend.position=c(0,1),
                         legend.justification=c(0,1)))
# plot the relationship between sheep population and annual mean temperature
ggplot(data=dt1,aes(T1,log(nosheep+1))) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="blue",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point() + commonTheme
# plot the relationship between sheep population and annual precipitation
ggplot(data=dt1,aes(log(P1+1),log(nosheep+1))) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="blue",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point() + commonTheme


#==========================================
# Exploring the relationships between sheep population and its potential variables
# by the comparison study of three methods:
#
# 1. linear regression
# 2. Generalized additive model: a nonlinear and nonparametric model
# 3. Random Forest: a machine learning method
#
# select the model with the best performance
#==========================================

# 1. linear regression
model1 <- lm(lognosheep ~  T1 + T2 + T4 + P1 + P2 + P3,
          data = dt1)
summary(model1) # show results


# 2. Generalized additive model
library(mgcv)
model2 <- gam(lognosheep ~  s(T1) +  s(T2) + s(T4)+s(P1) + s(P2)  + s(P3),
          data = dt1, method = "GCV.Cp")

plot(model2,pages=1,scheme=1) 
summary(model2)


# 3. Random Forest
library(randomForest)
model3 <- randomForest(lognosheep ~ T1 + T2 + T4 + P1 + P2 + P3,
                         data=dt1, 
                         importance=TRUE, 
                         ntree=2000)
pred1 <- model3$predicted
cor(dt1$lognosheep, pred1)^2

summary(model3)
plot(model3)
varImpPlot(model3, sort = T, n.var = 1)
var.imp <- data.frame(importance(model3, type=2))
var.imp


#==========================================
# Machine learning method (Random Forest) performs best
# so we use random forest method for the prediction of
# the distributions of sheep populations in 2020 and 2030
#==========================================

summary(dt1$nosheep)

# dt2: climate senarios in 2020
# dt3: climate senarios in 2030
pred.2020.lognosheep <- predict(model3, dt2, allow.new.levels=T)
dt2$nosheep <- exp(pred.2020.lognosheep) - 1
summary(dt2$nosheep)

pred.2030.lognosheep <- predict(model3, dt3, allow.new.levels=T)
dt3$nosheep <- exp(pred.2030.lognosheep) - 1
summary(dt3$nosheep)

nosheep <- cbind(dt1$nosheep,dt2$nosheep,dt3$nosheep)
write.table(nosheep,file="nosheep.txt")

nosheep <- nosheep[-which(dt1$nosheep<=1),]
nosheep <- as.data.frame(nosheep)
names(nosheep) <- c("2010","2020","2030")

# plot future sheep
library(reshape)
nosheep2 <- melt(nosheep)
median(nosheep$`2010`)
median(nosheep$`2020`)
median(nosheep$`2030`)
p <- ggplot(nosheep2, aes(x=variable, y=value)) + 
  geom_violin(trim=FALSE)
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_boxplot(width=0.1, fill="white")+
  scale_y_log10()+ 
  theme_bw()

# plot future climate
temp <- cbind(dt1$T1, dt2$T1, dt3$T1)
prec <- cbind(dt1$P1, dt2$P1, dt3$P1)
temp <- as.data.frame(temp)
names(temp) <- c("2010","2020","2030")
prec <- as.data.frame(prec)
names(prec) <- c("2010","2020","2030")
median(temp$`2010`)
median(temp$`2020`)
median(temp$`2030`)
median(prec$`2010`)
median(prec$`2020`)
median(prec$`2030`)
temp <- melt(temp)
prec <- melt(prec)

p <- ggplot(temp, aes(x=variable, y=value)) + 
  geom_violin(trim=FALSE)
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_boxplot(width=0.1, fill="white") + 
  theme_bw()

p <- ggplot(prec, aes(x=variable, y=value)) + 
  geom_violin(trim=FALSE)
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_boxplot(width=0.1, fill="white")+
  scale_y_log10()+ 
  theme_bw()


# =========================================
# end
#
# GovHack 2017
# 
# Project:
# Space Shepherd: Satellite remote sensing of sheep distributions in 2020 and 2030
# 
# Team Name:
# The Next 100 Years
#
#==========================================


