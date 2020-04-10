install.packages("ISLR")
library(ISLR)
View(Smarket)
dim(Smarket)
summary(Smarket)
str(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
plot(Today)

# LOGISTIC REGRESSION
glm1 <- glm(Direction~.,data = Smarket[,-1],family = binomial)
summary(glm1)
coef(glm1)
summary(glm1)$coef
glm_predict <- predict(glm1,type = "response")
glm_predict[1:10]
contrasts(Direction)
glm.pred <- rep("Down",1250)
glm.pred[glm_predict>.5] <- "Up"
head(glm.pred)
table(glm.pred,Direction)
mean(glm.pred==Direction)
table(Direction)
plot(glm1)

# LINEAR DISCRIMINANT ANALYSIS
library(MASS)
ldafit <- lda(Direction~.,data = Smarket[,-1])
ldafit
plot(ldafit)
lda_pred <- predict(ldafit,Smarket[-1])
table(lda_pred$class,Direction)
mean(lda_pred$class==Direction)
sum(lda_pred$posterior[,1]>=.5)
sum(lda_pred$posterior[,1]<.5)
lda_pred$posterior[1:10,1]
lda_pred$class[1:10]
sum(lda_pred$posterior[,1]>=.75)
sum(lda_pred$posterior[,1]<.75)

# QUADRATIC DISCRIMINANT ANALYSIS
qdafit <- qda(Direction~.,data = Smarket[-1])
qdafit
qda_pred <- predict(qdafit,Smarket[-1])
table(qda_pred$class,Direction)
mean(qda_pred$class==Direction)


install.packages("klaR")
library(klaR)
partimat(Direction~.,data = Smarket[-1],method="qda",plot.matrix=TRUE,col.correct='green',col.wrong='red')
partimat(Direction~.,data = Smarket[-1],method="lda",plot.matrix=TRUE,col.correct='green',col.wrong='red')