library(class)
library(e1071)
library(cluster)
library(rpart)
library(randomForest)
library(caret)

setwd("/Users/shaojun/Documents/Analysis of Multivariate Data/Project/")
load("Data/Meso.RData")
Meso <- Meso[, - 8]

# knn
tr <- trainControl(method="cv", number = 3)
meso.knn <- train(class.of.diagnosis ~ ., trControl = tr, data = Meso, method = "knn")
print(meso.knn)

# svm
error.svm <- matrix(0, 2, 2)

for (i in 1:324) {
  train <- Meso[- i, ]
  test <- Meso[i, ]

  Meso.svm <- svm(class.of.diagnosis ~ ., data = train, kernel = "radial")
  Meso.svm.pred <- predict(Meso.svm, test[, - 1], decision.values = T)
  error.svm <- error.svm + table(test$class.of.diagnosis, Meso.svm.pred)
}

1- sum(diag(error.svm)) / sum(error.svm) # 0


# random forest -----------------------------------------------------------
meso.rf <- randomForest(class.of.diagnosis ~ ., data = Meso, ntree = 10000, mtry = 5, importance = TRUE)
print(meso.rf)

tuneRF(Meso[, - 1], Meso[, 1], StepFactor = 1)

pdf("Images/varimp.pdf", width = 12)
varImpPlot(meso.rf, main = "")
dev.off()


# mds ---------------------------------------------------------------------
meso.diss <- daisy(Meso)
meso.mds <- cmdscale(meso.diss, k = 2)
fitted.diss <- daisy(meso.mds)
sum((meso.diss - fitted.diss) ^ 2) / sum(fitted.diss)

col <- rep(0, nrow(Meso))
col[Meso$class.of.diagnosis == 1] <- "blue"
col[Meso$class.of.diagnosis == 2] <- "red"

pdf("Images/mds.pdf")
plot(meso.mds[, 1], meso.mds[, 2], col = col, xlab = "MDS-1st dimension", ylab = "MDS-2nd dimension")
dev.off()

