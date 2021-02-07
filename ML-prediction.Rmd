---
title: "Prediction Assignment"
author: "Alexandre Moura"
date: "2/1/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Summary

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible
to collect a large amount of data about personal activity relatively - 
inexpensively. These type of devices are part of the quantified self movement – 
a group of enthusiasts who take measurements about themselves regularly to 
improve their health, to find patterns in their behavior, or because they are
tech geeks. One thing that people regularly do is quantify how much of a 
particular activity they do, but they rarely quantify how well they do it. In 
this project, your goal will be to use data from accelerometers on the belt, 
forearm, arm, and dumbell of 6 participants. They were asked to perform barbell 
lifts correctly and incorrectly in 5 different ways

## Data

The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

## Objective

The goal of your project is to predict the manner in which they did the exercise. 
This is the "classe" variable in the training set. You may use any of the other
variables to predict with. You should create a report describing how you built 
your model, how you used cross validation, what you think the expected out of 
sample error is, and why you made the choices you did. You will also use your 
prediction model to predict 20 different test cases.

### Running the Model

```{r}
library(caret)
library(rpart)
library(e1071)
library(randomForest)
set.seed(2807)
```

### Downloading the data

```{r}
url.treino = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

url.teste = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

file.treino = read.csv (url.treino, na.strings =  c("", "NA", "#DIV/0!"))

file.teste = read.csv (url.teste, na.strings = c("", "NA", "#DIV/0!"))
```

### Preprocessing

Some information from our databases is here.
```{r}
dim(file.treino)
table(file.treino$classe)

dim(file.teste)
```

Here we created training database and cross validation database.
```{r}
trainset = createDataPartition(file.treino$classe, p = 0.75, list = F)
treino = file.treino[trainset,]
validacao = file.treino[-trainset,]
```

### Dataset clean up

NA's values and informative fields are deleted to improve our model accuracy.

```{r}
#delete near zero columns
nzvcol = nearZeroVar(treino)
treino = treino[,-nzvcol]

#delete NA and descriptive columns
cnt = sapply(treino, function(x) {
    sum(!(is.na(x) | x == ""))
})

nullcol = names(cnt[cnt < 0.6 * length(treino$classe)])
description = c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
deletecols = c(description, nullcol)
treino = treino[, !names(treino) %in% deletecols]
```

### Model training

Here the model is trained and showed its accuracy.

```{r}
suppressMessages(library(randomForest))
rf = randomForest(classe ~ ., data = treino, importance = TRUE, ntrees = 10)
rf

#Test accuracy
ptreino = predict(rf, treino)
print(confusionMatrix(ptreino, treino$classe))
```

### Model Validation

Here it is showed validation accuracy out of sample. Accuracy is 99,3%.
```{r}
#Validation accuracy
pvalidacao = predict(rf, validacao)
print(confusionMatrix(pvalidacao, validacao$classe))
```

### Model prediction

The results from our model prediction is showed below.

```{r}
pteste = predict(rf, file.teste)
pteste
```