#install.packages('maptools');
#install.packages('rgdal');
#install.packages('PBSmapping')
library(maptools)
library(sp)
library(rgdal)
library(PBSmapping)

# Učitavnja SHAPEFILE-a Bioclim --------------------------------------------------

data.shapes = readOGR("./bioclim/33.shp", "33")

p4s.latlon = CRS(proj4string(data.shapes))

data.forPlot = spTransform(data.shapes, p4s.latlon);

# Učitavnja SHAPEFILE-a Dem --------------------------------------------------

data.shapesDem = readOGR("./dem/30.shp", "30")

# Učitavanje CSV-a -------------------------------------------------------

nalOp = read.csv("./nalOpMin.csv", header = TRUE, sep = ";", quote = "\"", stringsAsFactors = F)
head(nalOp, 1)

nalLit = read.csv("./nalLitMin.csv", header = TRUE, sep = ";", quote = "\"", stringsAsFactors = F)
head(nalLit, 1)

# Izdvanjanje koordinatnih matrica NalazisteOp ---------------------------------------

onlyXY = nalOp[c('HTRS96_X', 'HTRS96_Y')]

matKoor1 = matrix(transform(onlyXY, HTRS96_X = as.numeric(HTRS96_X))[,1], nrow = nrow(onlyXY), ncol = 1)
matKoor2 = matrix(transform(onlyXY, HTRS96_Y = as.numeric(HTRS96_Y))[,2], nrow = nrow(onlyXY), ncol = 1)

matrica = cbind(matKoor1, matKoor2)
mate = matrica[10270:17534,]
mate2 = matrica[6870:10260,]
mate3 = matrica[1:6860,]

koordinate1 = SpatialPoints(mate, proj4string = p4s.latlon)
koordinate2 = SpatialPoints(mate2, proj4string = p4s.latlon)
koordiante3 = SpatialPoints(mate3, proj4string = p4s.latlon)

koordinate = rbind(koordinate1, koordinate2, koordiante3)

# Izdvanjanje koordinatnih matrica NalazisteLit ---------------------------------------

onlyXYLit = nalLit[c('HTRS96_X', 'HTRS96_Y')]
onlyXY100Lit = head(onlyXYLit, 100) # 100 podataka samo za test
str(onlyXY100Lit)

matKoor1Lit = matrix(transform(onlyXYLit, HTRS96_X = as.double(HTRS96_X))[,1], nrow = nrow(onlyXYLit), ncol = 1)
matKoor2Lit = matrix(transform(onlyXYLit, HTRS96_Y = as.double(HTRS96_Y))[,2], nrow = nrow(onlyXYLit), ncol = 1)

matricaLit = cbind(matKoor1Lit, matKoor2Lit)

koordinateLit = SpatialPoints(matricaLit, proj4string = p4s.latlon)
koordinateLit

# Dohvaćanje presječnih podataka za Op i Lit -----------------------------------------

data.matrixOpBioclim = over(koordinate, data.shapes);

data.matrixOpDem = over(koordinate, data.shapesDem);

data.matrixLitBioclim = over(koordinateLit, data.shapes)

data.matrixLitDem = over(koordinateLit, data.shapesDem)

# Stvaranje spojene matrice za Op ------------------------------------------------------------

data.opFinal = nalOp[c('OznKoord', 'NazKlase')]
data.opFinal1 = data.opFinal[10270:17534,]
data.opFinal2 = data.opFinal[6870:10260,]
data.opFinal3 = data.opFinal[1:6860,]
data.opFinalPravi = rbind(data.opFinal1,data.opFinal2,data.opFinal3)

data.matrixOpBioclimFinal = data.matrixOpBioclim[c('Bio_1', 'Bio_2', 'Bio_3', 'Bio_4', 'Bio_5', 'Bio_6', 'Bio_7', 'Bio_8', 'Bio_9', 'Bio_10','Bio_11', 'Bio_12', 'Bio_13', 'Bio_14', 'Bio_15', 'Bio_16', 'Bio_17', 'Bio_18', 'Bio_19')]
data.matrixOpDemFinal = data.matrixOpDem[c('EKSPOZICIJ', 'NDM__M_', 'NAGIB_TERE')]

result.op = cbind(data.opFinalPravi, data.matrixOpBioclimFinal, data.matrixOpDemFinal)

# Stvaranje spojene matrice za Lit ------------------------------------------------------------

data.litFinal = nalLit[c('OznKoord', 'NazKlase')]

data.matrixLitBioclimFinal = data.matrixLitBioclim[c('Bio_1', 'Bio_2', 'Bio_3', 'Bio_4', 'Bio_5', 'Bio_6', 'Bio_7', 'Bio_8', 'Bio_9', 'Bio_10','Bio_11', 'Bio_12', 'Bio_13', 'Bio_14', 'Bio_15', 'Bio_16', 'Bio_17', 'Bio_18', 'Bio_19')]
data.matrixLitDemFinal = data.matrixLitDem[c('EKSPOZICIJ', 'NDM__M_', 'NAGIB_TERE')]

result.lit = cbind(data.litFinal, data.matrixLitBioclimFinal, data.matrixLitDemFinal)


# Stvaranje CSV rezultata za prikaz -----------------------------------------------------------

write.csv2(result.op, "resultOpazanje2.csv", quote = F)

write.csv2(result.lit, "resultLiteratura2.csv", quote = F)

# Zajednicki CSV -----------------------------------------------------------------------------
result.final = rbind(result.op, result.lit)

rows_to_keep = apply(result.final[c(3:24)], 1, function(row) !any(row == -9999))
result.final = result.final[rows_to_keep,]

result.final = na.omit(result.final)

write.csv2(result.final, "resultFinal2.csv", quote = F)



# Stvaranje podataka za pojedinu biljku ------------------------------------------------------

rows_ambrosia = apply(result.final[c(1:24)], 1, function(row) any(row == "Ambrosia artemisiifolia L."))
rows_fagus = apply(result.final[c(1:24)], 1, function(row) any(row == "Fagus sylvatica L."))
rows_carpinus = apply(result.final[c(1:24)], 1, function(row) any(row == "Carpinus betulus L."))
rows_quercus = apply(result.final[c(1:24)], 1, function(row) any(row == "Quercus petraea (Mattuschka) Liebl."))
rows_fraxinus = apply(result.final[c(1:24)], 1, function(row) any(row == "Fraxinus ornus L."))

ambrosia = result.final[rows_ambrosia,]
fagus = result.final[rows_fagus,]
carpinus = result.final[rows_carpinus,]
quercus = result.final[rows_quercus,]
fraxinus = result.final[rows_fraxinus,]



# Create final AMBROSIA data

ambrosia_zeros = fagus[sample(1:nrow(fagus), 500, replace = FALSE),]
y = rep(0, 500);

ambrosia_zeros = cbind(ambrosia_zeros, y)

y = rep(1, nrow(ambrosia))
ambrosia2 = cbind(ambrosia, y)

ambrosia_full = rbind(ambrosia2, ambrosia_zeros)

maxAmb = apply(ambrosia_full[3:24], 2, max)
meanAmb = apply(ambrosia_full[3:24], 2, mean)

#ambrosia_norm = cbind(scale(ambrosia_full[3:24]), ambrosia_full[25])
#ambrosia_norm
meanMatrix = do.call(rbind, replicate(nrow(ambrosia_full), meanAmb, simplify=FALSE)) 
ambrosia_full[3:24] - meanMatrix
ambrosia_norm = (ambrosia_full[3:24]) / rep(maxAmb, each = nrow(ambrosia_full))
ambrosia_norm = cbind(ambrosia_norm, ambrosia_full[25])
# Create final CARPINUS data

carpinus_zeros = fagus[sample(1:nrow(fagus), 1000, replace = FALSE),]
y = rep(0, 1000);

carpinus_zeros = cbind(carpinus_zeros, y)

y = rep(1, nrow(carpinus))
carpinus2 = cbind(carpinus, y)

carpinus_full = rbind(carpinus2, carpinus_zeros)

maxCar = apply(carpinus_full[3:24], 2, max)
meanCar = apply(carpinus_full[3:24], 2, mean)

meanMatrixC = do.call(rbind, replicate(nrow(carpinus_full), meanCar, simplify=FALSE)) 

carp_norm = (carpinus_full[3:24]) / rep(maxCar, each = nrow(carpinus_full))
carp_norm = cbind(carp_norm, carpinus_full[25])


# Create final FRAXINUS data

fraxinus_zeros = fagus[sample(1:nrow(fagus), 1000, replace = FALSE),]
y = rep(0, 1000);

fraxinus_zeros = cbind(fraxinus_zeros, y)

y = rep(1, nrow(fraxinus))
fraxinus2 = cbind(fraxinus, y)

fraxinus_full = rbind(fraxinus2, fraxinus_zeros)

maxFrx = apply(fraxinus_full[3:24], 2, max)
meanFrx = apply(fraxinus_full[3:24], 2, mean)

meanMatrixFx = do.call(rbind, replicate(nrow(fraxinus_full), meanFrx, simplify=FALSE)) 

frax_norm = (fraxinus_full[3:24]) / rep(maxFrx, each = nrow(fraxinus_full))
frax_norm = cbind(frax_norm, fraxinus_full[25])



# Create final FAGUS data

fagus_zeros = fraxinus[sample(1:nrow(fraxinus), 1500, replace = FALSE),]
y = rep(0, 1500);

fagus_zeros = cbind(fagus_zeros, y)

y = rep(1, nrow(fagus))
fagus2 = cbind(fagus, y)

fagus_full = rbind(fagus2, fagus_zeros)

maxFag = apply(fagus_full[3:24], 2, max)
meanFag = apply(fagus_full[3:24], 2, mean)

meanMatrixFg = do.call(rbind, replicate(nrow(fagus_full), meanFag, simplify=FALSE)) 

fag_norm = (fagus_full[3:24]) / rep(maxFag, each = nrow(fagus_full))
fag_norm = cbind(fag_norm, fagus_full[25])



# Create final quercus data

quercus_zeros = fagus[sample(1:nrow(fagus), 1000, replace = FALSE),]
y = rep(0, 1000);

quercus_zeros = cbind(quercus_zeros, y)

y = rep(1, nrow(quercus))
quercus2 = cbind(quercus, y)

quercus_full = rbind(quercus2, quercus_zeros)

maxQue = apply(quercus_full[3:24], 2, max)
meanQue = apply(quercus_full[3:24], 2, mean)

meanMatrixFx = do.call(rbind, replicate(nrow(quercus_full), meanQue, simplify=FALSE)) 

que_norm = (quercus_full[3:24]) / rep(maxQue, each = nrow(quercus_full))
que_norm = cbind(que_norm, quercus_full[25])

# Logistic Regression constants

alpha <- .1

gradient <- function(X, y, theta, alpha, iterations){
  m = nrow(X)
  length(X * theta )
  for(i in 1:iterations){
    temp = (t(X %*% theta - y) %*% X / m)
    theta = theta - alpha * t(temp)
  }
  
  return(theta)
}

# Train AMBROSIA

smp_size = floor(0.75 * nrow(ambrosia_norm))

set.seed(123)
train_indices = sample(seq_len(nrow(ambrosia_norm)), size = smp_size)

train.ambrosia = ambrosia_norm[train_indices,]
test.ambrosia = ambrosia_norm[-train_indices,]

y_amb = data.matrix(train.ambrosia[c("y")])
X_amb = data.matrix(train.ambrosia[, -which(names(train.ambrosia) %in% c("y"))])

theta_amb_initial <- matrix(rep(0, ncol(X_amb)), nrow = ncol(X_amb), ncol = 1)

#theta = gradient(X_amb, y_amb, theta_amb_initial, alpha, 1500)

m = nrow(X_amb)

for(i in 1:5000){
  temp = (t(X_amb %*% theta_amb_initial - y_amb) %*% X_amb / m)
  theta_amb_initial = theta_amb_initial - alpha * t(temp)
}

X_amb_test = data.matrix(test.ambrosia[, -which(names(test.ambrosia) %in% c("y"))])

results_amb = 1 / (1 + exp(-1 * X_amb_test %*% theta_amb_initial))
results_amb

temp = rep(0, nrow(results_amb))
for(j in 1:nrow(results_amb)){
  if(results_amb[j] > 0.63){
    temp[j] = 1
  }
}
temp

results_y_amb = test.ambrosia[, which(names(test.ambrosia) %in% c("y"))]
results_y_amb

temp == y_amb

#install.packages('e1071')
library(e1071)
X_amb
svm.model <- svm(y_amb ~ . , data = X_amb, cost = 100, gamma = 0.1, scale = TRUE, type="C-classification")
pred <- predict(svm.model, X_amb_test)

pred



## ANN

#install.packages('neuralnet')
#library("neuralnet")

#ann.model <- neuralnet(y_amb~Bio_1+Bio_2+Bio_3+Bio_4+Bio_5+Bio_6+Bio_7+Bio_8+Bio_9+Bio_10+Bio_11+Bio_12+Bio_13+Bio_14+Bio_15+Bio_16+Bio_17+Bio_18+Bio_19+EKSPOZICIJ+NDM__M_+NAGIB_TERE,X_amb, hidden=2, threshold=0.01)

#ann.result <- compute(ann.model, X_amb_test)
#str(ann.result$neurons)

#ann.pred <- prediction(ann.model, list.glm = NULL)

#install.packages('nnet')
library(nnet)

seedsANN = nnet(y_amb~., X_amb, size=23, rang = 0.5, maxit = 100)

predict(seedsANN, X_amb_test, type="raw")



### MAXENT

install.packages('maxent')
library(maxent)

mxent.model <- maxent(X_amb, y_amb)
maxent.result <- predict.maxent(mxent.model, X_amb_test)
maxent.result



#### DECISION TREES

install.packages('party')
library(party)

dataTree = as.factor(cbind(y_amb_df,X_amb))
TreeModel = ctree('y~Bio_1+Bio_2+Bio_3+Bio_4+Bio_5+Bio_6+Bio_7+Bio_8+Bio_9+Bio_10+Bio_11+Bio_12+Bio_13+Bio_14+Bio_15+Bio_16+Bio_17+Bio_18+Bio_19+EKSPOZICIJ+NDM__M_+NAGIB_TERE', dataTree)






# Train CAR

smp_size = floor(0.75 * nrow(carp_norm))

set.seed(123)
train_indices = sample(seq_len(nrow(carp_norm)), size = smp_size)

train.carp = carp_norm[train_indices,]
test.carp = carp_norm[-train_indices,]

y_car = data.matrix(train.carp[c("y")])
X_car = data.matrix(train.carp[, -which(names(train.carp) %in% c("y"))])

theta_car_initial <- matrix(rep(0, ncol(X_car)), nrow = ncol(X_car), ncol = 1)

#theta = gradient(X_amb, y_amb, theta_amb_initial, alpha, 1500)

mc = nrow(X_car)

for(i in 1:5000){
  temp = (t(X_car %*% theta_car_initial - y_car) %*% X_car / mc)
  theta_car_initial = theta_car_initial - alpha * t(temp)
}

X_car_test = data.matrix(test.carp[, -which(names(test.carp) %in% c("y"))])

results_car = 1 / (1 + exp(-1 * X_car_test %*% theta_car_initial))
results_car
nrow(results_car)
temp = rep(0, nrow(results_car))
for(j in 1:nrow(results_car)){
  if(results_car[j] > 0.63){
    temp[j] = 1
  }
}
temp

results_y_car = test.carp[, which(names(test.carp) %in% c("y"))]
results_y_car

sum(temp == results_y_car)

#install.packages('e1071')
library(e1071)
X_car
svm.modelC <- svm(y_car ~ . , data = X_car, cost = 100, gamma = 0.1, scale = TRUE, type="C-classification")
predC <- predict(svm.modelC, X_car_test)

predC



## ANN

#install.packages('neuralnet')
#library("neuralnet")

#ann.model <- neuralnet(y_amb~Bio_1+Bio_2+Bio_3+Bio_4+Bio_5+Bio_6+Bio_7+Bio_8+Bio_9+Bio_10+Bio_11+Bio_12+Bio_13+Bio_14+Bio_15+Bio_16+Bio_17+Bio_18+Bio_19+EKSPOZICIJ+NDM__M_+NAGIB_TERE,X_amb, hidden=2, threshold=0.01)

#ann.result <- compute(ann.model, X_amb_test)
#str(ann.result$neurons)

#ann.pred <- prediction(ann.model, list.glm = NULL)

#install.packages('nnet')
library(nnet)

seedsANNC = nnet(y_car~., X_car, size=23, rang = 5, maxit = 1000)

jok = round(predict(seedsANNC, X_car_test, type="raw"))
sum(jok == results_y_car)

### MAXENT

install.packages('maxent')
library(maxent)

mxent.modelC <- maxent(X_car, y_car)
maxent.resultC <- predict.maxent(mxent.modelC, X_car_test)
maxent.resultC
sum(maxent.resultC == results_y_car)







# Train FRX

smp_size = floor(0.75 * nrow(frax_norm))

set.seed(123)
train_indices = sample(seq_len(nrow(frax_norm)), size = smp_size)

train.frax = frax_norm[train_indices,]
test.frax = frax_norm[-train_indices,]

y_frx = data.matrix(train.frax[c("y")])
X_frx = data.matrix(train.frax[, -which(names(train.frax) %in% c("y"))])

theta_frx_initial <- matrix(rep(0, ncol(X_frx)), nrow = ncol(X_frx), ncol = 1)

#theta = gradient(X_amb, y_amb, theta_amb_initial, alpha, 1500)

mc = nrow(X_frx)

for(i in 1:5000){
  temp = (t(X_frx %*% theta_frx_initial - y_frx) %*% X_frx / mc)
  theta_frx_initial = theta_frx_initial - alpha * t(temp)
}

X_frx_test = data.matrix(test.frax[, -which(names(test.frax) %in% c("y"))])

results_frx = 1 / (1 + exp(-1 * X_frx_test %*% theta_frx_initial))
results_frx
nrow(results_frx)
temp = rep(0, nrow(results_frx))
for(j in 1:nrow(results_frx)){
  if(results_frx[j] > 0.63){
    temp[j] = 1
  }
}
temp

results_y_frx = test.frax[, which(names(test.frax) %in% c("y"))]
results_y_frx

sum(temp == results_y_frx)

#install.packages('e1071')
library(e1071)
X_frx
svm.modelFx <- svm(y_frx ~ . , data = X_frx, cost = 1000, gamma = 0.01, scale = TRUE, type="C-classification")
predC <- predict(svm.modelC, X_frx_test)

sum(predC == results_y_frx)



## ANN

#install.packages('neuralnet')
#library("neuralnet")

#ann.model <- neuralnet(y_amb~Bio_1+Bio_2+Bio_3+Bio_4+Bio_5+Bio_6+Bio_7+Bio_8+Bio_9+Bio_10+Bio_11+Bio_12+Bio_13+Bio_14+Bio_15+Bio_16+Bio_17+Bio_18+Bio_19+EKSPOZICIJ+NDM__M_+NAGIB_TERE,X_amb, hidden=2, threshold=0.01)

#ann.result <- compute(ann.model, X_amb_test)
#str(ann.result$neurons)

#ann.pred <- prediction(ann.model, list.glm = NULL)

#install.packages('nnet')
library(nnet)

seedsANNC = nnet(y_frx~., X_frx, size=23, rang = 5, maxit = 1000)

jok = round(predict(seedsANNC, X_frx_test, type="raw"))
sum(jok == results_y_frx)

### MAXENT

install.packages('maxent')
library(maxent)

mxent.modelC <- maxent(X_frx, y_frx)
maxent.resultC <- predict.maxent(mxent.modelC, X_frx_test)
maxent.resultC
sum(maxent.resultC == results_y_frx)







# Train FRX

smp_size = floor(0.75 * nrow(fag_norm))

set.seed(123)
train_indices = sample(seq_len(nrow(fag_norm)), size = smp_size)

train.fag = fag_norm[train_indices,]
test.fag = fag_norm[-train_indices,]

y_fag = data.matrix(train.fag[c("y")])
X_fag = data.matrix(train.fag[, -which(names(train.fag) %in% c("y"))])

theta_fag_initial <- matrix(rep(0, ncol(X_fag)), nrow = ncol(X_fag), ncol = 1)

#theta = gradient(X_amb, y_amb, theta_amb_initial, alpha, 1500)

mc = nrow(X_fag)

for(i in 1:5000){
  temp = (t(X_fag %*% theta_fag_initial - y_fag) %*% X_fag / mc)
  theta_fag_initial = theta_fag_initial - alpha * t(temp)
}

X_fag_test = data.matrix(test.fag[, -which(names(test.fag) %in% c("y"))])

results_fag = 1 / (1 + exp(-1 * X_fag_test %*% theta_fag_initial))
results_fag
nrow(results_fag)
temp = rep(0, nrow(results_fag))
for(j in 1:nrow(results_fag)){
  if(results_fag[j] > 0.63){
    temp[j] = 1
  }
}
temp

results_y_fag = test.fag[, which(names(test.fag) %in% c("y"))]
results_y_fag

sum(temp == results_y_fag)

#install.packages('e1071')
library(e1071)
X_fag
svm.modelFg <- svm(y_fag ~ . , data = X_fag, cost = 100, gamma = 0.1, scale = TRUE, type="C-classification")
predC <- predict(svm.modelFg, X_fag_test)

sum(predC == results_y_fag)



## ANN

#install.packages('neuralnet')
#library("neuralnet")

#ann.model <- neuralnet(y_amb~Bio_1+Bio_2+Bio_3+Bio_4+Bio_5+Bio_6+Bio_7+Bio_8+Bio_9+Bio_10+Bio_11+Bio_12+Bio_13+Bio_14+Bio_15+Bio_16+Bio_17+Bio_18+Bio_19+EKSPOZICIJ+NDM__M_+NAGIB_TERE,X_amb, hidden=2, threshold=0.01)

#ann.result <- compute(ann.model, X_amb_test)
#str(ann.result$neurons)

#ann.pred <- prediction(ann.model, list.glm = NULL)

#install.packages('nnet')
library(nnet)

seedsANNC = nnet(y_fag~., X_fag, size=23, rang = 5, maxit = 1000)

jok = round(predict(seedsANNC, X_fag_test, type="raw"))
sum(jok == results_y_fag)

### MAXENT

install.packages('maxent')
library(maxent)

mxent.modelC <- maxent(X_fag, y_fag)
maxent.resultC <- predict.maxent(mxent.modelC, X_fag_test)
maxent.resultC
sum(maxent.resultC == results_y_fag)







# Train FRX

smp_size = floor(0.75 * nrow(que_norm))

set.seed(123)
train_indices = sample(seq_len(nrow(que_norm)), size = smp_size)

train.q = que_norm[train_indices,]
test.q = que_norm[-train_indices,]

y_q = data.matrix(train.q[c("y")])
X_q = data.matrix(train.q[, -which(names(train.q) %in% c("y"))])

theta_q_initial <- matrix(rep(0, ncol(X_q)), nrow = ncol(X_q), ncol = 1)

#theta = gradient(X_amb, y_amb, theta_amb_initial, alpha, 1500)

mc = nrow(X_q)

for(i in 1:5000){
  temp = (t(X_q %*% theta_q_initial - y_q) %*% X_q / mc)
  theta_q_initial = theta_q_initial - alpha * t(temp)
}

X_q_test = data.matrix(test.q[, -which(names(test.q) %in% c("y"))])

results_q = 1 / (1 + exp(-1 * X_q_test %*% theta_q_initial))
results_q
nrow(results_q)
temp = rep(0, nrow(results_q))
for(j in 1:nrow(results_q)){
  if(results_q[j] > 0.63){
    temp[j] = 1
  }
}
temp

results_y_q = test.q[, which(names(test.q) %in% c("y"))]
results_y_q

sum(temp == results_y_q)

#install.packages('e1071')
library(e1071)
X_q
svm.modelq <- svm(y_q ~ . , data = X_q, cost = 100, gamma = 0.1, scale = TRUE, type="C-classification")
predC <- predict(svm.modelq, X_q_test)

sum(predC == results_y_q)



## ANN

#install.packages('neuralnet')
#library("neuralnet")

#ann.model <- neuralnet(y_amb~Bio_1+Bio_2+Bio_3+Bio_4+Bio_5+Bio_6+Bio_7+Bio_8+Bio_9+Bio_10+Bio_11+Bio_12+Bio_13+Bio_14+Bio_15+Bio_16+Bio_17+Bio_18+Bio_19+EKSPOZICIJ+NDM__M_+NAGIB_TERE,X_amb, hidden=2, threshold=0.01)

#ann.result <- compute(ann.model, X_amb_test)
#str(ann.result$neurons)

#ann.pred <- prediction(ann.model, list.glm = NULL)

#install.packages('nnet')
library(nnet)

seedsANNC = nnet(y_q~., X_q, size=23, rang = 5, maxit = 1000)

jok = round(predict(seedsANNC, X_q_test, type="raw"))
sum(jok == results_y_q)

### MAXENT

install.packages('maxent')
library(maxent)

mxent.modelC <- maxent(X_q, y_q)
maxent.resultC <- predict.maxent(mxent.modelC, X_q_test)
maxent.resultC
sum(maxent.resultC == results_y_q)
