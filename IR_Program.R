library(mlbench)
library(ggplot2)
library(e1071)
library(psych)
library(plyr)
library(dplyr)
library(nnet)
library(stringr)
library(reshape)

changementproba <- function(p){
  length <- nrow(p)
  for(i in 1:length) {
    if(p[i,1]>=p[i,2]){
      p[i,1] <- 1
      p[i,2] <- 0
    }
    else
    {
      p[i,1] <- 0
      p[i,2] <- 1
    }
  }
  return (p)
}
#fonction affichage matrice de confusion, taux d'erreur, rappel, precision
evaluation.prediction <- function(yobs,ypred,posLabel){
  #matrice de confusion
  mc <- table(yobs,ypred)
  print("Matrice de confusion")
  print(mc)
  #taux d'erreur
  err <- 1-sum(diag(mc))/sum(mc)
  print(paste("Taux d'erreur =", err))
}

#fonction de pre traitement de la data pour naive bayes
pretraitementbayes <- function(newdata){
  
  length <- nrow(newdata)
  
  for(i in 1:length) {
    
    for(j in 1:17) {
      if(j!=17 && (is.empty(newdata[i,j]) || newdata[i,j]==""))
      {
        newdata[i,j] <- "vide"
      }
      else{
        newdata[i,j] <- as.character(newdata[i,j])
      }

    }
  }
  return(newdata)
}

#fonction de pre traitement de la data pour reseau de neurones multiperceptron
pretraitement <- function(newdata){
  
  length <- nrow(newdata)
  
  for(i in 1:length) {
    
    for(j in 1:17) {
      if(is.empty(newdata[i,j]) || newdata[i,j]=="")
      {
        newdata[i,j] <- 0
      }
      else
      {
        if(j==1 || j==13 || j==15 || j==16){
          alea <- sample(1:10, 1)
          newdata[i,j] <- alea
        }
        
        if(j==2){
          if(newdata[i,j]=="IP"){
            newdata[i,j] <- 1
          }
          else if(newdata[i,j]=="TCP"){
            newdata[i,j] <- 2
          }
          else if(newdata[i,j]=="UDP"){
            newdata[i,j] <- 3
          }
          else{
            newdata[i,j] <- 4
          }
        }
        
        if(j==3 || j==5){
          if(newdata[i,j]=="localhost"){
            newdata[i,j] <- 1270001
          }
          else{
           # newdata[i,j] <- str_replace_all(newdata[i,j], pattern = "\\.", replacement = "")
            newdata[i,j] <- 1111111
          }
        }
        if(j==4 || j==6){
          if(newdata[i,j]=="domain"){
            newdata[i,j] <- 53
          }
          else{
            newdata[i,j] <- 80
          }
        }
        if(j==7){
          if(newdata[i,j]=="ICMP"){
            newdata[i,j] <- 4
          }
          else{
            newdata[i,j] <- 5
          }
        }
        if(j==8){
          if(newdata[i,j]=="echo request"){
            newdata[i,j] <- 1
          }
          else if(newdata[i,j]=="echo reply"){
            newdata[i,j] <- 2
          }
          else{
            newdata[i,j] <- 3
          }
        }
        if(j==14){
          if(str_detect(newdata[i,j],"A")){
            newdata[i,j] <- 1
          }
          else if(str_detect(newdata[i,j],"CNAME")){
            newdata[i,j] <- 2
          }
          else{
            newdata[i,j] <- 3
          }
        }
      
      }
    }
    
  } 
  newdata <- transform(newdata, Heure = as.numeric(Heure), 
                                Protocole1 = as.numeric(Protocole1),
                                IPSource = as.numeric(IPSource),
                                PortSource = as.numeric(PortSource),
                                IPDestination = as.numeric(IPDestination),
                                PortDestination = as.numeric(PortDestination),
                                Protocole2 = as.numeric(Protocole2),
                                Typedepaquet = as.numeric(Typedepaquet),
                                id = as.numeric(id),
                                seq = as.numeric(seq),
                                length = as.numeric(length),
                                Flags = as.numeric(Flags),
                                A = as.numeric(A),
                                B = as.numeric(B),
                                C = as.numeric(C),
                                D = as.numeric(D))
  return(newdata)
}

#chargement des data
cn1 <- read.csv(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
cn2 <- read.csv(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
cn3 <- read.csv(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
ca1 <- read.csv(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
ca2 <- read.csv(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)

predictcat1 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcat2 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcat3 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcat4 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcat4 <- predictcat4[,1:17]
predictcat5 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcnt1 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcnt1 <- predictcnt1[,1:17]
predictcnt2 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcnt3 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcnt4 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)
predictcnt5 <- read.csv2(file.choose(), header = T, sep = ";", row.names=NULL, as.is=TRUE)



#Naive Bayes model
training <- rbind.fill(cn1, cn2, cn3, ca1, ca2)
trainingnaivebayes <- training[,1:17]
trainingnaivebayes <- pretraitementbayes(trainingnaivebayes)
str(trainingnaivebayes)
model <- naiveBayes(Label ~., data = trainingnaivebayes)


#predict
predictcat1bayes <- pretraitementbayes(predictcat1)
predictcat2bayes <- pretraitementbayes(predictcat2)
predictcat3bayes <- pretraitementbayes(predictcat3)
predictcat4bayes <- pretraitementbayes(predictcat4)
predictcat5bayes <- pretraitementbayes(predictcat5)
predictcnt1bayes <- pretraitementbayes(predictcnt1)
predictcnt2bayes <- pretraitementbayes(predictcnt2)
predictcnt3bayes <- pretraitementbayes(predictcnt3)
predictcnt4bayes <- pretraitementbayes(predictcnt4)
predictcnt5bayes <- pretraitementbayes(predictcnt5)

p1 <- predict(model, predictcat1bayes, type = "raw")
p2 <- predict(model, predictcat2bayes, type = "raw")
p3 <- predict(model, predictcat3bayes, type = "raw")
p4 <- predict(model, predictcat4bayes, type = "raw")
p5 <- predict(model, predictcat5bayes, type = "raw")
p6 <- predict(model, predictcnt1bayes, type = "raw")
p7 <- predict(model, predictcnt2bayes, type = "raw")
p8 <- predict(model, predictcnt3bayes, type = "raw")
p9 <- predict(model, predictcnt4bayes, type = "raw")
p10 <- predict(model, predictcnt5bayes, type = "raw")

#passer les proba a 1 ou 0 pour comparer directement aux valeurs des Label de datapredict

p1 <- changementproba(p1)
p2 <- changementproba(p2)
p3 <- changementproba(p3)
p4 <- changementproba(p4)
p5 <- changementproba(p5)
p6 <- changementproba(p6)
p7 <- changementproba(p7)
p8 <- changementproba(p8)
p9 <- changementproba(p9)
p10 <- changementproba(p10)
evaluation.prediction(predictcat1bayes$Label, p1[,2], 1)
confusion (cn1[,17], p1)
evaluation.prediction(predictcat2bayes$Label, p2[,2], 1)
evaluation.prediction(predictcat3bayes$Label, p3[,2], 1)
evaluation.prediction(predictcat4bayes$Label, p4[,2], 1)
evaluation.prediction(predictcat5bayes$Label, p5[,2], 1)
evaluation.prediction(predictcnt1bayes$Label, p6[,2], 1)
evaluation.prediction(predictcnt2bayes$Label, p7[,2], 1)
evaluation.prediction(predictcnt3bayes$Label, p8[,2], 1)
evaluation.prediction(predictcnt4bayes$Label, p9[,2], 1)
evaluation.prediction(predictcnt5bayes$Label, p10[,2], 1)


###multiperceptron

#pre traitement des data, on les rend numeriques
trainingneurones <- training[,1:17]
trainingneurones <- pretraitement(trainingneurones)

predictcat1neurone <- pretraitement(predictcat1)
predictcat2neurone <- pretraitement(predictcat2)
predictcat3neurone <- pretraitement(predictcat3)
predictcat4neurone <- pretraitement(predictcat4)
predictcat5neurone <- pretraitement(predictcat5)
predictcnt1neurone <- pretraitement(predictcnt1)
predictcnt2neurone <- pretraitement(predictcnt2)
predictcnt3neurone <- pretraitement(predictcnt3)
predictcnt4neurone <- pretraitement(predictcnt4)
predictcnt5neurone <- pretraitement(predictcnt5)


#calcul des moyennes sur l'echantillon d'apprentissage
moyenne <- apply(trainingneurones, 2, mean)
moyenne
#idem pour l'ecart-type
ecarttype <- apply(trainingneurones, 2, sd)
ecarttype

#centrage-réduction de l'échantillon d'apprentissage
newtrainneurones <- data.frame(scale(trainingneurones,center=moyenne,scale=ecarttype))
newtrainneurones$Label <- trainingneurones$Label

modelneurones <- nnet(Label ~ ., data = newtrainneurones, skip = FALSE, size = 2)

#prediction
predictneurones1 <- predict(modelneurones,predictcat1neurone,type="raw")
predictneurones2 <- predict(modelneurones,predictcat2neurone,type="raw")
predictneurones3 <- predict(modelneurones,predictcat3neurone,type="raw")
predictneurones4 <- predict(modelneurones,predictcat4neurone,type="raw")
predictneurones5 <- predict(modelneurones,predictcat5neurone,type="raw")
predictneurones6 <- predict(modelneurones,predictcnt1neurone,type="raw")
predictneurones7 <- predict(modelneurones,predictcnt2neurone,type="raw")
predictneurones8 <- predict(modelneurones,predictcnt3neurone,type="raw")
predictneurones9 <- predict(modelneurones,predictcnt4neurone,type="raw")
predictneurones10 <- predict(modelneurones,predictcnt5neurone,type="raw")
evaluation.prediction(predictcat1neurone$Label,predictneurones1, 0)
evaluation.prediction(predictcat2neurone$Label,predictneurones2, 0)
evaluation.prediction(predictcat3neurone$Label,predictneurones3, 0)
evaluation.prediction(predictcat4neurone$Label,predictneurones4, 0)
evaluation.prediction(predictcat5neurone$Label,predictneurones5, 0)
evaluation.prediction(predictcnt1neurone$Label,predictneurones6, 0)
evaluation.prediction(predictcnt2neurone$Label,predictneurones7, 0)
evaluation.prediction(predictcnt3neurone$Label,predictneurones8, 0)
evaluation.prediction(predictcnt4neurone$Label,predictneurones9, 0)
evaluation.prediction(predictcnt5neurone$Label,predictneurones10, 0)
