list.of.packages <- c("caret", "repmis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,dependencies = TRUE)


library(repmis)
library(caret)
 source_data("https://github.com/R-S-P-MODELS/GravTree/blob/master/FeatureData.Rdata?raw=true")
vec=c()
Negativos=c()
Positivos=c()
#nomes=names(AmostraMaster)
#as.vector(nomes)
for(i in 1:80){
  AmostraMaster=FeatureData$FeaturesMaster[sample(1:nrow(FeatureData$FeaturesMaster),60),] 
  AmostraCastle=FeatureData$FeaturesLentesCastle[sample(1:nrow(FeatureData$FeaturesLentesCastle),60),]
  AmostraHubble=FeatureData$FeaturesHubble[sample(1:nrow(FeatureData$FeaturesHubble),60),]
  AmostraAstcam=FeatureData$FeaturesAstcam[sample(1:nrow(FeatureData$FeaturesAstcam),60),]
  AmostraUtah=FeatureData$FeaturesUtah[sample(1:nrow(FeatureData$FeaturesUtah),60),]
  AmostraSpace=FeatureData$FeaturesEspaciais[sample(1:nrow(FeatureData$FeaturesEspaciais),300),]
  AmostraLente=rbind(AmostraMaster,AmostraCastle,AmostraHubble,AmostraAstcam,AmostraUtah)
  AmostraLente=data.frame(AmostraLente,"Lentes")
  AmostraSpace=data.frame(AmostraSpace,"Não Lentes")
  names(AmostraLente)[ncol(AmostraLente)]=names(AmostraSpace)[ncol(AmostraSpace)]='target'
  Frame=rbind(AmostraLente,AmostraSpace)
#  x=sample(1:(ncol(Frame)-1),1000)
#Frame=Frame[,c(x,ncol(Frame))]
 Index=sample(1:nrow(Frame),0.8*nrow(Frame))
Training=Frame[Index,]
Test=Frame[-Index,]
modelo=train(target ~. ,data=Training,method="rpart")
  previsoes=predict(modelo,newdata=Test)
previsoes=as.character(previsoes)
vec[i]=sum(previsoes==Test$target)/length(Test$target)
Positivos[i]=sum(previsoes[which(Test$target=="Lentes")]==Test$target[which(Test$target=="Lentes")])/length(Test$target[which(Test$target=="Lentes")])  
Negativos[i]=sum(previsoes[which(Test$target=="Não Lentes")]==Test$target[which(Test$target=="Não Lentes")])/length(Test$target[which(Test$target=="Não Lentes")])  

  
}

return(list(Positive,Negative,vec))
