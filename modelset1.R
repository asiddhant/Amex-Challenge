
###############################################################################################

CentaurtrainDataset=subset(trainDataset,trainDataset$party_voted_past=="Centaur")
CentaurlbDataset=subset(lbDataset,lbDataset$party_voted_past=="Centaur")
CentaurfinalDataset=subset(finalDataset,finalDataset$party_voted_past=="Centaur")

set.seed(1)
dat=twoClassSim(100)

evalCriteria=function(actual,predicted,historical){
  score=rep(NA,length(actual))
  score[which(actual==predicted & actual==historical)]=50
  score[which(actual==predicted & !(actual==historical))]=100
  score[which(!(actual==predicted) & actual==historical)]=0
  score[which(!(actual==predicted) & !(actual==historical))]=-50
  fift=length(which(actual==predicted & actual==historical))/length(actual)
  hund=length(which(actual==predicted & !(actual==historical)))/length(actual)
  zero=length(which(!(actual==predicted) & actual==historical))/length(actual)
  mfif=length(which(!(actual==predicted) & !(actual==historical)))/length(actual)
  return(c(mean(score)*21207,fift,hund,zero,mfif))
}

myfun=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  hist=CentaurtrainDataset$party_voted_past[data$rowIndex]
  evalc=evalCriteria(data$obs,data$pred,hist)
  c(acc=acc,evalc=evalc[1],fift=evalc[2],hund=evalc[3],zero=evalc[4],mfif=evalc[5])
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=1,
                   classProbs = TRUE,
                   summaryFunction = myfun,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(100),.interaction.depth=c(3,4,5),.shrinkage=c(0.1,0.2),.n.minobsinnode=c(10,20))
#tgrid=expand.grid(.alpha=c(1),.lambda=c(0.00005,0.00020,0.00080,0.00320))

Centaurmodel=train(actual_vote~.,
                   data=CentaurtrainDataset[,-c(1,2,32,34,35,41:121)],
                   method="gbm",
                   metric="evalc",
                   #tuneGrid=tgrid,
                   trControl=tctrl
)
rm(tctrl,tgrid,myfun,dat,evalCriteria)

CentaurtrainDatasetv2=subset(CentaurtrainDataset,CentaurtrainDataset$actual_vote!="Centaur")
CentaurtrainDatasetv2$actual_vote=as.factor(as.character(CentaurtrainDatasetv2$actual_vote))

myfunv2=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  score=mean(ifelse(data$obs==data$pred,100,-50))
  c(acc=acc,evalc=score)
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=3,
                   classProbs = TRUE,
                   summaryFunction = myfunv2,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(40,50),.interaction.depth=c(2,3),.shrinkage=c(0.1),.n.minobsinnode=c(3,5))
#tgrid=expand.grid(.mtry=c(2,3,4))
#tgrid=expand.grid(.alpha=c(0,0.8,1),.lambda=c(0.001,0.002,0.004,0.008,0.016))

set.seed(1)

Centaurmodelv2=train(actual_vote~.,
                     data=CentaurtrainDatasetv2[,-c(1,2,34,35,41:121)],
                     method="gbm",
                     metric="evalc",
                     #tuneGrid=tgrid,
                     trControl=tctrl
)

rm(tctrl,tgrid,myfunv2)

CentaurlbDataset$pred_vote=predict(Centaurmodel,CentaurlbDataset)
prop.table(table(CentaurlbDataset$pred_vote))
prop.table(table(CentaurtrainDataset$actual_vote))
Centaurlbresponse=predict(Centaurmodel,CentaurlbDataset,type="prob")
boxplot(Centaurlbresponse)
CentaurThresh=0.61
Centaurpred=rep(NA,nrow(CentaurlbDataset))
Centaurpred=ifelse(Centaurlbresponse$Centaur>CentaurThresh,"Centaur",Centaurpred)
# Centaurlbresponse$Ebony=Centaurlbresponse$Ebony-0.05
# Centaurlbresponse$Odyssey=Centaurlbresponse$Odyssey+0.05
Centaurpred[is.na(Centaurpred)]=names(Centaurlbresponse[,2:5])[apply(Centaurlbresponse[is.na(Centaurpred),2:5], 
                                                                   1, which.max)]
prop.table(table(Centaurpred))
CentaurlbDataset$pred_vote=Centaurpred
rm(CentaurThresh,Centaurpred)

CentaurlbDataset$pred_vote[which(CentaurlbDataset$pred_vote!="Centaur")]=as.character(predict(Centaurmodelv2,CentaurlbDataset[which(CentaurlbDataset$pred_vote!="Centaur"),]))

CentaurfinalDataset$pred_vote=predict(Centaurmodel,CentaurfinalDataset)
prop.table(table(CentaurfinalDataset$pred_vote))
prop.table(table(CentaurtrainDataset$actual_vote))
Centaurfinalresponse=predict(Centaurmodel,CentaurfinalDataset,type="prob")
boxplot(Centaurfinalresponse)
CentaurThresh=0.61
Centaurpred=rep(NA,nrow(CentaurfinalDataset))
Centaurpred=ifelse(Centaurfinalresponse$Centaur>CentaurThresh,"Centaur",Centaurpred)
# Centaurfinalresponse$Ebony=Centaurfinalresponse$Ebony-0.05
# Centaurfinalresponse$Odyssey=Centaurfinalresponse$Odyssey+0.05
Centaurpred[is.na(Centaurpred)]=names(Centaurfinalresponse[,2:5])[apply(Centaurfinalresponse[is.na(Centaurpred),2:5], 
                                                                     1, which.max)]
prop.table(table(Centaurpred))
CentaurfinalDataset$pred_vote=Centaurpred
rm(CentaurThresh,Centaurpred)

CentaurfinalDataset$pred_vote[which(CentaurfinalDataset$pred_vote!="Centaur")]=as.character(predict(Centaurmodelv2,CentaurfinalDataset[which(CentaurfinalDataset$pred_vote!="Centaur"),]))


#save(list=c("Centaurmodel"),file="CentaurModel.RData")
###############################################################################################

CosmostrainDataset=subset(trainDataset,trainDataset$party_voted_past=="Cosmos")
CosmoslbDataset=subset(lbDataset,lbDataset$party_voted_past=="Cosmos")
CosmosfinalDataset=subset(finalDataset,finalDataset$party_voted_past=="Cosmos")

set.seed(1)
dat=twoClassSim(100)

evalCriteria=function(actual,predicted,historical){
  score=rep(NA,length(actual))
  score[which(actual==predicted & actual==historical)]=50
  score[which(actual==predicted & !(actual==historical))]=100
  score[which(!(actual==predicted) & actual==historical)]=0
  score[which(!(actual==predicted) & !(actual==historical))]=-50
  fift=length(which(actual==predicted & actual==historical))/length(actual)
  hund=length(which(actual==predicted & !(actual==historical)))/length(actual)
  zero=length(which(!(actual==predicted) & actual==historical))/length(actual)
  mfif=length(which(!(actual==predicted) & !(actual==historical)))/length(actual)
  return(c(mean(score)*21207,fift,hund,zero,mfif))
}

myfun=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  hist=CosmostrainDataset$party_voted_past[data$rowIndex]
  evalc=evalCriteria(data$obs,data$pred,hist)
  c(acc=acc,evalc=evalc[1],fift=evalc[2],hund=evalc[3],zero=evalc[4],mfif=evalc[5])
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=1,
                   classProbs = TRUE,
                   summaryFunction = myfun,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(100),.interaction.depth=c(3,4,5),.shrinkage=c(0.1,0.2),.n.minobsinnode=c(10,20))
#tgrid=expand.grid(.alpha=c(1),.lambda=c(0.00005,0.00020,0.00080,0.00320))

Cosmosmodel=train(actual_vote~.,
                  data=CosmostrainDataset[,-c(1,2,32,34,35,41:121)],
                  method="gbm",
                  metric="evalc",
                  #tuneGrid=tgrid,
                  trControl=tctrl
)
rm(tctrl,tgrid,myfun,dat,evalCriteria)

CosmostrainDatasetv2=subset(CosmostrainDataset,CosmostrainDataset$actual_vote!="Cosmos")
CosmostrainDatasetv2$actual_vote=as.factor(as.character(CosmostrainDatasetv2$actual_vote))

myfunv2=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  score=mean(ifelse(data$obs==data$pred,100,-50))
  c(acc=acc,evalc=score)
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=3,
                   classProbs = TRUE,
                   summaryFunction = myfunv2,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(40,50),.interaction.depth=c(2,3),.shrinkage=c(0.1),.n.minobsinnode=c(3,5))
#tgrid=expand.grid(.mtry=c(2,3,4))
#tgrid=expand.grid(.alpha=c(0,0.8,1),.lambda=c(0.001,0.002,0.004,0.008,0.016))

set.seed(1)

Cosmosmodelv2=train(actual_vote~.,
                    data=CosmostrainDatasetv2[,-c(1,2,34,35,41:121)],
                    method="gbm",
                    metric="evalc",
                    #tuneGrid=tgrid,
                    trControl=tctrl
)

rm(tctrl,tgrid,myfunv2)

CosmoslbDataset$pred_vote=predict(Cosmosmodel,CosmoslbDataset)
prop.table(table(CosmoslbDataset$pred_vote))
prop.table(table(CosmostrainDataset$actual_vote))
Cosmoslbresponse=predict(Cosmosmodel,CosmoslbDataset,type="prob")
boxplot(Cosmoslbresponse)
CosmosThresh=0.62
Cosmospred=rep(NA,nrow(CosmoslbDataset))
Cosmospred=ifelse(Cosmoslbresponse$Cosmos>CosmosThresh,"Cosmos",Cosmospred)
# Cosmoslbresponse$Ebony=Cosmoslbresponse$Ebony-0.05
# Cosmoslbresponse$Odyssey=Cosmoslbresponse$Odyssey+0.05
Cosmospred[is.na(Cosmospred)]=names(Cosmoslbresponse[,c(1,3:5)])[apply(Cosmoslbresponse[is.na(Cosmospred),c(1,3:5)], 
                                                                     1, which.max)]
prop.table(table(Cosmospred))
CosmoslbDataset$pred_vote=Cosmospred
rm(CosmosThresh,Cosmospred)

CosmoslbDataset$pred_vote[which(CosmoslbDataset$pred_vote!="Cosmos")]=as.character(predict(Cosmosmodelv2,CosmoslbDataset[which(CosmoslbDataset$pred_vote!="Cosmos"),]))


CosmosfinalDataset$pred_vote=predict(Cosmosmodel,CosmosfinalDataset)
prop.table(table(CosmosfinalDataset$pred_vote))
prop.table(table(CosmostrainDataset$actual_vote))
Cosmosfinalresponse=predict(Cosmosmodel,CosmosfinalDataset,type="prob")
boxplot(Cosmosfinalresponse)
CosmosThresh=0.62
Cosmospred=rep(NA,nrow(CosmosfinalDataset))
Cosmospred=ifelse(Cosmosfinalresponse$Cosmos>CosmosThresh,"Cosmos",Cosmospred)
# Cosmosfinalresponse$Ebony=Cosmosfinalresponse$Ebony-0.05
# Cosmosfinalresponse$Odyssey=Cosmosfinalresponse$Odyssey+0.05
Cosmospred[is.na(Cosmospred)]=names(Cosmosfinalresponse[,c(1,3:5)])[apply(Cosmosfinalresponse[is.na(Cosmospred),c(1,3:5)], 
                                                                       1, which.max)]
prop.table(table(Cosmospred))
CosmosfinalDataset$pred_vote=Cosmospred
rm(CosmosThresh,Cosmospred)

CosmosfinalDataset$pred_vote[which(CosmosfinalDataset$pred_vote!="Cosmos")]=as.character(predict(Cosmosmodelv2,CosmosfinalDataset[which(CosmosfinalDataset$pred_vote!="Cosmos"),]))

#save(list=c("Cosmosmodel"),file="CosmosModel.RData")

################################################################################################

EbonytrainDataset=subset(trainDataset,trainDataset$party_voted_past=="Ebony")
EbonylbDataset=subset(lbDataset,lbDataset$party_voted_past=="Ebony")
EbonyfinalDataset=subset(finalDataset,finalDataset$party_voted_past=="Ebony")

set.seed(1)
dat=twoClassSim(100)

evalCriteria=function(actual,predicted,historical){
  score=rep(NA,length(actual))
  score[which(actual==predicted & actual==historical)]=50
  score[which(actual==predicted & !(actual==historical))]=100
  score[which(!(actual==predicted) & actual==historical)]=0
  score[which(!(actual==predicted) & !(actual==historical))]=-50
  fift=length(which(actual==predicted & actual==historical))/length(actual)
  hund=length(which(actual==predicted & !(actual==historical)))/length(actual)
  zero=length(which(!(actual==predicted) & actual==historical))/length(actual)
  mfif=length(which(!(actual==predicted) & !(actual==historical)))/length(actual)
  return(c(mean(score)*21207,fift,hund,zero,mfif))
}

myfun=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  hist=EbonytrainDataset$party_voted_past[data$rowIndex]
  evalc=evalCriteria(data$obs,data$pred,hist)
  c(acc=acc,evalc=evalc[1],fift=evalc[2],hund=evalc[3],zero=evalc[4],mfif=evalc[5])
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=1,
                   classProbs = TRUE,
                   summaryFunction = myfun,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(100),.interaction.depth=c(3,4,5),.shrinkage=c(0.1,0.2),.n.minobsinnode=c(10,20))
#tgrid=expand.grid(.alpha=c(1),.lambda=c(0.00005,0.00020,0.00080,0.00320))

Ebonymodel=train(actual_vote~.,
                 data=EbonytrainDataset[,-c(1,2,32,34,35,41:121)],
                 method="gbm",
                 metric="evalc",
                 #tuneGrid=tgrid,
                 trControl=tctrl
)
rm(tctrl,tgrid,myfun,dat,evalCriteria)

EbonytrainDatasetv2=subset(EbonytrainDataset,EbonytrainDataset$actual_vote!="Ebony")
EbonytrainDatasetv2$actual_vote=as.factor(as.character(EbonytrainDatasetv2$actual_vote))

myfunv2=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  score=mean(ifelse(data$obs==data$pred,100,-50))
  c(acc=acc,evalc=score)
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=3,
                   classProbs = TRUE,
                   summaryFunction = myfunv2,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(40,50),.interaction.depth=c(2,3),.shrinkage=c(0.1),.n.minobsinnode=c(3,5))
#tgrid=expand.grid(.mtry=c(2,3,4))
#tgrid=expand.grid(.alpha=c(0,0.8,1),.lambda=c(0.001,0.002,0.004,0.008,0.016))

set.seed(1)

Ebonymodelv2=train(actual_vote~.,
                   data=EbonytrainDatasetv2[,-c(1,2,9,13:17,19:22,28:33,34,35,41:121)],
                   method="gbm",
                   metric="evalc",
                   #tuneGrid=tgrid,
                   trControl=tctrl
)

rm(tctrl,tgrid,myfunv2)

EbonylbDataset$pred_vote=predict(Ebonymodel,EbonylbDataset)
prop.table(table(EbonylbDataset$pred_vote))
prop.table(table(EbonytrainDataset$actual_vote))
Ebonylbresponse=predict(Ebonymodel,EbonylbDataset,type="prob")
boxplot(Ebonylbresponse)
EbonyThresh=0.60
Ebonypred=rep(NA,nrow(EbonylbDataset))
Ebonypred=ifelse(Ebonylbresponse$Ebony>EbonyThresh,"Ebony",Ebonypred)
# Ebonylbresponse$Ebony=Ebonylbresponse$Ebony-0.05
# Ebonylbresponse$Odyssey=Ebonylbresponse$Odyssey+0.05
Ebonypred[is.na(Ebonypred)]=names(Ebonylbresponse[,c(1,2,4,5)])[apply(Ebonylbresponse[is.na(Ebonypred),c(1,2,4,5)], 
                                                                    1, which.max)]
prop.table(table(Ebonypred))
EbonylbDataset$pred_vote=Ebonypred
rm(EbonyThresh,Ebonypred)

EbonylbDataset$pred_vote[which(EbonylbDataset$pred_vote!="Ebony")]=as.character(predict(Ebonymodelv2,EbonylbDataset[which(EbonylbDataset$pred_vote!="Ebony"),]))

EbonyfinalDataset$pred_vote=predict(Ebonymodel,EbonyfinalDataset)
prop.table(table(EbonyfinalDataset$pred_vote))
prop.table(table(EbonytrainDataset$actual_vote))
Ebonyfinalresponse=predict(Ebonymodel,EbonyfinalDataset,type="prob")
boxplot(Ebonyfinalresponse)
EbonyThresh=0.60
Ebonypred=rep(NA,nrow(EbonyfinalDataset))
Ebonypred=ifelse(Ebonyfinalresponse$Ebony>EbonyThresh,"Ebony",Ebonypred)
# Ebonyfinalresponse$Ebony=Ebonyfinalresponse$Ebony-0.05
# Ebonyfinalresponse$Odyssey=Ebonyfinalresponse$Odyssey+0.05
Ebonypred[is.na(Ebonypred)]=names(Ebonyfinalresponse[,c(1,2,4,5)])[apply(Ebonyfinalresponse[is.na(Ebonypred),c(1,2,4,5)], 
                                                                      1, which.max)]
prop.table(table(Ebonypred))
EbonyfinalDataset$pred_vote=Ebonypred
rm(EbonyThresh,Ebonypred)

EbonyfinalDataset$pred_vote[which(EbonyfinalDataset$pred_vote!="Ebony")]=as.character(predict(Ebonymodelv2,EbonyfinalDataset[which(EbonyfinalDataset$pred_vote!="Ebony"),]))


#save(list=c("Ebonymodel"),file="EbonyModel.RData")

###############################################################################################

OdysseytrainDataset=subset(trainDataset,trainDataset$party_voted_past=="Odyssey")
OdysseylbDataset=subset(lbDataset,lbDataset$party_voted_past=="Odyssey")
OdysseyfinalDataset=subset(finalDataset,finalDataset$party_voted_past=="Odyssey")

set.seed(1)
dat=twoClassSim(100)

evalCriteria=function(actual,predicted,historical){
  score=rep(NA,length(actual))
  score[which(actual==predicted & actual==historical)]=50
  score[which(actual==predicted & !(actual==historical))]=100
  score[which(!(actual==predicted) & actual==historical)]=0
  score[which(!(actual==predicted) & !(actual==historical))]=-50
  fift=length(which(actual==predicted & actual==historical))/length(actual)
  hund=length(which(actual==predicted & !(actual==historical)))/length(actual)
  zero=length(which(!(actual==predicted) & actual==historical))/length(actual)
  mfif=length(which(!(actual==predicted) & !(actual==historical)))/length(actual)
  return(c(mean(score)*21207,fift,hund,zero,mfif))
}

myfun=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  hist=OdysseytrainDataset$party_voted_past[data$rowIndex]
  evalc=evalCriteria(data$obs,data$pred,hist)
  c(acc=acc,evalc=evalc[1],fift=evalc[2],hund=evalc[3],zero=evalc[4],mfif=evalc[5])
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=1,
                   classProbs = TRUE,
                   summaryFunction = myfun,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(100),.interaction.depth=c(3,4,5),.shrinkage=c(0.1,0.2),.n.minobsinnode=c(10,20))
#tgrid=expand.grid(.alpha=c(1),.lambda=c(0.00005,0.00020,0.00080,0.00320))

Odysseymodel=train(actual_vote~.,
                   data=OdysseytrainDataset[,-c(1,2,32,34,35,41:121)],
                   method="gbm",
                   metric="evalc",
                   #tuneGrid=tgrid,
                   trControl=tctrl
)
rm(tctrl,tgrid,myfun,dat,evalCriteria)

OdysseytrainDatasetv2=subset(OdysseytrainDataset,OdysseytrainDataset$actual_vote!="Odyssey")
OdysseytrainDatasetv2$actual_vote=as.factor(as.character(OdysseytrainDatasetv2$actual_vote))

myfunv2=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  score=mean(ifelse(data$obs==data$pred,100,-50))
  c(acc=acc,evalc=score)
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=3,
                   classProbs = TRUE,
                   summaryFunction = myfunv2,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(40,50),.interaction.depth=c(2,3),.shrinkage=c(0.1),.n.minobsinnode=c(3,5))
#tgrid=expand.grid(.mtry=c(2,3,4))
#tgrid=expand.grid(.alpha=c(0,0.8,1),.lambda=c(0.001,0.002,0.004,0.008,0.016))

set.seed(1)

Odysseymodelv2=train(actual_vote~.,
                     data=OdysseytrainDatasetv2[,-c(1,2,34,35,41:121)],
                     method="gbm",
                     metric="evalc",
                     #tuneGrid=tgrid,
                     trControl=tctrl
)

rm(tctrl,tgrid,myfunv2)


OdysseylbDataset$pred_vote=predict(Odysseymodel,OdysseylbDataset)
prop.table(table(OdysseylbDataset$pred_vote))
prop.table(table(OdysseytrainDataset$actual_vote))
Odysseylbresponse=predict(Odysseymodel,OdysseylbDataset,type="prob")
boxplot(Odysseylbresponse)
OdysseyThresh=0.65
Odysseypred=rep(NA,nrow(OdysseylbDataset))
Odysseypred=ifelse(Odysseylbresponse$Odyssey>OdysseyThresh,"Odyssey",Odysseypred)
# Odysseylbresponse$Ebony=Odysseylbresponse$Ebony-0.05
# Odysseylbresponse$Odyssey=Odysseylbresponse$Odyssey+0.05
Odysseypred[is.na(Odysseypred)]=names(Odysseylbresponse[,c(1,2,3,5)])[apply(Odysseylbresponse[is.na(Odysseypred),c(1,2,3,5)], 
                                                                          1, which.max)]
prop.table(table(Odysseypred))
OdysseylbDataset$pred_vote=Odysseypred
rm(OdysseyThresh,Odysseypred)

OdysseylbDataset$pred_vote[which(OdysseylbDataset$pred_vote!="Odyssey")]=as.character(predict(Odysseymodelv2,OdysseylbDataset[which(OdysseylbDataset$pred_vote!="Odyssey"),]))

OdysseyfinalDataset$pred_vote=predict(Odysseymodel,OdysseyfinalDataset)
prop.table(table(OdysseyfinalDataset$pred_vote))
prop.table(table(OdysseytrainDataset$actual_vote))
Odysseyfinalresponse=predict(Odysseymodel,OdysseyfinalDataset,type="prob")
boxplot(Odysseyfinalresponse)
OdysseyThresh=0.65
Odysseypred=rep(NA,nrow(OdysseyfinalDataset))
Odysseypred=ifelse(Odysseyfinalresponse$Odyssey>OdysseyThresh,"Odyssey",Odysseypred)
# Odysseyfinalresponse$Ebony=Odysseyfinalresponse$Ebony-0.05
# Odysseyfinalresponse$Odyssey=Odysseyfinalresponse$Odyssey+0.05
Odysseypred[is.na(Odysseypred)]=names(Odysseyfinalresponse[,c(1,2,3,5)])[apply(Odysseyfinalresponse[is.na(Odysseypred),c(1,2,3,5)], 
                                                                            1, which.max)]
prop.table(table(Odysseypred))
OdysseyfinalDataset$pred_vote=Odysseypred
rm(OdysseyThresh,Odysseypred)

OdysseyfinalDataset$pred_vote[which(OdysseyfinalDataset$pred_vote!="Odyssey")]=as.character(predict(Odysseymodelv2,OdysseyfinalDataset[which(OdysseyfinalDataset$pred_vote!="Odyssey"),]))


#save(list=c("Odysseymodel"),file="OdysseyModel.RData")

###############################################################################################

TokugawatrainDataset=subset(trainDataset,trainDataset$party_voted_past=="Tokugawa")
TokugawalbDataset=subset(lbDataset,lbDataset$party_voted_past=="Tokugawa")
TokugawafinalDataset=subset(finalDataset,finalDataset$party_voted_past=="Tokugawa")

set.seed(1)
dat=twoClassSim(100)

evalCriteria=function(actual,predicted,historical){
  score=rep(NA,length(actual))
  score[which(actual==predicted & actual==historical)]=50
  score[which(actual==predicted & !(actual==historical))]=100
  score[which(!(actual==predicted) & actual==historical)]=0
  score[which(!(actual==predicted) & !(actual==historical))]=-50
  fift=length(which(actual==predicted & actual==historical))/length(actual)
  hund=length(which(actual==predicted & !(actual==historical)))/length(actual)
  zero=length(which(!(actual==predicted) & actual==historical))/length(actual)
  mfif=length(which(!(actual==predicted) & !(actual==historical)))/length(actual)
  return(c(mean(score)*21207,fift,hund,zero,mfif))
}

myfun=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  hist=TokugawatrainDataset$party_voted_past[data$rowIndex]
  evalc=evalCriteria(data$obs,data$pred,hist)
  c(acc=acc,evalc=evalc[1],fift=evalc[2],hund=evalc[3],zero=evalc[4],mfif=evalc[5])
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=1,
                   classProbs = TRUE,
                   summaryFunction = myfun,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(100),.interaction.depth=c(3,4,5),.shrinkage=c(0.1,0.2),.n.minobsinnode=c(10,20))
#tgrid=expand.grid(.alpha=c(1),.lambda=c(0.00005,0.00020,0.00080,0.00320))

Tokugawamodel=train(actual_vote~.,
                    data=TokugawatrainDataset[,-c(1,2,32,34,35,41:121)],
                    method="gbm",
                    metric="evalc",
                    #tuneGrid=tgrid,
                    trControl=tctrl
)
rm(tctrl,tgrid,myfun,dat,evalCriteria)

TokugawatrainDatasetv2=subset(TokugawatrainDataset,TokugawatrainDataset$actual_vote!="Tokugawa")
TokugawatrainDatasetv2$actual_vote=as.factor(as.character(TokugawatrainDatasetv2$actual_vote))

myfunv2=function(data,lev=NULL,model=NULL)
{
  acc=postResample(data$pred,data$obs)
  score=mean(ifelse(data$obs==data$pred,100,-50))
  c(acc=acc,evalc=score)
}

tctrl=trainControl(method="repeatedcv",
                   number=3,
                   repeats=3,
                   classProbs = TRUE,
                   summaryFunction = myfunv2,
                   allowParallel = FALSE
)

#tgrid=expand.grid(.n.trees=c(40,50),.interaction.depth=c(2,3),.shrinkage=c(0.1),.n.minobsinnode=c(3,5))
#tgrid=expand.grid(.mtry=c(2,3,4))
#tgrid=expand.grid(.alpha=c(0,0.8,1),.lambda=c(0.001,0.002,0.004,0.008,0.016))

set.seed(1)

Tokugawamodelv2=train(actual_vote~.,
                      data=TokugawatrainDatasetv2[,-c(1,2,5,10,13:17,20:22,28:32,34,35,41:121)],
                      method="gbm",
                      metric="evalc",
                      #tuneGrid=tgrid,
                      trControl=tctrl
)

rm(tctrl,tgrid,myfunv2)

TokugawalbDataset$pred_vote=predict(Tokugawamodel,TokugawalbDataset)
prop.table(table(TokugawalbDataset$pred_vote))
prop.table(table(TokugawatrainDataset$actual_vote))
Tokugawalbresponse=predict(Tokugawamodel,TokugawalbDataset,type="prob")
boxplot(Tokugawalbresponse)
TokugawaThresh=0.7
Tokugawapred=rep(NA,nrow(TokugawalbDataset))
Tokugawapred=ifelse(Tokugawalbresponse$Tokugawa>TokugawaThresh,"Tokugawa",Tokugawapred)
# Tokugawalbresponse$Ebony=Tokugawalbresponse$Ebony-0.05
# Tokugawalbresponse$Odyssey=Tokugawalbresponse$Odyssey+0.05
Tokugawapred[is.na(Tokugawapred)]=names(Tokugawalbresponse[,1:4])[apply(Tokugawalbresponse[is.na(Tokugawapred),1:4], 
                                                                      1, which.max)]
prop.table(table(Tokugawapred))
TokugawalbDataset$pred_vote=Tokugawapred
rm(TokugawaThresh,Tokugawapred)

#TokugawalbDataset$pred_vote[which(TokugawalbDataset$pred_vote!="Tokugawa")]=as.character(predict(Tokugawamodelv2,TokugawalbDataset[which(TokugawalbDataset$pred_vote!="Tokugawa"),]))

TokugawafinalDataset$pred_vote=predict(Tokugawamodel,TokugawafinalDataset)
prop.table(table(TokugawafinalDataset$pred_vote))
prop.table(table(TokugawatrainDataset$actual_vote))
Tokugawafinalresponse=predict(Tokugawamodel,TokugawafinalDataset,type="prob")
boxplot(Tokugawafinalresponse)
TokugawaThresh=0.7
Tokugawapred=rep(NA,nrow(TokugawafinalDataset))
Tokugawapred=ifelse(Tokugawafinalresponse$Tokugawa>TokugawaThresh,"Tokugawa",Tokugawapred)
# Tokugawafinalresponse$Ebony=Tokugawafinalresponse$Ebony-0.05
# Tokugawafinalresponse$Odyssey=Tokugawafinalresponse$Odyssey+0.05
Tokugawapred[is.na(Tokugawapred)]=names(Tokugawafinalresponse[,1:4])[apply(Tokugawafinalresponse[is.na(Tokugawapred),1:4], 
                                                                        1, which.max)]
prop.table(table(Tokugawapred))
TokugawafinalDataset$pred_vote=Tokugawapred
rm(TokugawaThresh,Tokugawapred)

#TokugawafinalDataset$pred_vote[which(TokugawafinalDataset$pred_vote!="Tokugawa")]=as.character(predict(Tokugawamodelv2,TokugawafinalDataset[which(TokugawafinalDataset$pred_vote!="Tokugawa"),]))


################################################################################################################

nlbDataset=rbind(CentaurlbDataset,CosmoslbDataset,EbonylbDataset,OdysseylbDataset,TokugawalbDataset)
nlbDataset$pred_vote=as.factor(nlbDataset$pred_vote)
nlbresponse=rbind(Centaurlbresponse,Cosmoslbresponse,Ebonylbresponse,Odysseylbresponse,Tokugawalbresponse)

nfinalDataset=rbind(CentaurfinalDataset,CosmosfinalDataset,EbonyfinalDataset,OdysseyfinalDataset,TokugawafinalDataset)
nfinalDataset$pred_vote=as.factor(nfinalDataset$pred_vote)
nfinalresponse=rbind(Centaurfinalresponse,Cosmosfinalresponse,Ebonyfinalresponse,Odysseyfinalresponse,Tokugawafinalresponse)

#output=read.csv("/home/abhi/Desktop/DualLevelAnalysis/faceless_men_IIT_Guwahati_65_best.csv",header = FALSE)

#all(output$V1==nlbDataset$citizen_id)
#table(output$V2,nlbDataset$pred_vote)

prop.table(table(nlbDataset$pred_vote))
prop.table(table(nfinalDataset$pred_vote))

write.csv(nfinalresponse,"response_final1.csv",row.names = FALSE)
write.csv(nlbresponse,"response_lb1.csv",row.names = FALSE)

write.table(nfinalDataset[,c(1,122)],"faceless_men_IIT_Guwahati_final1.csv",sep=",",row.names = FALSE,col.names = FALSE)
write.table(nlbDataset[,c(1,122)],"faceless_men_IIT_Guwahati_lb1.csv",sep=",",row.names = FALSE,col.names = FALSE)
