###
library(readxl)
##human read
humanPath='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/human'
male_eye=read_excel(paste0(humanPath,'/gongan_XJW_eye_hair_color.xlsx'),'male_eye')
female_eye=read_excel(paste0(humanPath,'/gongan_XJW_eye_hair_color.xlsx'),'female_eye')
read_eye=rbind(male_eye,female_eye)[,c(1,2,3,5,6)]##left_txm
###extraction data, median
hsvStatPath='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye/hsvStat'
hsv=read.csv(paste0(hsvStatPath,'/HSVPC3LRHuman.csv'))[,c(1,3,6,9,12,15,18)]
##merge
hsvRead=merge(read_eye,hsv,by.x=1,by.y=1)
##remove NA and replace 0
hsvRead=na.omit(hsvRead)
#leftside have 0, replace by rightside
row0=which(hsvRead[,2]==0)
read0=hsvRead[row0,]
read0[,2]=read0[,3]
read0[,4]=read0[,5]
##
hsvRead[row0,]=read0
##
names(hsvRead)[1]='id'
names(hsvRead)=gsub('0.5','',names(hsvRead))
names(hsvRead)=gsub('txm','rater1',names(hsvRead))
names(hsvRead)=gsub('zw','rater2',names(hsvRead))
## by right_rater1
hrL1=hsvRead[,c('id','rightside_rater1','H_right','S_right','V_right')];
##SVM training
#pHSV=LOO_SVM(hrL1[,-1])
#hrL1Pred=data.frame(hrL1,svmPredHuman=pHSV)

### predict by svm
library(e1071)
LOO_SVM<-function(d){
    rowN=nrow(d)
    pred=rep(0,rowN)
    names(d)[1]='y'
    for (i in 1:rowN){
        du=d[-i,]
        s=svm(y~.,data=du,type='C-classification')
        pred[i]=predict(s,d[i,-1])##Human is first colomn
    }
    return(pred)
}
##
pHSV=LOO_SVM(hrL1[,-1])
hrL1Pred=data.frame(hrL1,svmPredHuman=pHSV)
write.csv(hrL1Pred,file=paste0(hsvStatPath,'/right_txm_svm_predHuman.csv'),quote=F,row.names=F)
#write.csv(cor(hrL1Pred[,-1]),file=paste0(hsvStatPath,'/right_txm_svm_predHuman.cor.csv'),quote=F)
##predict by multinomial regression / logistic resgression
library(nnet)
LOO_GLM<-function(d){
    rowN=nrow(d)
    pred=rep(0,rowN)
    names(d)[1]='y'
    for (i in 1:rowN){
        du=d[-i,]
        s=multinom(y~.,data=du)
        pred[i]=predict(s,d[i,-1])##Human is first colomn
    }
    return(pred)
}
pHSV=LOO_GLM(hrL1[,-1])
hrL1Pred=data.frame(hrL1,glmPredHuman=pHSV)
write.csv(hrL1Pred,file=paste0(hsvStatPath,'/right_txm_glm_predHuman.csv'),quote=F,row.names=F)
#write.csv(cor(hrL1Pred[,-1]),file=paste0(hsvStatPath,'/left_txm_glm_predHuman.cor.csv'),quote=F)
##


###sensitivity specificity
##Kappa
library(irr)
humanPred=hrL1Pred[,c(2,6)]
stat=table(humanPred)
#k=kappa2(humanPred)
#k2=kappa2(humanPred,weight='equal')
k3=kappa2(humanPred,weight='squared')
##accuracy
acc=nrow(humanPred[humanPred[,1]==humanPred[,2],])/nrow(humanPred)
##





