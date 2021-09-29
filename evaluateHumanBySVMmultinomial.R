if(0){
library(readxl)
humanPath='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/human'
male_eye=read_excel(paste0(humanPath,'/gongan_XJW_eye_hair_color.xlsx'),'male_eye')
female_eye=read_excel(paste0(humanPath,'/gongan_XJW_eye_hair_color.xlsx'),'female_eye')
read_eye=rbind(male_eye,female_eye)[,c(1,2,3,5,6)]##leprob_txm
##check 0
read_eye=na.omit(read_eye)
row02=which(read_eye[,2]==0)
row03=which(read_eye[,3]==0)
#row04=which(read_eye[,4]==0)
#row05=which(read_eye[,5]==0)
omitRow=intersect(row02,row03)
keepRow=setdiff(row02,row03)
## keep fill first
read_eye[keepRow,2]=read_eye[keepRow,3]
read_eye[keepRow,4]=read_eye[keepRow,5]
read_eye=read_eye[-omitRow,]
write.csv(read_eye,file=paste0(humanPath,'/gongan_XJW_eye_color.csv'),quote=F,row.names=F)
}##

###
read_eye=read.csv('/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/human/gongan_XJW_eye_color.csv')
###extraction data, median
hsvStatPath='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye/hsvStat'
hsv=read.csv(paste0(hsvStatPath,'/HSVPC3LRHuman.csv'))[,c(1,3,6,9,12,15,18)]
##merge
hsvRead=merge(read_eye,hsv,by.x=1,by.y=1)
##
names(hsvRead)[1]='id'
names(hsvRead)=gsub('0.5','',names(hsvRead))
names(hsvRead)=gsub('txm','rater1',names(hsvRead))
names(hsvRead)=gsub('zw','rater2',names(hsvRead))
## by rightside
hr=hsvRead[,c('rightside_rater1','H_right','S_right','V_right')];
##SVM training
library(e1071)
svmModel=svm(rightside_rater1~.,data=hr,type='C-classification')
hs=data.frame(r1=hr[,1],svm_r1=as.numeric(svmModel$fitted))
#write.csv(data.frame(id=hsvRead[,1],hs),file=paste0(hsvStatPath,'/HSV_rightside_txm.svmpred.csv'),quote=F,row.names=F)
##evaluation
sum=summary(hs)
statSVM=as.matrix(table(hs))
#write.csv(statSVM,file=paste0(hsvStatPath,'/HSV_rightside_txm.svmpred.table.csv'),quote=F)
accSVM=sum(statSVM[row(statSVM)==col(statSVM)])/nrow(hs)
library(irr)
kapSVM=kappa2(hs,weight="squared")
corSVM=cor.test(hs[,1],hs[,2])
##
library(nnet)
mnModel=multinom(rightside_rater1~.,data=hr)
hm=data.frame(r1=hr[,1],mn_r1=as.numeric(predict(mnModel,hr[,-1])))
#write.csv(data.frame(id=hsvRead[,1],hm),file=paste0(hsvStatPath,'/HSV_rightside_txm.mnpred.csv'),quote=F,row.names=F)
##evaluation
sum=summary(hm)
statHM=as.matrix(table(hm))
#write.csv(statHM,file=paste0(hsvStatPath,'/HSV_rightside_txm.mnpred.table.csv'),quote=F)
##accuracy
accHM=sum(statHM[row(statHM)==col(statHM)])/nrow(hm)
library(irr)
##kappa
kapHM=kappa2(hm,weight="squared")
corHM=cor.test(hm[,1],hm[,2])
##AUC calcularion
##softmax result/probabilities
library(pROC)
prob=as.data.frame(mnModel$fitted.values)
##
getAUC<-function(response0,prob){
    aucAll=NULL
    for(classN in 1:5){
        response=response0
        response[response!=classN]=0;
        #response[response==classN]=1;
        roc1=roc(response,prob[,classN])
        auc1 = auc(roc1)
        aucAll=c(aucAll,auc1)
    }
    #plot(roc1,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)
    #return(list(auc1,roc1))
    return(aucAll)
}
##
aucAll=data.frame(getAUC(hr[,1],prob))
colnames(aucAll)='AUC'
rownames(aucAll)=1:5
#write.csv(aucAll,file=paste0(hsvStatPath,'/HSV_rightside_txm.mnpred.auc.csv'),quote=F)
###ROC of all classes
mulROC=multiclass.roc(hm[,1],prob)
mulAUC=auc(mulROC)

## right_rater1 vs right_rater2
R12=hsvRead[,c('rightside_rater1','rightside_rater2')];
#write.csv(data.frame(id=hsvRead[,1],R12),file=paste0(hsvStatPath,'/HSV_rightside_txm.R12.csv'),quote=F,row.names=F)
statR12=as.matrix(table(R12))
#write.csv(statR12,file=paste0(hsvStatPath,'/HSV_rightside_txm.R12.table.csv'),quote=F)
accR12=sum(statR12[row(statR12)==col(statR12)])/nrow(R12)
kapR12=kappa2(R12,weight="squared")
corR12=cor.test(R12[,1],R12[,2])

## acc kappa cor
akc=data.frame(rbind(rbind(c(accSVM,kapSVM$value,kapSVM$p.value,corSVM$estimate,corSVM$p.value),
    c(accHM,kapHM$value,kapHM$p.value,corHM$estimate,corHM$p.value),
    c(accR12,kapR12$value,kapR12$p.value,corR12$estimate,corR12$p.value))))

rownames(akc)=c('svm','multinom','rightside_rater2')
colnames(akc)=c('acc','kappa','kappa_p','R','R_P')
#write.csv(akc,file=paste0(hsvStatPath,'/HSV_rightside_txm.akc.csv'),quote=F)

### sensitivity and specificity
##class 1
getTPNR<-function(statSVM,classes){
    tpnrs=NULL
    for (classN in classes){
        ss1=data.frame(col1=c(statSVM[classN,classN],sum(statSVM[-classN,classN])),
        col2=c(sum(statSVM[classN,-classN]),sum(statSVM[-classN,-classN])))
        #rownames(ss1)=c('class_1','class_2')
        #colnames(ss1)=c('pred_1','pred_2')
        #Sensitivity/TPR = TP / (TP + FN) 
        TPR=ss1[1,1]/sum(ss1[1,])
        #Specificity/TNR = TN / (TN + FP)           
        TNR=ss1[2,2]/sum(ss1[2,])
        tpnrs=rbind(tpnrs,c(TPR,TNR))
    }
    colnames(tpnrs)=c('TPR','TNR')
    rownames(tpnrs)=1:5
    return(data.frame(tpnrs))
}
##
tpnr_SVM=getTPNR(statSVM,1:5);rownames(tpnr_SVM)=paste0('SVM_',rownames(tpnr_SVM))
tpnr_HM=getTPNR(statHM,1:5);rownames(tpnr_HM)=paste0('HM_',rownames(tpnr_HM))
tpnr_R12=getTPNR(statR12,1:5);rownames(tpnr_R12)=paste0('R12_',rownames(tpnr_R12))
##
tpnr=rbind(tpnr_SVM,tpnr_HM,tpnr_R12)
#write.csv(akc,file=paste0(hsvStatPath,'/HSV_rightside_txm.tpnr.csv'),quote=F)



#
#write.csv(data.frame(id=hsvRead[,1],hs,mn_r1=hm[,-1],r2=R12[,-1]),file=paste0(hsvStatPath,'/HSV_rightside_txm.svmhmr12.csv'),quote=F,row.names=F)




