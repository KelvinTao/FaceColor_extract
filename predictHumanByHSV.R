library(e1071)
##
LOO_SVM<-function(d){
    rowN=nrow(d)
    pred=rep(0,rowN)
    for (i in 1:rowN){
        du=d[-i,]
        s=svm(Human~.,data=du)
        pred[i]=predict(s,d[i,-ncol(d)])
    }
    return(pred)
}

#
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye/hsvStat'
###
file=paste0(path,'/HSVPC3Human.csv')
data=read.csv(file)
pHSV=LOO_SVM(data[,c(2,3,4,10)])
com=data.frame(data,predHuman=pHSV)
write.csv(com,file=paste0(path,'/HSVPC3_loosvm_predHuman.csv'),quote=F,row.names=F)
write.csv(cor(com[,-1]),file=paste0(path,'/HSVPC3_loosvm_predHuman.cor.csv'),quote=F)
###

d=read.csv(file=paste0(path,'/HSVPC3_loosvm_predHuman.csv'))