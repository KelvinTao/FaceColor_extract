###
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur'
read_eye=read.csv(paste0(path,'/human/gongan_XJW_eye_color.csv'))
###extraction data, median
hsvStatPath=paste0(path,'/eye/hsvStat')
#hsv=read.csv(paste0(hsvStatPath,'/HSVPC3LRHuman.csv'))[,c(1,3,6,9,12,15,18)]
hsv=read.csv(paste0(hsvStatPath,'/HSVPC3LRHuman.csv'))[,c(1,11:19)]
##merge
hsvRead=na.omit(merge(read_eye,hsv,by.x=1,by.y=1))
row0=which(hsvRead[,2]==0);read0=hsvRead[row0,];read0[,2]=read0[,3];read0[,4]=read0[,5];
##
names(hsvRead)[1]='id'
names(hsvRead)=gsub('0.5','',names(hsvRead))
names(hsvRead)=gsub('txm','rater1',names(hsvRead))
names(hsvRead)=gsub('zw','rater2',names(hsvRead))
## by rightside
#hr=hsvRead[,c(1,3,6:14)]
hr=hsvRead[,c(1,3,5,7,10,13)]
#4 classes
class=hr[,c(2,3)]-1;
class[class==0]=1;
hr[,c(2,3)]=class;
##SVM LOO prediction
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
hrPred=data.frame(hr[,1:3],svm_r1=LOO_SVM(hr[,-c(1,3)]))
write.csv(hrPred,paste0(hsvStatPath,'/HSV_R1_svmPred4classes.csv'),quote=F,row.names=F)


##HSV
#5 class 0.65
#4 class 0.68
#3 class 0.64

##HSV statistics
##5 class, 0.68
##4 class, 0.69
#3 class, 0.65

##prepare for DL
#human=hsvRead[,1:5]
write.csv(human,paste0(path,'/human/eye_color_use.csv'),quote=F,row.names=F)



