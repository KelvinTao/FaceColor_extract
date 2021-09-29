
##calaulate mean
part='eyebrow';th=0.7;
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur'
eyebrowPath=paste0(path,'/',part)
###
ratioMat=read.csv(paste0(eyebrowPath,'/hsvxyStat_local.threshold',th,'/densityByNumberEyelength.csv'),head=T,stringsAsFactors=F)
sampleNames=ratioMat[,1];ratioMat=ratioMat[,-1];
ratioMean=NULL
for (i in seq(1:(length(ratioMat)[1]/2))){
    matI=i*2-1;
    #meanValue=apply(ratioMat[matI:(matI+1)],2,mean)
    meanValue=mean(ratioMat[matI:(matI+1)])
    ratioMean=rbind(ratioMean,meanValue)
    rownames(ratioMean)[i]=substr(sampleNames[matI],1,nchar(sampleNames[matI])-5)
}
write.csv(ratioMean,paste0(eyebrowPath,'/hsvxyStat_local.threshold',th,'/densityByNumberEyelength.mean.csv'),quote=F)


###compare with human read
library(readxl)
path0='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/human'
pathStat=paste0(path0,'/eyebrowDensitySum.xlsx')
##human
human=read_excel(pathStat,'human')
##computer
comp=read.csv(paste0(eyebrowPath,'/hsvxyStat_local.threshold',th,'/densityByNumberEyelength.mean.csv'))
comp[,1]=gsub('Õý.JPG','',comp[,1])
##merge
sum=merge(comp,human,by.x=1,by.y=1)
sum=data.frame(sum,pz_mean=apply(sum[,(dim(sum)[2]-1):dim(sum)[2]],1,mean))
##correlation
cortable=cor(sum[,-1])
##boxplot
library(ggplot2)
sum=sum[,c(2,3)];sum=sum[sum[,2]!=0,];sum[,2]=as.character(sum[,2])
names(sum)=c('eyebrow_extraction_density','human_read')
p<-ggplot(data=sum,aes(x=human_read,y=eyebrow_extraction_density))+
geom_boxplot(aes(group=human_read,fill=human_read))+theme_bw()
ggsave(p,file=paste0(eyebrowPath,'/hsvxyStat_local.threshold',th,'/humanVSdensityByNumberEyelength.mean.pdf'))
