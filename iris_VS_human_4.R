##calculate mean of two eyes
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
#args=commandArgs(T)
#path=paste0(args[1],'/eye')
hsvStatPath=paste0(path,'/hsvStat')
hsvStat=read.csv(paste0(hsvStatPath,'/hsvStat.csv'),stringsAsFactors=F)
###
id=gsub('Õý.*','',hsvStat[,1])
LR=gsub('.use.PNG','',gsub('.*eye_','',hsvStat[,1]))
hsvStat=cbind(id,LR,hsvStat[,-1]);
write.csv(hsvStat,file=paste0(hsvStatPath,'/hsvStat2.csv'),row.names=F,quote=F)
###
hsvStatL=hsvStat[hsvStat$LR=='left',]
hsvStatR=hsvStat[hsvStat$LR=='right',]
hsvStatPair=merge(hsvStatL,hsvStatR,by.x=1,by.y=1)[,-c(2,12)]
names(hsvStatPair)=gsub('.y','_right',gsub('.x','_left',names(hsvStatPair)))
##single eye
hsvStatLsingle=hsvStatL[is.na(match(hsvStatL[,1],hsvStatR[,1])),]
if(nrow(hsvStatLsingle)>0){
	hsvStatLsingle=cbind(hsvStatLsingle[,-2],hsvStatLsingle[,-c(1,2)]);
	names(hsvStatLsingle)=names(hsvStatPair);
	hsvStatPair=rbind(hsvStatPair,hsvStatLsingle)
}
hsvStatRsingle=hsvStatR[is.na(match(hsvStatR[,1],hsvStatL[,1])),]
if(nrow(hsvStatRsingle)>0){
	hsvStatRsingle=cbind(hsvStatRsingle[,1],hsvStatRsingle[,-c(1,2)],hsvStatRsingle[,-c(1,2)])
    names(hsvStatRsingle)=names(hsvStatPair);
    hsvStatPair=rbind(hsvStatPair,hsvStatRsingle)
}
hsvStat=hsvStatPair
write.csv(hsvStat,file=paste0(hsvStatPath,'/hsvStatReplicateLorRonEnd.csv'),row.names=F,quote=F)
####get means
meanRow<-function(row){return(apply(data.frame(t(row[1:9]),t(row[10:18])),1,mean))}
hsvStatMean=NULL
for(i in 1:nrow(hsvStat)){hsvStatMean=rbind(hsvStatMean,meanRow(hsvStat[i,-1]))}
colnames(hsvStatMean)=gsub('left','mean',colnames(hsvStatMean));
###
hsvStatMeanAll=cbind(hsvStat,hsvStatMean)
write.csv(hsvStatMeanAll,paste0(hsvStatPath,'/hsvStatMeanAll.csv'),row.names=F,quote=F)
hsvStatMean=data.frame(id=hsvStat$id,hsvStatMean)
write.csv(hsvStatMean,paste0(hsvStatPath,'/hsvStatMean.csv'),row.names=F,quote=F)
##############



#comparision with humanread
hsvStatMean=read.csv(paste0(hsvStatPath,'/hsvStatMean.csv'),stringsAsFactors=F)
library(readxl)
##human read
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/human'
male_eye=read_excel(paste0(path,'/gongan_XJW_eye_hair_color.xlsx'),'male_eye')
female_eye=read_excel(paste0(path,'/gongan_XJW_eye_hair_color.xlsx'),'female_eye')
read_eye=rbind(male_eye,female_eye)[,c(1,4,7,8)]##mean
###merge
hsvRead=merge(hsvStatMean,read_eye,by.x=1,by.y=1)
write.csv(hsvRead,paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'),row.names=F,quote=F)
##correlation
corr=cor(hsvRead[,-1])
write.csv(corr,paste0(hsvStatPath,'/hsvStatMeanVShumanRead_cor.csv'),quote=F)

###scatter plot 
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
hsvStatPath=paste0(path,'/hsvStat')
library(ggplot2)
hsvRead=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))
##HS
hsv=hsvRead[,c(3,6,13)];
#hsv=hsvRead[,c(2,5,13)];
hsv[,3]=round(hsv[,3])
names(hsv)[3]='Human_Read'
hsv=hsv[(hsv[,1]<=60&hsv[,3]>0),]
names(hsv)=gsub('_mean','',names(hsv))
p<-ggplot(data=hsv, aes(x=S0.5,y=H0.5))+
geom_point(aes(color=cut(Human_Read,c(0,2.5,3.5,4.5,5.5))),size =1)+
scale_color_manual(name="Human_Read",values =c("(0,2.5]"="blue","(2.5,3.5]"="orange","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
labels =c("blue","intermediate","brown",'dark brown'))+
scale_x_continuous(limits = c(0.15,0.65))+scale_y_continuous(limits = c(10,42))+
theme_bw()    
ggsave(p,file=paste0(hsvStatPath,'/humanVShs0.5.scatter.pdf'))
###HV
hsv=hsvRead[,c(3,9,13)];
hsv[,3]=round(hsv[,3])
names(hsv)[3]='Human_Read'
#hsv=hsv[(hsv[,1]<=60&hsv[,3]>0&hsv[,2]>0.2),]
hsv=hsv[(hsv[,1]<=60&hsv[,3]>0),]
names(hsv)=gsub('_mean','',names(hsv))
###plot
p<-ggplot(data=hsv, aes(x=V0.5,y=H0.5))+
geom_point(aes(color=cut(Human_Read,c(0,2.5,3.5,4.5,5.5))),size =1)+
scale_color_manual(name="Human_Read",values =c("(0,2.5]"="blue","(2.5,3.5]"="orange","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
labels =c("blue","intermediate","brown",'dark brown'))+
scale_x_continuous(limits = c(0,0.3))+scale_y_continuous(limits = c(10,42))+
theme_bw()    
ggsave(p,file=paste0(hsvStatPath,'/humanVShv0.5.scatter.pdf'))
##SV
hsv=hsvRead[,c(6,9,13)];
hsv[,3]=round(hsv[,3])
names(hsv)[3]='Human_Read'
#hsv=hsv[(hsv[,1]<=60&hsv[,3]>0&hsv[,2]>0.2),]
hsv=hsv[(hsv[,3]>0),]
names(hsv)=gsub('_mean','',names(hsv))
###plot
p<-ggplot(data=hsv, aes(x=S0.5,y=V0.5))+
geom_point(aes(color=cut(Human_Read,c(0,2.5,3.5,4.5,5.5))),size =1)+
scale_color_manual(name="Human_Read",values =c("(0,2.5]"="blue","(2.5,3.5]"="orange","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
labels =c("blue","intermediate","brown",'dark brown'))+
scale_x_continuous(limits = c(0.15,0.65))+scale_y_continuous(limits = c(0,0.3))+
theme_bw()    
ggsave(p,file=paste0(hsvStatPath,'/humanVSsv0.5.scatter.pdf'))

##boxplot
library(ggplot2)
library(reshape2)
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
hsvStatPath=paste0(path,'/hsvStat')
hsvUse=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))[,c(13,3,6,9)]
names(hsvUse)[1]='Human_Read';names(hsvUse)=gsub('_mean','',names(hsvUse));
hsvUse[,1]=round(hsvUse[,1]);
##scale to 0-1
if(F){
for (j in 2:ncol(hsvUse)){
    low=min(hsvUse[,j]);high=max(hsvUse[,j]);range=high-low;
    for (i in 1:nrow(hsvUse))hsvUse[i,j]=(hsvUse[i,j]-low)/range
}
}
###
#hsvUse[,2:4]=scale(hsvUse[,2:4])
hsv.melt <- melt(hsvUse,id=1)##id is the colomn number for melt
hsv.melt[,1]=as.character(hsv.melt[,1])
p<-ggplot(data=hsv.melt, aes(x=Human_Read,y=value))+geom_boxplot(aes(fill=variable))+
facet_wrap(~ variable, scales="free")+
theme_bw() ##gackgraound
#theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
#panel.background = element_blank(),axis.line = element_line(colour = "black"))

#ggsave(p,file=paste0(hsvStatPath,'/hsv0.5MeanVShumanRead.boxplot.pdf'))







#############
##principle component
##calculate mean of two eyes
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
#args=commandArgs(T)
#path=paste0(args[1],'/eye')
hsvStatPath=paste0(path,'/hsvStat')
hsvRead=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))
col=ncol(hsvRead)
#######
#hsvUse=scale(hsvRead[,-c(1,col-2,col-1,col)])
#p=princomp(hsvUse)#0.450585 0.2285813 0.1940641 0.08343174;0.450585 0.6791663 0.8732304 0.95666214
#pc1=p$scores[,1];pc2=p$scores[,2];pc3=p$scores[,3];pc4=p$scores[,4];
#hsvPC=data.frame(id=hsvRead[,1],hsvUse,pc1,pc2,pc3,pc4,hsvRead[,c(col-2,col-1,col)])
#write.csv(hsvPC,paste0(hsvStatPath,'/hsv0.25-0.75.PC4.csv'),row.names=F,quote=F)
#write.csv(cor(hsvPC[,-1]),paste0(hsvStatPath,'/hsv0.25-0.75.PC4.cor.csv'),quote=F)
###3PC--hsv
hsvUse=scale(hsvRead[,c(3,6,9)])
p=princomp(hsvUse)#0.5084335 0.2643515 0.2272150
pc1=p$scores[,1];pc2=p$scores[,2];pc3=p$scores[,3]
hsvPC=data.frame(id=hsvRead[,1],hsvUse,hsvpc1=pc1,hsvpc2=pc2,hsvpc3=pc3,class5=kmeans(data.frame(pc1,pc2,pc3),5)$cluster,hsvRead[,c(col-2,col-1,col)])
write.csv(hsvPC,paste0(hsvStatPath,'/hsv0.5PC3.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(hsvStatPath,'/hsv0.5PC3.cor.csv'),quote=F)
##pcs scatter plot
library(ggplot2)
pc=data.frame(hsvpc1=pc1,hsvpc2=pc2,hsvpc3=pc3,Human_read=round(hsvRead[,col]))
p<-ggplot(data=pc, aes(x=hsvpc1,y=hsvpc2))+
geom_point(aes(color=cut(Human_read,c(0,2.5,3.5,4.5,5.5))),size =1,stat="identity")+
scale_color_manual(name="Human_Read",values =c("(0,2.5]"="blue","(2.5,3.5]"="orange","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
labels =c("blue","intermediate","brown",'dark brown'))+
scale_x_continuous(limits = c(-5.1,5.1))+scale_y_continuous(limits = c(-2.6,3.8))+
theme_bw()
ggsave(p,file=paste0(hsvStatPath,'/hsv0.5MeanVShumanRead.pc1pc2.scatter.pdf'))

p<-ggplot(data=pc, aes(x=hsvpc2,y=hsvpc3))+
geom_point(aes(color=cut(Human_read,c(0,2.5,3.5,4.5,5.5))),size =1,stat="identity")+
scale_color_manual(name="Human_Read",values =c("(0,2.5]"="blue","(2.5,3.5]"="orange","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
labels =c("blue","intermediate","brown",'dark brown'))+
scale_x_continuous(limits = c(-2.6,3.8))+
theme_bw()
ggsave(p,file=paste0(hsvStatPath,'/hsv0.5MeanVShumanRead.pc2pc3.scatter.pdf'))




###2PC--hs
hsvUse=scale(hsvRead[,c(3,6)])
p=princomp(hsvUse)
#Proportion of Variance 0.6157495 0.3842505
#          Comp.1 Comp.2
#H0.5_mean -0.707 -0.707
#S0.5_mean  0.707 -0.707
pc1=p$scores[,1];pc2=p$scores[,2]
hsvPC=data.frame(id=hsvRead[,1],hsvUse,hspc1=pc1,hspc2=pc2,hsvRead[,c(col-2,col-1,col)])
write.csv(hsvPC,paste0(hsvStatPath,'/hs0.5PC2.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(hsvStatPath,'/hs0.5PC2.cor.csv'),quote=F)
##pcs scatter plot
library(ggplot2)
pc=data.frame(hspc1=pc1,hspc2=pc2,Human_read=round(hsvRead[,col]))
p<-ggplot(data=pc, aes(x=hspc1,y=hspc2))+
geom_point(aes(color=cut(Human_read,c(0,2.5,3.5,4.5,5.5))),size =1,stat="identity")+
scale_color_manual(name="Human_Read",values =c("(0,2.5]"="blue","(2.5,3.5]"="orange","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
labels =c("blue","intermediate","brown",'dark brown'))+
scale_x_continuous(limits = c(-5,2.5))+scale_y_continuous(limits = c(-3,3))+
theme_bw()
ggsave(p,file=paste0(hsvStatPath,'/hs0.5MeanVShumanRead.pc1pc2.scatter.pdf'))

###2PC--hv
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
hsvStatPath=paste0(path,'/hsvStat')
hsvRead=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))
col=ncol(hsvRead)
hsvUse=scale(hsvRead[,c(6,9)])
p=princomp(hsvUse)#0.6591589 0.3408411
pc1=p$scores[,1];pc2=p$scores[,2]
hsvPC=data.frame(id=hsvRead[,1],hsvUse,hvpc1=pc1,hvpc2=pc2,hsvRead[,c(col-2,col-1,col)])
write.csv(hsvPC,paste0(hsvStatPath,'/hv0.5PC2.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(hsvStatPath,'/hv0.5PC2.cor.csv'),quote=F)
##pcs scatter plot
library(ggplot2)
pc=data.frame(hvpc1=pc1,hvpc2=pc2,Human_read=round(hsvRead[,col]))
p<-ggplot(data=pc, aes(x=hvpc1,y=hvpc2))+
geom_point(aes(color=cut(Human_read,c(0,2.5,3.5,4.5,5.5))),size =1,stat="identity")+
scale_color_manual(name="Human_Read",values =c("(0,2.5]"="blue","(2.5,3.5]"="orange","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
labels =c("blue","intermediate","brown",'dark brown'))+
#scale_x_continuous(limits = c(-5,2.5))+scale_y_continuous(limits = c(-3,3))+
theme_bw()
ggsave(p,file=paste0(hsvStatPath,'/hv0.5MeanVShumanRead.pc1pc2.scatter.pdf'))





##independent component analysis
##calculate mean of two eyes
if(F){
library(fastICA)
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
hsvStatPath=paste0(path,'/hsvStat')
hsvRead=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))
col=ncol(hsvRead)
###3iC--hsv
hsvUse=scale(hsvRead[,c(3,6,9)])
p=fastICA(hsvUse,n.comp=3)#0.5084335 0.2643515 0.2272150
pc1=p$S[,1];pc2=p$S[,2];pc3=p$S[,3]
hsvPC=data.frame(id=hsvRead[,1],hsvUse,hsvic1=pc1,hsvic2=pc2,hsvic3=pc3,hsvicclass5=kmeans(data.frame(pc1,pc2,pc3),5)$cluster,hsvRead[,c(col-2,col-1,col)])
write.csv(hsvPC,paste0(hsvStatPath,'/hsv0.5IC3.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(hsvStatPath,'/hsv0.5IC3.cor.csv'),quote=F)
###2iC--hs
hsvUse=scale(hsvRead[,c(3,6)])
p=fastICA(hsvUse,n.comp=2)#
pc1=p$S[,1];pc2=p$S[,2]
hsvPC=data.frame(id=hsvRead[,1],hsvUse,hsic1=pc1,hsic2=pc2,hsvRead[,c(col-2,col-1,col)])
write.csv(hsvPC,paste0(hsvStatPath,'/hs0.5IC2.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(hsvStatPath,'/hs0.5IC2.cor.csv'),quote=F)
}



