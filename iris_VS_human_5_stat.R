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

##boxplot
library(ggplot2)
library(reshape2)
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
hsvStatPath=paste0(path,'/hsvStat')
#hsvUse=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))[,c(13,3,6,9)]
hsvUse=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))[,c(13,2:10)]
names(hsvUse)[1]='Human_Read';names(hsvUse)=gsub('_mean','',names(hsvUse));
hsvUse[,1]=round(hsvUse[,1]);
###
hsv.melt <- melt(hsvUse,id=1)##id is the colomn number for melt
hsv.melt[,1]=as.character(hsv.melt[,1])
p<-ggplot(data=hsv.melt, aes(x=Human_Read,y=value))+geom_boxplot(aes(fill=variable))+
facet_wrap(~ variable, scales="free")+
theme_bw() ##gackground
#theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
#panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(p,file=paste0(hsvStatPath,'/hsv0.5MeanVShumanRead.boxplot.pdf'))
#####


###scatter plot 
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
hsvStatPath=paste0(path,'/hsvStat')
library(ggplot2)
hsvRead=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))
##HS
hsv=hsvRead[,-c(1,11,12)];
col=ncol(hsv)
hsv[,col]=round(hsv[,col])
names(hsv)[col]='Human_Read'
#hsv=hsv[(hsv[,1]<=60&hsv[,3]>0),]
names(hsv)=gsub('_mean','',names(hsv))
##
plotScatter<-function(hsv,i,j){
	p<-ggplot(data=hsv, aes(x=hsv[,i],y=hsv[,j]))+
	geom_point(aes(color=cut(Human_Read,c(0,1.5,2.5,3.5,4.5,5.5))),size =1.2)+
	scale_color_manual(name="Human_Read",values =c("(0,1.5]"='#99CCCC',"(1.5,2.5]"="#669999","(2.5,3.5]"="#6633CC","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
	labels =c("blue","blue brown","intermediate","brown",'dark brown'))+
	labs(x=names(hsv)[i],y=names(hsv)[j])+theme_bw()
	return(p)
}
######
a=colnames(hsv)
scatterPath=paste0(hsvStatPath,'/hsvScatter');
dir.create(scatterPath)
#14,17,47;25,28,58;36,39,69;
y=c(1,1,4,2,2,5,3,3,6)
x=c(4,7,7,5,8,8,6,9,9)
for(i in seq(length(x))){
	p=plotScatter(hsv,x[i],y[i])
	ggsave(p,file=paste0(scatterPath,'/',a[x[i]],'_',a[y[i]],'.scatter.pdf'))
}
#geom_point(aes(color=cut(Human_Read,c(0,2.5,3.5,4.5,5.5))),size =2)+
#scale_color_manual(name="Human_Read",values =c("(0,2.5]"="blue","(2.5,3.5]"="orange","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
#labels =c("blue","intermediate","brown",'dark brown'))+

#############
##principle component
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
hsvStatPath=paste0(path,'/hsvStat')
hsvRead=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))
pcPath=paste0(hsvStatPath,'/pc');
dir.create(pcPath)

#######3PC--hsv all
hsvUse=scale(hsvRead[,-c(1,11,12,13)])
write.csv(data.frame(id=hsvRead[,1],hsvUse,Human_Read=hsvRead[,ncol(hsvRead)]),paste0(pcPath,'/hsvscale_human.csv'),row.names=F,quote=F)
###
p=princomp(hsvUse)# 0.4778583 0.2199068 0.1804812#0.4778583 0.6977651 0.8782463
hsvPC=data.frame(id=hsvRead[,1],hsvUse,hsvpc1=p$scores[,1],hsvpc2=p$scores[,2],hsvpc3=p$scores[,3],
Human_Read=hsvRead[,ncol(hsvRead)])
write.csv(hsvPC,paste0(pcPath,'/hsvPC3.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(pcPath,'/hsvPC3.cor.csv'),quote=F)
###scatter plot
a=colnames(hsvPC)
scatterPath=paste0(hsvStatPath,'/hsvScatter');
dir.create(scatterPath)
#14,17,47;25,28,58;36,39,69;
x=c(11,11,12)
y=c(12,13,13)
for(i in seq(length(x))){
	p=plotScatter(hsvPC,x[i],y[i])
	ggsave(p,file=paste0(pcPath,'/',a[x[i]],'_',a[y[i]],'.scatter.pdf'))
}
###hsv0.25,0.5,0.75
#2,5,8;3,6,9;4,7,10;
#type='0.25';colU=c(2,5,8);
type='0.5';colU=c(3,6,9);
#type='0.75';colU=c(4,7,10);
x=c(5,5,6);y=c(6,7,7)
##
hsvUse=scale(hsvRead[,colU])
p=princomp(hsvUse)# 0.4964437 0.3248476 0.1787087#0.4964437 0.8212913 1.0000000
hsvPC=data.frame(id=hsvRead[,1],hsvUse,hsvpc1=p$scores[,1],hsvpc2=p$scores[,2],hsvpc3=p$scores[,3],
Human_Read=hsvRead[,ncol(hsvRead)])
write.csv(hsvPC,paste0(pcPath,'/hsv',type,'PC3.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(pcPath,'/hsv',type,'PC3.cor.csv'),quote=F)
a=colnames(hsvPC)
#14,17,47;25,28,58;36,39,69;
library(ggplot2)
for(i in seq(length(x))){
	p=plotScatter(hsvPC,x[i],y[i])
	ggsave(p,file=paste0(pcPath,'/',a[x[i]],'_',a[y[i]],'_',type,'.scatter.pdf'))
}
###hs0.25,0.5,0.75
#type='0.25';colU=c(2,5);
#type='0.5';colU=c(3,6);
type='0.75';colU=c(4,7);
x=c(4);y=c(5);
hsvUse=scale(hsvRead[,colU])
p=princomp(hsvUse)# 0.4964437 0.3248476 0.1787087#0.4964437 0.8212913 1.0000000
hsvPC=data.frame(id=hsvRead[,1],hsvUse,hspc1=p$scores[,1],hspc2=p$scores[,2],
Human_Read=hsvRead[,ncol(hsvRead)])
write.csv(hsvPC,paste0(pcPath,'/hs',type,'PC2.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(pcPath,'/hs',type,'PC2.cor.csv'),quote=F)
a=colnames(hsvPC)
#14,17,47;25,28,58;36,39,69;
library(ggplot2)
for(i in seq(length(x))){
	p=plotScatter(hsvPC,x[i],y[i])
	ggsave(p,file=paste0(pcPath,'/',a[x[i]],'_',a[y[i]],'_',type,'.scatter.pdf'))
}



###plot in one figure
if(F){
require(grid)
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}
p1=plotScatter(hsvPC,x[1],y[1])
p2=plotScatter(hsvPC,x[2],y[2])
p3=plotScatter(hsvPC,x[3],y[3])
#####scatter plot
file=paste0(pcPath,'/hsvPC','.scatter.jpg')
jpeg(file,width = 3400, height = 1800,res=300)
grid.newpage()  
pushViewport(viewport(layout = grid.layout(1,3))) ####将页面分成2*1矩阵
print(p1, vp = vplayout(1,1))   
print(p2, vp = vplayout(1,2)) 
print(p3, vp = vplayout(1,3))
dev.off()
}