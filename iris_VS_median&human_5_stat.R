##calculate mean of two eyes
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
#args=commandArgs(T)
#path=paste0(args[1],'/eye')
hsvStatPath=paste0(path,'/hsvStat')
#hsvStatPath=paste0(path,'/hsvStatNorm')
#hsvStatPath=paste0(path,'/hsvStatNormMean')
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
##############

#############

#comparison with humanread
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


#####scatter plot 
###HSV median vs Human1, Human2, Human
#path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
#hsvStatPath=paste0(path,'/hsvStat')
hsvRead=read.csv(paste0(hsvStatPath,'/hsvStatMeanVShumanRead.csv'))

##HSV and artificial classes
####scatter plot function
library(ggplot2)
plotScatter<-function(hsv,i,j){
	p<-ggplot(data=hsv, aes(x=hsv[,i],y=hsv[,j]))+
	geom_point(aes(color=cut(Human,c(0,1.5,2.5,3.5,4.5,5.5))),size =1.2)+
	scale_color_manual(name="Human",values =c("(0,1.5]"='#99CCCC',"(1.5,2.5]"="#669999","(2.5,3.5]"="#6633CC","(3.5,4.5]"="brown","(4.5,5.5]"="black"),
	labels =c("blue","blue brown","intermediate","brown",'dark brown'))+
	labs(x=names(hsv)[i],y=names(hsv)[j])+theme_bw()
	return(p)
}

###HSV and principle components, 0.5331022 0.2845351 0.1823627
hsvPC1=hsvRead[,c(1,3,6,9,11:13)];
names(hsvPC1)=c('id','H','S','V','Human1','Human2','Human')
p=princomp(scale(hsvPC1[,2:4]))
hsvPC=data.frame(hsvPC1[,1:4],PC1=p$scores[,1],PC2=p$scores[,2],
	PC3=p$scores[,3],hsvPC1[,5:7])
write.csv(hsvPC,paste0(hsvStatPath,'/HSVPC3Human.csv'),row.names=F,quote=F)
write.csv(cor(hsvPC[,-1]),paste0(hsvStatPath,'/HSVPC3Human.cor.csv'),quote=F)


###ordinal
###HSV ordinal classification
getOrdinal<-function(Hord,q){
quants=c(0,q[1],sum(q[1:2]),sum(q[1:3]),sum(q[1:4]),sum(q[1:5]))
threshes=quantile(Hord,probs=quants)
threshes[1]=-0.001
for (i in 2:length(threshes)){
	ci=i-1
	Hord[Hord>threshes[ci]&Hord<=threshes[i]]=ci
}
return(Hord)
}
#####

q=c(6,56,49,352,241)/704  ###number in each group
Hord=getOrdinal(hsvPC$H,q)
Sord=getOrdinal(hsvPC$S,q)
Vord=getOrdinal(hsvPC$V,q)
####
hsvPCc=data.frame(hsvPC[,c(1,8:10,2:7)],Hord,Sord,Vord)
write.csv(hsvPCc,paste0(hsvStatPath,'/HSVPC3HumanOrd.csv'),row.names=F,quote=F)
write.csv(cor(hsvPCc[,-1]),paste0(hsvStatPath,'/HSVPC3HumanOrd.cor.csv'),quote=F)



######plot scatter
hsvPC$Human=round(hsvPC$Human)
scatterPath=paste0(hsvStatPath,'/hsvScatter');
dir.create(scatterPath)
hsvPC=hsvPC[(hsvPC$H<=60&hsvPC$Human>0),]
hsvPC=hsvPC[,-1]
y=c(1,1,2,5,6,6)
x=c(2,3,3,4,4,5)
a=colnames(hsvPC)
for(i in seq(length(x))){
	p=plotScatter(hsvPC,x[i],y[i])
	ggsave(p,file=paste0(scatterPath,'/',a[x[i]],'_',a[y[i]],'.scatter.pdf'))
}

##boxplot and hand save
library(ggplot2)
library(reshape2)
hsvPC$Human=round(hsvPC$Human)
hsv=hsvPC[,c(9,1:3)]
hsvP <- melt(hsv,id=1)##id is the colomn number for melt
hsvP[,1]=as.character(hsvP[,1])
p<-ggplot(data=hsvP, aes(x=Human,y=value))+
geom_boxplot(aes(fill=variable))+facet_wrap(~ variable, scales="free")+
theme_bw()
#ggsave(p,file=paste0(hsvStatPath,'/HSVvsHuman.boxplot.pdf'))






