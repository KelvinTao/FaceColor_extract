library(imager)
##
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/face/'
#hsvPath=paste0(path,'/hsv')
hsvStatPath=paste0(path,'/hsvStat')
#dir.create(hsvPath);
dir.create(hsvStatPath);
files=Sys.glob(paste0(path,'/pic/*.PNG'))
###
hsvStat=NULL
for(fi in 1:length(files)){
	print(fi)
	fileName=unlist(strsplit(files[fi],'/'))
    fileName=fileName[length(fileName)]
    img=load.image(files[fi])#0-1 for r g b
    ##change to HSV
    imgHsv=RGBtoHSV(img)
    ##gray for search
    imgGray=grayscale(img)
    errorRate=0.99##error for save jpg
    #plot(imgGray)
    index=imgGray<=1*errorRate # bool matrix
    imgHsvMid=imgHsv[index]
    ##reshape
    number=length(imgHsvMid)/3
    if (number>0){
        imgHsvUse=cbind(imgHsvMid[1:number],imgHsvMid[(number+1):(number*2)],imgHsvMid[(number*2+1):(number*3)])
        #####save hsv for each iris
        #save(imgHsvUse,file=paste0(hsvPath,'/',fileName,'.hsv.RData'))
        stat=as.vector(apply(imgHsvUse,2,function(x) {return(quantile(x,probs=seq(0,1,0.25)))}))
        qt=as.character(seq(0,1,0.25))
        names(stat)=c(paste0(c('H'),qt),paste0(c('S'),qt),paste0(c('V'),qt))
        hsvStat=rbind(hsvStat,stat)
        rownames(hsvStat)[fi]=fileName
    }
}
##save stat data
hsvStat=round(hsvStat,4)
write.csv(hsvStat,paste0(hsvStatPath,'/hsvStat.csv'),quote=F)
##calculate mean of two face sides
hsvStat=read.csv(paste0(hsvStatPath,'/hsvStat.csv'),head=T,stringsAsFactors=F)
hsvStatMean=NULL
for (rn in seq(1,dim(hsvStat)[1],2)){
    hsvStatMean=rbind(hsvStatMean,apply(hsvStat[rn:(rn+1),-1],2,mean))
    id=unlist(strsplit(hsvStat[rn,1],'Õý'))[1]
    rownames(hsvStatMean)[(rn+1)/2]=id
}
write.csv(hsvStatMean,paste0(hsvStatPath,'/hsvStatMean.csv'),col.names=NA,quote=F)


