library(imager)
##
#args=commandArgs(T)
#path=paste0(args[1],'/eye')
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
#hsvPath=paste0(path,'/hsv')
hsvStatPath=paste0(path,'/hsvStat')
dir.create(hsvStatPath);
files=Sys.glob(paste0(path,'/loc/*.use.PNG'))
###
hsvStat=NULL
okFile=NULL
noFile=NULL
for(fi in 1:length(files)){
	#fileName=unlist(strsplit(files[fi],'/'))
    #fileName=fileName[length(fileName)]
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
        print(fi)
        imgHsvUse=cbind(imgHsvMid[1:number],imgHsvMid[(number+1):(number*2)],imgHsvMid[(number*2+1):(number*3)])
        #####save hsv for each iris
        #save(imgHsvUse,file=paste0(hsvPath,'/',fileName,'.hsv.RData'))
        stat=as.vector(apply(imgHsvUse,2,function(x) {return(quantile(x,probs=seq(0.25,0.75,0.25)))}))
        qt=as.character(seq(0.25,0.75,0.25))
        names(stat)=c(paste0(c('H'),qt),paste0(c('S'),qt),paste0(c('V'),qt))
        #stat=stat[-c(1,5,6,10,11,15)]
        hsvStat=rbind(hsvStat,stat)
        okFile=c(okFile,fi)
        #rownames(hsvStat)[fi]=fileName
    }else{
        noFile=c(noFile,fi)
        print(files[fi])
        print(number)
    }
}
fileNames=gsub(paste0(path,'/loc/'),'',files)[okFile]
rownames(hsvStat)=fileNames
noFile=gsub(paste0(path,'/loc/'),'',files)[noFile]
##save stat data
hsvStat=round(hsvStat,4)
write.csv(hsvStat,paste0(hsvStatPath,'/hsvStat.csv'),quote=F)


