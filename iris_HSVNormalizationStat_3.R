###normalization of Histgram of each color channel
path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur/eye'
library(imager)
###
locPath=paste0(path,'/loc');
locFiles=Sys.glob(paste0(locPath,'/*.loc.PNG'))
files=Sys.glob(paste0(path,'/loc/*.use.PNG'))
hsvStat=NULL
okFile=NULL
noFile=NULL
for(fi in 1:length(files)){
    imgLoc0=load.image(locFiles[fi])#0-1 for r g b
    imgDim=dim(imgLoc0);imgDim[1]=imgDim[1]/3;
    ##
    img_eye=array(0,dim=imgDim);
    img_eye[,,1,]=imgLoc0[1:imgDim[1],,1,]
    ##img_eye=cimg(img_eye)  #for plot
    R=img_eye[,,1,1];G=img_eye[,,1,2];B=img_eye[,,1,3];   
    ##plot for check
    #R=(R-min(R))/(max(R)-min(R))
    #G=(G-min(G))/(max(G)-min(G))
    #B=(B-min(B))/(max(B)-min(B))
    ##
    #img_eye[,,1,1]=R;img_eye[,,1,2]=G;img_eye[,,1,3]=B;
    ##
    #img_eye=cimg(img_eye);  #for plot
    img=load.image(files[fi])
    ##normalization
    img[,,1,1]=(img[,,1,1]-min(R))/(max(R)-min(R));
    img[,,1,2]=(img[,,1,2]-min(G))/(max(G)-min(G));
    img[,,1,3]=(img[,,1,3]-min(B))/(max(B)-min(B));
    ##RGB2HSV
    imgHsv=RGBtoHSV(img)
    ##gray for search
    imgGray=grayscale(img)
    errorRate=0.999##error for save jpg
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
###hsvStat norm save path
hsvStatPath=paste0(path,'/hsvStatNorm')
dir.create(hsvStatPath)
write.csv(hsvStat,paste0(hsvStatPath,'/hsvStat.csv'),quote=F)
