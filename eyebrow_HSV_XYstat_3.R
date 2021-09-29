library(imager)
args=commandArgs(T)
path=paste0(args[1],'/eyebrow')
th=0.7
thlevel=paste0('local.threshold',th)
#hsvxyPath=paste0(path,'/hsv_xy_',thlevel)
hsvxyStatPath=paste0(path,'/hsvxyStat_',thlevel)
#dir.create(hsvxyPath)
dir.create(hsvxyStatPath)
files=Sys.glob(paste0(path,'/filter_image/*',thlevel,'.jpg'))
hsvxyStat=NULL
for(fi in 1:length(files)){
	print(fi)
	fileName=unlist(strsplit(files[fi],'/'))
    fileName=fileName[length(fileName)]
    img=load.image(files[fi])#0-1 for r g b
    ##change to HSV
    imgHsv=RGBtoHSV(img)
    ##gray for search
    imgGray=grayscale(img)
    index=imgGray<=1*0.95 # bool matrix
    imgHsvMid=imgHsv[index]
    ##reshape
    number=length(imgHsvMid)/3
    qt=as.character(seq(0,1,0.25))
    qtName=c(paste0(c('H'),qt),paste0(c('S'),qt),paste0(c('V'),qt))
    if (number>1){
        imgHsvUse=cbind(imgHsvMid[1:number],imgHsvMid[(number+1):(number*2)],imgHsvMid[(number*2+1):(number*3)])
        #get x y
        xy=which(index, arr.ind=T)[,1:2]
        #save(imgHsvUse,xy,file=paste0(hsvxyPath,'/',fileName,'.hsv.xy.RData'))
        #####statistics
        x=xy[,1];y=xy[,2];
        xsd=sd(xy[,1]);ysd=sd(xy[,2]);
        xystat=data.frame(number,xsd,ysd,covabs=abs(cov(x,y)))
        hsvstat=as.vector(apply(imgHsvUse,2,function(x) {return(quantile(x,probs=seq(0,1,0.25)))}))
        names(hsvstat)=qtName
    }else{
        xystat=data.frame(number=0,xsd=0,ysd=0,covabs=0)
        hsvstat=rep(0,15);names(hsvstat)=qtName;
        #imgHsvUse=0
        #xy=0
        #save(imgHsvUse,xy,file=paste0(hsvxyPath,'/',fileName,'.hsv.xy.RData'))
    }
    hsvxyStat=rbind(hsvxyStat,cbind(t(hsvstat),xystat))
    rownames(hsvxyStat)[fi]=strsplit(fileName,'.PNG.')[[1]][1]
    #if (fi>10){break}
}
write.csv(hsvxyStat,paste0(hsvxyStatPath,'/hsvxyStat.csv'),col.names=NA,quote=F)
