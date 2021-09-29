###
distance<-function(p1,p2){
    return(sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2))
}

args=commandArgs(T)
path=args[1]
eyebrowPath=paste0(path,'/eyebrow')
lmFiles=Sys.glob(paste0(path,'/landmarks/*.txt'))
####
th=0.7
pointsNum=read.csv(paste0(eyebrowPath,'/hsvxyStat_local.threshold',th,'/hsvxyStat.csv'),head=T,stringsAsFactors=F)[,c(1,17)]
ratioMat=NULL
for (i in 1:dim(pointsNum)[1]){
	###get sample name
	s=pointsNum[i,1]
	sample=strsplit(s,'.eyebrow')[[1]][1]
	##get landmarks
    lmFile=paste0(path,'/landmarks/',sample,'.landmarks.txt')
    lms=read.table(lmFile,sep='\t',head=F,stringsAsFactors=F)
    ##calculate left eye length,37 40 right eye length 43 46
    if (gregexpr("left", s)[[1]][1]>0){
        d=distance(lms[37,],lms[40,])
        side='left'
    }else if(gregexpr("right", s)[[1]][1]>0){
        d=distance(lms[43,],lms[46,])
        side='right'
    }
    ratioMat=rbind(ratioMat,pointsNum[i,2]/d/d)
    rownames(ratioMat)[i]=paste0(sample,'_',side)
}
ratioMat=round(ratioMat,4)
write.csv(ratioMat,paste0(eyebrowPath,'/hsvxyStat_local.threshold',th,'/densityByNumberEyelength.csv'),quote=F)
