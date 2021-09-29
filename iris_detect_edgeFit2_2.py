import sys,os,glob,cv2
import numpy as np

def mkdir(mkPath):
	if not os.path.exists(mkPath):
		os.makedirs(mkPath)

def getPos(lms,side,sideLms):
	leftPoint=lms[sideLms[side][0]].split('\t')
	lpPos=[int(leftPoint[0]),int(leftPoint[1])]
	rightPoint=lms[sideLms[side][1]].split('\t')
	rpPos=[int(rightPoint[0]),int(rightPoint[1])]
	return([lpPos,rpPos])

def getColor(picName,picPath,locPath,lmPath):
	##get landmarks for detail location
	lmFile=lmPath+picName[0:picName.find('.')]+'.JPG.landmarks.txt'
	lms=open(lmFile).read().split('\n')
	side=picName[(picName.find('_')+1):picName.find('.PNG')]
	sideLms = {'left':np.array([37,40])-1,'right':np.array([43,46])-1};
	#sideLmsIn = {'left':np.array([38,39,41,42])-1,'right':np.array([44,45,47,48])-1};
	lpPos,rpPos=getPos(lms,side,sideLms)
	##read picture
	picFile=picPath+picName
	img0 = cv2.imread(picFile)
	#cv2.imshow('eye', img0)
	##cut
	jumpRatio=0.4  #0.3 in face detection process
	length=rpPos[0]-lpPos[0];wgap=int(length*jumpRatio)
	[h,w,color]=img0.shape;
	upRatio=0.15;downRatio=0.1;up0=int(h*upRatio);
	#wgap=int(w*jumpRatio)
	img=img0[up0:int(h-h*downRatio),wgap:w-wgap]
	imgI=np.copy(img);
	#cv2.imshow('eye_use', imgI)
	##bring gray to 0-255
	img_gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
	img_gray0=img_gray.copy()
	#cv2.imshow('initial gray',img_gray0)
	#cv2.imshow('initial binary',closed)

	##removing pupil
	pupil=np.percentile(img_gray,15)
	ret, closed2 = cv2.threshold(img_gray,pupil,255,cv2.THRESH_BINARY_INV)
	kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))#88
	closed2 = cv2.morphologyEx(closed2, cv2.MORPH_CLOSE, kernel)
	##removing bright
	white=np.percentile(img_gray,75)
	ret, closed3 = cv2.threshold(img_gray,white,255,cv2.THRESH_BINARY)
	thresh=57;pad=8
	#cv2.imshow('removed pupil',closed2)
	mid=np.percentile(img_gray,thresh)#mid=np.mean(img_gray)##57
	ret, closed = cv2.threshold(img_gray,mid,255,cv2.THRESH_BINARY_INV)
	binary0=closed
	##fill middle
	kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (pad, pad))#88
	closed = cv2.morphologyEx(closed, cv2.MORPH_CLOSE, kernel)
	# perform a series of erosions and dilations
	closed = cv2.erode(closed, None, iterations=1)
	closed = cv2.dilate(closed, None, iterations=1)
	#cv2.imshow("filled binary", closed)
	##search edges##
	canny = cv2.Canny(closed, 254, 254)
	canny = np.uint8(np.absolute(canny))
	#cv2.imshow("edge", canny)
	[h,w,color]=img.shape;
	##split to 3 parts
	sideLmsIn={'left':np.array([38,41])-1,'right':np.array([44,47])-1};
	lpPos2,rpPos2=getPos(lms,side,sideLms)
	yCenter=int((rpPos2[0]+lpPos2[0])/2-lpPos[0]+0.3*length-jumpRatio*length)
	hdratio = 0.2;huratio=0.2;# if w/h>1.75 else 0.25
	xBottom=int(round(h*(1-hdratio)))
	xUp=int(round(h*huratio))
	##get iris edge: find first meet x left and right
	edgeL=[];edgeR=[]
	for x in range(xBottom,xUp,-1):
		for y in range(yCenter,1,-1):
			if canny[x,y]==0 and canny[x,y-1]==255:
				edgeL.append([x,y])
				img_gray[x,y]=125
				break
		for y in range(yCenter,int(w)-1):
			if canny[x,y-1]==0 and canny[x,y]==255:
				edgeR.append([x,y])
				img_gray[x,y]=125
				break

	##get new yCenter by edgeL and edgeR
	##same x, get pair y
	eLdic={edgeL[i][0]:edgeL[i][1] for i in range(0,len(edgeL))}
	eRdic={edgeR[i][0]:edgeR[i][1] for i in range(0,len(edgeR))}
	edgePair=[[eLdic[key],eRdic[key]] for key in list(set(eLdic.keys()).intersection(set(eRdic.keys())))]
	yCenter=np.array(edgePair).mean()
	##circle fit along ycenter by edges
	edges=edgeL;edges.extend(edgeR);edges=np.array(edges)
	topRatio=0.2;downRatio=0.66;##3366
	xUp=int(h*topRatio);xDown=int(h*downRatio);
	dists=[]
	for x in range(xUp,xDown+1):
		dist=0
		for i in range(0,edges.shape[0]):
			dist=dist+(edges[i,0]-float(x))**2+(edges[i,0]-yCenter)**2
		dists.append([x,dist])

	dists=np.array(dists)
	xCenter=dists[dists[:,1].argmin(),0]
	img_gray[int(xCenter),:]=255;
	img_gray[:,int(yCenter)]=255;
	
	r=(dists[:,1].min()/(xDown-xUp+1))**0.5
	##plot and get img color:H[0,179],S[0,255],V[0,255]
	imgUse=np.copy(img);
	'''
	topIn=h*0.2;downIn=h*0.8;rightIn=w*0.85;
	leftIn=w*0.15;
	'''
	######set edge used marksInside
	sideLms = {'left':np.array(range(37,43))-1,'right':np.array(range(43,49))-1};
	sideLmsIn = {'left':np.array([38,39,41,42])-1,'right':np.array([44,45,47,48])-1};
	##get landmarks for detail location
	lms=np.loadtxt(lmFile);
	marks=lms[sideLms[side],];
	jumpRatio=0.3;
	left=min(marks[:,0]);right=max(marks[:,0]);top=min(marks[:,1]);down=max(marks[:,1]);
	jumpHorizonal=int((right-left)*jumpRatio);jumpVertical=int((down-top)*(jumpRatio));
	marksIn=lms[sideLmsIn[side],];
	marksIn[:,0]=marksIn[:,0]-left+jumpHorizonal-wgap;
	marksIn[:,1]=marksIn[:,1]-top+jumpVertical-up0;
	tdJumpRatioT=0.1;tdJumpRatioD=0.05;tdJumpRatioL=0.05;tdJumpRatioR=0.05;
	#tdJumpRatioT=0;tdJumpRatioD=0;tdJumpRatioL=0;tdJumpRatioR=0.05;
	leftIn=(min(marksIn[:,0])-(right-left)*tdJumpRatioL).astype(int);
	rightIn=(max(marksIn[:,0])+(right-left)*tdJumpRatioR).astype(int);#+
	topIn=(min(marksIn[:,1])+(down-top)*tdJumpRatioT).astype(int);
	downIn=(max(marksIn[:,1])-(down-top)*tdJumpRatioD).astype(int);
	leftIn4=(min(marksIn[:,0])+(right-left)*tdJumpRatioL).astype(int);
	rightIn4=(max(marksIn[:,0])-(right-left)*tdJumpRatioR).astype(int);#+
	gray4Thresh=img_gray0[topIn:downIn,leftIn4:rightIn4]
	lightThresh=np.percentile(img_gray0[topIn:downIn,leftIn:rightIn],95)
	#print(lightThresh)
	#print(np.mean(img_gray0))
	#print(np.median(gray4Thresh))
	#print(np.mean(img_gray0)-np.median(gray4Thresh))
	r=max((rightIn-leftIn)/2,(downIn-topIn)/2)
	#print(pupil)
	#########
	#HSV=[];img_hsv = cv2.cvtColor(img,cv2.COLOR_BGR2HSV)
	for xi in range(0,h):
		for yi in range(0,w):
			dist=((xi-xCenter)**2+(yi-yCenter)**2)**0.5
			#print(dist)#
			if dist<=r  and binary0[xi,yi]==255 and closed2[xi,yi]<255:# and img_gray[xi,yi]<irisThresh:# and closed3[xi,yi]<255:
			#if dist<=r  and closed2[xi,yi]<255 and img_gray[xi,yi]<irisThresh:
				img_gray[xi,yi]=255
				if  xi>topIn and xi<downIn and yi>leftIn and yi<rightIn:
					#and dist >= r/4:
					img[xi,yi,1]=255
					#HSV.append(img_hsv[xi,yi])
				else:
					imgUse[xi,yi,:]=255
			else:
				imgUse[xi,yi,:]=255
	img_gray[topIn,:]=255;img_gray[downIn,:]=255;img_gray[:,leftIn]=255;img_gray[:,rightIn]=255;
	#cv2.imshow("gray fit", img_gray)
	#cv2.imshow("used green part", img)
	#cv2.imshow("image used", imgUse)
	###save images
	locFile=locPath+picName.replace('PNG','loc.PNG')
	cv2.imwrite(locFile,np.hstack([imgI,img,imgUse]))
	locGFile=locPath+picName.replace('PNG','loc.gray.PNG')
	cv2.imwrite(locGFile,np.hstack([img_gray0,binary0,closed2,closed,canny,img_gray]))#,closed3
	useFile=locPath+picName.replace('PNG','use.PNG')
	cv2.imwrite(useFile,imgUse)



'''
initial eye: img0
eye after cut: imgI
initial gray: img_gray0
initial target region: binary0
removed pupil region: closed2
filled target region: closed
ege of filled target region: canny
ciecle fit result of initial gray: img_gray
used green part on eye after cut: img
imaged used for extract color: imgUse
'''


#path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur'
path=sys.argv[1];
print(path)
picPath=path+'/eye/pic/';
locPath=path+'/eye/loc/';mkdir(locPath)
##get landmarks for detail location
lmPath=path+'/landmarks/'
picNames=os.listdir(picPath)
print(len(picNames))
for i,picName in enumerate(picNames):
	try:
		#print(i)
		if picName.endswith('PNG'):
			print(picName)
			getColor(picName,picPath,locPath,lmPath)
			#if i>300:
				#break
	except:
		print("error")
		continue


