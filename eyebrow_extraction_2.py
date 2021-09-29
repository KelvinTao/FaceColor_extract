import sys
import dlib
import matplotlib.pyplot as plt
import numpy as np
import os
import cv2

#example: python eyebrow_quantification.py eyebrow_image_file out_file

def makedir(path):
	if not os.path.exists(path):os.makedirs(path)

def getImgList(path):
	img_list=[]
	for fileName in os.listdir(path):
		if fileName.endswith(".JPG") or fileName.endswith(".jpg") or fileName.endswith(".PNG"):
			img_list.append(fileName)
	return(img_list)

def blur(Img,rate):
	r=int(Img.shape[0]*rate)
	blur1 = cv2.blur(Img,(r,r))
	#blur2 = cv2.GaussianBlur(img,(r,r),0)
	#blur3 = cv2.medianBlur(img,r)
	return(blur1)

def cutface(Img,rect):
	###False initially
	img0=np.zeros((Img.shape[0],Img.shape[1]),np.bool)
	###region in rect is kept
	img0[rect[1]:rect[1]+rect[3],rect[0]:rect[0]+rect[2]]=Img[rect[1]:rect[1]+rect[3],rect[0]:rect[0]+rect[2]]
	return(img0)

def getPointsBool(img_gray,method,thNum,rect):
	#only use the pixes in the rectangle
	pointsBool=np.zeros((img_gray.shape[0],img_gray.shape[1],thNum),bool)
	if method=='global':
		for pi in range(0,thNum):
			pointsBool[:,:,pi]=cutface(cv2.threshold(img_gray,100-(pi+1)*10,255,cv2.THRESH_BINARY_INV)[1],rect)
			##low gray kept
	elif method=='local':
		for pi in range(0,thNum):
			gray_mean=blur(img_gray,0.9)##window size is fixed
			pointsBool[:,:,pi]=cutface(img_gray<=gray_mean*(1-(pi+1)*0.1),rect)
	return(pointsBool)

def getPoints(img,pointsBool,thNum):
	pointsNum=np.zeros((1,thNum),np.uint32)
	##x,y position in original eyebrow image
	for pi in range(0,thNum):
		region=pointsBool[:,:,pi]
		pointsNum[0,pi]=region[region].shape[0]
	return(pointsNum)

def getImg(img,pointsBool,thNum):
	imgAll=[]
	for pi in range(0,thNum):
		imgC=img.copy()
		region=pointsBool[:,:,pi]
		imgC[region==False]=255##unused points to white
		imgAll.append(imgC)##RGB
	return(imgAll)

def main():
	path_out=sys.argv[1]+'/eyebrow';
	figure_path=path_out+'/pic'
	locPath=os.path.join(path_out,'loc')
	colorPath=os.path.join(path_out,'filter_image')
	#pointNumPath=os.path.join(path_out,'point_number')
	makedir(locPath);#makedir(pointNumPath);
	makedir(colorPath);
	img_list=getImgList(figure_path)
	thNum=4#num of threshold levels
	for imgN,imgName in enumerate(img_list):
		print("%s: %d"%(imgName,imgN))
		img=cv2.imread(os.path.join(figure_path,imgName))
		[h,w,color]=img.shape
		rect=(int(w/9),int(h/5),int(7*w/9),int(3*h/5))
		#imgRect=img[rect[1]:rect[1]+rect[3],rect[0]:rect[0]+rect[2]]
		#a small rectangle because we expanded the rectangle when location
		img_gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
		###get points bool by thNum threshold levels and get points number
		pointNum = np.zeros((1,thNum*2),np.uint32)
		###global
		globPointsBool=getPointsBool(img_gray,'global',thNum,rect)
		pointNum[0,0:thNum]=getPoints(img,globPointsBool,thNum)
		#imgGlobal=getImg(img,globPointsBool,thNum)
		#print((colorGlobal[0].shape))
		##local
		localPointsBool=getPointsBool(img_gray,'local',thNum,rect)
		pointNum[0,thNum:thNum*2]=getPoints(img,localPointsBool,thNum)
		imgLocal=getImg(img,localPointsBool,thNum)
		##plot
		#add used rectangle on eyebrow image
		cv2.rectangle(img,(int(rect[0]),int(rect[1])),(int(rect[0]+rect[2]),int(rect[1]+rect[3])),(0,0,255),2)
		##plot
		plt.figure(figsize=(18,18))
		##first colomn for statistics
		plt.subplot(thNum,3,1),plt.imshow(img[:,:,[2,1,0]]),plt.title("origin image")
		plt.subplot(thNum,3,4),plt.imshow(img_gray,'gray'),plt.title("GRAY")
		plt.subplot(thNum,3,7),plt.plot(pointNum[0,0:thNum],'k.-',label="global threshold"),plt.title("pixel point num."),plt.legend(loc='best')
		plt.subplot(thNum,3,10),plt.plot(pointNum[0,thNum:thNum*2],'ko--',label="local threshold"),plt.title("pixel point num."),plt.legend(loc='best')
		##plot
		for i in range(0,thNum):
			####save extrction image####
			thresh=1-(i+1)*0.1
			cv2.imwrite(os.path.join(colorPath,imgName+".local.threshold"+str(thresh)+".jpg"),imgLocal[i])
			###plot#####
			#global and local
			plt.subplot(thNum,3,(i+1)*3-1),plt.imshow(globPointsBool[:,:,i]==False,'gray')##for display
			plt.title("global threshold ({}) {}".format(100-(i+1)*10,pointNum[0,i]))
			plt.subplot(thNum,3,(i+1)*3),plt.imshow(localPointsBool[:,:,i]==False,'gray')##for display
			plt.title("local threshold ({}): {}".format(1-(i+1)*0.1,pointNum[0,i+thNum]))
		####plot to file
		outfile=imgName+'.location.jpg'
		plt.savefig(os.path.join(locPath,outfile), dpi=75)
		plt.close()
		###save points number
		#np.savetxt(os.path.join(pointNumPath,imgName+".global.thNum"+str(thNum)+".txt"),pointNum[0,0:thNum],fmt="%d")
		#np.savetxt(os.path.join(pointNumPath,imgName+".local.thNum"+str(thNum)+".txt"),pointNum[0,thNum:thNum*2],fmt="%d")

if __name__=='__main__':
	main()

