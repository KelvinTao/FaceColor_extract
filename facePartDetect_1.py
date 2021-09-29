#default python
######necessary module
import sys,os,dlib,glob,cv2
import numpy as np
from skimage import io,transform,draw
##for add numer on image
from PIL import Image,ImageDraw
######
##for calculate time
import time

###for import by other script
#sys.path.append('/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/script')

def shrinkImg(img0,shrink):
    img=np.uint8(np.around(transform.resize(img0,(round(img0.shape[0]*shrink),
        round(img0.shape[1]*shrink)),mode='constant')*255))
    return(img)

def getRectLandmark(img,detector,predictor):
    #upsample the image 1 time
    dets = detector(img,1)
    for d in dets:
        #get rectangle line
        rect=[d.top(),d.bottom(),d.left(),d.right()]
        # get the landmarks/parts for the face in box d
        shape = predictor(img, d)  #np.mat(shape.parts())
        landmarks=np.mat([[points.x,points.y] for points in shape.parts()])
    return([rect,landmarks])

def getMarksImg(img,rect,landmarks):
    ##draw face rectangle on the image
    rr,cc = draw.polygon_perimeter([rect[0],rect[0],rect[1],rect[1]],[rect[3],rect[2],rect[2],rect[3]])
    img[rr-1,cc-1] = (255, 0, 0)
    ##draw landmarks circle on the image
    for rc in landmarks:
       lr,lc = draw.circle_perimeter(rc[0,1],rc[0,0],2)##y,x,radius
       img[lr,lc] = (0, 255, 0)
    return(img)

def drawRect(img,rect):
    ##draw face rectangle on the image
    rr,cc = draw.polygon_perimeter([rect[0],rect[0],rect[1],rect[1]],[rect[3],rect[2],rect[2],rect[3]])
    img[rr-1,cc-1] = (255, 0, 0)
    return(img)

def drawLandmarks(img,landmarks):
    ##draw landmarks circle on the image
    for rc in landmarks:
       lr,lc = draw.circle_perimeter(rc[0,1],rc[0,0],2)##y,x,radius
       img[lr,lc] = (0, 255, 0)
    return(img)

def addNumOnImg(markFile,landmarks):
    ##add landmarks NO at the corresponding point
    imgNO = Image.open(markFile)
    imgdraw = ImageDraw.Draw(imgNO)
    for NO,rc in enumerate(landmarks):
        NO=NO+1 #from 1
        imgdraw.text((rc[0,0],rc[0,1]),str(NO),fill=(255,0,0))
    return(imgNO)

def cut_face(img0,landmarks,shrink):
    oriMarks=np.around(landmarks/shrink).astype(int)
    ##for face: 40,41,1,2,3 left part; 46,47,15,14,13,right part
    ##left side:
    topL=oriMarks[1,1];downL=oriMarks[3,1];
    leftL=oriMarks[3,0]+10;rightL=oriMarks[40,0];
    faceL=img0[topL:downL,leftL:rightL,];
    ##right side:
    top=oriMarks[15,1];down=oriMarks[13,1];
    left=oriMarks[47,0];right=oriMarks[13,0]-10;
    faceR=img0[top:down,left:right,]
    return([[faceL,faceR],[[int(topL*shrink),int(downL*shrink),int(leftL*shrink),int(rightL*shrink)],
        [int(top*shrink),int(down*shrink),int(left*shrink),int(right*shrink)]]])

def cut(img0,landmarks,shrink,partIndex,part):
    ##original position
    oriMarks=np.around(landmarks/shrink).astype(int)
    ##get corresponding landmarks
    index=partIndex[part].split('-')
    marks=oriMarks[int(index[0]):int(index[1]),]
    top=np.amin(marks[:,1]);down=np.amax(marks[:,1])
    left=np.amin(marks[:,0]);right=np.amax(marks[:,0])
    ###jump
    jumpRatio=0.15  ###need another jump methods
    if part.split('_')[0]=='eye':
        jumpRatio=0.3
    jumpVertical=int((down-top)*jumpRatio);
    jumpHorizonal=int((right-left)*jumpRatio);
    ##original cut part
    oriCut=img0[top-jumpVertical:down+jumpVertical,
        left-jumpHorizonal:right+jumpHorizonal,]
    return([oriCut,[int(top*shrink),int(down*shrink),int(left*shrink),int(right*shrink)]])
    ###a.astype(np.int)
def cutHair(img0,rect,shrink):
    ##top down left right
    oriRect=[round(pos/shrink) for pos in rect]
    width=oriRect[1]-oriRect[0]
    upRatio=1;sideRadio=0.5;
    jumpUp=width*upRatio;jumpSide=width*sideRadio;
    hairRect=[oriRect[0]-jumpUp,np.mean(oriRect[0:2]),
        oriRect[2]-jumpSide,oriRect[3]+jumpSide]
    hairRect=[int(p) for p in hairRect]
    imgUse=img0.copy()###attention
    hairImg=imgUse[hairRect[0]:hairRect[1],hairRect[2]:hairRect[3],]
    ##incircle region to white
    radius=int(width/2)
    r2=pow(radius,2)
    x,y=[int(np.mean(oriRect[0:2])-hairRect[0]),int(np.mean(oriRect[2:4])-hairRect[2])]
    for r in range(x-radius,x):
        for c in range(y-radius,y+radius):
            if pow((r-x),2)+pow((c-y),2)<=r2:
                hairImg[r,c,]=[255,255,255]
    return(hairImg)

def mkdir(mkPath):
    if not os.path.exists(mkPath):
        os.makedirs(mkPath)

def main():
    ###
    landmarksRef=sys.argv[1]
    path=sys.argv[2]
    suffix=sys.argv[3]
    #path='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur'
    #landmarksRef='/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/script/shape_predictor_68_face_landmarks.dat'
    mkdir(path+'/landmarks')
    mkdir(path+'/mark')
    mkdir(path+'/eye/pic');
    mkdir(path+'/eyebrow/pic');
    mkdir(path+'/face/pic');
    #######
    oriImgPath=path+'/pic'
    ##instantiation
    detector = dlib.get_frontal_face_detector()
    predictor = dlib.shape_predictor(landmarksRef)
    ###part and index
    ##for eyebrow: 17-21, left part; 22-26, right part;  from index 0
    ##for eye: 36-41,left part; 42-47, right part; from index 0
    ##for face: 40,41,1,2,3 left part; 46,47,15,14,13,right part
    partIndex={'eyebrow_left':'17-22','eyebrow_right':'22-27',
        'eye_left':'36-42','eye_right':'42-48'}
    #####start
    imgFiles=glob.glob(os.path.join(oriImgPath, "*"+suffix))
    shrink=0.3#1for unclear pictures,0.3 for clear pictures
    for n,f in enumerate(imgFiles):
       try:
            #if n<90:
            #    continue
            print("Processing file: {}".format(f))
            time_start=time.time()
            #####start
            fName=f.replace(oriImgPath+'/','')#.replace('.JPG','')
            #img0 = io.imread(f)
            img0 = cv2.imread(oriImgpath+'/'+fName)
            img=shrinkImg(img0,shrink)
            rect,landmarks=getRectLandmark(img,detector,predictor)
            ###landmarks save
            oriMarks=np.around(landmarks/shrink).astype(int)
            oriMarksFile=(path+'/landmarks/{}.landmarks.txt').format(fName)
            np.savetxt(oriMarksFile,oriMarks,delimiter='\t',fmt='%d')
            ###hair cut 
            #hairCut=cutHair(img0,rect,shrink)
            #hairFile=(path+'/hair/{}.hair.PNG').format(fName)
            #io.imsave(hairFile,hairCut)
            ## cut regions
            for part in partIndex:
                partImg=cut(img0,landmarks,shrink,partIndex,part)
                #mkdir(path+'/'+part.split('_')[0])
                cutFile=(path+'/{}/pic/{}.{}.PNG').format(part.split('_')[0],fName,part)
                io.imsave(cutFile,partImg[0])##iamge save
                img=drawRect(img,partImg[1])
            face=cut_face(img0,landmarks,shrink);
            io.imsave(path+'/face/pic/'+fName+'.left.PNG',face[0][0]);
            io.imsave(path+'/face/pic/'+fName+'.right.PNG',face[0][1]);
            ###draw marks on face
            img=drawRect(img,rect)
            img=drawLandmarks(img,landmarks)
            img=drawRect(img,face[1][0])
            img=drawRect(img,face[1][1])
            markFile=(path+'/mark/{}.shrink{}.JPG').format(fName,shrink)
            io.imsave(markFile,img)
            ############ add landmark number
            ifAddNO=False
            if ifAddNO:
                imgNO=addNumOnImg(markFile,landmarks)
                imgNO.save(markFile+'.NO.JPG')
            #############
            print(time.time()-time_start)
        except:
            print("error")
            continue
#main()

if __name__ == '__main__':
    main()
    ##iamge display
    #io.imshow(img);io.show();
