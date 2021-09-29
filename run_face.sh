#python face_eyebrow_detetor.py origin_image_file shrink_image_file ouput_file
script_dir=/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/script/scriptNow
landmarksRef=$script_dir/shape_predictor_68_face_landmarks.dat
###data path, pic inside is the picture folder
dataPath=/Users/taoxianming/Documents/face2DExtract/eyebrow_tao/uygur
picSuffix=JPG
###detect eye, eyebrow, face
#python $script_dir/facePartDetect_1.py $landmarksRef $dataPath $picSuffix

##attention
#check landmark error in the mark file by human !
#check unsuitable eye/eyebrow images by human !
##
#iris detect and color HSV statistics
###python $script_dir/iris_detect_edgeFit2_2.py $dataPath  ##pure fit
#python $script_dir/iris_detect_edgeFit_landmarks_2.py $dataPath
python $script_dir/iris_detect_edgeFit_landmarks_2_plot.py $dataPath
##
#bash rmError.sh
##check location result images
Rscript $script_dir/iris_HSVandStat_3.R $dataPath
Rscript $script_dir/iris_VS_human_4.R $dataPath


###eyebrow detect and color, eyebrow density extraction
#python $script_dir/eyebrow_extraction_2.py $dataPath
#eyebrow color HSV and xy statistics
#Rscript $script_dir/eyebrow_HSV_XYstat_3.R $dataPath
#eyebrow density calculation
#Rscript $script_dir/eyebrow_density_4.R $dataPath