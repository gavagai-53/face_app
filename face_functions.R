library(pls)
library(MASS)

#label data
labels=read.csv("dimension_ratings.csv",header = T,sep = ";",stringsAsFactors = F)
rating_labels = labels[,c(2,seq(8,20,2))]
rating_labels[,2:8]=sapply(rating_labels[,2:8],as.numeric)

connections <- read.csv('adj_face_points.csv', header = FALSE)

euclidean=function(a,b){
  return(sqrt(sum((a-b)^2)))
}

compute_mean_frame=function(frame_vec){
  return(colMeans(matrix(frame_vec,ncol=136,byrow = T)))
}

plot_mean_frame=function(frame_vec){
  
  x <- frame_vec[seq(1,length(frame_vec),2)]
  y <- frame_vec[seq(2,length(frame_vec),2)]*-1
  
  plot(
    x,
    y,
    ylim=c(-0.2,0.13),xlim=c(-0.2,0.2),
    xlab="X", ylab="Y"
    )

  segments(
    x[connections[,1]],
    y[connections[,1]],
    x[connections[,2]],
    y[connections[,2]]
  )
}


plot_middle_prediction = function(new_dat_vector,pls_obj,frame){
  vec_len=length(new_dat_vector)
  new_dat_vector=matrix(new_dat_vector,byrow=T,ncol=vec_len)
  be_colnames=c("happy", "sad", "surprised", "disgusted", "angry", "fearful", "interested","sex","age")[1:length(new_dat_vector)]
  colnames(new_dat_vector)=be_colnames
  prediction=predict(pls_obj,newdata=new_dat_vector,ncomp=vec_len)
  
  plot_mean_frame(prediction[(1+136*(frame-1)):(136*frame)])
}


find_differences = function(new_dat_vector,pls_obj1,pls_obj2){
  vec_len=length(new_dat_vector)
  new_dat_vector=matrix(new_dat_vector,byrow=T,ncol=vec_len)
  be_colnames=c("happy", "sad", "surprised", "disgusted", "angry", "fearful", "interested","sex","age")[1:length(new_dat_vector)]
  colnames(new_dat_vector)=be_colnames
  prediction1=predict(pls_obj1,newdata=new_dat_vector,ncomp=vec_len)
  prediction2=predict(pls_obj2,newdata=new_dat_vector,ncomp=vec_len) 
  difference=abs(prediction2)-abs(prediction1)
  difference = matrix(difference,byrow = T,ncol=136)
  plot(rowSums(difference),xlab="Frame",ylab="Difference in Prediction")
}


plot_same_window=function(new_dat_vector,pls_obj1,pls_obj2,frame){
  vec_len=length(new_dat_vector)
  new_dat_vector=matrix(new_dat_vector,byrow=T,ncol=vec_len)
  be_colnames=c("happy", "sad", "surprised", "disgusted", "angry", "fearful", "interested","sex","age")[1:length(new_dat_vector)]
  colnames(new_dat_vector)=be_colnames
  prediction1=predict(pls_obj1,newdata=new_dat_vector,ncomp=vec_len)
  prediction2=predict(pls_obj2,newdata=new_dat_vector,ncomp=vec_len) 
  
  frame_vec = prediction1[(1+136*(frame-1)):(136*frame)]
  
  x <- frame_vec[seq(1,length(frame_vec),2)]
  y <- frame_vec[seq(2,length(frame_vec),2)]*-1
  
  plot(
    x,
    y,
    ylim=c(-0.2,0.13),xlim=c(-0.2,0.2),
    xlab="X", ylab="Y"
  )
  
  segments(
    x[connections[,1]],
    y[connections[,1]],
    x[connections[,2]],
    y[connections[,2]]
  )
  
  frame_vec = prediction2[(1+136*(frame-1)):(136*frame)]
  
  x <- frame_vec[seq(1,length(frame_vec),2)]
  y <- frame_vec[seq(2,length(frame_vec),2)]*-1
  points(x,y,col="red")
  
  segments(
    x[connections[,1]],
    y[connections[,1]],
    x[connections[,2]],
    y[connections[,2]]
    ,col="red")
  
}


find_labels=function(rating_vec, threshold=25){
  distances=rep(NA,nrow(rating_labels))
  for (i in 1:nrow(rating_labels)){
    distances[i]=euclidean(rating_labels[i,2:8],rating_vec)
  }
  label_indices=distances<threshold
  matching_labels = rating_labels[label_indices,1][order(distances[distances<threshold])]
}

