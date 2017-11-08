library(pls)
library(MASS)
#library(colorRamps)
connections <- read.csv('adj_face_points.csv', header = FALSE)

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

plot_mean_prediction = function(new_dat_vector,pls_obj){
  new_dat_vector=matrix(new_dat_vector,byrow = T,ncol=length(new_dat_vector))
  be_colnames=c("happy", "sad", "surprised", "disgusted", "angry", "fearful", "interested","sex","age")[1:length(new_dat_vector)]
  colnames(new_dat_vector)=be_colnames
  
  prediction=predict(pls_obj,newdata=new_dat_vector)
  plot_mean_frame(prediction[1:13600])
}


plot_middle_prediction = function(new_dat_vector,pls_obj,frame){
  vec_len=length(new_dat_vector)
  new_dat_vector=matrix(new_dat_vector,byrow=T,ncol=vec_len)
  be_colnames=c("happy", "sad", "surprised", "disgusted", "angry", "fearful", "interested","sex","age")[1:length(new_dat_vector)]
  colnames(new_dat_vector)=be_colnames
  prediction=predict(pls_obj,newdata=new_dat_vector,ncomp=vec_len)
  
  plot_mean_frame(prediction[(1+136*(frame-1)):(136*frame)])
}
