library(pls)
library(MASS)
#library(colorRamps)

compute_mean_frame=function(frame_vec){
  return(colMeans(matrix(frame_vec,ncol=136,byrow = T)))
}

plot_mean_frame=function(frame_vec, connections){
  mean_vec = compute_mean_frame(frame_vec)
  x <- mean_vec[seq(1,length(mean_vec),2)]
  y <- mean_vec[seq(2,length(mean_vec),2)]*-1
  
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

plot_mean_prediction = function(new_dat_vector,pls_obj, connections){
  new_dat_vector=matrix(new_dat_vector,byrow = T,ncol=7)
  be_colnames=c("happy", "sad", "surprised", "disgusted", "angry", "fearful", "interested")
  colnames(new_dat_vector)=be_colnames
  
  prediction=predict(pls_obj,newdata=new_dat_vector)
  plot_mean_frame(prediction[1:13600], connections)
}


plot_middle_prediction = function(new_dat_vector,pls_obj,frame, connections){
  new_dat_vector=matrix(new_dat_vector,byrow=T,ncol=7)
  be_colnames=c("happy", "sad", "surprised", "disgusted", "angry", "fearful", "interested")
  colnames(new_dat_vector)=be_colnames
  
  prediction=predict(pls_obj,newdata=new_dat_vector,ncomp=7)
  
  plot_mean_frame(prediction[(1+136*(frame-1)):(136*frame)], connections)
}