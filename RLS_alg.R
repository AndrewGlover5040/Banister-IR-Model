#RLS


dat <- load("DFfromPDF (1).Rda")
Athlete.data <- df

day.1=Athlete.data[[1]]
params.1=c(256, 0.10, 0.10, 15, 11)
Training.1=Athlete.data[[2]]
Performance.1=Athlete.data[[3]]

#View(Training.1)


#assumes training load has a zero day
get_performance_matrix <- function(training_load,
                                bounds=c(1,50),
                                by=1){
  #need to check that this is a positive integer
  num_tau=(bounds[[2]]-bounds[[1]])/by+1
  e_to_the_tau_vec=c(rep(0,num_tau))  
  for (i in 1:num_tau){
    e_to_the_tau_vec[[i]]=exp(1/(bounds[[1]]+(i-1)*by))
  }
  e_to_the_tau_vec=as.matrix(e_to_the_tau_vec)
  max_day=length(training_load)
  out_matrix=matrix(0,num_tau,max_day)
  for (i in 2:max_day){
    out_matrix[,i]=e_to_the_tau_vec*(out_matrix[,i-1]+training_load[[i-1]])
  }
   e_to_the_tau_vec
  out_matrix
}

T_matrix=get_performance_matrix(Training.1)
#View(T_matrix)


#for testing RLS function
initializations = list(matrix(c(.1,.1),byrow = TRUE),
                       matrix(c(1,0,0,1),nrow=2),
                       matrix(0,2,1),
                       matrix(c(1,0,0,1),nrow=2),
                       numeric(len_indexes_perf),
                       0,
                       1,
                       as.list(rep(NA,len_indexes_perf))
)
initializations
indexes_perf <- which(!is.na(Performance.1))
len_indexes_perf <- length(indexes_perf)


##assume T_2 is posititve
RLS <- function(training_load,
             performance,
             indexes_perf,
             initializations,
             p_0,
             T_1_vec,
             T_2_vec,
             alpha=1,
             ){
  
  #initializations
  ##fix theta to be the right thing
  theta = initializations[[1]]
  #=matrix(c(.1,.1),byrow = TRUE)
  
  P = initializations[[2]]
    #=delta*matrix(c(1,0,0,1),nrow=2)
  
  K=initializations[[3]]
    #=matrix(0,2,1)
  
  #make dim two identity matrix
  I=initializations[[4]]
    #=matrix(c(1,0,0,1),nrow=2)
  
  S_vector=initializations[[5]]
    #=numeric(len_indexes_perf)
  
  #sum
  S=initializations[[6]]
    #=0
  
  #counter
  j=initializations[[7]]
    #=1
  
  theta_list=initializations[[8]]
    #=as.list(rep(NA,len_indexes_perf))
  
  #actual recursion algorithm
  for (i in indexes_perf){
    x=matrix(c(T_1_vec[[i]],T_2_vec[[i]]),byrow=TRUE)
    #so we don't have to compute this twice
    denom=as.numeric(alpha+t(x)%*%P%*%x)
    
    #update K (with old P)
    K=P%*%x/denom
    
    #update P ##fix to make more efficent
    P=(I-P%*%x%*%t(x)/denom)%*%P/alpha
    
    #update theta
    error = performance[[i]]-as.numeric(p_0+t(x)%*%theta)
    theta = theta+K*(error)
    S = S*alpha+error^2
    S_vector[[j]] = S
    theta_list[[j]] = theta
    j=j+1
  }
  list(theta_list,S_vector)
}


RLS(Training.1, Performance.1,
    indexes_perf, initializations,
    256, T_matrix[36,], -T_matrix[36,],
    alpha=.98, delta=1)



#maybe make initializations list in the server environment
estimate_RLS_best <- function(training_load,
                           performance,
                           p_0,
                           alpha=1,
                           delta=1,
                           bounds_T_1,
                           by_T_1,
                           bounds_T_2,
                           by_T_2
){
  T_1_matrix = get_performance_matrix(training_load,bounds_T_1,by_T_1)
  T_2_matrix = get_performance_matrix(training_load,bounds_T_2,by_T_2)
  num_tau_1 = (bounds_T_1[[2]]-bounds_T_1[[1]])/by_T_1+1
  num_tau_2 = (bounds_T_2[[2]]-bounds_T_2[[1]])/by_T_2+1
  indexes_perf = which(!is.na(performance))
  len_indexes_perf = length(indexes_perf)
  
  ##fix theta
  RLS_initializations = list(matrix(c(.1,.1),byrow = TRUE),
                             delta*matrix(c(1,0,0,1),nrow=2),
                             matrix(0,2,1),
                             matrix(c(1,0,0,1),nrow=2),
                             numeric(len_indexes_perf),
                             0,
                             1,
                             as.list(rep(NA,len_indexes_perf))
  )
  
  cost_array <- array(0,c(len_indexes_perf,num_tau_1,num_tau_2))
  
  for (i in 1:num_tau_1){
    for (j in 1:num_tau_2){
      tmp_ans = RLS(training_load, 
          performance,
          indexes_perf,
          RLS_initializations,
          p_0,
          T_1_matrix[i,], 
          -T_2_matrix[j,],
          alpha=alpha
          )
      costarray[,i,j] = tmp_ans[[2]]
      
    }
  }
}


num_reps=10000
temp_list=list(rep(0,num_reps))
for (i in 1:num_reps){ 
  temp_list[[i]]=6
}


a=list(rep(1,200))
num_reps=10000
test_list=list(rep(0,num_reps))
for (i in 1:num_reps){ 
  test_list[[i]]=a
}



zero_array=array(0,dim=c(3,1))
zero_array
zero_array[,1,1]=c(1,2,3)
