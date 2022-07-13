#for testing the influence function in clark&skiba fig. 8b. 


Athlete.data=read.csv("clarkeSkibaData - Sheet1.csv")

day.1=Athlete.data[[1]]
Training.1=Athlete.data[[2]]
Performance.1=Athlete.data[[3]]
params.1=c(262, 0.18, 0.23, 36, 21)


Influence=function(params,startDay,t_p=0){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  out=c(rep(0,(t_p-startDay)))
  n=t_p-startDay+1
  for(i in 1:n){
    out[i]=k_1*exp(-(n-i+1)/tau_1)-k_2*exp(-(n-i+1)/tau_2)
  }
  return(out)
}

get_t_n=function(params){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  t_g=(tau_1*tau_2)/(tau_1-tau_2)*log(k_2/k_1)
}

get_t_g=function(params){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  t_g=(tau_1*tau_2)/(tau_1-tau_2)*log((k_2*tau_1)/(k_1*tau_2))
  return(t_g)
}

(Influence(params.1,-165,0))

(get_t_g(params.1))
(get_t_n(params.1))



