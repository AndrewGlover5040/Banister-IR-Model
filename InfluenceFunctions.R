#for testing the influence function in clark&skiba fig. 8b. 

Influence=function(params,startDay,t_p){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  out=c(0)*(t_p-startDay)
  for(i in 1:(t_p-startDay+1)){
    out[i]=k_1*exp((i-1)/tau_1)-k_2*exp((i-1)/tau_2)
  }
  return(out)
}

get_t_n=function(params){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  t_g=(tau_1*tau_2)/(tau_1-tau_2)ln(k_2/k_1)
}

get_t_g=function(params){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  t_g=(tau_1*tau_2)/(tau_1-tau_2)ln((k_2*tau_1)/(k_1*tau_2))
  return(t_g)
}


