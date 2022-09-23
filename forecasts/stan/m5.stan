
data{
  int N_questions;   
  int N_polls;    
  int N_days;   
  int N_states;  
  int N_pollsters;    
  int N_population;
  int N_methods; 
  int direction_flag;
  
  array[N_questions] int<lower = 1, upper = N_states> state_index; // State index
  array[N_questions] int<lower = 1, upper = N_days>  day_state_index;   // Day index
  
  array[N_questions] int<lower = 1, upper = N_polls>  poll_index; // poll index
  array[N_questions] int<lower = 1, upper = N_pollsters> pollster_index; // which pollster
  array[N_questions] int<lower = 1, upper = N_methods> methods_index;   // which polling method
  array[N_questions] int<lower = 1, upper = N_population> population_index;   // which method

  vector[N_states] last_dem_logit;   // which incumbency party in that state?
 
  array[N_questions] int dem_num;
  array[N_questions] int n_two_party;
  
  real state_day_sigma_scaler;
  real states_poll_bias_scaler;
  real sigma_scaler_pollster;
  real sigma_scaler;
}

parameters {
  
  vector[N_pollsters] mu_pollster; 
  vector[N_methods] mu_methods; 
  vector[N_population] mu_population; 
  
  vector[N_polls] poll_noise;
  vector[N_states] polling_bias_states;  
  
  matrix[N_states, N_days] err;
  vector[N_states] raw_mu_first_day;

}

transformed parameters {
 

  matrix[N_states,N_days] mu; 

  // this is forward
  if(direction_flag > 0){
    
    mu[:, 1] = raw_mu_first_day;
    
    for (t in 2:N_days) {
      mu[:, t] = mu[:,  t - 1] + err[:, t] * state_day_sigma_scaler;
    }
    
  }else{
    // this is backward
    mu[:, N_days] = raw_mu_first_day;
    
    for (t in 1:(N_days-1)) {
      mu[:, N_days - t] = mu[:,  N_days - t + 1] + err[:, N_days - t + 1] * state_day_sigma_scaler;
    }
  }



 vector[N_questions] logit_dem_pct;  

 for (i in 1:N_questions){
   logit_dem_pct[i] =
    mu[state_index[i], day_state_index[i]] + 
    mu_pollster[pollster_index[i]] * sigma_scaler_pollster + 
   // mu_incumbent_party[incumbent_party_index[state_index[i]]] * sigma_scaler + 
    mu_methods[methods_index[i]] * sigma_scaler +
    mu_population[population_index[i]] * sigma_scaler + 
    polling_bias_states[state_index[i]] * states_poll_bias_scaler + 
    poll_noise[poll_index[i]] * sigma_scaler;
 }

}

model {

  mu_pollster ~ std_normal();
  //mu_incumbent_party ~ std_normal();
  mu_methods ~ std_normal();
  mu_population ~ std_normal();
  poll_noise ~  std_normal();
  polling_bias_states ~  std_normal();
  raw_mu_first_day ~ normal(last_dem_logit, .3); 
  
  to_vector(err) ~  std_normal();

  dem_num ~ binomial_logit(n_two_party, logit_dem_pct);
  
}


