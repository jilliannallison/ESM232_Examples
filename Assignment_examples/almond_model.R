almond_model = function(Tn_2, P_1, 
                        Tn_2_coeff = -0.015, 
                        Tn_2_2_coeff = -0.0046,
                        P_1_coeff = -0.07, 
                        P_1_2_coeff = 0.0043, 
                        intercept = 0.28){
  
  
  result = (Tn_2_coeff * Tn_2) + 
    (Tn_2_2_coeff * (Tn_2)^2) +
    (P_1_coeff * P_1) + 
    (P_1_2_coeff * (P_1)^2) + 
    intercept
  
  
  return(list(yields = result, maxyield=max(result),minyield=min(result),meanyield=mean(result)))
  
}
