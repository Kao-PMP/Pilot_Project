get_auc = function(cox, xtest, time, status, t_eval) {
  
  lp = predict(cox, newdata=xtest, type='lp')
  
  auc = risksetROC(
    Stime  = time,          # Time-to-Event variable
    status = status,        # Censor variable
    marker = lp,            # Linear predictor
    predict.time = t_eval,  # Time point for prediction (should be on same scale as Stime)
    method = "Cox",         # Using Cox PH model
    plot   = FALSE)
  
  return(auc$AUC)
  
}