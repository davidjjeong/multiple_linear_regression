install.packages("moments")
library(moments)

# Performance Evaluation Function for MLR
perf_eval_reg <- function(tgt_y, pred_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pred_y)^2))
  
  # MAE
  mae <- mean(abs(tgt_y - pred_y))
  
  # MAPE
  mape <- 100*mean(abs(tgt_y - pred_y))
  
  return(c(rmse, mae, mape))
}

# Initialize Performance Summary
perf_mat <- matrix(0, nrow=2, ncol=3)
rownames(perf_mat) <- c("Toyota Corolla", "Boston Housing")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat