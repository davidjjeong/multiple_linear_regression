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

# Dataset 1: Toyota Corolla
corolla <- read.csv("ToyotaCorolla.csv")
View(corolla)

# Indices for Inactivated Input Variables
id_idx <- c(1, 2)

# Hence Remove Irrelevant Columns
corolla_data <- corolla[, -id_idx]

# Remove Categorical Data (Fuel Type)
# Check linearity between X variables and Y variable
plot_data <- corolla_data[, -6]
corolla_names <- colnames(plot_data)[-1]

par(mfrow=c(5,7))
par(mar=c(3,3,3,3))
for(i in 1:length(corolla_names)){
  plot(Price ~ plot_data[,i+1], data = plot_data,
       xlab = corolla_names[i])
}
dev.off()

# One outlier exists for cc variable
cc_outlier <- which(plot_data$cc > 15000)

# Select some linearly related variables with "Price"
plot_data_selected <- plot_data[-cc_outlier, c(1,2,4,5,6,9,13,14)]
corolla_names <- colnames(plot_data_selected)[-1]

par(mfrow=c(2, 4))
for(i in 1:length(corolla_names)){
  plot(Price ~ plot_data_selected[,i+1], data = plot_data_selected,
       xlab = corolla_names[i])
}
dev.off()

# Split data into training/validation sets
corolla_mlr_data <- corolla_data[-cc_outlier,]
nCar <- nrow(corolla_mlr_data)

# Fix seed for random number generation
set.seed(12345)
corolla_trn_idx <- sample(1:nCar, round(0.7*nCar)) # Training Dataset: 70%
corolla_trn_data <- corolla_mlr_data[corolla_trn_idx,]
corolla_tst_data <- corolla_mlr_data[-corolla_trn_idx,] # Test Dataset: 30%

# Train the MLR
mlr_corolla <- lm(Price ~ ., data = corolla_trn_data)
mlr_corolla
summary(mlr_corolla)
plot(mlr_corolla)