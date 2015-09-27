#qq plot for the multivariate data in 
raw_data <- data.frame(read.csv("4_25_top_10_companies_data.csv"))
raw_data
chi_square_quantiles <- c(0.3518,0.7978,1.2125,1.6416,2.1095,2.6430,3.2831,4.1083,5.3170,7.8147)
sigma_mat <- cov(raw_data[,2:4])
sigma_mat
sigma_inverse <- solve(sigma_mat)
sigma_inverse
mean_matrix <- c(mean(raw_data[,2]),mean(raw_data[,3]),mean(raw_data[,4]))
mean_matrix
scaled_comp_data <- scale(raw_data[,2:4],scale=FALSE)
scaled_comp_data
scaled_comp_data_transpose <- t(scaled_comp_data)
scaled_comp_data_transpose
distances <- sort(mahalanobis(raw_data[,2:4],mean_matrix,sigma_mat))
distances
plot(chi_square_quantiles,distances)
#this is the plot of the quantiles of the data against the chi square quantiles

dist_alt <- diag(scaled_comp_data%*%sigma_inverse%*%scaled_comp_data_transpose)
sort(dist_alt)
sort(distances)



#radiation data analysis
radiation_data <- data.frame(read.csv("radiation_data.csv"))
radiation_data
abs_rad_val <- radiation_data[,1:2]
log_rad_val <- radiation_data[,c(1,3)]
lambda_4_val <- radiation_data[,c(1,4)]
unsorted_normal_quantile_val <- rnorm(42)
sorted_normal_quantile_val <- sort(unsorted_normal_quantile_val)
plot(sorted_normal_quantile_val,sort(abs_rad_val[,2]))
plot(sorted_normal_quantile_val,sort(log_rad_val[,2]))
plot(sorted_normal_quantile_val,sort(lambda_4_val[,2]))


#4.29 question program
pollution_data <- data.frame(read.table("4_29_data_air_pollution.dat",header = FALSE,fill = TRUE))
pollution_data
data2 <- pollution_data[,5:6]
colnames(data2) <- c("NO2","03")
data2
scaled_pol_data <- scale(data2,scale=FALSE)
scaled_pol_data_transpose <-t(scaled_pol_data)
sig_mat <- cov(data2)
inv_sig_mat <- solve(sig_mat)
dist_alt <- diag(scaled_pol_data%*%inv_sig_mat%*%scaled_pol_data_transpose)

quantile(dist_alt,c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))

data_points <- nrow(data2)

chi_square_vals <- qchisq((1:data_points-1/2)/data_points,df=2)

#construct an ordered chi squared plot
par(mfrow = c(1,1))
plot(chi_square_vals, sort(dist_alt))
abline(a=0, b=1)

#4.30 question data analysis
#transformation lambda analysis
x1_values <- c(1,2,3,3,4,5,6,8,9,11)
x2_values <- c(18.95,19.00,17.95,15.54,14.00,12.95,8.94,7.49,6.00,3.99)
l_value = numeric(0)
log_array = log(x1_values)
for (lambda in seq(from=0.01,to=1,by=0.01)){
  first_term_sum=0
  for (i in 1:length(x1_values)){
    first_term_sum = first_term_sum+((x1_values[i])^lambda - mean(log_array))^2
  }
  l_value[lambda*100] = (lambda-1)*sum(log_array) + (-0.5)*(length(x1_values))*(log(first_term_sum/length(x1_values)))
  }

l_value
plot(seq(from=0.01,to=1,by=0.01),l_value)

l_value_2 = numeric(0)
log_array = log(x2_values)
for (lambda in seq(from=0.01,to=1,by=0.01)){
  first_term_sum=0
  for (i in 1:length(x2_values)){
    first_term_sum = first_term_sum+((x2_values[i])^lambda - mean(log_array))^2
  }
  l_value_2[lambda*100] = (lambda-1)*sum(log_array) + (-0.5)*(length(x2_values))*(log(first_term_sum/length(x2_values)))
}

l_value_2
plot(seq(from=0.01,to=1,by=0.01),l_value_2)

#generating the Q-Q plots
#for X1 values
sorted_quantile_values_for_4_30 <- sort(c(rnorm(n=length(x1_values))))
plot(sorted_quantile_values_for_4_30,sort(x1_values))
plot(sorted_quantile_values_for_4_30,sort(x1_values^0.2))
plot(sorted_quantile_values_for_4_30,sort(x2_values))
plot(sorted_quantile_values_for_4_30,sort(x2_values^0.2))


