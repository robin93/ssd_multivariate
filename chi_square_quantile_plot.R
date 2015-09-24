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
