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