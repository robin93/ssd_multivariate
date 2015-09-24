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

chi_square_vals <- qchisq((1:nrow(data2)-1/2)/nrow(data2),df=2)

#construct an ordered chi squared plot
par(mfrow = c(1,1))
plot(qchisq((1:nrow(data2)-1/2)/nrow(data2),df=2), sort(dist_alt),
     xlab = expression(paste(chi[2]^2, "Quantile")),
     ylab = "Ordered Distances",
     main = "Chi-square Plot for (NO2, O3)"); abline(a=0, b=1)


