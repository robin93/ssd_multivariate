#question 5.4- ellipsoidal axes direction and length
raw_sweat_data <- data.frame(read.table('T5-1.DAT',header = FALSE))
raw_sweat_data
summary(raw_sweat_data)
sweat_data <- cbind(seq(from=1,to = nrow(raw_sweat_data),by = 1),raw_sweat_data)
colnames(sweat_data) <- c("Individual","Sweat Rate","Sodium","Potassium")
sweat_data
s_matrix_raw_sweat_data <- cov(raw_sweat_data)
s_matrix_raw_sweat_data
eigen_val_var_mat <- eigen(s_matrix_raw_sweat_data)$values
sqrt(eigen_val_var_mat)
p = ncol(raw_sweat_data)
n = nrow(raw_sweat_data)
f_statistic_bound_value <- sqrt(df(0.1,df1 = 3,df2 = 17))*(p*(n-1)/(n*(n-p)))
f_statistic_bound_value
sqrt(eigen_val_var_mat)*f_statistic_bound_value*2
distances <- sort(mahalanobis(raw_sweat_data,colMeans(raw_sweat_data),s_matrix_raw_sweat_data))
distances
qqnorm(distances)  #generates the qq plot for the data given
qqline(distances)  #adds the line to the qq plot
pairs(raw_sweat_data)



#question 5.8 , analysing theorem 5.23 on data of 5.1
raw_data <- matrix(c(2,8,6,8,12,9,9,10),nrow = 4,ncol = 2)
s_mat_raw_data <- cov(raw_data)

a_vect <- solve(s_mat_raw_data)%*%(colMeans(raw_data)-c(7,11))
t_square <- (nrow(raw_data))*((t(a_vect)%*%colMeans(raw_data)- t(a_vect)%*%as.matrix(c(7,11)))^2)/(t(a_vect)%*%s_mat_raw_data%*%a_vect)
T_square <- nrow(raw_data)*(t(colMeans(raw_data)-c(7,11))%*%(solve(s_mat_raw_data))%*%(colMeans(raw_data)-c(7,11)))                  
