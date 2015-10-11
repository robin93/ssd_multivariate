#question 5.4- ellipsoidal axes direction and length
raw_sweat_data <- data.frame(read.table('T5-1.DAT',header = FALSE))
raw_sweat_data
summary(raw_sweat_data)
print("number of rows")
nrow(raw_sweat_data)
print ("Number of columns")
ncol(raw_sweat_data)

sweat_data <- cbind(seq(from=1,to = nrow(raw_sweat_data),by = 1),raw_sweat_data)
colnames(sweat_data) <- c("Individual","Sweat Rate","Sodium","Potassium")
sweat_data
s_matrix_raw_sweat_data <- cov(raw_sweat_data)
s_matrix_raw_sweat_data
eigen_val_var_mat <- eigen(s_matrix_raw_sweat_data)$values
sqrt(eigen_val_var_mat)
p = 3
n = 20
f_statistic_bound_value <- sqrt(df(0.1,df1 = 3,df2 = 17))*(p*(n-1)/(n*(n-p)))
f_statistic_bound_value
sqrt(eigen_val_var_mat)*f_statistic_bound_value*2

mahalanobis(raw_sweat_data)