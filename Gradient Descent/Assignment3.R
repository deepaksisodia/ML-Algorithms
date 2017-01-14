setwd("C:\\Users\\Deepak Kumar Sisodia\\Desktop\\ML Assignments\\Multiple Regression")
train=read.csv("kc_house_train_data.csv")
test = read.csv("kc_house_test_data.csv")

train$id=factor(train$id)
train$zipcode=factor(train$zipcode)
train$floors=factor(train$floors)
train$bedrooms_squared=train$bedrooms*train$bedrooms
train$bed_bath_rooms=train$bedrooms*train$bathrooms
train$log_sqft_living=log(train$sqft_living)
train$lat_plus_long=train$lat+train$long
str(train)

test$id=factor(test$id)
test$zipcode=factor(test$zipcode)
test$floors=factor(test$floors)
test$bedrooms_squared=test$bedrooms*test$bedrooms
test$bed_bath_rooms=test$bedrooms*test$bathrooms
test$log_sqft_living=log(test$sqft_living)
test$lat_plus_long=test$lat+test$long
str(test)


get_data=function(data, features, output){
  f=data[,features]
  f=cbind(rep(1,nrow(data)),f,data[,output])
  colnames(f)=c("const",features,output)
  return(f)
}


gradient_descent=function(feature_matrix, output, initial_weights, step_size, tolerance){
  
  convergence=F
  weights = initial_weights
  while(convergence == F){
    
    predict_outcome= feature_matrix %*% weights
    errors = predict_outcome - output
    
    grad = 0
    for(ft in 1:ncol(feature_matrix)){
      feature_derivative = 2*crossprod(feature_matrix[,ft],errors)
      weights[ft]=weights[ft] - (step_size*feature_derivative)
      grad = grad + feature_derivative^2
      
    }
    
    convergence=ifelse(sqrt(grad)<tolerance,T,F)
    
  }
  
  return(weights)
  
}

features=c("sqft_living")
output=c("price")
initial_weights_vector= c(-47000, 1)
step_size=7e-12
tolerance=2.5e7

working.dataset=get_data(train,features,output)
head(working.dataset)
feature_matrix = as.matrix(working.dataset[,-ncol(working.dataset)])
head(feature_matrix)
output_vector=working.dataset[,output]
head(output_vector)
gradient.output = gradient_descent(feature_matrix,output_vector,initial_weights_vector,step_size,tolerance)


test.dataset=get_data(test,features,output)
head(test.dataset)
test.feature.matrix=as.matrix(test.dataset[,-ncol(test.dataset)])
head(test.feature.matrix)
predicted.price.test=test.feature.matrix %*% gradient.output
head(predicted.price.test)
residual = predicted.price.test - test$price
RSS.test=sum(residual^2)




