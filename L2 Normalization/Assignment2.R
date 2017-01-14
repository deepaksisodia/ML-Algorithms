setwd("C:\\Users\\Deepak Kumar Sisodia\\Desktop\\ML Assignments\\Ridge Regression\\Assignment2")

getfeaturematrix=function(inp.data,feature.names){
  
  inp.feature.matrix=as.matrix(inp.data[,feature.names])
  inp.feature.matrix=cbind(rep(1,nrow(inp.feature.matrix)),inp.feature.matrix)
  return(inp.feature.matrix)
  
}

ridge_regression_gradient_descent=function(feature_matrix, output, initial_weights, step_size, l2_penalty, max_iterations=100){

  weights=initial_weights
  diff=1-(2*step_size*l2_penalty)
  multfactor=c(1,rep(diff,nrow(weights)-1))
  print(multfactor)
  
  for(i in 1:max_iterations){
    
    predictions=feature_matrix %*% weights
    errors=output-predictions
    # Not regularizing the intercept term

    weight.change.part1=multfactor*weights
    #print(weight.change.part1)
    
    weight.change.part2=2*step_size*t(feature_matrix)%*%errors
    
    weights=weight.change.part1 + weight.change.part2
    
  }
  return(weights)

}

sales=read.csv("kc_house_train_data.csv")
sales.test=read.csv("kc_house_test_data.csv")
str(sales)
output.matrix=as.matrix(sales$price)
tail(output.matrix)
input.feature.matrix=getfeaturematrix(sales,c("sqft_living"))
tail(input.feature.matrix)
#initial.weights=rep(0,ncol(input.feature.matrix))
initial.weights=c(0,0)
initial.weights=as.matrix(initial.weights)
step.size=1e-12
max.iterations=1000
L2.penalty=1e11

ridge.output1=ridge_regression_gradient_descent(input.feature.matrix,output.matrix,initial.weights,step.size,0,max.iterations)
ridge.output2=ridge_regression_gradient_descent(input.feature.matrix,output.matrix,initial.weights,step.size,L2.penalty,max.iterations)


input.feature.test.matrix=getfeaturematrix(sales.test,c("sqft_living"))
output.test.matrix=as.matrix(sales.test$price)

test.predictions = input.feature.test.matrix %*% initial.weights
RSS=sum((output.test.matrix-test.predictions)^2)
RSS

test.predictions.1 = input.feature.test.matrix %*% ridge.output1
RSS1=sum((output.test.matrix-test.predictions.1)^2)
RSS1

test.predictions.2 = input.feature.test.matrix %*% ridge.output2
RSS2=sum((output.test.matrix-test.predictions.2)^2)
RSS2

######################################################################
initial.weights.2features=c(0,0,0)
initial.weights.2features=as.matrix(initial.weights.2features)
step.size=1e-12
max.iterations=1000
L2.penalty=1e11

input.feature.matrix.2=getfeaturematrix(sales,c("sqft_living","sqft_living15"))
tail(input.feature.matrix.2)

ridge.output.2features.1=ridge_regression_gradient_descent(input.feature.matrix.2,output.matrix,initial.weights.2features,step.size,0,max.iterations)
ridge.output.2features.1

ridge.output.2features.2=ridge_regression_gradient_descent(input.feature.matrix.2,output.matrix,initial.weights.2features,step.size,L2.penalty,max.iterations)
ridge.output.2features.2

input.feature.test.matrix.2=getfeaturematrix(sales.test,c("sqft_living","sqft_living15"))
output.test.matrix.2=as.matrix(sales.test$price)

test.2feature.predictions = input.feature.test.matrix.2 %*% initial.weights.2features
RSS=sum((output.test.matrix.2-test.2feature.predictions)^2)
RSS

test.2feature.predictions.1 = input.feature.test.matrix.2 %*% ridge.output.2features.1
RSS1=sum((output.test.matrix.2-test.2feature.predictions.1)^2)
RSS1
head(test.2feature.predictions.1)
head(output.test.matrix.2-test.2feature.predictions.1)

test.2feature.predictions.2 = input.feature.test.matrix.2 %*% ridge.output.2features.2
RSS2=sum((output.test.matrix.2-test.2feature.predictions.2)^2)
RSS2
head(test.2feature.predictions.2)
head(output.test.matrix.2-test.2feature.predictions.2)
