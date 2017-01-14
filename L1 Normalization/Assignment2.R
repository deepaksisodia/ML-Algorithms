setwd("C:\\Users\\Deepak Kumar Sisodia\\Desktop\\ML Assignments\\Lasso Regression\\Assignment2")

getfeaturematrix=function(inp.data,feature.names){
  inp.feature.matrix=as.matrix(inp.data[,feature.names])
  inp.feature.matrix=cbind(rep(1,nrow(inp.feature.matrix)),inp.feature.matrix)
  return(inp.feature.matrix)
  
}

normalize_features= function(inp.data.frame){
  # inp.data.frame=data.frame(c1=c(1,2,3,4,5),c2=c(6,7,8,9,10),c3=c(2,7,9,4,7))
  # feature.names=c("c1","c2")
  normalized.matrix=sapply(inp.data.frame,function(x) x/sqrt(sum(x^2)))
  return(normalized.matrix)

}

norms=function(inp.data.frame){
  #inp.data.frame=data.frame(c1=c(1,2,3,4,5),c2=c(6,7,8,9,10))
  normvector=sapply(inp.data.frame,function(x) sqrt(sum(x^2)))
  return(normvector)
}

lasso_weights=function(norm.feature.matrix,output.matrix,new.weights,L1penalty,Tolerance){
  
  continueloop = TRUE
  
  while(continueloop){
    old.weights= new.weights
    
    for (i in 1:length(new.weights)){
      
      new.weights.matrix=as.matrix(new.weights)
      
      predicted.values=norm.feature.matrix %*% new.weights.matrix
      errors.matrix=output.matrix - predicted.values + new.weights[i]*norm.feature.matrix[,i]
      rho=t(norm.feature.matrix[,i]) %*% errors.matrix
      
      if(i==1){
        new.weights[i]=rho
      }
      else if(rho > L1penalty/2){
        new.weights[i]=rho - L1penalty/2
      }
      else if(rho < -L1penalty/2){
        new.weights[i]=rho + L1penalty/2
      }
      else{
        new.weights[i]=0
      }
      
      
    }
    
    #print(new.weights)
    
    weights_change=abs(new.weights - old.weights)
    continueloop=ifelse(max(weights_change)>=Tolerance,TRUE,FALSE)
    
  }
  
  return(new.weights)
}


sales=read.csv("kc_house_data.csv")
str(sales)
columns=c("sqft_living","bedrooms")

output.sales.matrix=as.matrix(sales$price)
tail(output.sales.matrix)

input.sales.feature.matrix=getfeaturematrix(sales,columns)
input.sales.feature.df=data.frame(input.sales.feature.matrix)

#Normalize the features
normalized.sales.feature.matrix=normalize_features(input.sales.feature.df)
head(normalized.sales.feature.matrix)

norm.sales.vector=norms(input.sales.feature.df)

###################################################################
# Set initial weights
initial.weights=c(1,4,1)
loopcount=length(initial.weights)

initial.weights.matrix=as.matrix(initial.weights)
predicted.values=normalized.sales.feature.matrix %*% initial.weights.matrix

for (i in 1:loopcount){
  
  errors.matrix=output.sales.matrix - predicted.values + initial.weights[i]*normalized.sales.feature.matrix[,i]
  rho=t(normalized.sales.feature.matrix[,i]) %*% errors.matrix
  initial.weights[i]=rho

}

2*initial.weights[2]
2*initial.weights[3]

##########################################################
initial.weights=c(0,0,0)
L1penalty = 1e7
Tolerance = 1.0

weights= lasso_weights(normalized.sales.feature.matrix,output.sales.matrix,initial.weights,L1penalty,Tolerance)

cat("final weights : ",weights,"\n")
weights.matrix=as.matrix(weights)

Residuals= output.sales.matrix - (normalized.sales.feature.matrix %*% weights.matrix)
RSS=sum(Residuals^2)
print(paste("RSS : ",RSS))

#####################################################################

train=read.csv("kc_house_train_data.csv")
str(train)
columns=c("bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","grade",
          "sqft_above","sqft_basement","yr_built","yr_renovated")
str(train)
output.train.matrix=as.matrix(train$price)
tail(output.train.matrix)

input.feature.matrix=getfeaturematrix(train,columns)
input.feature.df=data.frame(input.feature.matrix)

#Normalize the features
normalized.feature.matrix=normalize_features(input.feature.df)
head(normalized.feature.matrix)

norm.vector=norms(input.feature.df)

initial.weights=rep(0,14)
Tolerance = 1.0

L1penalty = 1e7
weights1e7 = lasso_weights(normalized.feature.matrix,output.train.matrix,initial.weights,L1penalty,Tolerance)
cat("final weights : ",weights1e7,"\n")

L1penalty = 1e8
weights1e8 = lasso_weights(normalized.feature.matrix,output.train.matrix,initial.weights,L1penalty,Tolerance)
cat("final weights : ",weights1e8,"\n")

L1penalty = 1e4
Tolerance = 5e5
weights1e4 = lasso_weights(normalized.feature.matrix,output.train.matrix,initial.weights,L1penalty,Tolerance)
cat("final weights : ",weights1e4,"\n")

weights1e7_normalized = weights1e7 / norm.vector
weights1e7_normalized_matrix=as.matrix(weights1e7_normalized)

weights1e8_normalized = weights1e8 / norm.vector
weights1e8_normalized_matrix=as.matrix(weights1e8_normalized)

weights1e4_normalized = weights1e4 / norm.vector
weights1e4_normalized_matrix=as.matrix(weights1e4_normalized)

test=read.csv("kc_house_test_data.csv")
output.test.matrix=as.matrix(test$price)
test.feature.matrix=getfeaturematrix(test,columns)
head(test.feature.matrix)

Test.predict.weights1e7=test.feature.matrix %*% weights1e7_normalized_matrix
Test.RSS.weights1e7 = sum((output.test.matrix - Test.predict.weights1e7)^2)

Test.predict.weights1e8=test.feature.matrix %*% weights1e8_normalized_matrix
Test.RSS.weights1e8 = sum((output.test.matrix - Test.predict.weights1e8)^2)

Test.predict.weights1e4=test.feature.matrix %*% weights1e4_normalized_matrix
Test.RSS.weights1e4 = sum((output.test.matrix - Test.predict.weights1e4)^2)

      