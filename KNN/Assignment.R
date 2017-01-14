setwd("C:\\Users\\Deepak Kumar Sisodia\\Desktop\\ML Assignments\\KNN")

getfeaturematrix=function(inp.data,feature.names){
  inp.feature.matrix=as.matrix(inp.data[,feature.names])
  inp.feature.matrix=cbind(rep(1,nrow(inp.feature.matrix)),inp.feature.matrix)
  return(inp.feature.matrix)
  
}

normalize_features= function(inp.data.frame,norm_f){
  # inp.data.frame=data.frame(c1=c(1,2,3,4,5),c2=c(6,7,8,9,10),c3=c(2,7,9,4,7))
  # feature.names=c("c1","c2")
    for (i in 1:ncol(inp.data.frame)){
      inp.data.frame[,i] = inp.data.frame[,i]/norm_f[i]
      
    }
    return(as.matrix(inp.data.frame))

}

norms=function(inp.data.frame){
  #inp.data.frame=data.frame(c1=c(1,2,3,4,5),c2=c(6,7,8,9,10))
  normvector=apply(inp.data.frame,2,function(x) sqrt(sum(x^2)))
  return(normvector)
}

CalculateDistance=function(f1,f2){
  distance= sqrt(sum((f1 - f2)^2))
  
}

sales=read.csv("kc_house_data_small_train.csv")
valid=read.csv("kc_house_data_validation.csv")
test=read.csv("kc_house_data_small_test.csv")
str(sales)
columns=c("bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","grade","sqft_above",
          "sqft_basement","yr_built","yr_renovated","zipcode","lat","long","sqft_living15","sqft_lot15")

output.sales.matrix=as.matrix(sales$price)
tail(output.sales.matrix)

input.sales.feature.matrix=getfeaturematrix(sales,columns)
input.valid.feature.matrix=getfeaturematrix(valid,columns)
input.test.feature.matrix=getfeaturematrix(test,columns)

input.sales.feature.df=data.frame(input.sales.feature.matrix)
input.valid.feature.df=data.frame(input.valid.feature.matrix)
input.test.feature.df=data.frame(input.test.feature.matrix)

#Normalize the features
norm.sales.vector=norms(input.sales.feature.df)

normalized.sales.feature.matrix=normalize_features(input.sales.feature.df,norm.sales.vector)
normalized.valid.feature.matrix=normalize_features(input.valid.feature.df,norm.sales.vector)
normalized.test.feature.matrix=normalize_features(input.test.feature.df,norm.sales.vector)

###################
d=CalculateDistance(normalized.sales.feature.matrix[10,],normalized.test.feature.matrix[1,])
d

###################
for(j in 1:10){
  
  diff =CalculateDistance(normalized.sales.feature.matrix[j,],normalized.test.feature.matrix[1,])
  print(paste("Distance b/w ",j,"th train house and first test house is ",diff,sep = ""))
}

#####################
distance.vector.1=apply(normalized.sales.feature.matrix,1,function(x) CalculateDistance(x,normalized.test.feature.matrix[1,]))

distance.vector.3=apply(normalized.sales.feature.matrix,1,function(x) CalculateDistance(x,normalized.test.feature.matrix[3,]))
index.NN=which.min(distance.vector.3)
NN.pred=distance.vector.3[index.NN]
price.pred.1=sales$price[index.NN]
########################

k_nearest_neighbors=function(k,feature_train,features_query){
  distance.vector=apply(feature_train,1,function(x) CalculateDistance(x,features_query))
  l=sort(distance.vector,index.return=T)
  out.df=as.data.frame(l)
  return(out.df[seq(1,k),])
}

outd=k_nearest_neighbors(4,normalized.sales.feature.matrix,normalized.test.feature.matrix[3,])
head(outd)
###########################

predict_output_of_query=function(k,feature_train,output_train,features_query){
  distance.vector=apply(feature_train,1,function(x) CalculateDistance(x,features_query))
  l=sort(distance.vector,index.return=T)
  out.df=as.data.frame(l)
  top.k.indices=out.df$ix[1:k]
  top.k.average=mean(output_train[top.k.indices,1])
  return(top.k.average)
}

out.price=predict_output_of_query(4,normalized.sales.feature.matrix,output.sales.matrix,normalized.test.feature.matrix[3,])
#############################
predict_output=function(k,feature_train,output_train,features_query){
  
  average.price=apply(features_query,1,function(x) predict_output_of_query(k,feature_train,output_train,x))
  return(average.price)

}
out.price.10=predict_output(10,normalized.sales.feature.matrix,output.sales.matrix,normalized.test.feature.matrix[c(1:10),])
###########################
RSS.k=c()
for (k in 1:15){
  predict.for.k=predict_output(k,normalized.sales.feature.matrix,output.sales.matrix,normalized.valid.feature.matrix)
  RSS.k[k]=sum((predict.for.k - valid$price)^2)
  
}

RSS.k # k=8

##########################

predict.test.8=predict_output(8,normalized.sales.feature.matrix,output.sales.matrix,normalized.test.feature.matrix)
RSS.8=sum((predict.test.8 - test$price)^2)









