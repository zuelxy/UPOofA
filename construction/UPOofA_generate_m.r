position2component=function(perms){ 
  drugs=perms
  for(i in 1:nrow(perms)) { if(sum(abs(perms[i,]))>0) drugs[i,]=order(perms[i,]) }
  drugs-1 
}

component2position<-function (x){
  x <- x+1
  m=length(x[1,]);
  n=length(x[,1]);
  y=matrix(rep(0,n*m),nrow=n);
  for(i in 1:n){
    for(j in 1:m){
      loc=x[i,j]
      y[i,loc]=j
    }}
  return(y)
}

PM <- YuanRu(82)
UPOofA <- position2component(PM)

save(PM, UPOofA, file = "UPOofA_AD_20_1.RData")

save(PM, UPOofA, file = "UPOofA_AD_20_2.RData")

L1_min(PM)

AD_OofA(PM)

ADOofA_LB(14, 14)
