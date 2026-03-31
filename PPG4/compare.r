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

### find the corresponding indexes 
findingsub <- function(pos, data_full){
  # pos: the position matrix of design
  # data_full: the position matrix of full permutations
  sub = c()
  for(i in 1:dim(pos)[1])
  {
    sub =c(sub,which(apply(data_full, 1, function(x) all(x == pos[i,]))))
  }
  sub = sort(c(sub))
  return(sub)
}

library(mlegp)
######### full data #######
# full data (component order):
dat = dat.full = as.matrix(read.csv("data_full4.csv", row.names = 1, header = T))
rownames(dat) <- colnames(dat) <- rownames(dat.full) <- colnames(dat.full) <-  NULL

# mapping to component position
dat.full[,1:4] <- component2position(dat.full[,1:4])
# transform component position into [0, 1] space (for fitting model)
dat[,1:4] = (dat.full[,1:4]-0.5)/4

all_index <- 1:nrow(dat)
result_a <- result_b <- result_d <- result_e <- matrix(ncol = 3, nrow = 10)

for (t in 1:10) {
  o <- paste0("design/PPG4_oofa_design_", t, ".RData")
  load(o)
  
  ######## maximin #######
  maximin_pm <- component2position(maximin_oofa)
  
  sub_a <- findingsub(maximin_pm, dat.full[,1:4])
  
  md_a = mlegp(dat[sub_a,1:4], dat[sub_a,5])
  py_a <- predict(md_a, dat[,1:4])
  
  
  result_a[t, ] <- c(cor(dat[,5], py_a[,1]), cor(dat[-sub_a,5], py_a[-sub_a,1]), 
                     sqrt(mean((dat[-sub_a,5] - py_a[-sub_a,1])^2)))
  pred_Mm <- data.frame(pred_vecindx = setdiff(all_index, sub_a),
                           true_resp = dat[-sub_a,5],
                           pred_resp = py_a[-sub_a,1])
  
  ####### minimax #######
  minimax_pm <- component2position(minimax_oofa)
  
  sub_b <- findingsub(minimax_pm, dat.full[,1:4])
  
  md_b = mlegp(dat[sub_b,1:4], dat[sub_b,5])
  py_b <- predict(md_b, dat[,1:4])
  
  result_b[t, ] <- c(cor(dat[,5], py_b[,1]), cor(dat[-sub_b,5], py_b[-sub_b,1]), 
                     sqrt(mean((dat[-sub_b, 5] - py_b[-sub_b,1])^2)))
  pred_mMD <- data.frame(pred_vecindx = setdiff(all_index, sub_b),
                        true_resp = dat[-sub_b,5],
                        pred_resp = py_b[-sub_b,1])
  
  ######## UPOofA_AD #######
  UPOofA_AD_pm <- component2position(UPOofA_AD_oofa)
  
  sub_d <- findingsub(UPOofA_AD_pm, dat.full[,1:4])
  
  md_d = mlegp(dat[sub_d,1:4], dat[sub_d,5])
  py_d <- predict(md_d, dat[,1:4])
  
  
  result_d[t, ] <- c(cor(dat[,5], py_d[,1]), cor(dat[-sub_d,5], py_d[-sub_d,1]), 
                     sqrt(mean((dat[-sub_d, 5] - py_d[-sub_d,1])^2)))
  pred_ad <- data.frame(pred_vecindx = setdiff(all_index, sub_d),
                        true_resp = dat[-sub_d,5],
                        pred_resp = py_d[-sub_d,1])
  
  
  ######## UOofA #######
  UOofA_pm <- component2position(UOofA_oofa)
  
  sub_e <- findingsub(UOofA_pm, dat.full[,1:4])
  
  md_e = mlegp(dat[sub_e,1:4], dat[sub_e,5])
  py_e <- predict(md_e, dat[,1:4])
  
  result_e[t, ] <- c(cor(dat[,5], py_e[,1]), cor(dat[-sub_e,5], py_e[-sub_e,1]), 
                     sqrt(mean((dat[-sub_e, 5] - py_e[-sub_e,1])^2)))
  
  pred_dd <- data.frame(pred_vecindx = setdiff(all_index, sub_e),
                        true_resp = dat[-sub_e,5],
                        pred_resp = py_e[-sub_e,1])
  
  p <- paste0("prediction/PPG4_pred_", t, ".RData")
  save(pred_Mm, pred_mMD, pred_ad, pred_dd, file = p)
}

apply(result_a, 2, mean)
#  0.95778164 0.93346181 0.09161247 0.20000000
#  0.8850072 0.7919882 0.1396446 0.6000000
#  0.96231290 0.94173916 0.08826999 0.50000000
#  0.9193366 0.8599538 0.1092253 0.5000000

