"n" <- 10
"Y" <- c(0,1,1,1,1,1,0,0,0,0)
"R" <- c(100,230,123,120,1231,100,230,1230,1200,123010)
temp <- Y * R
V <- temp[which(temp != 0)]
nV <- length(V)
"missing" <- rep(0, n)
missing[which(is.na(Y))] <- R[which(is.na(Y))]