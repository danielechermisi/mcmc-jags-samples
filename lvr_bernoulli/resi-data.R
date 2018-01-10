"n" <- 10
"Y" <- c(NA,NA,1,1,1,1,0,0,0,0)
"fatturato" <- c(100,230,123,120,1231,100,230,1230,1200,123010)
"missing" <- rep(0, n)
missing[which(is.na(Y))] <- fatturato[which(is.na(Y))]