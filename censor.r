
####### handling censored data

cat('\nckecking censored data: \n')
.tmpMtrs = .Mtrs
source('functions.r')
.Mtrs <- as.matrix(.Mtrs)
bb <- .Mtrs

.label <- sum(grepl('[<a-df-zA-Z]',.Mtrs))
        
if(.label!=0) {
cat('\ncells with non-numeric type detected:\n')
cat('1. I want to detect all non-numeric characters\n')
cat('2. I want to specify the flags for censoring data\n')
.ccen <- .scanf(1,c(1:2))
if(.ccen == 2) {
   cat('specify the flags, for example: U < \n')
   .flag <- scan("",character(0),nlines=1,quiet=T) }

.indi <- matrix(numeric(n*p),ncol=p); .ii <- 1; .qualifier <- 0
for(i in 1:n){
  for(j in 1:p){
      if(.ccen == 1) .indi[i,j] <- grepl('[<a-df-zA-Z]',.Mtrs[i,j])
      if(.ccen == 2) {
           .tmpIndi <- numeric(length(.flag))
           for(ij in 1:length(.flag)) 
              .tmpIndi[ij] <- grepl(.flag[ij],.Mtrs[i,j])
           .indi[i,j] = !all(.tmpIndi==0)       
       }
      if(.indi[i,j]){
        .mix <- .Mtrs[i,j]
        .quaind <- as.vector(sapply(gregexpr('[<a-df-zA-Z]',.mix),function(x) x))
        for(.jj in (1:length(.quaind))){
                   .po <- .quaind[.jj]
                   .qualifier[.ii] <- substr(.mix,.po,.po); .ii <- .ii+1
                                     }
                    }
               }
           }

.uq <- unique(.qualifier); .counts <- 0
for (i in 1:length(.uq)) .counts[i] <- sum(.qualifier==.uq[i])
cat('\n',sum(.indi),'potential censoring flags over total of ',n*p,' are detected\n')
cat('\nthe flags with frequencies: ','\n')
cat(rbind(.uq,':',.counts,' '),'\n')
cat('\nthe percentage for each column with censoring data:\n')
cat("\n Please press Enter key to continue...\n")
readline()
cat("\n--------------------------------------------------------------------------\n")
.k1 <- which(colSums(.indi)!=0)                   
for(.k2 in 1:length(.k1)) cat(.k1[.k2],' ',.attri[.k1[.k2]],': ',
                 round(sum(.indi[,.k1[.k2]])/nrow(.indi),4)*100,'%', fill=T)
cat("\n--------------------------------------------------------------------------")

.ansDrop <- 'n'
while(.ansDrop=='n'){
cat("\nI want to drop the columns with censoring percentage higher than: (e.g 50)\n")
.ansn <- scan("",double(0),nlines=1,quiet=T)/100*nrow(.indi)
.tmpDrop <- which(colSums(.indi)>.ansn) 
.tmpInd <- which(colSums(.indi)!=0 & colSums(.indi)<=.ansn); 
cat("the following comluns will be dropped:\n")
cat(.tmpDrop)
cat("\nare you sure to drop them? y/n\n")
.ansDrop <- .scanf(2)
}

cat("\nfor data imputation:")
cat("\n1. I want to assign the half of the detection limit(DL)\n")
cat("2. I want to fit a normal/lognormal distribution with ML estimation\n")       
cat("3. I want to fit the ECDF with Kaplan-Meier method\n")
.ans <- .scanf(1,c(1:3))

for(j in 1:p){
  for(i in 1:n){
    if(grepl('[<a-df-zA-Z]',.Mtrs[i,j])){
      .tmp <- gsub('[<a-zA-Z]',' ',.Mtrs[i,j])           
      .Mtrs[i,j] <- as.numeric(.tmp)   
        }
    }
}

if(.ans == 1){
for(j in .tmpInd){
  for(i in 1:n){
    if(.indi[i,j]){
      .tmp <- as.numeric(.Mtrs[i,j])
      .Mtrs[i,j] <- .tmp/2 }
    }
  }
}

if(.ans==2){
  cat('specify a number of replications for imputation:\n')
  .Time <- .scanf(1,c(1:1e5))
  .pp <- .optmn(length(.tmpInd))
  .par0()
  par(mfrow=c(.pp[1],.pp[2]), cex.main=.7,
      mar=c(5,4,4,2)+.01,mai=c(1,1,1,.5)*.1, mex=.3)
   for(j in .tmpInd){
    .tmp0 <- .tmp <- as.numeric(.Mtrs[,j])
    .tmp0[which(.indi[,j]!=0)] <- 0 
    .tmpp <- shapiro.test(.tmp)$p.value; .tmpI <- as.integer(.tmpp>.1)
    if(!.tmpI) .tmpFit <- cenmle(.tmp,.indi[,j]==1)
    if(.tmpI) .tmpFit <- cenmle(.tmp,.indi[,j]==1,dist='gaussian') 
    .mean <- as.numeric(mean(.tmpFit)[1])
    .sd <- sd(.tmpFit)
    for(.time in 1:.Time){
     for( i in which(.indi[,j]!=0)) 
       {.tmp[i] <- .rlog(.tmpI+1,.mean,.sd,i,j, as.numeric(.Mtrs[i,j]))
        .tmp0[i] <- .tmp0[i] + .tmp[i] }
     }
     for( i in which(.indi[,j]!=0)) .tmp0[i] <- .tmp0[i]/.Time
        .Mtrs[,j] <- .tmp0
   plot(.tmpFit,main=paste(j,':',.attri[j]))
   #plot.ecdf(.tmp0/.Time,col='red',add=T)
  }
  .par0()
}

if(.ans==3){
     cat('specify a number of replications for imputation:\n')
     .Time <- .scanf(1,c(1:1e5))
     .pp <- .optmn(length(.tmpInd))
     .par0()
     par(mfrow=c(.pp[1],.pp[2]), cex.main=.7,
      mar=c(5,4,4,2)+.001,mai=c(1,.5,1,.5)*.1, mex=.3)
     for(j in .tmpInd){
       .tmp0 <- .tmp <- as.numeric(.Mtrs[,j])
       .tmp0[which(.indi[,j]!=0)] <- 0 
       .tmpFit <- cenfit(.tmp,.indi[,j]==1)
       .tmpCdf <- summary(.tmpFit)$prob; .tmpX <- summary(.tmpFit)$obs
       for(.time in 1:.Time){
         for( i in which(.indi[,j]!=0)){
         .tmp[i] <- .rcdf(.tmpCdf, .tmpX, i,j, as.numeric(.Mtrs[i,j]))    
         .tmp0[i] <- .tmp0[i] + .tmp[i] }
           }
       for( i in which(.indi[,j]!=0)) .tmp0[i] <- .tmp0[i]/.Time
        .Mtrs[,j] <- .tmp0
       plot(.tmpFit,main=paste(j,':',.attri[j]))
       plot.ecdf(.tmp0,col='red',add=T)
     }
    .par0()
}
   
if(.ansDrop == 'y'&& length(.tmpDrop)!=0) {.Mtrs <- .Mtrs[,-.tmpDrop] 
                 .attri <- .attri[-.tmpDrop]}

}

if(!.label) cat('no censored data was found.\n')
.read()


