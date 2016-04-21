
####### handling missing data

.nam <- names(.Mtrs); .attri <- .nam
.rnam <- row.names(.Mtrs)
n <- nrow(.Mtrs); p <- ncol(.Mtrs)

.kind <- 1; .kvec <- 0
for(i in 1:ncol(.Mtrs)){
    if(all(as.matrix(.Mtrs[,i])==.Mtrs[1,i])){
    print(i)
        if(.kind==1) cat("\n these constant variables will be omitted:\n")
        cat(.nam[i],"\t"); .kvec[.kind] <- i; .kind <- .kind + 1;
        }
}
if(.kind!=1) .Mtrs <- .Mtrs[-.kvec]

.nam <- names(.Mtrs); .attri <- .nam
p <- ncol(.Mtrs)


#.ind <- sort(as.numeric(na.action(na.omit(.Mtrs))))
.rr <- 1;  .ind <- 0
for(.i in 1:n){
      .j <- 1
      while(.j<=p&&grepl('[0-9]',.Mtrs[.i,.j]))  .j <- .j+1
      if(.j<=p) {.ind[.rr] <- .i; .rr <- .rr+1   }
 }

if(.ind[1]){
cat("missing data are detected.\nDo you want to exclude the corresponding columns or rows?\n")
cat("\n1. I want to exclude the rows\n")
cat("2. I want to exclude the columns\n")
#cat("3. I want to impute the missing counts as censored data with large detection limit\n")
.ans <- .scanf(1,c(1:2))
if(.ans == 1){
cat('these samples with missing data will be omitted:\n')
print(.rnam[.ind])
.Mtrs <- .Mtrs[-.ind,]
    }
if(.ans == 2){
  .rr <- 1;  .indC <- 0
  for(.j in 1:p){
      .i <- 1
      while(.i<=n&&grepl('[0-9]',.Mtrs[.i,.j]))  .i <- .i+1
      if(.i<=n) {.indC[.rr] <- .j; .rr <- .rr+1   }
   }
cat('these variables with missing data will be omitted:\n')
print(.nam[.indC])
.Mtrs <- .Mtrs[,-.indC]
    }
#if(.ans == 3){
#for(.i = 1:nrow(trs)){
#  for(.j = 1:ncol(trs)){
#     if(is.na(trs[.i,.j])) trs[.i,.j] <- 'NA'
#  }}}
}

.attri <- .nam <- names(.Mtrs)
.sites <- .rnam <- row.names(.Mtrs)
n <- nrow(.Mtrs); p <- ncol(.Mtrs)

.read()