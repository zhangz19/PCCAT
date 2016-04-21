

#######################  Functions List  #############################
#########   for PCCAT 1.0    Zhen Zhang zhangz19@stt.msu.du   ########
######################################################################

#### check required R packages
.checkpkg <- function(){
options(warn=-1)
.lib <- library('rgl',logical.return=T, verbose = F)               
if(!.lib) install.packages('rgl',lib=.libPaths()[1])
.lib <- library('NADA',logical.return=T, verbose = F, warn.conflicts = F)               
if(!.lib) install.packages('NADA',lib=.libPaths()[1])
.lib <- library('fpc',logical.return=T, verbose = F, warn.conflicts = F)               
if(!.lib) install.packages('fpc',lib=.libPaths()[1])
.lib <- library('mclust',logical.return=T, verbose = F, warn.conflicts = F)               
if(!.lib) install.packages('mclust',lib=.libPaths()[1])
.lib <- library('ecodist',logical.return=T, verbose = F, warn.conflicts = F)               
if(!.lib) install.packages('ecodist',lib=.libPaths()[1])
library(rgl); library(NADA); library(fpc); library(mclust); library(ecodist)
}

##### basic 
.read <- function(){
cat("\n\n Please press Enter key to continue...\n")
readline()
}

#this function exists for R version > 2.9.2
#grepl <- function(pattern, x){
#  if(length(grep(pattern,x))!=0) return(1)
#  else return(0)
#}

##### customize the graphic output in pccat
.par0 <- function(){
    par(las = 0, cex.axis=.8, font.axis=1, font.lab=8, font.main=7, 
        cex.main=1.2, font.sub=9, cex.lab=.9, 
        mar=c(5,4,4,2)+.1, mex=1, mai = c(1.02,0.82,0.82,0.42),
        yaxt="s", mfrow=c(1,1))
}

#### get and check the input by users
.scanf <- function(ind, ranges = 1){
    ## ind: 1: integer; avector is also allowed
    if(ind == 1)
      { leave <- 0
        while(!leave){
        input <- scan("",character(0),nlines=1,quiet=T)
        if(input[1] == 'quit') stop('Running stopped')
        ninput <- 0; nn <- 1
        for(i in 1:length(input)){
          if(grepl(":",input[i])){
            tmp <- input[i]
            tmpind <- as.vector(sapply(gregexpr(':',tmp),function(x) x))
            pstart <- as.numeric(substr(tmp,1,tmpind-1))
            pend <- as.numeric(substr(tmp,tmpind+1,nchar(tmp)))
            len <- pend - pstart
            ninput[nn:(nn+len)] <- c(pstart:pend); nn <- nn+len+1}
          else {ninput[nn] <- as.numeric(input[i]); nn <- nn+1}
        }
        input <- sort(unique(ninput))     
        leng <- length(input)
        checkInd <- numeric(leng)
        for (j in 1:leng)  checkInd[j] <- all(input[j] != ranges)
        leave <- all(checkInd == 0)
        if(!leave) cat("Some index not found. Please type again:\n")
        }
      }
    if(ind == 2)             # yes or no
      { input <- scan("",character(0),nlines=1,quiet=T)
        while(input != 'y' & input != 'n'){
        cat("please type y or n:\n")
        input <- scan("",character(0),nlines=1,quiet=T)
        }
      }
    return(input)
}

##### customized setting for scatterplot matrix
.panelhist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
.panelcor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- .9/strwidth(txt)
    tcol <- 1; if(r > .85) tcol <- 2
    else if(r < .15) tcol <- 4
    text(0.5, 0.5, txt, cex = cex.cor, col = tcol)
}

##### find the optimal number of rows and columns for multiple plot
.optmn <- function(n) {
     a <- ceiling(sqrt(n)); b <- floor(sqrt(n))
     if(a*b < n) a <- a+1
     return(c(a,b))
}


##### principle coordinates plot for clustering results:
.PCplot <- function(dat, grp, .colGrou, .ugrou, retpca = FALSE){
     stay <- 1;
     while(stay == 1){
     .clpca <- princomp(dat)
     cat(" Choose two principle components you want to plot\n")
     .pccoords <- .scanf(1,c(1:p));  
     .pccoords <- sort(.pccoords)
     .pc1 <- as.character(.pccoords[1]); .pc2 <- as.character(.pccoords[2])
     .xpccl <- predict(.clpca)[,.pccoords]
     palette("default")
     .par0()
     eqscplot(.xpccl,type="n",main='PCA plot for Clustering Analysis', 
         xlab=paste('Principle Component ',.pc1),ylab=paste('Principle Component ',.pc2))
     text(.xpccl,labels=grp, col=.ngrou+1, cex = .8, font=3)
     if(.colGrou) {
         Pmin <- min(.xpccl[,1])
         tick <- (max(.xpccl[,1])-Pmin)/length(.ugrou)
      mtext(.ugrou, side = 3, at = Pmin + tick/2 + (c(1:(length(.ugrou)))-1)*tick, 
      line= .2, col = 2:(length(.ugrou)+1), font = rep(2,length(.ugrou)))
      }   
     if(retpca){ stay <- 0; return(list(pcas = .xpccl)) }
     else{
     cat("1. I want to specify another two Principle Components\n")
     cat("2. I want to specify another number of clusters\n")
     cat("3. exit\n")
     ans <- .scanf(1,c(1:3))
     if(ans == 2) {stay <- 0; return("y")}
     if(ans == 3) {stay <- 0; return("n")}
        }
     }
}
     
     
##### calculate the within and between square sum for clustering results
##### if out = 2, return the criterions about within and between scatter matrix 
.WB <- function(dat, cl){ 
     n <- nrow(dat); p <- ncol(dat); ucl <- unique(cl); k <- length(ucl)
     sm <- matrix(rep(0,k*p),ncol=p); mm <- colMeans(dat)
     bbs <- ws <- numeric(k)
     for(i in 1:k) {
         sdat <- dat[which(cl == ucl[i]),] ; ni <- nrow(sdat)
         sm[i,] <- colMeans(sdat)
            ws[i] <- sum((ni-1)*apply(sdat,2,var))
            bbs[i] <- as.numeric(ni*t(sm[i,]-mm)%*%(sm[i,]-mm))
            }
         sumWs <- sum(ws); sumBs <- sum(bbs)
     return(c(sumWs,sumBs))
}

.WB2 <- function(dat, cl){  #out=1
     n <- nrow(dat); p <- ncol(dat); ucl <- unique(cl); k <- length(ucl)
     sm <- matrix(rep(0,k*p),ncol=p); mm <- colMeans(dat)
     #if(out == 1)  
     bbs <- ws <- numeric(k)
     #if(out == 2)  
     bss <- wss <- matrix(rep(0,p*p),ncol=p)
     #if(out != 1 && out != 2) stop("out should be either 1 or 2")
     for(i in 1:k) {
         sdat <- dat[which(cl == ucl[i]),] ; ni <- nrow(sdat)
         sm[i,] <- colMeans(sdat)
         #if(out == 1){
            ws[i] <- sum((ni-1)*apply(sdat,2,var))
            bbs[i] <- as.numeric(ni*t(sm[i,]-mm)%*%(sm[i,]-mm))
            #}
         #if(out == 2){
            for(jj in 1:ni){
                 tmp1 <- ((as.numeric(sdat[jj,]-sm[i,])))
                 wss <- wss + tmp1%*%t(tmp1)
         #        }
             bss <- bss + ni*(sm[i,]-mm)%*%t(sm[i,]-mm)
         }
     }
     #if(out == 1) {
                   sumWs <- sum(ws); sumBs <- sum(bbs)
                   #return(c(sumWs,sumBs))
                   #}
     #if(out == 2) {
                   tW <- sum(diag(wss))
                   tWB <- dWB <- dW <- 0
                   if(det(wss))  dW <- det(wss)
                   if(det(wss+bss)) {tWB <- sum(diag(solve(wss+bss)%*%wss))
                                 dWB <- det(wss)/det(wss+bss)}
                   list("sumWs"=sumWs, "sumBs"=sumBs,  "dW"=dW, "tWB"=tWB, "dWB"=dWB)      #"W"=wss    "tW"=tW,
                #   }
}
     
         
##### visualization methods for model-based and fuzzy clustering results
.Vcluster <- function(dat, method=1){
   n <- nrow(dat); .par0()
   if(method == 1){
      #################   model-based clustering   
      #largest BIC for EM initialized by hierarchical clustering
      fit <- Mclust(dat)
      print(fit); plot(fit,dat)
      pro <- fit$z; cl <- fit$classification; ncl <- length(unique(cl))  
      if(ncl == 1) stop("The model-based clustering produces trivial results\n") 
      .sumCluster(dat,cl,.sites)}
   if(method == 2){
      ################# fuzzy analysis 
      cat("please specify a number of clusters:\n")
      ncl <- .scanf(1,c(1:n))
      fit <- fanny(dat,ncl)
      pro <- fit$membership; cl <- fit$clustering 
      .sumCluster(dat,cl,.sites) }
   loc <- .PCplot(dat, cl, .colGrou, .ugrou, retpca=TRUE)$pcas  
   cat("Do you want to superimpose the segment plot of membership?\n")
   palette(rainbow(ncl))
   if(.scanf(2)=="y") stars(pro, locatio=loc, draw.segm = T, add = T, 
                                              scale=F,len=.5, cex = .7)  
   palette("default")
}
     
##### find the maximum of absolute values and return with signs
.maxa <- function(a){
    tmp1 <- abs(a); tmp2 <- sign(a[tmp1 == max(tmp1)]); tmp3 <- tmp2>0
    if(tmp3[1]) return(max(tmp1))
    else return(-max(tmp1))
}


#####  draw a random sample from any cdf. If detect limit(DL) is specified, 
#####  the sample will be restricted to be less than DL.
.rcdf <- function(.cdf, X, i, j, DL = NULL){
    jj <- 1; len <- length(X) 
    while(jj<1e2){
    u <- runif(1)
    if(u<.cdf[2]) X0 <- X[1]
    else {if(u>.cdf[len]) X0 <- X[len]
    else{ ii <- sum(.cdf<u)
          if(.cdf[ii]==.cdf[ii+1]) X0 <- X[ii]
          else X0 <- X[ii]+(u-.cdf[ii])*(X[ii+1]-X[ii])/(.cdf[ii+1]-.cdf[ii])
          }}
    if(!length(DL)) jj <- 1e2+1
    else {if(X0 < DL) jj <- 1e2+1
         else jj <- jj+1 }
    }
    if(jj == 1e2) 
    { warning('row ',i,' col ',j,': the DL is too small to impute. DL/2 is assigned.\n')
      return(DL/2) }
    else return(X0)
}
     
#####  generate normal/log-normal distribution 
.rlog <- function(Index=1, .mean, .sd, i,j,DL = NULL){
    if(Index == 1) # log-normal
    { .mu <- log(.mean) - .5*log(1+.sd^2/.mean^2)
      .sig <- sqrt(log(1+.sd^2/.mean^2))   }
    if(Index == 2) {
      .mu <- .mean; .sig <- .sd  } 
      jj <- 1; 
      while(jj<1e2){
      u <- rnorm(1,.mu,.sig)
      if(!length(DL)) jj <- 1e2+1
      else {if(u < DL) jj <- 1e2+1
         else jj <- jj+1 }
      }
      if(jj == 1e2) 
      { warning('row ',i,' col ',j,': the DL is too small to impute. DL/2 is assigned.\n')
        return(DL/2) }
      else { if(Index==1) return(exp(u))
             else return(u) }
}
    
##### summarize a clustering result as a matrix
.sumCluster <- function(dat,cl,nam){
      cat('\nclustering results:\n')
      print(matrix(c(nam,cl),ncol=2), quote = F)
      .tmp <- .WB2(dat,cl)
      cat('\ntrace of within-cluster scatter matrix:       ',.tmp$sumWs, '   small->better\n')
      cat('\ntrace of between-cluster scatter matrix:      ',.tmp$sumBs, '   large->better\n')
      cat('\ndeterminant of within-cluster scatter matrix: ',.tmp$dW,    '   small->better\n')
      cat('\ntrace of (S_total^-1 S_Between):              ',.tmp$tWB,   '   large->better\n')
      cat('\ndeterminant of (S_Between / S_total):         ',.tmp$dWB,   '   large->better\n\n')
}
      
          