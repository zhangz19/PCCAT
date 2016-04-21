
##################  Clustering   #################
setwd(.WD)
source("functions.r")

n <- nrow(dat)
p <- ncol(dat)

.stay <- "y"
while(.stay == "y")
{
cat(" ################  Clustering analysis  #################### \n")
cat(" 1. Hierachical Clustering \n")
cat(" 2. Partitioning Clustering \n")
cat(" 3. Fuzzy Clustering \n")
cat(" 4. Model-based Clustering \n")
cat(" 5. Exit\n")
.choice <- .scanf(1,c(1:5))

if(.choice == 5) .stay <- "n"
else {

if(.choice == 1 || .choice == 2){

cat(" Which distance measure do you want to adopt?\n")
cat(" 1.euclidean \n 2.maximum \n 3.manhattan \n 4.canberra \n",
   "5.binary \n 6.minkowski \n 7.gower\n 8.bary-curtis\n")# 8.bray\n 9.kulczynski\n 10.morisita \n 11.horn\n 12.binomial\n 13.jaccard\n 14.mountford\n 15.raup\n")
.distind <- .scanf(1,c(1:8))
.distlist <- c("euclidean", "maximum", "manhattan", "canberra",  "binary", "minkowski", "gower")#, "bray")#, "kulczynski", "morisita", "horn", "binomial", "jaccard", "mountford", "raup")
if (.distind == 6) {
cat(" Choose the power of minkowski distance:\n")
.Minp <- scan("",integer(0),nlines=1,quiet=T)
.d <- dist(dat, method = .distlist[.distind], p = .Minp)
}
else if(.distind == 7) .d <- as.dist(as.matrix(daisy(dat, metric = 'gower')))
else if(.distind == 8) .d <- bcdist(dat)
else .d <- dist(dat, method = .distlist[.distind])
}


if(.choice == 1){
##################   Hierarchical clustering #############################
.hstay <- 1
while(.hstay == 1){
cat(" Which hierarchical clustering algorithm do you want to implement?\n")
cat(" 1.ward \n 2.single \n 3.complete \n 4.average\n") # \n 5.mcquitty\n 6.median\n 7.centroid\n")
.method <- .scanf(1,c(1:4))
.methodlist <- c("ward", "single", "complete", "average")#, "mcquitty", "median", "centroid")
.hcl <- hclust(.d, method = .methodlist[.method])
.hcl$labels <- .sites
.hopt <- 1
 while(.hopt == 1){
  cat("#################   Hierarchical Clustering  ###################\n")
  cat("1. Dendrogram for the clustering results\n")
  cat("2. Principle Coordinates Plot for the clustering results\n")
  cat("3. Choose another methods for Hierarchical clustering\n")
  cat("4. Exit\n")
  .hans <- .scanf(1,c(1:4))
  if(.hans == 4) {.hstay <- 0; .hopt <- 0}
  if(.hans == 3) .hopt <- 0
  if(.hans == 1){  
    .stayh <- "y"; .par0(); par(las=3, cex.axis=.7, font.axis=8)
    while(.stayh == "y"){  
    plot(.hcl,hang=-1,xlab=paste('method = ',
             .methodlist[.method]),ylab='',sub='',main='Dendrogram')
    .CC <- round(cor(.d,cophenetic(.hcl)),3)
    legend('topright', legend= paste("cophenetic coeff. =",
                      as.character(.CC)),cex=.8,text.col= 'blue',
                                             inset=.01, bg="gray90")
    cat("How many clusters do you want to specify in the dendrogram?\n")
    .clnum <- .scanf(1,c(1:n))
    rect.hclust(.hcl, .clnum, border='red')
    .sumCluster(dat,cutree(.hcl,.clnum),.sites)
    cat("Do you want to specify another number of clusters? y/n\n")
    .stayh <- .scanf(2)
                         }
    .par0()
                 }
if(.hans == 2){
   .clpc <- "y"
   while(.clpc == "y"){
   cat(" How many clusters do you want to specify in the principle component coordinates?\n")
   .clpcnum <- .scanf(1,c(1:n)); .hgrp <- cutree(.hcl,k=.clpcnum)  
   .sumCluster(dat,.hgrp,.sites)
   .clpc <- .PCplot(dat, .hgrp, .colGrou, .ugrou)
                       }
              }
          }    
      }
}



if(.choice == 2){
#################   partitioning clustering  ###################
.kopt <- 1;
while(.kopt == 1){
cat("#################   Partitioning Clustering  ###################\n")
cat("1. I want to specify a number of clusters\n")
cat("2. I want to use the number of clusters estimated\n")
cat("3. Exit\n")
.ans <- .scanf(1,c(1:3))
if(.ans == 3) .kopt <- 0
if(.ans == 1){    
   .wss <- numeric(15) #number of clusters from 1 to 15  
   .wss[1] <- (nrow(dat)-1)*sum(apply(dat,2,var))
   for (i in 2:15) {
        .tmpfit <- pam(.d, i, diss = T, cluster.only=T)
        .wss[i] <- .WB(dat,.tmpfit)[1]
       }
  .kcl <- "y"; dev.new()
   while(.kcl == "y"){
   par(lab = c(10,8,7), las = 3, cex = 1.1)
   plot(c(1:15),.wss[1:15],type='b',xlab='Number of clusters',ylab='within groups sum of squares(wss)')
   cat("To have a good choice, try different numbers to see whether it greatly reduces the WSS\n")
   cat("Please try a number of clusters\n")
   .kclnum <- .scanf(1,c(2:15))
   .percen_redu <- round(100*(1-.wss[.kclnum]/.wss[1]),3)
   sprintf("The percentage reduction of wss for %d is %.2f%%", .kclnum, .percen_redu)
   .par0(); legend(.wss[.kclnum],paste(.percen_redu,"%, rope ",
             round(.wss[.kclnum]-.wss[.kclnum-1],3)), bty='n', cex=.7)
   abline(h=.wss[.kclnum],lty=2) ; abline(v=.kclnum,lty=3)
   cat("Do you want to try another number of clusters? y/n\n")
   .kcl <- .scanf(2)
                  }
   cat("Finally determine the number of clusters:\n")
   .kncl <- .scanf(1,c(1:p))
   .kgrp <- pam(.d, .kncl, diss=T)$clustering
   .sumCluster(dat,.kgrp,.sites)
            } 

if(.ans == 2){
## estimated by optimum average silhouette width
.par0()
.pamfit <- pamk(dat,2:15)
.pamfitk <- .pamfit$nc; .pamfitobj <- .pamfit$pamobject
#plot(silhouette(pam(.d,.pamfitk, diss=T)))
sprintf("The optimal number of cluster is %d", .pamfitk)
.kgrp <- .pamfitobj$clustering
.read()
.sumCluster(dat,.kgrp,.sites)
}

cat("Principle Coordinate plot of clustering results:\n")
cat("\n Please press Enter key to continue...\n")
readline()
.stayk <- .PCplot(dat, .kgrp, .colGrou, .ugrou)
if(.stayk == "n") .kopt <- 0
  }
}


if(.choice == 4){
.Vcluster(dat, method=1)
}

else if(.choice == 3){
################   fuzzy cluster ##############################
.Vcluster(dat, method=2)
    }

 }

}