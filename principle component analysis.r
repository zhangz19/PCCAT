
#######################  PCA   #################
setwd(.WD)
source("functions.r")
options(warn=-1)

.pca <- princomp(dat)  #default cor = F

n <- nrow(dat)
p <- ncol(dat)

sumpca <- matrix(rep(0,3*p), nrow = 3)
sumpca[1,] <- .pca$sdev^2
sumpca[2,] <- cumsum(.pca$sdev^2)
sumpca[3,] <- cumsum(.pca$sdev^2)/sum(.pca$sdev^2)
cat("Summary of Principle Component Analysis is stored in 'sumpca'\n")

dev.new()
par(las = 3, cex.axis=.8)
plot(.pca,main='Variance of Principle Components', ylim = c(0,.pca$sdev[1]^2*1.2))
with(.pca,
     text(x = (1:ncol(dat)*1.1),
          y = .pca$sdev^2,
          labels = paste(round(sumpca[3,]*100, 1),"%"),
          pos = 3, cex = .8))
.par0()
cat("\n Please press Enter key to continue...\n")
readline()

palette("default")
biplot(.pca,cex=.8)
cat("First two Principle Components Coordinates Plot:\n")
cat("\n Please press Enter key to continue...\n")
readline()
print(summary(.pca))
cat('\n\n')

## obtain the loadings
.loadpca <- .pca$loadings
cat("Sort the variables by its loading to which principle component? \n")
.pcNum <- .scanf(1,c(1:p))
.Ind <- order(abs(.loadpca[,.pcNum]),decreasing = T)
.pcMat <- matrix(c(.attri[.Ind],round(.loadpca[.Ind,1:4],3)),ncol=5)
.namMat <- c('variable', 'PC1', 'PC2', 'PC3', 'PC4')
print(rbind(as.character(.namMat), .pcMat), quote = F)
.loadMat4 = matrix(as.numeric(.pcMat[,2:5]),ncol=4)
cat("\n Please press Enter key to continue...\n")
readline()

.gnum <- 2
if(.colGrou) .gnum <- length(.ugrou) 


.stay <- 1
while(.stay){
cat("***************  visualization with PCs  *****************\n")
cat("\n 1. scattermatrix plot\n")
cat("\n 2. detailed scatter plot with 2PCs\n")
cat("\n 3. 3D scatter plot\n")
cat("\n 4. exit\n")
.choi <- .scanf(1,c(1:4))

if(.choi == 1){
.pc4 <- predict(.pca)[,1:5]
.par0()
palette(rainbow(.gnum))
pairs(.pc4,
      diag.panel=.panelhist, cex.labels = 1.5, font.labels = 2,
      pch = 21, bg = c(1:(length(.ugrou)))[unclass(.ngrou)])
.tick <- 1/.gnum
if(.colGrou)
mtext(.ugrou, side = 3, at = c(0.5:(length(.ugrou)-.5))*.tick, 
      line= 2.5, col = 1:length(.ugrou), font = rep(2,length(.ugrou)) )
palette("default")
cat("\n Please press Enter key to continue...\n")
readline()
}


if(.choi == 2){
cat("Choose the two Principle Components:\n")
.chosen <- .scanf(1,c(1:p))
while(length(.chosen) != 2){
  cat('Number of input PCs is not 2. Please type again:\n')
  .chosen <- .scanf(1,c(1:p))}
.pc1 <- as.character(.chosen[1]); .pc2 <- as.character(.chosen[2])
.xpc <- predict(.pca)[,.chosen]
par(las = 0, cex.axis=.8, font.lab=7, font.main=7, cex.main=1.5, font.sub=9)
eqscplot(.xpc,type="n",main='PCA plot for Clustering Analysis',
         xlab=paste('Principle Component ',.pc1),
         ylab=paste('Principle Component ',.pc2))
text(.xpc,labels=c(1:n), col=.ngrou+1, cex=.8, font=8)
if(.colGrou) {
         .Pmin <- min(.xpc[,1])
         .tick <- (max(.xpc[,1])-.Pmin)/length(.ugrou)
      mtext(.ugrou, side = 3, at = .Pmin + .tick/2 + (c(1:(length(.ugrou)))-1)*.tick, 
      line= .2, col = 2:(length(.ugrou)+1), font = rep(2,length(.ugrou)))
      }
      .par0()   
}

if(.choi == 3){
  cat("Choose the three Principle Components:\n")
  .chosen <- .scanf(1,c(1:p))
  while(length(.chosen) != 3){
  cat('Number of input PCs is not 3. Please type again:\n')
  .chosen <- .scanf(1,c(1:p))}
.pc1 <- as.character(.chosen[1])
.pc2 <- as.character(.chosen[2])
.pc3 <- as.character(.chosen[3]);
.xpc <- predict(.pca)[,.chosen]
palette(rainbow(.gnum))
plot3d(.xpc, col=.ngrou, size=10)
.rang <- range(.xpc[,3])[2]-range(.xpc[,3])[1]
if(.colGrou)
  text3d(rep(max(.xpc[,1]),.gnum),rep(max(.xpc[,2]),.gnum),
         rep(max(.xpc[,3]),.gnum)-c(1:.gnum)*.rang/20,
         text=paste(as.vector(.ugrou)),
                    adj=c(1,1), color=c(1:.gnum), cex = .8)
cat("Do you want to impose the names of sites? y/n\n")
ans <- .scanf(2)
if(ans == "y") text3d(.xpc,text = paste(.sites),col = .ngrou, cex = .7)
palette("default")
cat("\n Please press Enter key to continue...\n")
readline()
}

if(.choi == 4) .stay <- 0
}
