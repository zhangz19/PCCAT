
################   visualization  #################
setwd(.WD)
source("functions.r")

n <- nrow(dat); p <- ncol(dat)
.gnum <- length(.ugrou)

cat("The attributes, or chemical parameters are:\n")
for (i in 1:p){ cat(i,.attri[i],'\t', fill=T)};
cat("\n Please press Enter key to continue...\n")
readline()

.stay0 <- 1
while(.stay0){
cat("for data visualization:\n")
cat("1. I want to apply to all columns\n")
cat("2. I want to select a subset of all columns\n")
cat("3. I want to drop a subset of all columns\n")
cat("4. exit\n")
.cho <- .scanf(1,c(1:4))
if(.cho == 4) .stay0 <- 0


else{
.stay1 <- 1
while(.stay1 == 1){
.chosen <- c(1:p)
if(.cho != 1){
cat("Which columns?\n")
.colAttr <- .scanf(1,c(1:p))
if(.cho == 2)  .chosen <- .colAttr 
if(.cho == 3)  .chosen <- .chosen[-.colAttr]
}

.stay <- 1
while(.stay){
cat("********************* visualization ********************\n")
cat("\n 1. scattermatrix plot\n")
cat("\n 2. boxplot with outlier detection\n")
cat("\n 3. segment plot\n")
cat("\n 4. 3D scatter plot\n")
cat("\n 5. choose different columns\n")
cat("\n 6. exit\n")

.ans <- .scanf(1,c(1:6))

if(.ans == 5) .stay <- 0
if(.ans == 6) {.stay <- .stay1 <- 0}

if(.ans == 1) {
######################  scatterplot matrix  ##################
#palette(rainbow(.gnum))
.par0()
pairs(dat[.chosen], #panel=panel.smooth,
      diag.panel=.panelhist, #cex.labels = 1.5, font.labels = 2,
      pch = 21, bg = c(2:(length(.ugrou)+1))[unclass(.ngrou)])
if(.colGrou){
.tick <- 1/length(.ugrou)
mtext(.ugrou, side = 3, at = c(.5:(length(.ugrou)-.5))*.tick, 
      line= 2.5, col = 2:(length(.ugrou)+1), font = rep(2,length(.ugrou)))
   }
cat("\n Please press Enter key to continue...\n")
readline()
pairs(dat[.chosen], lower.panel=panel.smooth, 
       diag.panel=.panelhist, upper.panel=.panelcor) 
if(.colGrou){
.tick <- 1/length(.ugrou)
mtext(.ugrou, side = 3, at = c(.5:(length(.ugrou)-.5))*.tick, 
      line= 2.5, col = 2:(length(.ugrou)+1), font = rep(2,length(.ugrou)))
   }
.par0()
cat("\n Please press Enter key to continue...\n")
readline()
palette("default")
}



if(.ans == 2){
######################  boxplot  ##################
.subdat <- dat[.chosen]
.subattri <- .attri[.chosen]
.par0()
par(las=3, cex.axis=.7, font.axis=8)
.tmp2 <- range(.subdat); .tick <- (.tmp2[2] - .tmp2[1])/6
.tmp <- boxplot(.subdat,col='light blue',ylim=c(min(.subdat)-.tick,max(.subdat)+.tick))
.y = tapply(.tmp$out, .tmp$group, .maxa); .y <- .y-(1-sign(.y))*.tick*.2
with(.tmp,
     text(x = unique(.tmp$group)+1/length(unique(.tmp$group)), .y, 
          labels = round(tapply(out, group, .maxa), 2),
          pos = 3, cex = .8))   
.par0()
cat("Outlier detection:\n")
cat("\n Please press Enter key to continue...\n")
readline()
if(length(.tmp$out)==0) cat("No outlier detected!\n")
if(length(.tmp$out)==1) cat("The unique outlier detected is ", .sites[.tmp$out], "for attribute ", .tmp$names[.tmp$group], "\n") 
if(length(.tmp$out)>1){
cat(.subattri[.tmp$group[1]],":","\t\t",.sites[dat[,.tmp$group[1]]==.tmp$out[1]])
for(j in 2:length(.tmp$out)){
if(.tmp$group[j]!=.tmp$group[j-1]) cat("\n",.subattri[.tmp$group[j]],":","\t\t")
else cat("\t")
.tmpsite <- .sites[.subdat[,.tmp$group[j]]==.tmp$out[j]]
cat(.tmpsite[1],sep = '\t\t')
}}
cat("\n\n Please press Enter key to continue...\n")
readline()
}



######################  segment plot  ##################
if(.ans == 3){
cat("Segment plots\n")
palette(rainbow(length(.chosen)))
.par0()
stars(dat[.chosen],labels=.sites,draw.segments=T,nrow=floor(sqrt(n))+1,
                                       len=.5, cex=.6, key.loc=c(-1,2.5))
.par0()
cat("\n Please press Enter key to continue...\n")
readline()
palette("default") 
}




######################  3D visualization  ##################
if(.ans == 4){
.substay <- "y"
while(.substay == "y"){
cat("3d visualization:\n")
cat("\n Please press Enter key to continue...\n")
readline()
cat("The attributes are:\n")
print(cbind(.attri,c(1:p)))
cat("\n Please press Enter key to continue...\n")
readline()
cat("Please choose 3 attributes: \n")
.colAttr <- .scanf(1,c(1:p))
.gnum <- 2
if(.colGrou) .gnum <- length(.ugrou) 
palette(rainbow(.gnum))
open3d()
plot3d(dat[.colAttr],col = .ngrou, size=10)
.rang <- range(dat[.colAttr[3]])[2]-range(dat[.colAttr[3]])[1]
if(.colGrou) text3d(rep(max(dat[.colAttr[1]]),.gnum),rep(max(dat[.colAttr[2]]),.gnum),
         rep(max(dat[.colAttr[3]]),.gnum)-c(1:.gnum)*.rang/20, text=paste(as.vector(.ugrou)),
         adj=c(1,1), color=c(1:.gnum), cex = .8)
cat("Do you want to impose the names of sites? y/n\n")
.sans <- .scanf(2)
if(.sans == "y") text3d(dat[.colAttr],text = paste(.sites),col = .ngrou, cex = .7)
.par0()
palette("default")
cat("Do you want to try other three attributes? y/n\n")
.substay <- .scanf(2)
         }
      }
     }
   }
}
}
