
#####################  Data pre-processing ######################

setwd(.WD)
source("functions.r")

.rnam <- row.names(trs)
.nam <- names(trs)
n <- length(.rnam); p <- length(.nam)      
      
cat("The column(variable) names: \n")

for (i in 1:p) cat(i,.nam[i],'\t', fill=T)

.read()
cat("The row(sample) names: \n")
for (i in 1:n) cat(i,.rnam[i],'\t', fill=T)
sprintf("There are %d rows and %d columns involved",length(.rnam),length(.nam))
.read()

.stay = 1; .colGrou <- .colGeo <- .colInd <- .opt <- 0
while(.stay == 1){
cat("1. I want to specify the grouping information\n")
cat("2. I want to specify the spatial coordinates\n")
cat("3. I want to select/drop some columns\n")
cat("4. I want to continue\n")
.ans <- .scanf(1,c(1:4))

if(.ans == 1){
  cat("Choose the number of the column for location grouping variable:\n")
  .colGrou <- .scanf(1,c(0:ncol(trs))); cat('Done\n\n')
}
if(.ans == 2){
  cat("Choose the number of the column for spatial coordinates:\n")
  .colGeo <- .scanf(1,c(0:ncol(trs)));  cat('Done\n\n')
}
if(.ans == 3){
  cat("I want to: 1.select  -1.drop\n")
  .opt <- .scanf(1,c(-1,1))
  cat("Which columns do you want to select or drop? Separated by space. for example: 2 3\n")
  .colInd <- .scanf(1,c(1:ncol(trs)));  cat('Done\n\n')
}
if(.ans == 4){.stay <- 0}
}

if(.opt > 0)  .Mtrs <- (trs[,c(.opt*.colInd)])
if(.opt < 0)  .Mtrs <- (trs[,c(-.colGrou, -.colGeo, .opt*.colInd)])
if(.opt == 0){
if(!all(.colGrou==0))  .Mtrs <- (trs[,c(-.colGrou, -.colGeo)])
if(all(.colGrou==0)) .Mtrs <- (trs)
}


                     
.geo <- .grou <- NULL
if(!all(.colGrou==0)) .grou <- trs[.colGrou]
if(!all(.colGeo==0)) .geo <- as.matrix(trs[,.colGeo])


####################################

source('missing.r')

source('censor.r')

####################  numeric data type encoding  ############
p <- dim(.Mtrs)[2]
.PM <- matrix(as.numeric(.Mtrs),ncol=p)    

n <- dim(.PM)[1]; p <- dim(.PM)[2]    

.attriind <- as.numeric(na.action(na.omit(.PM)))
dat <- as.data.frame(na.omit(.PM))
names(dat) <- .attri
if(length(.attriind)) .sites <- .sites[-.attriind]




#####################  data transformation  ##################
.stay <- 1
while(.stay == 1){
.ndat <- as.matrix(dat)
p<-ncol(dat)
.pp <- .optmn(p); .SW <- numeric(p)
palette("default") 
par(mfrow=c(.pp[1],.pp[2]), yaxt="n", cex.main=.7,
      mar=c(5,4,4,2)+.001,mai=c(1,.5,1,.5)*.1, mex=.3)
   for(i in 1:p){
   plot.ecdf(.ndat[,i], xlab='', ylab='',main =paste(i,':',.attri[i]))#,add=T)
   .SW[i] <- shapiro.test(.ndat[,i])$p.value
   legend('bottomright',paste('p=',as.character(round(.SW[i],4))),
                   cex=.7,text.col= as.integer(.SW[i]>.01)+2, bty='n')
}
.par0()
rm(.ndat)

cat("for data transformation:\n")
cat("1. I want to apply to all columns\n")
cat("2. I want to apply to a subset of all columns\n")
cat("3. I want to apply to those who fail to pass the Normality test\n")
cat("4. I want to drop some variables\n")
cat("5. I want to select only a subset of variables\n")
cat("6. I want to continue to next step\n")
.cho <- .scanf(1,c(1:6))

if(.cho == 6) .stay <- 0
else if(.cho == 4){
  cat("select the variables you want to drop:\n")
  .varDrop <- .scanf(1,c(1:p))
  dat <- dat[,-.varDrop]
  .attri <- .attri[-.varDrop]; n <- ncol(dat)
  }
else if(.cho == 5){
  cat("select the variables:\n")
  .varKeep <- .scanf(1,c(1:p))
  dat <- dat[,.varKeep]
  .attri <- .attri[.varKeep]; n <- ncol(dat)
  }
else{
if(.cho == 1) .col <- 1:ncol(dat)
if(.cho == 2) {
  cat("please specify the columns the transformation applies to:\n")
  .col <- .scanf(1,1:p)
}

if(.cho == 3) .col <- which(.SW<=.01)

datt <- as.matrix(dat[,.col])
.stayhere <- 1
while(.stayhere == 1){
cat("Choose the type of data transformation:\n", "1.none  2.standardization  3.logarithm  4.square root  5.box-cox  6.scale\n")
.ans <- .scanf(1,c(1:6))
if(.ans == 1) .stayhere <- 0
if(.ans == 2) 
{   if(!all(apply(datt,2,var)!=0))  cat("Cannot be done since there are some constant variables(columns)\n")
  if(all(apply(datt,2,var)!=0)) {datt <- as.data.frame(scale(datt)); .stayhere <- 0 }
}    
                    
if(.ans == 3)  
{   if(!(all(datt>0)))  cat("Cannot be done since there are nonpositive values\n")
    if(all(datt>0)) { datt <- log(datt); .stayhere <- 0}
}

if(.ans == 4)  
{   if(!(all(datt>=0)))  cat("Cannot be done since there are negative values\n")
    if(all(datt>=0)) { datt <- sqrt(datt); .stayhere <- 0}
}

if(.ans == 5)  {
  #cat("please choose the power for Box-Cox transformation:\n")
  .lambda <- seq(0,3,.01)
  if(!all(datt>0)) cat("Cannot be done since there are nonpositive values\n")
  if(all(datt>0)){
     for(.ii in 1:ncol(datt)){
     .gm <- exp( mean( log(datt[,.ii]) ) ); .P <- numeric(length(.lambda))
     for(.jj in 2:length(.lambda)) {
        .tmp <- (datt[,.ii]^.lambda[.jj] - 1)/( .lambda[.jj] * .gm^(.lambda[.jj]-1) ) 
        .P[.jj] <- shapiro.test(.tmp)$p.value  }
        .tmp <- log(datt[,.ii])*.gm
        .P[1] <- shapiro.test(.tmp)$p.value  
        .ind <- which(.P == max(.P))[1]
        .optlambda <- .lambda[.ind]; if(.ii == 1) cat('the power selected:')
        cat(.optlambda, ' ')
        if(.optlambda!=0) datt[,.ii] <-  (datt[,.ii]^.optlambda - 1)/( .optlambda * .gm^(.optlambda-1) )
        else  datt[,.ii] <- log(datt[,.ii])*.gm}
        .stayhere <- 0  
    }
   }
   

if(.ans == 6) {
   cat("specify the number to be divided by:\n")
   .scale <- scan("",double(0),nlines=1,quiet=T)
   datt <- datt/.scale; .stayhere <- 0 
      }
   
   if(.stayhere) readline()
   if(!.stayhere) {dat[,.col] <- datt; cat('\n transformation is done\n') }
  }
  rm(datt)
 }

}

########  transfer location group labels into number for visualization
.ugrou <- 1; .ngrou <- rep(1,length(.sites))
if(.colGrou){
.Grou <- .grou[,1]
.ugrou <- unique(.Grou)  ;  .len_ug <- length(.ugrou)
.ngrou <- numeric(length(.Grou))
for(i in 1:.len_ug){.ngrou <- .ngrou + as.vector((.Grou == .ugrou[i])*i)}
}

cat("\n Data preprocessing step is done:\n")
cat("\n Please press Enter key to continue...\n")
readline()


if(.colGrou) {
cat(sprintf("These location groups are detected:"), "\n")
print(rbind(1:length(.ugrou),as.vector(.ugrou)))  
cat("\n Please press Enter key to continue...\n")
readline()
}

n <- nrow(dat); p <- ncol(dat)

cat(sprintf(" The final pattern matrix involve %d samples and %d attributes", n, p),"\n")
cat("It is stored in the object 'dat'\n")
cat("Do you want to display it?  y/n", "\n")
.ans <- .scanf(2)
if(.ans =='y') print(dat)





