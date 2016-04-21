

######################################################################
#                       PCCAT main menu                              #
######################################################################

######################################################################   
# PCCAT is an add-on module of R. To use it please first change the  #
# path where all its modules locate. Then source the .pccat file.    #
######################################################################   

source("functions.r")  

## basic R packages
library(stats);library(MASS);library(lattice);library(cluster);

.checkpkg()


options(warn=1, keep.source=T)
#, prompt="pccat: ")

############## store the path for pccat  ########
.WD <- getwd()

###############  Welcome to use PCCAT  ###########################
cat("\n")
cat(sprintf(" %s",date()), "\n", "\n",
sprintf("***************************"),"\n",
sprintf("     Welcome to PCCAT!    "), "\n", 
sprintf("***************************"), "\n", "\n",
sprintf("Please press Enter key to continue..."), "\n")
readline()
##################################################################

cat("The objects in the current workspace are:\n")
print(ls())
cat("\n which one do you choose as the data frame?\n")
cat("1. I want to type the name of the object\n")
cat("2. I want to use the training data set in PCCAT\n")
cat("3. I want to import from csv/txt file\n")

.ans <- .scanf(1,c(1:3))

if(.ans == 1){
cat("\n please type the name:\n")
.ansnam <- scan("",character(0),nlines=1,quiet=T)
while(all(.ansnam != ls())) {
     if(.ansnam[1] == 'quit') stop('Running stopped')
     cat("object not found. type again:\n"); 	 
     .ansnam <- scan("",character(0),nlines=1,quiet=T) }
trs <- get(.ansnam)
}

if(.ans == 2){
       cat("select the data set:\n")
       cat("1.TRSRSB_W_ND_0        2.TRSRSB_W_ND_05    3.T_BASF_Adjusted \n")
       cat("4.T_Trenton_Adjusted   5.T_Trenton_Given   6.T_Trenton_Qualifiers \n")
       .chos <- .scanf(1,c(1:6))
            if(.chos == 1) load("TRSRSB_W_ND_0")         #20*18
            if(.chos == 2) load("TRSRSB_W_ND_05")        #20*18
            if(.chos == 3) load("T_BASF_Adjusted")       #232*73
	    if(.chos == 4) load("T_Trenton_Adjusted")    #232*69  
            if(.chos == 5) load("T_Trenton_Given")       #255*69
            if(.chos == 6) load("T_Trenton_Qualifiers")  #232*138  
     }

if(.ans == 3){
cat("\n please type the path and filename, e.g.: C:/projects/MDEQ/pccat/example.txt \n")
.ansnam2 <- scan("",character(0),nlines=1,quiet=T)
cat("\n Is the first row the variable name? y/n\n")
.ans1 <- .scanf(2)
.ans2 <- (.ans1=='y')
if(length(grep('.txt',.ansnam2))) trs <- read.table(.ansnam2, header=.ans2)
if(length(grep('.csv',.ansnam2))) trs <- read.csv(.ansnam2, header=.ans2)
}

source('data_preprocessing.r')


.staypccat <- 1
while(.staypccat == 1){
cat("\n Please press Enter key to continue...\n")
readline()
cat("********************* pccat ********************\n")
cat("\n 1. visualization of data\n")
cat("\n 2. principle component analysis\n")
cat("\n 3. clustering analysis\n")
cat("\n 4. exit\n")
.ans <- .scanf(1,c(1:4))
if(.ans == 1)  source('visualization.r')
if(.ans == 2)  source('principle component analysis.r')
if(.ans == 3)  source('clustering.r')
if(.ans == 4)  {.staypccat <- 0; cat("Thank you for using PCCAT!\n\n")}
}

#rm(list=ls(all.names=T)[substr(ls(all.names=T),1,1)=='.'])
options(warn=1, keep.source=T)
#, prompt="> ")
