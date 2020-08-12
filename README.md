# PCCAT
PCCAT (Principle Component and Clustering Analysis Tool) is an add-on module for R designed for Michigan Department of Environmental Quality (MDEQ) clients via Center for Statistical Training and Consulting (CSTAT). It is mainly designed for multivariate environmental data analysis. Based on statistical software R functions and packages, it consists of four main steps: pre-processing, visualization, principle component analysis and clustering analysis. It has been designed to be user-friendly with graphic output and user prompts to take advantage of the many useful functions and packages coded in R, combined with Excel for data analysis.

To run and test current PCCAT shiny version, in Rstudio simply do:   
require(shiny)  
runGitHub("PCCAT", "zhangz19")  

Or download the folder, setwd to where the folder locates, do:  
library(shiny)  
runApp("PCCAT", launch.browser=TRUE)  

Recent shiny version developed by CSTAT team: https://jsanket.shinyapps.io/PCCAT/
