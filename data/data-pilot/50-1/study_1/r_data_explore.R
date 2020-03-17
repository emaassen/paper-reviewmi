#########################################################################################################
#
# Analysis for the paper: controling social stress in virtual environments [Study 1]
# 2018 TUDelft
# This script will generate output: "data_explore_controling_social_stress_in_virtual_environments.txt" 
#
# Required datafiles:
# a. r_data.csv
# 
#########################################################################################################

# Adjust this to root folder that contain data files
setwd("D:/Surfdrive/PhD Research/#2/R_chapter_03/01/")

# Capture all the output to a file
sink("data_explore_controling_social_stress_in_virtual_environments.txt")

# Print R version and operating system version used by this data analyses
sessionInfo()

# Dataset loading:
dataset_main <- read.csv("r_data.csv")

############################################### LIBRARIES ###############################################
# Libraries used for this analyses 

# if you not have one of this library, you can run this command to install your specific library
# install.packages("ez", dependencies = TRUE)
library(reshape2)
library(car)

# Functions:
boxplot_main <- function(data1, data2, data3, text) {
  boxplot(data1, data2, data3, 
          main = text,
          names = c("Neutral", "Blind date", "Job interview"))
}

##########################################################################################################

# Data explorer

cat("\nBoxplot: SUD Score - study 1 generated...\n")
#
# SUD Score
cat("\nBoxplot - SUD for 3 different conditions sucessfully generated...\n\n")
boxplot_main(dataset_main$sud_neutral, dataset_main$sud_bd, dataset_main$sud_job, "SUD Score")

cat("\nBoxplot: Heart rate - study 1 generated...\n")
#
# Heart rate
cat("\nBoxplot - Heart rate for 3 different conditions sucessfully generated...\n\n")
boxplot_main(dataset_main$hr_neutral, dataset_main$hr_bd, dataset_main$hr_job, "Heart rate")

#
# Normality test:
cat("\n=== Normality test ===\n")
# SUD
cat("\nNormality test: SUD score - neutral virtual world\n")
shapiro.test(dataset_main$sud_neutral)
cat("\nNormality test: SUD score - blind date virtual world\n")
shapiro.test(dataset_main$sud_bd)
cat("\nNormality test: SUD score - job interview virtual world\n")
shapiro.test(dataset_main$sud_job)

# Heart rate 
cat("\nNormality test: heart rate - neutral virtual world\n")
shapiro.test(dataset_main$hr_neutral)
cat("\nNormality test: heart rate - blind date virtual world\n")
shapiro.test(dataset_main$hr_bd)
cat("\nNormality test: heart rate - job interview virtual world\n")
shapiro.test(dataset_main$hr_job)

######### stop redireting output.
sink()

cat("\n\nOutput of this R script file: data_explore_controling_social_stress_in_virtual_environments.txt")








