#########################################################################################################
#
# Analysis for the paper/ chapter: controling social stress in virtual environments [Study 1]
# 2018 TUDelft
# This script will generate output: "data_analyses_controling_social_stress_in_virtual_environments_1.txt" 
#
# Required datafiles:
# - r_data.csv
# - r_ipq_online.sav 
#
#########################################################################################################

# Adjust this to root folder that contain data files
#setwd("D:/Surfdrive/PhD Research/#2/R_chapter_03/01/")

# Capture all the output to a file
#sink("data_analyses_controling_social_stress_in_virtual_environments_1_reanalysis.txt")

# Print R version and operating system version used by this data analyses
#sessionInfo()

############################################### LIBRARIES ###############################################
# Libraries used for this analyses 
library(foreign)
library(reshape2)
library(ez)
library(car)
#########################################################################################################

################################################ DATA ####################################################

main_data <- read.csv("50-1.csv")

# 01. Pid = participants ID number
# 02. gender = gender of  participants, coded: male = m, female = f
# 03. age = age of participants
# 04. sud_neutral = SUD score in the neutral virtual world
# 05. sud_bd = SUD score in the blind date virtual world
# 06. sud_job = SUD score in the job interview virtual world
# 07. hr_neutral = Heart rate measurement in the neutral virtual world
# 08. hr_bd = Heart rate measurement in the blind date virtual world
# 09. hr_job = Heart rate measurement in the job interview virtual world
# 10. ipq_e_sp = experiment IPQ questionnaire, total item of SP [spatial present - sense of being present in virtual environment]
# 11. ipq_e_inv = experiment IPQ questionnaire, total item of INV [involvment - awareness of real environment]
# 12. ipq_e_real = experiment IPQ questionnaire, total item of real [realism - How real did the virtual environment seem to you]
# 13. ipq_e_g1 = experiment IPQ questionnaire, total item of G1 [generated - In the computer generated world I had a sense of "being there"]
# 14. ipq_o_sp = online IPQ questionnaire, total item of SP [spatial present - sense of being present in virtual environment]
# 15. ipq_o_inv = online IPQ questionnaire, total item of INV [involvment - awareness of real environment]
# 16. ipq_o_real = online IPQ questionnaire, total item of real [realism - How real did the virtual environment seem to you]
# 17. ipq_o_g1 = online IPQ questionnaire, total item of G1 [generated - In the computer generated world I had a sense of "being there"]

## E & D COMMENTS
## THE RELEVANT VARIABLES FOR US ARE TOTAL SCORES FOR FOUR SUBSCALES (SP, INV, REAL, G1)
# 10. ipq_e_sp = experiment IPQ questionnaire, total item of SP [spatial present - sense of being present in virtual environment]
# 11. ipq_e_inv = experiment IPQ questionnaire, total item of INV [involvment - awareness of real environment]
# 12. ipq_e_real = experiment IPQ questionnaire, total item of real [realism - How real did the virtual environment seem to you]
# 13. ipq_e_g1 = experiment IPQ questionnaire, total item of G1 [generated - In the computer generated world I had a sense of "being there"]

library(semTools)
model <- 'ipq = ipq_e_sp + ipq_e_inv + ipq_e_real + ipq_e_g1'

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "Condition", missing = "ML")
# no grouping variable that I can use here.

#ipq_online <- read.spss("r_ipq_online.sav", use.value.labels=TRUE, to.data.frame=TRUE)
ipq_online <- read.spss("50-1-online.sav", use.value.labels=TRUE, to.data.frame=TRUE)



# this IPQ data is downloaded from http://igroup.org/pq/ipq/download.php, we took the most relevants items only to be compared with our experiment's results
#1.   res         - not used
#2.   study       - not used
#3.   lang        - not used
#4.   version     - not used
#5.   nofcase     - not used
#6.   video       - not used
#7.   audio       - not used
#8.   perspect    - not used
#9.   dura        - not used
#10.  and_r       - not used
#11.  and_s       - not used
#12.  app         - not used
#13.  age         - not used
#14.  gender      - not used
#15.  type        - not used
#16.  g1          - sense of being there - In the computer generated world I had a sense of "being there"
#17.  sp1         - sense of VE behind - Somehow I felt that the virtual world surrounded me.
#18.  sp2         - only pictures - I felt like I was just perceiving pictures.   
#19.  sp3         - not sense of being in v. space - I did not feel present in the virtual space.
#20.  sp4         - sense of acting in VE - I had a sense of acting in the virtual space, rather than operating something from outside.
#21.  sp5         - sense of being present in VE - I felt present in the virtual space.
#22.  inv1        - awareness of real env. - How aware were you of the real world surrounding while navigating in the virtual world? (i.e. sounds, room temperature, other people, etc.)?
#23.  inv2        - not aware of real env. - I was not aware of my real environment.
#24.  inv3        - no attention to real env. - I still paid attention to the real environment.
#25.  inv4        - attention captivated by VE - I was completely captivated by the virtual world.
#26.  real1       - VE real (real/not real) - How real did the virtual world seem to you?
#27.  real2       - experience similar to real env. - How much did your experience in the virtual environment seem consistent with your real world experience ?
#28.  real3       - VE real (imagined/real) - How real did the virtual world seem to you?
#29.  real4       - VE wirklich - The virtual world seemed more realistic than the real world.
#
# Note SP2, INV3, and REAL1 are reversed, eg. (-1 * sp2 + 6)

## REANALYSIS
main_data$ipq_exp_total <- rowSums(main_data[10:13])
main_data$ipq_exp_total <- rowSums(main_data[10:13])

expsp <- main_data[,10]
expinv <- main_data[,11]
expreal <- main_data[,12]
expgp <- main_data[,13]
onlinesp <- rowSums(ipq_online[17:21])
onlineinv <- rowSums(ipq_online[22:25])
onlinereal <- rowSums(ipq_online[26:29])
onlinegp <- ipq_online[,16]

sp <- c(expsp,onlinesp)
env <- c(expinv,onlineinv)
real <- c(expreal,onlinereal)
gp  <- c(expgp,onlinegp)
df <- cbind(sp,env,real,gp)
group <- c(rep(1,16),rep(2,37))
df <- cbind(df,group)

model <- 'ipq =~ sp + env + real + gp'
measurementInvariance(model = model, data = df, group = "group", missing = "ML")

