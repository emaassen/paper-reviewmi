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

main_data <- read.csv("r_data.csv")

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
measurementInvariance(model = model, data = df, group = "Condtion", missing = "ML")
# no grouping variable that I can use here.

ipq_online <- read.spss("r_ipq_online.sav", use.value.labels=TRUE, to.data.frame=TRUE)

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






































############################################### FUNCTIONS ###############################################
mean_sd <-function(x){
  # print the mean and standard deviation
  cat("Mean: ", mean(x), " sd: ", sd(x), "\n") 
}

# count gender variable
count_gender <- function(x){
  cat("Female:", length(x[x == "female"]), "Male: ", length(x[x == "male"]))
}

#transform Maindata dataframe for a wide format to long format
tranformToLong <- function(mshort,idlist, measurelist,measurename)
{ 
  msubset <- subset(mshort, select = c(idlist,measurelist))
  mlong <- melt(msubset, id=idlist, measured = measurelist)
  names(mlong) <- c("pid","condition",measurename)
  return(mlong)
}

#print output ezANOVA, including effectsize eta, and Greenhouse-Geisser corrected dfs
print_ezANOVA <- function(m){
  print(m)
  m$ANOVA$eta <- m$ANOVA$SSn/(m$ANOVA$SSn + m$ANOVA$SSd)
  for (i in 1:length(m$ANOVA$Effect)) 
  { cat(m$ANOVA$Effect[i]," : eta ", m$ANOVA$eta[i]," \n")
  }
  cat("see ges-value for Generalized Eta-Squared) \n\n")
  
  cat("Greenhouse-Geisser corrected Dfs. \n") 
  for (i in 1:length(m$`Sphericity Corrections`$Effect))
  { 
    for (j in 1:length(m$ANOVA$Effect)) {
      if (m$ANOVA$Effect[j] == m$`Sphericity Corrections`$Effect[i]){
        m$`Sphericity Corrections`$DFn_G[i] <- m$ANOVA$DFn[j] * m$`Sphericity Corrections`$GGe[i]
        m$`Sphericity Corrections`$DFd_G[i] <- m$ANOVA$DFd[j] * m$`Sphericity Corrections`$GGe[i]
        cat(m$`Sphericity Corrections`$Effect[i],
            " numerator ", m$`Sphericity Corrections`$DFn_G[i], " , denominator ", m$`Sphericity Corrections`$DFd_G[i], "\n")
      }
    }
  }
}


posthoc_sidak<-function(x1, x2, c, data1, data2){
  #paired t test with sidak correction on p-value, c = number of comparisons
  cat("\nMeasurement: ", data1, "\n")
  mean_sd(x1)
  cat("\nMeasurement: ", data2, "\n")
  mean_sd(x2)
  #posthoc test
  test <- t.test(x1, x2, paired = TRUE)
  test$.psidak <- 1-(1-test$p.value)^c
  print(test)
  cat("p value after Sidak correction:", test$.psidak,"\n" )
}

######################################### ANALYSIS #########################################################
# Participants
cat("Subject analysis\n")
main_data$gender <- factor(main_data$gender, levels = c("m","f"), labels = c("male", "female"))
count_gender(main_data$gender)
cat("\nParticipants age range: ", range(main_data$age))
cat("\nParticipants age M and SD: \n")
mean_sd(main_data$age)

cat("\nIPQ analysis\n")
cat("Mean and SD IPQ experiment: \n")
main_data$ipq_exp_total <- rowSums(main_data[10:13])
mean_sd(main_data$ipq_exp_total)



cat("\nMean and SD IPQ online: \n")
ipq_online$total_online_ipq <- (rowMeans(ipq_online[16:29], na.rm = TRUE)) * (ncol(ipq_online) - 15)
mean_sd(ipq_online$total_online_ipq)

cat("\nT-test between IPQ experiment and IPQ online dataset: \n")

# (step 1) General code to reshape two vectors into a long data.frame
# Taken From https://stats.stackexchange.com/questions/15722/how-to-use-levene-test-function-in-r#15723
twoVarWideToLong <- function(sample1,sample2) {
  res <- data.frame(
    GroupID=as.factor(c(rep(1, length(sample1)), rep(2, length(sample2)))),
    DV=c(sample1, sample2)
  )   
}   

# (step 2) Reshaping the example data
long.data <- twoVarWideToLong(ipq_online$total_online_ipq, main_data$ipq_exp_total)

# (step 3) Check equality of variance
leveneTest(DV~GroupID,long.data)

# (step 4) As Levenetest is sign, homogeneity of variance should not be assumpted
a <- t.test(ipq_online$total_online_ipq, main_data$ipq_exp_total, paired = FALSE, var.equal = FALSE, alternative = "t")

cat("\nFor overall doubly MANOVA analyses please see the SPSS output.\n")

# convert pid into factor
main_data$pid <- factor(main_data$pid)

cat("\nSUD univariate analysis\n\n")
Maindatalong_SUD <- tranformToLong(main_data, c("pid"), c("sud_neutral", "sud_bd", "sud_job"), "sud")
SUD_model <- ezANOVA(data = Maindatalong_SUD, dv = .(sud), wid = .(pid), within = .(condition), detailed = TRUE, type = 3)
print_ezANOVA(SUD_model)

cat("\nHeart rate univariate analysis\n\n")
Maindatalong_HR <- tranformToLong(main_data, c("pid"), c("hr_neutral", "hr_bd", "hr_job"), "hr")
HR_model <- ezANOVA(data = Maindatalong_HR, dv = .(hr), wid = .(pid), within = .(condition), detailed = TRUE, type = 3)
print_ezANOVA(HR_model)

# POSTHOC TEST
cat("\nTable 1. Comparison between different conditions on SUD score and heart rate.\n")
cat("Measurement: SUD\n")
posthoc_sidak(main_data$sud_neutral, main_data$sud_bd, 3, "Neutral", "Blind date")
posthoc_sidak(main_data$sud_neutral, main_data$sud_job, 3, "Neutral", "Job Interview")
posthoc_sidak(main_data$sud_bd, main_data$sud_job, 3, "Blind date", "Job Interview")
cat("\nMeasurement: Heart rate\n")
posthoc_sidak(main_data$hr_neutral, main_data$hr_bd, 3, "Neutral", "Blind date")
posthoc_sidak(main_data$hr_neutral, main_data$hr_job, 3, "Neutral", "Job Interview")
posthoc_sidak(main_data$hr_bd, main_data$hr_job, 3, "Blind date", "Job Interview")

## stop redireting output.
sink()

cat("\n\nOutput of this R script file: data_analyses_controling_social_stress_in_virtual_environments_1.txt") 



