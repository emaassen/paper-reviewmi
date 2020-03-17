#########################################################################################################
#
# Analysis for the paper/ chapter: controling social stress in virtual environments [Study 2]
# 2018 TUDelft
# This script will generate output: "data_analyses_controling_social_stress_in_virtual_environments_2.txt" 
#
# Required datafiles:
# - r_main_data.csv
# - r_questionnaires_data.csv
# - r_ipq_online.sav 
#
#########################################################################################################

# Adjust this to root folder that contain data files
#setwd("D:/Surfdrive/PhD Research/#2/R_chapter_03/02/")

# Capture all the output to a file
#sink("data_analyses_controling_social_stress_in_virtual_environments_2.txt")

# Print R version and operating system version used by this data analyses
#sessionInfo()

############################################### LIBRARIES ###############################################
# Libraries used for this analyses 
library(foreign)
library(reshape2)
library(psych)
library(ez)
library(car)
#########################################################################################################

################################################ DATA ####################################################

main_data <- read.csv("r_main_data.csv")

# 01. Pid = participants ID number
# 02. gender = gender of  participants, coded: male = m, female = f
# 03. age = age of participants
# 04. up_sud_50 = sud score in the up condition with 50% ratio
# 05. up_sud_75 = sud score in the up condition with 75% ratio
# 06. up_sud_100 = sud score in the up condition with 100% ratio
# 07. up_sud_50a = sud score in the up condition with 50% ratio again after maximum at 100% ratio (called control condition)
# 08. up_hr_50 = heart rate measurement in the up condition with 50% ratio
# 09. up_hr_75 = heart rate measurement in the up condition with 75% ratio
# 10. up_hr_100 = heart rate measurement in the up condition with 100% ratio
# 11. up_hr_50a = heart rate measurement in the up condition with 50% ratio again after maximum at 100% ratio (called control condition)
# 12. up_speak_50 = length of speak in the up condition with 50% ratio
# 13. up_speak_75 = length of speak in the up condition with 75% ratio
# 14. up_speak_100 = length of speak in the up condition with 100% ratio
# 15. up_speak_50a = length of speak in the up condition with 50% ratio again after maximum at 100% ratio (called control condition)
# 16. down_sud_50 = sud score in the down condition with 50% ratio
# 17. down_sud_75 = sud score in the down condition with 25% ratio
# 18. down_sud_100 = sud score in the down condition with 0% ratio
# 19. down_sud_50a = sud score in the down condition with 50% ratio again after minimum at 0% ratio (called control condition)
# 20. down_hr_50 = heart rate measurement in the down condition with 50% ratio
# 21. down_hr_75 = heart rate measurement in the down condition with 25% ratio
# 22. down_hr_100 = heart rate measurement in the down condition with 0% ratio
# 23. down_hr_50a = heart rate measurement in the down condition with 50% ratio again after minimum at 0% ratio (called control condition)
# 24. down_speak_50 = length of speak in the down condition with 50% ratio
# 25. down_speak_75 = length of speak in the down condition with 25% ratio
# 26. down_speak_100 = length of speak in the down condition with 0% ratio
# 27. down_speak_50a = length of speak in the down condition with 50% ratio again after minimum at 0% ratio (called control condition)

df <- read.csv("r_questionnaires_data.csv")

# 01. Pid = participants ID number
# 02. gender = gender of  participants, coded: male = m, female = f
# 03. age = age of participants
# 04. ieq_cond1_1 = IEQuestionnaire (pleasant - unpleasant) - condition 1, 7-likert scale (1 - 7)
# 05. ieq_cond1_2 = IEQuestionnaire (Not relaxed - ralaxed) - condition 1, 7-likert scale (1 - 7)
# 06. ieq_cond1_3 = IEQuestionnaire (aggresive - non aggresive) - condition 1, 7-likert scale (1 - 7)
# 07. ieq_cond1_4 = IEQuestionnaire (uncomfortable - comfortable) - condition 1, 7-likert scale (1 - 7)
# 08. ieq_cond1_5 = IEQuestionnaire (polite - impolite) - condition 1, 7-likert scale (1 - 7)
# 09. ieq_cond1_6 = IEQuestionnaire (energizing - exhausting) - condition 1, 7-likert scale (1 - 7)

# 10. ieq_cond2_1 = IEQuestionnaire (pleasant - unpleasant) - condition 2, 7-likert scale (1 - 7)
# 11. ieq_cond2_2 = IEQuestionnaire (Not relaxed - ralaxed) - condition 2, 7-likert scale (1 - 7)
# 12. ieq_cond2_3 = IEQuestionnaire (aggresive - non aggresive) - condition 2, 7-likert scale (1 - 7)
# 13. ieq_cond2_4 = IEQuestionnaire (uncomfortable - comfortable) - condition 2, 7-likert scale (1 - 7)
# 14. ieq_cond2_5 = IEQuestionnaire (polite - impolite) - condition 2, 7-likert scale (1 - 7)
# 15. ieq_cond2_6 = IEQuestionnaire (energizing - exhausting) - condition 2, 7-likert scale (1 - 7)

# 16. deq_cond1_spe = DEQuestionnaire (Speed) - condition 1
# 17. deq_cond1_int = DEQuestionnaire (Interruption) - condition 1
# 18. deq_cond1_crl = DEQuestionnaire (Correct Locally) - condition 1
# 19. deq_cond1_crg = DEQuestionnaire  (Correct Globally) - condition 1
# 20. deq_cond1_inv = DEQuestionnaire (Involvement) - condition 1
# 21. deq_cond1_sat = DEQuestionnaire (Discussion Satisfaction) - condition 1
# 22. deq_cond1_rea = DEQuestionnaire (Reality) - condition 1

# 23. deq_cond2_spe = DEQuestionnaire (Speed) - condition 2
# 24. deq_cond2_int = DEQuestionnaire (Interruption) - condition 2
# 25. deq_cond2_crl = DEQuestionnaire (Correct Locally) - condition 2
# 26. deq_cond2_crg = DEQuestionnaire  (Correct Globally) - condition 2
# 27. deq_cond2_inv = DEQuestionnaire (Involvement) - condition 2
# 28. deq_cond2_sat = DEQuestionnaire (Discussion Satisfaction) - condition 2
# 29. deq_cond2_rea = DEQuestionnaire (Reality) - condition 2

deq1 <- c(df$deq_cond1_spe,df$deq_cond2_spe)
deq2 <- c(df$deq_cond1_int,df$deq_cond2_int)
deq3 <- c(df$deq_cond1_crl,df$deq_cond2_crl)
deq4 <- c(df$deq_cond1_crg,df$deq_cond2_crg)
deq5 <- c(df$deq_cond1_inv,df$deq_cond2_inv)
deq6 <- c(df$deq_cond1_sat,df$deq_cond2_sat)
deq7 <- c(df$deq_cond1_rea,df$deq_cond2_rea)
group <- c(rep(1,24),rep(2,24))
df <- cbind(deq1,deq2,deq3,deq4,deq5,deq6,deq7,group)
library(semTools)
deq <- 'deq =~ deq1 + deq2 + deq3 + deq4 + deq5 + deq6 + deq7' 

measurementInvariance(model = deq, data = df, group = "group", missing = "ML")


# 30. ipq_sp1 = experiment IPQ questionnaire, item of SP [spatial present - sense of being present in virtual environment] number 1
# 31. ipq_sp2 = experiment IPQ questionnaire, item of SP [spatial present - sense of being present in virtual environment] number 2
# 32. ipq_sp3 = experiment IPQ questionnaire, item of SP [spatial present - sense of being present in virtual environment] number 3
# 33. ipq_sp4 = experiment IPQ questionnaire, item of SP [spatial present - sense of being present in virtual environment] number 4
# 34. ipq_sp5 = experiment IPQ questionnaire, item of SP [spatial present - sense of being present in virtual environment] number 5
# 35. ipq_inv1 = experiment IPQ questionnaire, item of INV [involvment - awareness of real environment] number 1
# 36. ipq_inv2 = experiment IPQ questionnaire, item of INV [involvment - awareness of real environment] number 2
# 37. ipq_inv3 = experiment IPQ questionnaire, item of INV [involvment - awareness of real environment] number 3
# 38. ipq_inv4 = experiment IPQ questionnaire, item of INV [involvment - awareness of real environment] number 4
# 39. ipq_e_g1 = experiment IPQ questionnaire, total item of G1 [generated - In the computer generated world I had a sense of "being there"]
# 40. ipq_rea1 = experiment IPQ questionnaire, item of real [realism - How real did the virtual environment seem to you] number 1
# 41. ipq_rea2 = experiment IPQ questionnaire, item of real [realism - How real did the virtual environment seem to you] number 2
# 42. ipq_rea3 = experiment IPQ questionnaire, item of real [realism - How real did the virtual environment seem to you] number 3
# 43. ipq_rea4 = experiment IPQ questionnaire, item of real [realism - How real did the virtual environment seem to you] number 4

sp <- 'sp =~ ipq_sp1 + ipq_sp2 + ipq_sp3 + ipq_sp4 + ipq_sp5' 
inv <- 'inv =~ ipq_inv1 + ipq_inv2 + ipq_inv3 + ipq_inv4' 
rea <- 'rea =~ ipq_rea1 + ipq_rea2 + ipq_rea3 + ipq_rea4' 

# only measured once so we cannot compare pre to post, we're not checking MI with this measure




# 44. sam_p_1_v = participant's own emotion (SAM) in codition 1 - valence dimension
# 45. sam_p_1_a = participant's own emotion (SAM) in codition 1 - arousal dimension
# 46. sam_p_1_d = participant's own emotion (SAM) in codition 1 - dominance dimension
# 47. sam_p_2_v = participant's own emotion (SAM) in codition 2 - valence dimension
# 48. sam_p_2_a = participant's own emotion (SAM) in codition 2 - arousal dimension
# 49. sam_p_2_d = participant's own emotion (SAM) in codition 2 - dominance dimension
# 50. sam_a_1_v = participant's perception of virtual human (SAM) in codition 1 - valence dimension
# 51. sam_a_1_a = participant's perception of virtual human (SAM) in codition 1 - arousal dimension
# 52. sam_a_1_d = participant's perception of virtual human (SAM) in codition 1 - dominance dimension
# 53. sam_a_2_v = participant's perception of virtual human (SAM) in codition 2 - valence dimension
# 54. sam_a_2_a = participant's perception of virtual human (SAM) in codition 2 - arousal dimension
# 55. sam_a_2_d = participant's perception of virtual human (SAM) in codition 2 - dominance dimension
# 56. sias = social interaction anxiety scale questionnaire

df2 <- read.spss("r_ipq_online.sav", use.value.labels=TRUE, to.data.frame=TRUE)

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
expsp <- rowSums(df[30:34])
expinv <- rowSums(df[35:38])
expreal <- rowSums(df[40:43])
expgp <- rowSums(df[39])
onlinesp <- rowSums(df2[17:21])
onlineinv <- rowSums(df2[22:25])
onlinereal <- rowSums(df2[26:29])
onlinegp <- df2[,16]

sp <- c(expsp,onlinesp)
env <- c(expinv,onlineinv)
real <- c(expreal,onlinereal)
gp  <- c(expgp,onlinegp)
df3 <- cbind(sp,env,real,gp)
group <- c(rep(1,24),rep(2,37))
df3 <- cbind(df,group)

model <- 'ipq =~ sp + env + real + gp'
measurementInvariance(model = model, data = df3, group = "group", missing = "ML")

config <- cfa(model, data=df3, group="group") 
summary(config, fit.measures=T)

# No configural invariance




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
tranformToLong <- function(mshort, idlist, measurelist, measurename)
{ #transform Maindata dataframe for a wide format to long format
  msubset <- subset(mshort, select = c(idlist, measurelist))
  mlong <- melt(msubset, id=idlist, measured = measurelist)
  names(mlong) <- c("participant", "SIAS_LowHi", "dialog_stressors", measurename)
  mlong$SIAS_LowHi<-factor(mlong$SIAS_LowHi, levels = c(0:1), labels = c("low","high"))
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

tranformToLongSAM <- function(mshort, idlist, measurelist, measurename)
{ #transform Maindata dataframe for a wide format to long format
  msubset <- subset(mshort, select = c(idlist, measurelist))
  mlong <- melt(msubset, id=idlist, measured = measurelist)
  names(mlong) <- c("participant", "SIAS_LowHi", "condition", measurename)
  mlong$SIAS_LowHi<-factor(mlong$SIAS_LowHi, levels = c(0:1), labels = c("low","high"))
  return(mlong)
}

print_ezANOVA_sam <- function(m){
  print(m)
  m$ANOVA$eta <- m$ANOVA$SSn/(m$ANOVA$SSn + m$ANOVA$SSd)
  for (i in 1:length(m$ANOVA$Effect)) 
  { cat(m$ANOVA$Effect[i]," : eta ", m$ANOVA$eta[i]," \n")
  }
  cat("see ges-value for Generalized Eta-Squared) \n\n")
  # No Mauchly's test of sphericity
  # Basically, it's because there are only 2 levels of repeated measures. 
  # As such, there is only one set of difference scores and nothing to compare those difference scores against to indicate a violation of sphericity. 
}

pearson_cor <- function(data1, data2, text) {
  r <- cor.test(data1, data2, method = "pearson", paired = TRUE, exact = FALSE, continuity = FALSE, conf.level = 0.95)
  cat(text, "\n")
  cat("Pearson's correlation:", r$estimate, "t:", r$statistic, "df:", r$parameter, "p-value:", r$p.value, "\n")
} 


testbetween <-function(x,g)
{
  # x integer data being tested
  # g group
  
  # convert group into factor:
  g <- factor(g)
  
  l<-leveneTest(x,g)
  if (l$`Pr(>F)`[1]<.05)
  {
    cat("\nLevene's test shows sign difference between the group variance, therefore  Welch Two Sample t-test conducted\n")
    print(t.test(x~g, na.rm = TRUE,var.equal = FALSE))
  } else
  {
    cat("\nLevene's test shows no sign difference between the group variance, therefore  Two Sample t-test conducted\n")
    print(t.test(x~g, na.rm = TRUE,var.equal = TRUE))
  }
}

testbetween_def <-function(x,g,o)
{
  # x integer data being tested
  # g group
  
  # convert group into factor:
  g <- factor(g)
  
  if (o == "m"){
  
    l<-leveneTest(x,g,center=mean)
    if (l$`Pr(>F)`[1]<.05)
    {
      cat("\nLevene's test shows sign difference between the group variance, therefore  Welch Two Sample t-test conducted\n")
      print(t.test(x~g, na.rm = TRUE,var.equal = FALSE))
    } else
    {
    cat("\nLevene's test shows no sign difference between the group variance, therefore  Two Sample t-test conducted\n")
    print(t.test(x~g, na.rm = TRUE,var.equal = TRUE))
   }
  }
  
  else {
    cat("center = median")
    l<-leveneTest(x,g,center=median)
    if (l$`Pr(>F)`[1]<.05)
    {
      cat("\nLevene's test shows sign difference between the group variance, therefore  Welch Two Sample t-test conducted\n")
      print(t.test(x~g, na.rm = TRUE,var.equal = FALSE))
    } else
    {
      cat("\nLevene's test shows no sign difference between the group variance, therefore  Two Sample t-test conducted\n")
      print(t.test(x~g, na.rm = TRUE,var.equal = TRUE))
    }
    
  }
}

######################################### ANALYSIS #########################################################
# Participants
cat("PARTICIPANTS\n")
main_data$gender <- factor(main_data$gender, levels = c("m","f"), labels = c("male", "female"))
count_gender(main_data$gender)
cat("\nParticipants age range: ", range(main_data$age))
cat("\nParticipants age M and SD: \n")
mean_sd(main_data$age)

cat("\nLow and high social anxiety group (SIAS)\n")
mean_sd(data_questionnaires$sias)

cat("\nPresence (IPQ) analyses\n")
cat("Mean and SD IPQ experiment: \n")
data_questionnaires$ipq_exp_total <- rowSums(data_questionnaires[30:43])
mean_sd(data_questionnaires$ipq_exp_total)

cat("Mean and SD IPQ online: \n")
ipq_online$total_online_ipq <- (rowMeans(ipq_online[16:29], na.rm = TRUE)) * (ncol(ipq_online) - 15)
mean_sd(ipq_online$total_online_ipq)

cat("\nT-test between IPQ experiment and IPQ online dataset: \n")

# Taken From https://stats.stackexchange.com/questions/15722/how-to-use-levene-test-function-in-r#15723
#General code to reshape two vectors into a long data.frame
twoVarWideToLong <- function(sample1,sample2) {
  res <- data.frame(
    GroupID=as.factor(c(rep(1, length(sample1)), rep(2, length(sample2)))),
    DV=c(sample1, sample2)
  )   
}   

# (Step 1) Reshaping the example data
long.data <- twoVarWideToLong(data_questionnaires$ipq_exp_total, ipq_online$total_online_ipq)

# (Step 2) There are many different calls here that will work... but here is an example
leveneTest(DV~GroupID,long.data)

# (Step 3) As Levenetest is sign, homogeneity of variance should not be assumpted
t.test(data_questionnaires$ipq_exp_total, ipq_online$total_online_ipq, paired = FALSE, var.equal = FALSE, alternative = "t")


#add SIAS group into experiment IPQ
data_questionnaires$SIAS_LowHi_check[data_questionnaires$sias < mean(data_questionnaires$sias)] <- 0
data_questionnaires$SIAS_LowHi_check[data_questionnaires$sias > mean(data_questionnaires$sias)] <- 1

cat("\n# Compare total IPQ of two SIAS group\n")
testbetween(data_questionnaires$ipq_exp_total, data_questionnaires$SIAS_LowHi_check)

cat("\n*Anxiety Level*\n")
cat("For overall doubly MANOVA (overall) analyses please see the SPSS output.\n") 

cat("\nTable 2 - Result of univariate analyses:\n")

#convert participants into factor
main_data$pid <- factor(main_data$pid)

#add SIAS group into main data
main_data$SIAS_LowHi_check[data_questionnaires$sias < mean(data_questionnaires$sias)] <- 0
main_data$SIAS_LowHi_check[data_questionnaires$sias > mean(data_questionnaires$sias)] <- 1

#average of starting points
# starting point = (UP 50% + DOWN 50%) / 2
# SUD
main_data$sud_starting_avg <- (main_data$up_sud_50 + main_data$down_sud_50) / 2
# Heart rate
main_data$hr_starting_avg <- (main_data$up_hr_50 + main_data$down_hr_50) / 2
#Length of speak
main_data$speak_starting_avg <- (main_data$up_speak_50 + main_data$down_speak_50) / 2


cat("\n *SUD score* \n")
MaindatalongSUD <-tranformToLong(main_data,c("pid","SIAS_LowHi_check"), c("sud_starting_avg", "up_sud_75", "up_sud_100", "up_sud_50a",
                                                                         "down_sud_25", "down_sud_0", "down_sud_50a"), "SUD")
SUDmodel <- ezANOVA(data = MaindatalongSUD, dv = .(SUD), wid = .(participant), within = .(dialog_stressors), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA(SUDmodel)

cat("\n *Heart rate* \n")
MaindatalongHR <-tranformToLong(main_data,c("pid","SIAS_LowHi_check"), c("hr_starting_avg", "up_hr_75", "up_hr_100", "up_hr_50a",
                                                                          "down_hr_25", "down_hr_0", "down_hr_50a"), "HeartRate")
HRmodel <- ezANOVA(data = MaindatalongHR, dv = .(HeartRate), wid = .(participant), within = .(dialog_stressors), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA(HRmodel)

cat("\n *Length of speak (Audio)* \n")
MaindatalongAudio <-tranformToLong(main_data,c("pid","SIAS_LowHi_check"), c("speak_starting_avg", "up_speak_75", "up_speak_100", "up_speak_50a",
                                                                         "down_speak_25", "down_speak_0", "down_speak_50a"), "Audio")
Audiomodel <- ezANOVA(data = MaindatalongAudio, dv = .(Audio), wid = .(participant), within = .(dialog_stressors), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA(Audiomodel)

cat("\nTable 3. Comparison between dialog stressor on SUD score rating, heart rate (bpm) and audio length (s).\n")
cat("Measurement: SUD (0-10)\n")
posthoc_sidak(main_data$down_sud_0, main_data$down_sud_50a, 6, "Down SUD 0%", "Down SUD 50% again")
posthoc_sidak(main_data$down_sud_25, main_data$down_sud_0, 6, "Down SUD 25%", "Down SUD 0%")
posthoc_sidak(main_data$sud_starting_avg, main_data$down_sud_25, 6, "average of 2 starting points SUD", "Down SUD 25%")
posthoc_sidak(main_data$up_sud_75, main_data$sud_starting_avg, 6, "Up SUD 75%", "average of 2 starting points SUD")
posthoc_sidak(main_data$up_sud_100, main_data$up_sud_75, 6, "Up SUD 100%", "Up SUD 75%")
posthoc_sidak(main_data$up_sud_50a, main_data$up_sud_100, 6, "Up SUD 50% again", "Up SUD 100%")

cat("\nMeasurement: Heart Rate (bpm)\n")
posthoc_sidak(main_data$down_hr_50a, main_data$down_hr_0, 6, "Down hr 50% again", "Down hr 0%")
posthoc_sidak(main_data$down_hr_25, main_data$down_hr_0, 6, "Down hr 25%", "Down hr 0%")
posthoc_sidak(main_data$down_hr_25, main_data$hr_starting_avg, 6, "Down hr 25%", "average of 2 starting points hr")
posthoc_sidak(main_data$up_hr_75, main_data$hr_starting_avg, 6, "Up hr 75%", "average of 2 starting points hr")
posthoc_sidak(main_data$up_hr_75, main_data$up_hr_100, 6, "Up hr 75%", "Up hr 100%")
posthoc_sidak(main_data$up_hr_100, main_data$up_hr_50a, 6, "Up hr 100%", "Up hr 50% again")

cat("\nMeasurement: Audio Length (s)\n")
posthoc_sidak(main_data$down_speak_0, main_data$down_speak_50a, 6, "Down speak 0%", "Down speak 50% again")
posthoc_sidak(main_data$down_speak_25, main_data$down_speak_0, 6, "Down speak 25%", "Down speak 0%")
posthoc_sidak(main_data$down_speak_25, main_data$speak_starting_avg, 6, "Down speak 25%", "average of 2 starting points speak")
posthoc_sidak(main_data$up_speak_75, main_data$speak_starting_avg, 6, "Up speak 75%", "average of 2 starting points speak")
posthoc_sidak(main_data$up_speak_75, main_data$up_speak_100, 6, "Up speak 75%", "Up speak 100%")
posthoc_sidak(main_data$up_speak_100, main_data$up_speak_50a, 6, "Up speak 100%", "Up speak 50% again")

cat("\nPearson Correlation:\n")
#SUD score data
cat("\n*SUD Score*:\n")
SUD_data_cor <- data.frame(dialog_stressors = c(50, 0, 25, 50, 50, 75, 100, 50), mean_sud = c(mean(main_data$down_sud_50), mean(main_data$down_sud_0), mean(main_data$down_sud_25), mean(main_data$down_sud_50a),
                                                                                              mean(main_data$up_sud_50), mean(main_data$up_sud_75), mean(main_data$up_sud_100), mean(main_data$up_sud_50a)))
pearson_cor(SUD_data_cor$dialog_stressors, SUD_data_cor$mean_sud, "SUD score correlation:")

#Heart rate data
cat("\n*Heart Rate*:\n")
HR_data_cor <- data.frame(dialog_stressors = c(50, 0, 25, 50, 50, 75, 100, 50), mean_hr = c(mean(main_data$down_hr_50), mean(main_data$down_hr_0), mean(main_data$down_hr_25), mean(main_data$down_hr_50a),
                                                                                              mean(main_data$up_hr_50), mean(main_data$up_hr_75), mean(main_data$up_hr_100), mean(main_data$up_hr_50a)))
pearson_cor(HR_data_cor$dialog_stressors, HR_data_cor$mean_hr, "Heart rate correlation:")

#Audio length data
cat("\n*Audio length*:\n")
Audio_data_cor <- data.frame(dialog_stressors = c(50, 0, 25, 50, 50, 75, 100, 50), mean_audio = c(mean(main_data$down_speak_50), mean(main_data$down_speak_0), mean(main_data$down_speak_25), mean(main_data$down_speak_50a),
                                                                                            mean(main_data$up_speak_50), mean(main_data$up_speak_75), mean(main_data$up_speak_100), mean(main_data$up_speak_50a)))
pearson_cor(Audio_data_cor$dialog_stressors, Audio_data_cor$mean_audio, "Audio length correlation:")

cat("\n*Anxiety Level: Paragraph 3, the result of the overall analysis also showed that there was no significant overall main effect for higher and lower social anxiety group.\n")
cat("For this overall doubly MANOVA analyses please see the SPSS output.\n") 

cat("\n*Anxiety Level: Paragraph 4, the overall doubly repeated-measure MANOVA found no significant overall two-way interaction effect between dialog stressor and the two social anxiety groups on anxiety level.\n")
cat("For this overall doubly MANOVA analyses please see the SPSS output.\n") 

cat("\n *Anxiety Level: Paragraph 4, compare 100% negative stressor of SUD score on two SIAS group\n")
testbetween(main_data$up_sud_100, main_data$SIAS_LowHi_check)

cat("\n *Anxiety Level: Paragraph 4, compare 0% negative stressor of SUD score on two SIAS group\n")
testbetween(main_data$down_sud_0, main_data$SIAS_LowHi_check)

cat("\n *Anxiety Level: Paragraph 4, compare 100% negative stressor of heart rate on two SIAS group\n")
testbetween(main_data$up_hr_100, main_data$SIAS_LowHi_check)


cat("\n *Anxiety Level: Paragraph 4, compare 0% negative stressor of heart rate on two SIAS group\n")
testbetween(main_data$down_hr_0 , main_data$SIAS_LowHi_check)

cat("\n*Participants Emotion*\n")

cat("Table 4. Resut of univariate analyses with dialog stressors as within subject factor and social anxiety group as between-subjects factor
    on the individual's own valence, arousal and dominance state.")

#convert participants into factor
data_questionnaires$pid <- factor(data_questionnaires$pid)

cat("\n *Valence* \n")
MaindatalongSAM_PV <-tranformToLongSAM(data_questionnaires,c("pid","SIAS_LowHi_check"), c("sam_p_1_v", "sam_p_2_v"), "SAM_participant_valence")
SAM_PV_model <- ezANOVA(data = MaindatalongSAM_PV, dv = .(SAM_participant_valence), wid = .(participant), within = .(condition), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA_sam(SAM_PV_model)

cat("\n *Arousal* \n")
MaindatalongSAM_PA <-tranformToLongSAM(data_questionnaires,c("pid","SIAS_LowHi_check"), c("sam_p_1_a", "sam_p_2_a"), "SAM_participant_arousal")
SAM_PA_model <- ezANOVA(data = MaindatalongSAM_PA, dv = .(SAM_participant_arousal), wid = .(participant), within = .(condition), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA_sam(SAM_PA_model)

cat("\n *Dominance* \n")
MaindatalongSAM_PD <-tranformToLongSAM(data_questionnaires,c("pid","SIAS_LowHi_check"), c("sam_p_1_d", "sam_p_2_d"), "SAM_participant_dominance")
SAM_PD_model <- ezANOVA(data = MaindatalongSAM_PD, dv = .(SAM_participant_dominance), wid = .(participant), within = .(condition), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA_sam(SAM_PD_model)

cat("\nTable. 5. Comparison between different conditions on the individual's own valence, arousal and dominance state.\n")
cat("*Valence*\n")
cat("condition 1:\n")
mean_sd(data_questionnaires$sam_p_1_v)
cat("condition 2:\n")
mean_sd(data_questionnaires$sam_p_2_v)
# t-test
t.test(data_questionnaires$sam_p_1_v, data_questionnaires$sam_p_2_v, paired = TRUE)

cat("\n*Arousal*\n")
cat("condition 1:\n")
mean_sd(data_questionnaires$sam_p_1_a)
cat("condition 2:\n")
mean_sd(data_questionnaires$sam_p_2_a)
# t-test
t.test(data_questionnaires$sam_p_1_a, data_questionnaires$sam_p_2_a, paired = TRUE)

cat("\n*Dominance*\n")
cat("condition 1:\n")
mean_sd(data_questionnaires$sam_p_1_d)
cat("condition 2:\n")
mean_sd(data_questionnaires$sam_p_2_d)
# t-test
t.test(data_questionnaires$sam_p_1_d, data_questionnaires$sam_p_2_d, paired = TRUE)

cat("\nAs depicted in Figure 8, on the valence dimension, the higher social anxiety group rated valence approaching significantly higher than lower 
    social anxiety group in the positive condition: \n")
testbetween(data_questionnaires$sam_p_2_v, data_questionnaires$SIAS_LowHi_check)

cat("\n while there is no significant difference was found in the negative condition between two groups:\n")
testbetween_def(data_questionnaires$sam_p_1_v, data_questionnaires$SIAS_LowHi_check, "m")

cat("\nOn the arousal dimension, the high social anxiety group reported significanly more arousal in the positive condition: \n")
testbetween_def(data_questionnaires$sam_p_2_a, data_questionnaires$SIAS_LowHi_check, "m")

cat("\nWhile again no significant difference was found in the negative condition: \n")
testbetween_def(data_questionnaires$sam_p_1_a, data_questionnaires$SIAS_LowHi_check, "m")

cat("Finally for the dominance affective dimension, the higher social anxiety group felt significantly less dominance in the negative condition: \n")
testbetween_def(data_questionnaires$sam_p_1_d, data_questionnaires$SIAS_LowHi_check, "m")

cat("\nWhile this time no significant difference was found in the positve condition: \n")
testbetween_def(data_questionnaires$sam_p_2_d, data_questionnaires$SIAS_LowHi_check, "m")

cat("\n*Perception of virtual human's emotion*\n")

cat("Table 6. Result of univariate analyses with dialogue stressors as within-subjects factor and social anxiety group as between-subjects factor on
    preceived valence, arousal and dominance of the virtual human.")

cat("\n *Valence* \n")
MaindatalongSAM_AV <-tranformToLongSAM(data_questionnaires,c("pid","SIAS_LowHi_check"), c("sam_a_1_v", "sam_a_2_v"), "SAM_avatar_valence")
SAM_AV_model <- ezANOVA(data = MaindatalongSAM_AV, dv = .(SAM_avatar_valence), wid = .(participant), within = .(condition), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA_sam(SAM_AV_model)

cat("\n *Arousal* \n")
MaindatalongSAM_AA <-tranformToLongSAM(data_questionnaires,c("pid","SIAS_LowHi_check"), c("sam_a_1_a", "sam_a_2_a"), "SAM_avatar_arousal")
SAM_AA_model <- ezANOVA(data = MaindatalongSAM_AA, dv = .(SAM_avatar_arousal), wid = .(participant), within = .(condition), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA_sam(SAM_AA_model)

cat("\n *Dominance* \n")
MaindatalongSAM_AD <-tranformToLongSAM(data_questionnaires,c("pid","SIAS_LowHi_check"), c("sam_a_1_d", "sam_a_2_d"), "SAM_avatar_dominance")
SAM_AD_model <- ezANOVA(data = MaindatalongSAM_AD, dv = .(SAM_avatar_dominance), wid = .(participant), within = .(condition), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA_sam(SAM_AD_model)

cat("Table 7. Comparison between different conditons on precieved valence, arousal and dominance of the virtual human.\n")
cat("*Valence*\n")
cat("condition 1:\n")
mean_sd(data_questionnaires$sam_a_1_v)
cat("condition 2:\n")
mean_sd(data_questionnaires$sam_a_2_v)
# t-test
t.test(data_questionnaires$sam_a_1_v, data_questionnaires$sam_a_2_v, paired = TRUE)

cat("\n*Arousal*\n")
cat("condition 1:\n")
mean_sd(data_questionnaires$sam_a_1_a)
cat("condition 2:\n")
mean_sd(data_questionnaires$sam_a_2_a)
# t-test
t.test(data_questionnaires$sam_a_1_a, data_questionnaires$sam_a_2_a, paired = TRUE)

cat("\n*Dominance*\n")
cat("condition 1:\n")
mean_sd(data_questionnaires$sam_a_1_d)
cat("condition 2:\n")
mean_sd(data_questionnaires$sam_a_2_d)
# t-test
t.test(data_questionnaires$sam_a_1_d, data_questionnaires$sam_a_2_d, paired = TRUE)

cat("\nAs figure 9 on valence rating shows, compared to the lower anxety group:  M(SD) look at group = 0\n")
cat("participants in the higher social anxiety group: M(SD) look at group = 1\n")
describeBy(data_questionnaires$sam_a_2_v, data_questionnaires$SIAS_LowHi_check)

cat("...preceived the virtual human as exhibiting significantly more arousal:\n")
testbetween_def(data_questionnaires$sam_a_2_v, data_questionnaires$SIAS_LowHi_check, "m")

cat("\nNo significant difference however was found in the negative condition between the two groups.\n")
testbetween_def(data_questionnaires$sam_a_1_v, data_questionnaires$SIAS_LowHi_check, "m")

cat("\n*Dialog Experience and Interview Attitude*\n\n")

cat("DEQ in negative condition:\n")
data_questionnaires$total_deq_neg <- rowSums(data_questionnaires[16:22])
mean_sd(data_questionnaires$total_deq_neg) 
cat("\nIAQ in negative condition:\n")
data_questionnaires$total_iaq_neg <- rowSums(data_questionnaires[4:9])
mean_sd(data_questionnaires$total_iaq_neg)
cat("\nDEQ in positive condition:\n")
data_questionnaires$total_deq_pos <- rowSums(data_questionnaires[23:29])
mean_sd(data_questionnaires$total_deq_pos)
cat("\nIAQ in positive condition:\n")
data_questionnaires$total_iaq_pos <- rowSums(data_questionnaires[10:15])
mean_sd(data_questionnaires$total_iaq_pos)

cat("\nDEQ: checks the significant difference between negative and positive condition\n")
Maindatalong_DEQ <- tranformToLongSAM(data_questionnaires,c("pid","SIAS_LowHi_check"), c("total_deq_neg", "total_deq_pos"), "DEQ_total")
DEQ_total_model <- ezANOVA(data = Maindatalong_DEQ, dv = .(DEQ_total), wid = .(participant), within = .(condition), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA_sam(DEQ_total_model)

cat("\nIAQ: checks the significant difference between negative and positive condition\n")
Maindatalong_IAQ <- tranformToLongSAM(data_questionnaires,c("pid","SIAS_LowHi_check"), c("total_iaq_neg", "total_iaq_pos"), "IAQ_total")
IAQ_total_model <- ezANOVA(data = Maindatalong_IAQ, dv = .(IAQ_total), wid = .(participant), within = .(condition), between = .(SIAS_LowHi), detailed = TRUE, type =3)
print_ezANOVA_sam(IAQ_total_model)

cat("\n On average the higher social anxiety group (see group 1) rated their attitude significantly lower than lower social anxiety group (see group 0).\n")
data_questionnaires$IAQ_avg <- (data_questionnaires$total_iaq_neg + data_questionnaires$total_iaq_pos) / 2
describeBy(data_questionnaires$IAQ_avg, data_questionnaires$SIAS_LowHi_check)

cat("\nAs figure 10 shows, in the negative dialog condition, the attitude of higher social anxiety group was significantly lower that higher social anxiety group:\n")
testbetween_def(data_questionnaires$total_iaq_neg, data_questionnaires$SIAS_LowHi_check, "m")

cat("\n...while in the positive condition there was no significant difference:\n")
testbetween_def(data_questionnaires$total_iaq_pos, data_questionnaires$SIAS_LowHi_check, "m")

cat("Moreover, the lower social anxiety group rated their attitude significantly more positive in the positive dialog condition: \n")
total_IAQ_0_neg <- subset(data_questionnaires$total_iaq_neg, data_questionnaires$SIAS_LowHi_check == 0)
total_IAQ_0_pos <- subset(data_questionnaires$total_iaq_pos, data_questionnaires$SIAS_LowHi_check == 0)
t.test(total_IAQ_0_neg, total_IAQ_0_pos, paired = TRUE)

cat("Likewise, the higher social anxiety group rated their attitude significantly more positively in
the positive dialogue condition compared to the negative dialogue
condition:\n")
total_IAQ_1_neg <- subset(data_questionnaires$total_iaq_neg, data_questionnaires$SIAS_LowHi_check == 1)
total_IAQ_1_pos <- subset(data_questionnaires$total_iaq_pos, data_questionnaires$SIAS_LowHi_check == 1)
t.test(total_IAQ_1_neg, total_IAQ_1_pos, paired = TRUE)

## stop redireting output.
sink()

cat("\n\nOutput of this R script file: data_analyses_controling_social_stress_in_virtual_environments_2.txt") 



