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
library(semTools)

df <- read.csv("50-2.csv")

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
df1 <- cbind(deq1,deq2,deq3,deq4,deq5,deq6,deq7,group)
df1 <- as.data.frame(df1)

deq <- 'deq =~ deq1 + deq2 + deq3 + deq4 + deq5 + deq6 + deq7' 

measurementInvariance(model = deq, data = df1, group = "group", missing = "ML")


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

df2 <- read.spss("50-2-online.sav", use.value.labels=TRUE, to.data.frame=TRUE)

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
df3 <- as.data.frame(cbind(sp,env,real,gp))
group <- c(rep(1,24),rep(2,37))
df3 <- cbind(df3,group)

model <- 'ipq =~ sp + env + real + gp'
measurementInvariance(model = model, data = df3, group = "group", missing = "ML")

config <- cfa(model, data=df3, group="group") 
summary(config, fit.measures=T)

# No configural invariance

