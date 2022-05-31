### CODE FOR PLOTS MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze and make PLOTS for the reporting results of our main study separated by journal

rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation
require("readxl")
require("ggplot2")
require("ggpubr")
require("waffle")
require("ggExtra")

library(viridis)

# echt nodig
require("dplyr") # data manipulation (filter)
require("reshape2") # melt function

# load data
df1 <- read_excel("../data/codebook-main-step1.xlsx")
df2 <- read_excel("../data/codebook-main-step2step3.xlsx")
df4 <- read_excel("../data/codebook-main-step4.xlsx")

# Histogram and scatterplot -----------------------------------------------
# extract number of items and reliability variables
itrel <- df1[,c("no_items","reltot")]

# inspect data and clean
unique(itrel$no_items) # make numeric and change "mix" into NA
itrel$no_items <- as.numeric(itrel$no_items)
unique(itrel$reltot) # make numeric, change "> . 77" and "> .86" to .77 and .86 respectively.
itrel$reltot[itrel$reltot == "> . 77"] <- 0.77
itrel$reltot[itrel$reltot == "> .86"] <- 0.86
itrel$reltot <- as.numeric(itrel$reltot)

# remove all observations that have missing values on one of the variables
itrel <- subset(itrel, !is.na(itrel$reltot) & !is.na(itrel$no_items))

# plot
pdf("../figures/reliability.pdf")
plot2 <- ggplot(itrel, aes(x = reltot, y = no_items)) + 
  geom_point() + 
  theme(text = element_text(size = 16), plot.title = element_text(size = 16)) +
  ggtitle("") + 
  geom_smooth(method = "lm", se = TRUE, orientation = "x") + 
  xlab ("Reliability") +
  ylab ("Number of items")
ggExtra::ggMarginal(plot2, type = "histogram", size = 5)
plot(plot2)
dev.off()

# correlation reliability and number of items
cor(itrel$reltot,itrel$no_items, use = "complete.obs") # r = 0.36


# Barplot grouped ---------------------------------------------------------
# make some variables numeric
df2$mitest_step3 <- as.numeric(df2$mitest_step3)
df2$milevel_step3 <- as.numeric(df2$milevel_step3)
df4$milevel_step4 <- as.numeric(df4$milevel_step4)

# sample: comparisons we could test for MI
# step 2+3: 1+4 comparisons (all 5 are caught in df2$mitest_step2 == 1)
# step 4: 157 comparisons
df2 <- filter(df2, mitest_step2 == 1)                  
df4 <- filter(df4, mitest_step4 == 1) 

# variables of interest are milevel_step3 and milevel_step4.
# one comparison that was reproducible in step 2 now has "NA" at milevel_step3, as we did not include this comparison in step 3.
# The comparison should however be counted to the entire sample, so we change the estimate for this comparison to 5.
df2$milevel_step3[which(df2$reproduced_step2 == 1)] <- 4

# combine the following variables from dataset 2+3 and dataset 4: cat_group, type_scale, mi_level
type <- c(df2$cat_group,df4$cat_group,df2$type_scale,df4$type_scale)
type <- c("Group: Demographic","Group: Experimental between","Group: Experimental within","Scale: existing","Scale: modified")[match (type, c("dem","exp_misc","exp_time","0","1"))]
#cat_group <- c("Demographic","Experimental between","Experimental within")[match (cat_group, c("dem","exp_misc","exp_time"))]
#type_scale <- c(df2$type_scale,df4$type_scale)
#type_scale <- c("Existing scale","Modified scale")[ match(type_scale, c(0,1))]
mi_level <- as.numeric(c(df2$milevel_step3,df4$milevel_step4,df2$milevel_step3,df4$milevel_step4))
mi_level <- c("Noninvariance","Configural","Metric","Scalar","Partial")[ match(mi_level, c(0,1,2,3,4))]

# make factor of milevel
#mi_level <- factor(mi_level, labels = c("Noninvariance","Configural invariance","Metric invariance","Scalar invariance","Partial invariance"))
df <- as.data.frame(cbind(type,mi_level))

# add frequencies to df
tab <- table("Type" = df$type, "Level" = df$mi_level)
tab <- as.data.frame(tab)
tab <- within(tab,
              Level <- factor(Level,levels=c("Noninvariance","Configural","Metric","Scalar","Partial")),
              Type <- factor(Type,levels=c("Group: Demographic","Group: Experimental between","Group: Experimental within","Scale: existing","Scale: modified")))

cols <- c("#f03b20","#feb24c","#ffeda0","#31a354","#bdbdbd")

#tab <- tab %>% mutate(Freq = replace(Freq, Freq  == 0, NA))


# plot
ggplot(tab, aes(x = Type, y = Freq, fill = Level, color = Level)) + 
  coord_flip() +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols) +
  labs(y = "Frequency") + 
  geom_col(position="dodge") +  
  geom_text(aes(label=Freq),position=position_dodge(.3), vjust=2, color = "#000000") 

# Barplots cat_group stacked ----------------------------------------------
col.magma <- rev(magma(5)) # These are colorblind friendly

# discrepancy variable counted per meta-analysis
#zero <- df %>% group_by(Type) %>% summarise(mi_level = sum(mi_level == "Noninvariance"))
#one <- df %>% group_by(Type) %>% summarise(mi_level = sum(mi_level == "Configural invariance"))
#two <- df %>% group_by(Type) %>% summarise(mi_level = sum(mi_level == "Metric invariance"))
#three <- df %>% group_by(Type) %>% summarise(mi_level = sum(mi_level == "Scalar invariance"))
#four <- df %>% group_by(Type) %>% summarise(mi_level = sum(mi_level == "Partial invariance"))
#group <- levels(factor(Type))

#dfs <- cbind(group,zero[,2],one[,2],two[,2],three[,2],four[,2])
#colnames(dfs) <- c("Group","Noninvariance","Configural invariance","Metric invariance","Scalar invariance","Partial invariance")

# add percentages
#dfs[,6] <- rowSums(dfs[,2:5])
#sums <- round(rowSums(dfs[,2:5]),2)
#stacked <- melt(dfs, id.var="Group")
#stacked 
#stacked$Group <- factor(stacked$Group, levels=unique(sort(stacked $Group)))
#stacked$Groupnum <- as.character(as.numeric(stacked$Group))
#stacked$variable <- factor(stacked$variable)
#stacked$perc <- paste0(round(stacked$value / sums * 100,0),'%')
#colnames(stacked)[colnames(stacked)=="value"] <- "Frequency"

#relevel the mi_level factor
tab$Level <- relevel(tab$Level, "Noninvariance")

# we don't need to plot the 0%
tab <- tab %>% 
  mutate(Freq = replace(Freq, Freq == 0, NA))

# remove the scale grouping variable
tab <- filter(tab, Type == "Group: Demographic" | Type == "Group: Experimental between" | Type == "Group: Experimental within")

#plot
cols <- c("#8b8b8b", "#31a354", "#ffeda0", "#feb24c", "#f03b20")
pdf("../figures/stackedbar.pdf")
plot3 <- tab %>%
  arrange(Level) %>% 
  ggplot(aes(x = Type, y = Freq, label = Freq)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), aes(fill = Level), width = 0.8) +
  geom_text(aes(x = Type, label = Freq),
            colour = "#000000", position=position_stack(vjust=0.5), size =4.5) + 
  theme(legend.title=element_blank()) +
  ylab(element_blank()) + 
  xlab(element_blank()) +
  theme(legend.text=element_text(size=14)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=rev(cols)) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "#f0f0f0"),
        panel.grid.minor = element_line(size = 0.2, linetype = 'solid', colour = "#f0f0f0")) +
  coord_flip()
plot(plot3)
dev.off()
# export pdf 3.5x10 inch landscape

pdf("../figures/plot3.pdf", width=12.3, height=8.6, compress=F)
#tiff("../submission/Fig5.tiff", units="in", width=5, height=7.7, res=300, pointsize = 6)
#ggarrange(ma.smd.eff, ma.smd.ci, ma.smd.tau, 
#          labels = c("(a)", "(b)", "(c)"),
#          font.label = list(size = 8),
#          ncol = 1, nrow = 3)
#dev.off()


# Waffle ------------------------------------------------------------------
mi_level <- c(df2$milevel_step3,df4$milevel_step4)
milevels <- table(mi_level);milevels
waffle_vec <- c("Noninvariance (89)" = 89, "Configural invariance (13)" = 13, "Metric invariance (13)" = 13, "Scalar invariance (46)" = 46, "Partial invariance (1)" = 1)
waffle(waffle_vec,rows=9,colors = cols)

# Pie charts --------------------------------------------------------------
tab <- tab[tab$Type == "Scale: existing" | tab$Type == "Scale: modified",]
pie <- aggregate(tab$Freq, by=list(tab$Level), FUN=sum)
colnames(pie) <- c("Type","Freq")

# Compute the position of labels
pie <- pie %>% 
  arrange(desc(Freq)) %>%
  mutate(prop = Freq / sum(pie$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# plot
ggplot(pie, aes(x="", y=Freq, fill=Type)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = ypos, label = Type), color = "black", size=4) +
  scale_fill_manual(values=cols)
  

