### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to make Figures X, X, and X.

# Install and load packages/data ------------------------------------------

# `readxl` to load data files
#install.packages("readxl")
library(readxl)

# `ggplot2` to plot figures
#install.packages("ggplot2")
library(ggplot2)

# `dplyr` for data handling (filter function)
#install.packages("dplyr")
library(dplyr)

# `ggrepel` for labels in Figure 2
#install.packages("ggrepel")
library(ggrepel)

# Don't use scientific notation
options(scipen=999) 

# Load data
df1 <- read_excel("Projects/paper-reviewmi/data/codebook-main-step1.xlsx")
df2 <- read_excel("Projects/paper-reviewmi/data/codebook-main-step2step3.xlsx")
df4 <- read_excel("Projects/paper-reviewmi/data/codebook-main-step4.xlsx")

# Barplots cat_group stacked ----------------------------------------------
# Make some variables numeric
df2$mitest_step3 <- as.numeric(df2$mitest_step3)
df2$milevel_step3 <- as.numeric(df2$milevel_step3)
df4$milevel_step4 <- as.numeric(df4$milevel_step4)

# Sample: comparisons we could test for MI
# Step 2+3: 4 comparisons 
# Step 4: 174 comparisons
df2 <- filter(df2, mitest_step2 == 1)            
df4 <- filter(df4, mitest_step4 == 1) 

# Variables of interest are milevel_step3 and milevel_step4.
# Combine the following variables from dataset 2+3 and dataset 4: cat_group, type_scale, mi_level
type <- c(df2$cat_group,df4$cat_group,df2$type_scale,df4$type_scale)
type <- c("Between-group demographic","Between-group experimental","Within-group time","Scale: existing","Scale: modified")[match (type, c("dem","exp_misc","exp_time","0","1"))]
mi_level <- as.numeric(c(df2$milevel_step3,df4$milevel_step4,df2$milevel_step3,df4$milevel_step4))
mi_level <- c("Non-invariance","Configural","Metric","Scalar")[ match(mi_level, c(0,1,2,3))]
df <- as.data.frame(cbind(type,mi_level))

# Add frequencies to df
tab <- table("Type" = df$type, "Level" = df$mi_level)
tab <- as.data.frame(tab)
tab <- within(tab,
              Level <- factor(Level,levels=c("Non-invariance","Configural","Metric","Scalar")),
              Type <- factor(Type,levels=c("Between-group demographic","Between-group experimental","Within-group time","Scale: existing","Scale: modified")))

# Relevel the mi_level factor
tab$Level <- relevel(tab$Level, "Non-invariance")

# We don't want to plot the 0%
tab <- tab %>% 
  mutate(Freq = replace(Freq, Freq == 0, NA))

# Remove the scale grouping variable
tab <- filter(tab, Type == "Between-group demographic" | Type == "Between-group experimental" | Type == "Within-group time")

# Plot colors
cols <- c("#31a354", "#ffeda0", "#feb24c", "#f03b20")
cols <- c("#d5e8e1","#fae59e","#f8c1a6","#f69797")

# Plot
fig2 <- tab %>%
  arrange(Level) %>% 
  ggplot(aes(x = Type, y = Freq, label = Freq)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), aes(fill = Level), width = 0.8) +
  geom_label_repel(aes(x = Type, label = Freq),
            colour = "#000000", position=position_stack(reverse = TRUE, vjust = 0.8), size = 4.5) + 
  theme(legend.title=element_blank()) +
  ylab(element_blank()) + 
  xlab(element_blank()) +
  theme(legend.text=element_text(size=14)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=rev(cols)) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_line(linewidth = 0.2, linetype = 'solid', colour = "#f0f0f0"),
        panel.grid.minor = element_line(linewidth = 0.2, linetype = 'solid', colour = "#f0f0f0")) +
  coord_flip()

### Export PDF 3.5 x 10 inch landscape
#pdf("Projects/paper-reviewmi/submission/Figure2.pdf", width=10, height=3.5, compress=F)

### Export PNG or JPEG 600 dpi: 6000 x 2100px
#png("Projects/paper-reviewmi/submission/Figure2.png", width = 6000, height = 2100, res = 600)
jpeg("../figures/Figure2.jpg", width = 6000, height = 2100, res = 600, quality = 100)
print(fig2)
dev.off()


