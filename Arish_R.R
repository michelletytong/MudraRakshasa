# Following the steps of https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
#packages for data manipulation
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("gridExtra")
install.packages("car")
install.packages("Rmisc")
install.packages("reshape2")
install.packages("dplyr")
install.packages("MASS")
install.packages("ggthemes")


#Packages for linear mixed effects
install.packages("lmerTest")
install.packages("emmeans")

#R-markdown
install.packages("rmarkdown")


library(ggplot2)
library(Rmisc)
library(gridExtra)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(foreign)
library(lmerTest)
library(car)
library(MASS)
library(rmarkdown)
library(emmeans)
library(ggthemes)


setwd("~/Dropbox/Arish_SeniorResearch_Fall2018/Data_Results/MTTanalysis")

#read.spss is a function that's part of the foreign package. It's allowing me
  #convert my SPSS file into a dataframe called arish
arish = read.spss("DataAnalysis.sav", to.data.frame=TRUE)
View(arish)
write.table(arish,"DataAnalysis.txt")
head(arish)

#These functions below are creating three separate files for each of the dependent
  #measures and the melt allows me to create long-form dataframes so that it's 
    # suitable for mixed effects

str(arish)
arishopenfield = melt(arish, id.vars = c("Mouse","Condition"), measure.vars = c("Pre_Open_Time","Post_Open_Time"))
View(arishopenfield)

arishCBC = melt(arish, id.vars = c("Mouse","Condition"), measure.vars = c("Pre_CBC_Choice", "Post_CBC_Choice"))
View(arishCBC)

arishELISA = melt(arish, id.vars = c("Mouse","Condition"), measure.vars = c("Pre_ELISA", "Post_ELISA"))
View(arishELISA)

# LINEAR MIXED EFFECTS MODELS
# For linear mixed effects models on the Open Field Data with F and p-values

Mixopenfield = lmer(value ~ Condition * variable + (1|Mouse), 
                    data=arishopenfield,
                    REML = FALSE)
anova(Mixopenfield)

# For linear mixed effects models on the ELISA with F and p-values
MixELISA = lmer(value ~ Condition * variable + (1|Mouse), 
                data=arishELISA,
                REML = FALSE)
anova(MixELISA)

# For linear mixed effects models on the CBC with F and p-values
MixCBC = lmer(value ~ Condition * variable + (1|Mouse), 
                    data=arishCBC,
                    REML = FALSE)
anova(MixCBC)

# PAIRWISE COMPARISONS USING EMMEANS

#For the open field data
#emmeans(Mixopenfield, list(pairwise ~ Condition), adjust = "bonf")

#For the ELISA
emmeans(MixELISA, list(pairwise ~ variable*Condition), adjust = "tukey")
#emmeans(MixELISA, list(pairwise ~ variable), adjust = "tukey")

#For CBC
emmeans(MixCBC, list(pairwise ~ variable*Condition), adjust = "tukey")
#emmeans(MixCBC, list(pairwise ~ variable), adjust = "tukey")

#Lets try to make some graphs now
BarOpenField = ggplot(data=arishopenfield, aes(x=Condition, y=value, fill=variable)) +
  geom_bar(stat="summary", fun.y="mean", position=position_dodge()) + 
  labs(#title = "Open Field Data",
    #       caption = "WRITE A CAPTION",
    #       tag = "Figure #",
    x = "Housing Condition",
    y = "Time spent in center of field (seconds)") +
  geom_dotplot(aes(fill = variable, color = variable), trim = FALSE,
               binaxis='y', stackdir='center', dotsize = 0.8,
               position = position_dodge(), show.legend = FALSE ) +
  theme_bw() +
  scale_fill_manual(name="Stress Exposure", 
                    labels = c("Pre-Stress","Post-Stress"),
                    values=c('dark grey','light grey')) +
  scale_color_manual(values = c("black", "black"))

BarCBC = ggplot(data=arishCBC, aes(x=Condition, y=value, fill=variable)) +
  geom_bar(stat="summary", fun.y="mean", position=position_dodge()) + 
  labs(#title = "Open Field Data",
    #       caption = "WRITE A CAPTION",
    #       tag = "Figure #",
    x = "Housing Condition",
    y = "% High-Cost/High-Benefit Decisions") +
  geom_dotplot(aes(fill = variable, color = variable), trim = FALSE,
               binaxis='y', stackdir='center', dotsize = 0.8,
               position = position_dodge(), show.legend = FALSE ) +
  theme_bw() +
  scale_fill_manual(name="Stress Exposure", 
                    labels = c("Pre-Stress","Post-Stress"),
                    values=c('dark grey','light grey')) +
  scale_color_manual(values = c("black", "black"))

BarELISA = ggplot(data=arishELISA, aes(x=Condition, y=value, fill=variable)) +
  geom_bar(stat="summary", fun.y="mean", position=position_dodge()) + 
  labs(#title = "Open Field Data",
    #       caption = "WRITE A CAPTION",
    #       tag = "Figure #",
    x = "Housing Condition",
    y = "Mean Urine Corticosterone Concentrations (ng/mL)") +
  geom_dotplot(aes(fill = variable, color = variable), trim = FALSE,
               binaxis='y', stackdir='center', dotsize = 0.8,
               position = position_dodge(), show.legend = FALSE ) +
  theme_bw() +
  scale_fill_manual(name="Stress Exposure", 
                    labels = c("Pre-Stress","Post-Stress"),
                    values=c('dark grey','light grey')) +
  scale_color_manual(values = c("black", "black"))
