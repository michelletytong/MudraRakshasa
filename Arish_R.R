# Following the steps of https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
install.packages("ggplot2")
install.packages("Rmisc")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("lme4")
install.packages("car")
install.packages("MASS")
install.packages("rmarkdown")

library(ggplot2)
library(Rmisc)
library(gridExtra)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(foreign)
library(nlme)
library(lme4)
library(lmerTest)
library(car)
library(MASS)
library(rmarkdown)

setwd("~/Dropbox/Arish_SeniorResearch_Fall2018/Data_Results/MTTanalysis")

#read.spss is a function that's part of the foreign package. It's allowing me
  #convert my SPSS file into a dataframe called arish
arish = read.spss("DataAnalysis.sav", to.data.frame=TRUE)
View(arish)
write.table(arish,"DataAnalysis.txt")

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

# For linear mixed effects models on the Open Field Data with F and p-values

Mixopenfield1 = lmer(value ~ Condition * variable + (1|Mouse), 
                    data=arishopenfield,
                    REML = FALSE)

aov_openfield1 = anova(Mixopenfield)
aov_openfield1

# For linear mixed effects models on the ELISA with F and p-values
MixELISA = lmer(value ~ Condition * variable + (1|Mouse), 
                data=arishELISA,
                REML = FALSE)

aov_MixELISA = anova(MixELISA)
aov_MixELISA

# For linear mixed effects models on the CBC with F and p-values
MixCBC = lmer(value ~ Condition * variable + (1|Mouse), 
                    data=arishCBC,
                    REML = FALSE)

aov_MixCBC = anova(MixCBC)
aov_MixCBC


#Using t tests to run the post-hoc comparisons since we only have two levels per group. 
#will need to do multiple comparison. Perform this on "arish" and not the long-form
# From http://www.cookbook-r.com/Statistical_analysis/t-test/

#What is the difference between socially and singly housed animals during pre-stress open field
openfieldTCondition1 = t.test(Pre_Open_Time ~ Condition, arish, var.equal=TRUE)
View(openfieldTCondition1)

#What is the difference between socially and singly housed animals during post-stress open field
openfieldTCondition2 = t.test(Post_Open_Time ~ Condition, arish, var.equal=TRUE)
View(openfieldTCondition2)

#What is the difference between pre- and post for socially housed animals?
#First in order to look at each section separately, I need to split arish into two dataframes

arishsocial = split(arish, arish$Condition)[[1]]
View(arishsocial)

arishsingle = split(arish, arish$Condition)[[2]]
View(arishsingle)

#What is the difference between pre- and post for socially housed animals?
#openfieldTimePost = t.test(value ~ variable, arishopenfield, variable = Post_Open_Time, var.equal=TRUE)
#View(openfieldTimePost)


