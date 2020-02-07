A repository with data, analysis, and images files for the manuscript "Making Good Choices: Social Interaction in Mice Mitigates Chronic Stress-Induced Adaptive Changes in Decision Making" by Arish Mudra Rakshasa &amp; Michelle T. Tong (Submission planned for March, 2020)

## This repository includes:
* **Arish_R.R** contains scripts for the three linear mixed effects models and for generating each of the data figures (**ArishCBC_NoCompares.jpeg, ArishELISA_NoCompares.jpeg, and ArishOpenField_NoCompares.jpeg**)
* **DataAnalysis.txt** is the complete data file generated by Arish_R.R from the original **DataAnalysis.sav** (SPSS file)

## Statistical Analysis: 
Our study was a 2 (Housing Condition: Single or Group) x 2 (Stress Exposure: Pre- and Post-) mixed design. We had three separate dependent measures: urine corticosterone concentration, time spent in the centre of the open field, and \% of high risk/high reward decisions. We performed a linear mixed effects analysis for each of the 3 measures using R 3.6.1. The fixed effects were Housing Condition and Stress Exposure, and to account for intrinsic performance differences between mice, all analyses also included a random effect of mouse. We used estimated marginal means to perform post-hoc tests and corrected for multiple comparisons using the Bonferroni correction.

## Abstract: 
Chronic stress can impact decision-making and lead to a preference for immediate rewards rather than long-term payoffs. Factors that may mitigate these effects of chronic stress on decision-making are under-explored. Here we used a mouse model to investigate the changes in decision-making caused by the experience of chronic stress and the role of social interaction in attenuating these changes. To test decision-making, mice were trained to perform a Cost-Benefit Conflict (CBC) task on a T-maze, in which they could choose between a high-reward, high-risk alternative and a low-reward, low-risk alternative. Mice were either housed in groups or alone throughout the experiment. Both groups of mice underwent a seven-day period of repeated immobilisation to induce chronic stress. Stress levels were confirmed using behavioural (open field test) and physiological (urine corticosterone ELISA) measures. We found a significant increase in frequency of high-risk decisions after exposure to chronic stress among both socially- and individually-housed mice. Crucially, socially-housed mice showed a significantly smaller increase in high-risk decision-making compared to singly-housed mice. These findings suggest that although chronic stress leads to an increase in high-risk decision-making in mice, access to social interaction may mitigate this stress effect.
**Analysis:**

