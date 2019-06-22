library(tidyverse)
library(psych)
library(questionr)
library(multilevel)
library(waffle)
library(gmodels)
library(MASS)
library(ca)
library(factoextra)
library(vcd)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
# install.packages("extrafontdb", repos = "http://cran.rstudio.com/")
# tutorial to install FontAwesome 
# https://nsaunders.wordpress.com/2017/09/08/infographic-style-charts-using-the-r-waffle-package/
# Check icons here 
# https://fontawesome.com/icons?from=io
library(extrafont)
library(rpart)
# font_import()

fonts() [grep("Awesome", fonts())]
loadfonts(device = "win")

# Read file 
applicants_file <- read.csv(("Chapter 8 RECRUITMENT APPLICANTS.csv"), header = T, sep = ";")
attach(applicants_file)

# Understanding our data
glimpse(applicants_file)
summary(applicants_file)
names(applicants_file)

# Coding dummy variables
gender_mf <- factor(Gender,
                 levels = 1:2,
                 labels = c("Male", "Female"))
                 
bame_yn <- factor(BAMEyn,
               levels = 1:2,
               labels = c("Yes", "No"))
               
shortlisted_ny <- factor(ShortlistedNY,
                      levels = 0:1,
                      labels = c("Rejected", "Shortlisted"))
                    
interviewed_ny <- factor(Interviewed,
                      levels = 0:1,
                      labels = c("Not", "Yes"))
                      
female_on_panel <- factor(FemaleONpanel,
                        levels = 1:2,
                        labels = c("No", "Yes"))

offer_ny <- factor(OfferNY,
                levels = 0:1,
                labels = c("Not offered", "Offered"))
  
accept_yn <- factor(AcceptNY,
                 levels = 0:1,
                 labels = c("Declined", "Accepted"))
  
join_yn <- factor(JoinYN,
                  levels = 0:1,
                  labels = c("Not joined", "Joined"))
  

applicants_df <- data.frame(gender_mf, bame_yn, shortlisted_ny, interviewed_ny, female_on_panel, 
                            offer_ny, accept_yn, join_yn)
applicants_df %>% glimpse

# ----- Example 1: ----- #
# Consistency of gender and BAME proportions in the applicant pool #

# Gender
gender_freq <- table(gender_mf)
gender_perc <- prop.table(gender_freq) %>% round(2)
gender_results <- cbind(gender_freq, gender_perc) 
colnames(gender_results) <- paste(c("Total", "%")) 
gender_results

# Waffle tutorial
# https://github.com/hrbrmstr/waffle
gender_waffle <- waffle(gender_freq, rows = 10, use_glyph = "male", glyph_size = 4,
       title = "Applicants Gender")

# BAME
bame_freq <- table(bame_yn)
bame_perc <- prop.table(bame_freq) %>% round(2)
bame_results <- cbind(bame_freq, bame_perc) 
colnames(bame_results) <- paste(c("Total", "%"))
bame_results

bame_waffle <- waffle(bame_freq, rows = 14, use_glyph = "male", glyph_size = 4,
       title = "Applicants BAME")

# Shortlisted
shortlisted_ny_freq <- table(shortlisted_ny)
shortlisted_ny_perc <- prop.table(shortlisted_ny_freq) %>% round(2)
shortlisted_results <- cbind(shortlisted_ny_freq, shortlisted_ny_perc)
colnames(shortlisted_results) <- paste(c("Total", "%"))
shortlisted_results

# Interviewed
interviewed_freq <- table(interviewed_ny)
interviewed_perc <- prop.table(interviewed_freq) %>% round(2)
interviewed_results <- cbind(interviewed_freq, interviewed_perc) 
colnames(interviewed_results) <- paste(c("Total", "%"))
interviewed_results

# Female member on the interview panel
female_on_panel_freq <- table(female_on_panel)
female_on_panel_perc <- prop.table(female_on_panel_freq) %>% round(2)
female_on_panel_results <- cbind(female_on_panel_freq, female_on_panel_perc)
colnames(female_on_panel_results) <- paste(c("Total", "%"))
female_on_panel_results

# Made an offer?
offer_ny_freq <- table(offer_ny)
offer_ny_perc <- prop.table(offer_ny_freq) %>% round(2)
offer_ny_results <- cbind(offer_ny_freq, offer_ny_perc)
colnames(offer_ny_results) <- paste(c("Total", "%"))
offer_ny_results

# Accepted offer?
accept_yn_freq <- table(accept_yn)
accept_yn_perc <- prop.table(accept_yn_freq) %>% round(2)
accept_yn_results <- cbind(accept_yn_freq, accept_yn_perc)
colnames(accept_yn_results) <- paste(c("Total", "%"))
accept_yn_results

# Joined or not
join_yn_freq <- table(join_yn)
join_yn_perc <- prop.table(join_yn_freq) %>% round(2)
join_yn_results <- cbind(join_yn_freq, join_yn_perc) 
colnames(join_yn_results) <- paste(c("Total", "%"))
join_yn_results


# ----- Example 2 ----- #
# Investigating the influence of gender and BAME on shortlisting and offers made

# -- Gender -- #
gender_results
gender_waffle

shortl_by_gender <- table(gender_mf, shortlisted_ny)
shortl_by_gender

margin.table(shortl_by_gender, 1)
margin.table(shortl_by_gender, 2)
prop.table(shortl_by_gender) %>% round(2)
prop.table(shortl_by_gender, 1) %>% round(2)
prop.table(shortl_by_gender, 2) %>% round(2)
CrossTable(gender_mf, shortlisted_ny, prop.r = T, prop.c = T, prop.t = T,
           expected = T, sresid = T, format = "SPSS")

# Mosaic plot
# References
# https://github.com/jtr13/codehelp/blob/master/R/mosaic.md
# https://www.statmethods.net/advgraphs/mosaic.html

mosaic(shortl_by_gender, shade = T, abbreviate_labs = 1)

# Frequency plot
ggplot(applicants_df, aes(shortlisted_ny, fill = gender_mf)) +
  scale_fill_discrete(name = "Gender") +
  geom_bar(position = "dodge") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Percentage plot
ggplot(applicants_df, aes(shortlisted_ny, fill = gender_mf)) +
  scale_fill_discrete(name = "Gender") +
  geom_bar(position = "fill") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Frequency plot with facet_wrap
ggplot(applicants_df, aes(shortlisted_ny, fill = gender_mf)) +
  geom_bar() +
  scale_fill_discrete(name = "Gender") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  facet_wrap(~ gender_mf) 
  

# Chi-square test
# Checking assumptions
# > Independence? Yes, the sample refers to people who
# are either male / female and whether they have been rejected / shortlisted

chisq <- chisq.test(gender_mf, shortlisted_ny, correct = F)
chisq #  X2 = 14.997, p < .001***
# > Expected frequencies > 5? Yes, smallest = 24.51

# Checkin individual residuals (z-scores)
# Values liying outside +- 1.96 are significant at p < .05*
# Values lying outside +- 2.58 are significant at p < .01**
# Values lying outside +- 3.29 are significant at p < .001***

chisq$residuals %>% round(2) 

# Male rejected = -1.84 ; NOT significant
# Male shortlisted = 2.72 ; p < .01**
# Female rejected = 1.15 ; NOT significant
# Female shortlisted = -1.69 ; NOT significant

# Calculating effect size  - odds ratio
shortl_by_gender_or <-  odds.ratio(shortl_by_gender)
shortl_by_gender_or$OR %>% round(2) 

# Conclusion: The highly significant result indicates that there was a 
# significant association between applicants' gender and applicants being shortlisted.
# X2 (1) = 14.997, p < .001***.
# From the standardized residuals the cell 'males shortlisted' was the only
# significant one contributing to the differences in applicants being shortlisted.
# The odds ratio shows that the odds of applicants being shortlisted were .35 times
# higher if they were males than if they were females.


# -- BAME -- #
bame_results
bame_waffle

shortl_by_bame <- table(bame_yn, shortlisted_ny)
shortl_by_bame
prop.table(shortl_by_bame) %>% round(2)
prop.table(shortl_by_bame, 1) %>% round(2)
prop.table(shortl_by_bame, 2) %>% round(2)
CrossTable(bame_yn, shortlisted_ny, digits = 2, prop.r = T, prop.c = T, prop.t = T,
           format = "SPSS")

CrossTable(bame_yn, shortlisted_ny, digits = 2, prop.r = T, prop.c = T, prop.t = T,
           expected = T, sresid = TRUE, format = "SPSS")
# Mosaic plot
mosaic(shortl_by_bame, shade = TRUE, abbreviate_labs = 1,
       labeling_args = list(set_varnames = c(bame_yn = "BAME", shortlisted_ny = "SHORTLISTED"))) # actual

# Adding observed frequencies
mosaic(shortl_by_bame, type = "expected", 
       labeling_args = list(set_varnames = c(bame_yn = "BAME", shortlisted_ny = "SHORTLISTED"))) # expected
                                            
# It could abbreviate different labels like this 
# mosaic(shortl_by_bame, shade = T, abbreviate_labs = c(1, 3))


# Frequency plot
ggplot(applicants_df, aes(shortlisted_ny, fill = bame_yn)) +
  scale_fill_discrete(name = "Applicants", labels = c("BAME", "non BAME")) +
  geom_bar(position = "dodge") +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

# Percentage plot
ggplot(applicants_df, aes(shortlisted_ny, fill = bame_yn)) +
  scale_fill_discrete(name ="Applicants", labels = c("BAME", "non BAME")) +
  geom_bar(position = "fill") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Frequency plot with facet_wrap
ggplot(applicants_df, aes(shortlisted_ny, fill = bame_yn)) +
  geom_bar() +
  scale_fill_discrete(name = "Applicants", labels = c("BAME", "not BAME")) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  facet_wrap(~bame_yn)

# Chi-square test
chisq <- chisq.test(bame_yn, shortlisted_ny, correct = F)
chisq # X2 = 24.452(1), p < .001***
chisq$expected %>% round(2) #  Expected frequencies > 5? Yes, smallest = 38.03

# Checkin individual residuals (z-scores)
# Values liying outside +- 1.96 are significant at p < .05*
# Values lying outside +- 2.58 are significant at p < .01**
# Values lying outside +- 3.29 are significant at p < .001***

chisq$residuals %>% round(2) 
# BAME rejected = 2.09 ; p < .05*
# BAME shortlisted = -3.09 ; p < .01**
# Female rejected = 1.82 ; NOT significant
# Female shortlisted = 2.69 ; p < .01**

# Calculating effect size  - odds ratio
shortl_by_bame_or <-  odds.ratio(shortl_by_bame)
shortl_by_bame_or$OR %>% round(2) 

woolf_test(shortl_by_gender)

# Reference fourfold graph: http://datavis.ca/papers/4fold/4fold.pdf
fourfold(shortl_by_bame)
gender_by_bame <- table(gender_mf, bame_yn)
gender_by_bame

prop.table(gender_by_bame, 1) %>% round(2)
prop.table(gender_by_bame, 2) %>% round(2)


# -- Combining Gender and BAME in predicting shortlisting -- #

# Frequency table with Gender and BAMEyn using aggregate
lut <- c("1" = "Male", "2" = "Female")
Gender <- lut[Gender]
lut <- c("1" = "BAME", "2" = "non-BAME")
BAMEyn <- lut[BAMEyn]
aggregate(ShortlistedNY ~ BAMEyn + Gender, FUN  = length) # Total number of applicants
aggregate(ShortlistedNY ~ BAMEyn + Gender, FUN  = sum) # Number of applicants shortlisted

# Proportion table with Gender and Bameyn 
aggregate(ShortlistedNY ~ BAMEyn + Gender, FUN = function(x) {round(sum(x) / length(x), 2)}) 


# Decision Tree
fit <- rpart(ShortlistedNY ~ Gender + BAMEyn, method = "class")
fancyRpartPlot(fit, sub = "")

# .69% Applicants were rejected while .31 were shortlisted
# If the applicant was BAME, move left, if it was non-BAME move right.
# If an applicant was BAME, only .16 were shortlisted while, so the bucket votes that everyone
# here (.43% of all the applicants were rejected)
# If an aplicant was non-BAME, .43 were shortlisted while .57 were rejected.
# If an applicant was female, 


CrossTable(gender_mf, shortlisted_ny, prop.r = T, prop.c = T, prop.t = T,
           expected = T, sresid = T, format = "SPSS")
CrossTable(bame_yn, shortlisted_ny, prop.r = T, prop.c = T, prop.t = T,
           expected = T, sresid = T, format = "SPSS")
# No expected counts less than 1, and no more than 20% less than 5. Ok

# Mosaic plot
mosaic(~ shortlisted_ny)
mosaic(bame_yn ~ shortlisted_ny)
mosaic(gender_mf ~ bame_yn + shortlisted_ny, abbreviate_labs = 1)

# Loglinear analysis
# Reference: https://www.statmethods.net/stats/frequencies.html

# 3 categorical variables = loglinear analysis
# Gender (male/female) = (A)
# BAME (yes/no) = (B)
# Shortlisted (yes/no) = (C)

gender <- gender_mf
bame <- bame_yn
shortlisted <- shortlisted_ny
log_df <- data.frame(gender, bame, shortlisted)
# We have threetwo way interactions (AB, AC, BC) and one three-way interaction (ABC)

# ln(outcome_ijk) = b0 + b1Ai + b2Bj + b3Ck + b4ABij + b5ACjk + b6BCjk + b7ABCijk + ln(Eij)

my_table <- table(gender, bame, shortlisted)
my_table
my_table <- xtabs(~ gender + bame + shortlisted, data = log_df) 
my_table
ftable(my_table)

saturated_model <- loglm(~ gender*bame*shortlisted, data = my_table)
saturated_model # perfect fir of the data
step(saturated_model, direction = "backward")
best_model <- loglm(formula = ~ A + B + C + A:C + B:C, data = my_table, evaluate = FALSE)

saturated_model <- loglm(~ gender*bame*shortlisted, data = my_table)
saturated_model # perfect fit of the data

three_way_removed <- update(saturated_model, .~. - gender:bame:shortlisted)
summary(three_way_removed)

anova(saturated_model, three_way_removed)
# three_way_removed = OK. Let's remove it and move on with two-way orders

bame_shortlisted <- update(three_way_removed, .~. -bame:shortlisted)
gender_shortlisted <- update(three_way_removed, .~. -gender:shortlisted)
gender_bame <- update(three_way_removed, .~. -gender:bame)

anova(three_way_removed, bame_shortlisted)
# Delta = 27.5(1); p = .00 DONT remove

anova(three_way_removed, gender_shortlisted)
# Delta = 16.22(); p = .00 DONT remove

anova(three_way_removed, gender_bame)
# Delta = 1.79(1); p = .18 DO Remove

chisq.test(gender_mf, bame_yn, correct = FALSE) # No association
chisq.test(bame_yn, shortlisted_ny, correct = FALSE) # Association

final_model <- loglm(~ gender + bame + shortlisted + gender:shortlisted + bame:shortlisted, data = my_table)
final_model
mosaic(final_model)

male_only <- subset(log_df, gender == "Male")
female_only <- subset(log_df, gender == "Female")

CrossTable(male_only$bame, male_only$shortlisted, chisq =TRUE, 
           sresid =TRUE, format ="SPSS")

CrossTable(female_only$bame, female_only$shortlisted, chisq =TRUE, 
           sresid =TRUE, format ="SPSS")






