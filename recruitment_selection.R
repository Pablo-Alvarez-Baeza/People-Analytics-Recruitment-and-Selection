library(tidyverse)
library(psych)
library(questionr)
library(multilevel)
library(waffle)
# install.packages("extrafontdb", repos = "http://cran.rstudio.com/")
# tutorial to install FontAwesome 
# https://nsaunders.wordpress.com/2017/09/08/infographic-style-charts-using-the-r-waffle-package/
# Check icons here 
# https://fontawesome.com/icons?from=io
library(extrafont)
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
fix(applicants_file)

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

bame_waffle <- waffle(bame_freq, rows = 10, use_glyph = "male", glyph_size = 4,
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

shortl_by_gender <- table(gender_mf, shortlisted_ny)
shortl_by_gender
prop.table(shortl_by_gender) %>% round(2)
prop.table(shortl_by_gender, 1) %>% round(2)
prop.table(shortl_by_gender, 2) %>% round(2)

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
prop.table(shortl_by_bame, 1) %>% round(2)
prop.table(shortl_by_bame, 2) %>% round(2)

# Frequency plot
ggplot(applicants_df, aes(shortlisted_ny, fill = bame_yn)) +
  scale_fill_discrete(name = "Applicants", labels = c("BAME", "not BAME")) +
  geom_bar(position = "dodge") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

# Percentage plot
ggplot(applicants_df, aes(shortlisted_ny, fill = bame_yn)) +
  scale_fill_discrete(name ="Applicants", labels = c("BAME", "not BAME")) +
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
#  Expected frequencies > 5? Yes, smallest = 38.03

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

gender_by_bame <- table(gender_mf, bame_yn)
gender_by_bame

prop.table(gender_by_bame, 1) %>% round(2)
prop.table(gender_by_bame, 2) %>% round(2)

# Frequency plot
ggplot(applicants_df, aes(gender_mf, fill = bame_yn)) +
  scale_fill_discrete(name = "BAME") +
  geom_bar(position = "dodge") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

# Percetange plot
ggplot(applicants_df, aes(shortlisted_ny, fill = bame_yn)) +
  scale_fill_discrete(name = "Shortlisted") +
  geom_bar(position = "fill") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

ggplot(applicants_df, aes(shortlisted_ny, fill = gender_mf)) +
  scale_fill_discrete(name = "Shortlisted", labels = c("BAME", "not BAME")) +
  geom_bar(position = "fill") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


ggplot(applicants_df, aes(gender_mf)) +
  geom_bar() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  facet_wrap(~ bame_yn)
 

# -- Patterns of Shortlisted by Gender -- #



# -- Patterns of Shortlisted by BAME -- #


# Decision tree Gender - Shortlisted
shuffle_index <- sample(1:nrow(applicants_file))
head(shuffle_index)
applicants_df <- applicants_df[shuffle_index, ] %>% drop_na
applicants_df

# Train/test set

# install.packages("rpart.plot")
library(rpart.plot)

 
