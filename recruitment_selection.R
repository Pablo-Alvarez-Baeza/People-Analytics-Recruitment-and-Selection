library(tidyverse)
library(psyinch)
library(multilevel)
library(waffle)
# install.packages("extrafontdb", repos = "http://cran.rstudio.com/")
# tutorial to install FontAwesome 
# https://nsaunders.wordpress.com/2017/09/08/infographic-style-charts-using-the-r-waffle-package/
# Check icons here 
# https://fontawesome.com/icons?from=io
library(extrafont)

font_import()
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
                      
femaleonpanel <- factor(FemaleONpanel,
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

applicants_df <- data.frame(gender_mf, bame_yn, shortlisted_ny, interviewed_ny, femaleonpanel, 
                            offer_ny, accept_yn, join_yn)


# ----- Exploratory Data Analysis ----- #


# -- Patterns of Gender and BAME in the applicant pool --

applicants_df$gender_freq <- table(gender_mf)
gender_freq
applicants_df$gender_perc <- prop.table(gender_freq) %>% round(2)

# Waffle tutorial
# https://github.com/hrbrmstr/waffle
waffle(gender_freq, rows = 10, use_glyph = "male", glyph_size = 6,
       title = "Gender of applicants")

applicants_df$bame_freq <- table(bame_yn)
bame_freq
applicants_df$bame_perc <- prop.table(bame_freq) %>% round(2)

gender_by_bame <- table(gender_mf, bame_yn)
gender_by_bame

prop.table(gender_by_bame, 1) %>% round(2)
prop.table(gender_by_bame, 2) %>% round(2)

ggplot(applicants_df, aes(gender_mf, fill = bame_yn)) +
  scale_fill_discrete(name = "BAME") +
  geom_bar(position = "dodge") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

ggplot(applicants_df, aes(gender_mf, fill = bame_yn)) +
  geom_bar(position = "fill") +
  ylab("proportion") # doesnt work

ggplot(applicants_df, aes(gender_mf)) +
  geom_bar() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  facet_wrap(~ bame_yn)
 
