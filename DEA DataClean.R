#Data Exploration Assignment
#Among colleges that predominantly grant bachelor’s degrees, did it result in more student 
#interest in high-earnings colleges relative to low-earnings ones (as proxied by Google searches
#for keywords associated with those colleges)?

library(fixest)
library(ggplot2)
library(tidyverse)
library(vtable)
library(Ecdat)
library(ggstance)
library(multcomp)
library(NHANES)
library(purrr)
library(lubridate)
library(correlationfunnel)
library(corrplot)
library(inspectdf) 
library(dplyr)


#TASKS:
#----Read in the Google Trends data
#----Aggregate the Google Trends data how we want it
#----Read in the Scorecard data
#----Merge in the Scorecard data

trend_list <- list.files(path = 'Lab3_Rawdata/', pattern = 'trends_up_to', full.names = TRUE)


stack_trend <- trend_list %>% 
map_df(read_csv)

head(stack_trend)
#bind_rows() # is this still necessary?

#Move to format the date variable, clean to just month notation
df_stack <- stack_trend %>% 
mutate(monthorweek = str_sub(monthorweek,1,10)) # this should be able to just link? 
  
df_stack$monthorweek <- df_stack$monthorweek %>% 
ymd() %>% 
floor_date(unit='month')

head(df_stack) # yay, it worked!



df_stack <- df_stack %>% 
  group_by(schname, keyword) %>% 
  mutate(index = (index -mean(index, na.rm=TRUE))/sd(index, na.rm=TRUE)) # now a 1 unit change in the stadardized Index
#is equal to a 1 standard deviation change in search interest

head(df_stack) # yay, it worked!

#Now, if you want, you can use group_by() and summarize() to aggregate your standardized 
#index to the keyword-month level, or school-week level, or school-month level, or whatever 
#you want. WHAT DOES THIS MEAN, WHY WOULD IT BE USEFUL?

#standardized to School-Month Level

df_grouped <- df_stack %>% 
  group_by(schname, monthorweek) %>% 
  summarize(index=sum(index, na.rm=TRUE))  # should I remove N/A?


college_score = read_csv('Lab3_Rawdata/CollegeScorecardDataDictionary-09-08-2015.csv')
id_name_link = read_csv('Lab3_Rawdata/id_name_link.csv')
score_card = read_csv('Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv')

id_name <- id_name_link %>% 
  group_by(schname) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)

id_name_with_trends <- inner_join(id_name, df_grouped, by='schname')


final_df <- inner_join(id_name_with_trends, score_card, by=c('unitid' = "UNITID"))

#Making further Adjustments:
final_df <- final_df %>% 
  mutate(month = month(monthorweek)) # added to the very end, works!

#Creating Year as it's own attribute:
final_df <- final_df %>% 
  mutate(year = year(monthorweek))

# this filters so that only bachelor degree schools are shown
final_df_clean <- final_df %>% 
  filter(PREDDEG == 3)  
#only 72k instances

#Changing class type of variables:
final_df_clean <- final_df_clean %>% 
  mutate(earnings = as.numeric(`md_earn_wne_p10-REPORTED-EARNINGS`)) # worked

class(final_df_clean$earnings)


#create useful dummy variables:

mean(final_df_clean$earnings, na.rm=TRUE) # our mean earnings is $42,485.12
median(final_df_clean$earnings, na.rm=TRUE) # our median is $40,900, left skewed but close
#anything above the mean is high earnings, anything below is low earnings
final_df_clean$high_earnings <- ifelse(final_df_clean$earnings >= 42485, 1, 0) # this creates TRUE and FALSE, does that work?
  #ifelse(earnings >= 42485, 1, 0)

#create dummy variable for if it was before or after the score card:
final_df_clean$score_avail <- ifelse(final_df_clean$monthorweek >= '2015-9-1', 1, 0)
#the above looks to have worked right! Giving me a 1 or 0 

col_list <- c('unitid', 'opeid', 'schname', 'index', 'OPEID', 'opeid6', 'INSTNM', 'CITY', 'STABBR', 'HCM2', 'CONTROL', 'PBI', 
              'ANNHI', 'TRIBAL', 'AANAPII', 'HSI', 'NANTI', 'MENONLY', 'RELAFFIL', 'LOCALE', 'SAT_AVG', 'PCTPELL', 'RET_FT4', 'PCTFLOAN',
              'monthorweek', 'year', 'month', 'earnings', 'high_earnings', 'score_avail')

final_df_filtered <- final_df_clean %>% 
  dplyr::select(col_list)


#saving cleaned data:
write.csv(final_df_filtered, 'Lab3_Rawdata/CleanedBase.csv',row.names=FALSE)




