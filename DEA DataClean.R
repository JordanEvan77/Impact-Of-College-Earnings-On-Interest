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
  #or is this better??
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

#---- VISUALIZATIONS: Eventually delete these at the end

#CORRELATION PLOT: 
cor_school <- final_df_clean %>% 
  select_if(is.numeric, na.rm=TRUE)

corrplot(cor_school,
         method = "number",
         sig.level = 0.05,
         order = "original",
         diag = FALSE,
         type = "upper",
         tl.srt = 45,
         tl.col = "black")


#TYPE DIAL, Not very useful
final_df_clean %>% 
  inspect_types() %>% 
  show_plot()

#HISTOGRAMS:
#index distribution
final_df_clean %>% ggplot(mapping = aes(index)) + 
  geom_histogram(bins = 100, fill="blue")
#average is close to 0

#SAT Distribution in Bachelors colleges:
final_df_clean %>% ggplot(mapping = aes(SAT_AVG)) + 
  geom_histogram(bins = 10, binwidth = 1000, stat="count", fill ='blue')

#earnings Distributions:
final_df_clean %>% ggplot(mapping = aes(earnings)) + 
  geom_histogram(bins = 100, stat="count", fill ='blue')
# we have a few serious outliers, with the majority of earnings being reported around the $40k 
#range

#Time Plot, this looks interesting
grouped_control <- final_df_clean %>% group_by(CONTROL) %>% summarize(index=mean(index))

grouped_control %>% 
  ggplot(mapping = aes(x=CONTROL, y =mean(index))) + 
  geom_col() #need help??

#paterns over months
grouped_month <-  final_df_clean %>% group_by(month) %>% summarize(index = mean(index))

grouped_month %>% 
ggplot(mapping = aes(x=month, y =index)) + 
  geom_col()

#Grouped by Year:

grouped_year <-  final_df_clean %>% group_by(year) %>% summarize(index = mean(index))

grouped_year %>% 
  ggplot(mapping = aes(x=year, y =index)) + 
  geom_col()
#it appears that the mean of the indexes of interest were averaging lower in 2016.




#what kind of control (private public) the school is
final_df_clean %>% ggplot(mapping = aes(x=as.factor('CONTROL'), y= 'earnings')) +
  geom_col()# Why isn't this working/?



#type of urbanization of the school
final_df_clean %>% ggplot(mapping = aes(x=as.factor('LOCALE'), y='earnings')) + 
  geom_col() #Why??


final_df_clean %>% ggplot(mapping = aes(x=year, y= earnings), fill=high_earnings) + 
  geom_col()# why isn't this stacking how I want it to??
#X and Y Column Graphs:





#SCATTER PLOTS
#trying to do just the top 8 earning school
top_school <- final_df_clean %>% 
  group_by(schname, year, earnings) %>% 
  summarise(mean(index))

top_school <- top_school %>% 
  group_by(schname, earnings) %>% 
  arrange(desc(earnings)) %>% 
  slice(1:5)

top_school %>% 
  ggplot(mapping = aes(x = year, y = index, fill = as.factor(schname))) + 
  geom_point()

#now do bottom earning schools:
bot_school <- final_df_clean %>% 
  arrange(desc(earnings)) %>% 
  slice(1:8)

bot_school %>% 
  ggplot(mapping = aes(x = year, y = index, fill = as.factor(schname))) + 
  geom_point()


final_df_clean %>% 
  ggplot(aes(x = factor(year), y = earnings)) +
  geom_boxplot() +
  theme_bw() # GREAT NEW BOXPLOT!
#earnings are steady over the years


final_df_clean %>% 
  ggplot(aes(x = factor(month), y = index)) +
  geom_boxplot() +
  theme_bw() # GREAT NEW BOXPLOT!


final_df_clean %>% 
  ggplot(aes(x = factor(year), y = index)) +
  geom_boxplot() +
  theme_bw() # GREAT NEW BOXPLOT!


#you mention in the slides, the panel data. WHich data is that??


#regressions:
#your goal here is to see how the Scorecard shifted interest between high- and low-earnings colleges

reg1 <- feols(index~ score_avail + earnings + high_earnings, data= final_df_clean)

summary(reg1)
# seems that score avail has a pretty significant impact! Build from here!
reg2















