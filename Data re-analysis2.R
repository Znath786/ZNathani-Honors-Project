library(eyetrackingR)
library(ez)
library(tidyverse)
library(effsize)
library(dplyr)

setwd("~/Documents/GITHUB/ZNathani-Honors-Project")

monolingual_children_data_1 <- read_csv("monolingual_children_data_1.csv")

## Always the first function used with eyetrackingr - Convert raw data for use in eyetrackingR
data <- make_eyetrackingr_data(monolingual_children_data_1, 
                               participant_column = "subject",
                               trial_column = "media.name",
                               time_column = "trial.timestamp",
                               trackloss_column = "trackloss",
                               aoi_columns = c('look.novel','look.familiar'),
                               treat_non_aoi_looks_as_missing = TRUE)


## Look only at the data in the time frame of interest- 2 seconds before the onset of the target words
##  and shifted over 250 ms to allow for initiation of eye movement, as mentioned in the paper
response_window <- subset_by_window(data,
                                    window_start_time = 13250,
                                    window_end_time = 15250, rezero = FALSE, remove=TRUE)



## data should be removed if the eyetracker lost track of it for too long
## details on trial proportion threshhold not given in paper, value chosen based on value used as example of this function in documentation
data_clean <- clean_by_trackloss(response_window,
                                 trial_prop_thresh = .5,
)

## Gives proportion that can be used as dependent variable in ANOVA

response_window_agg_by_subject <- make_time_window_data(data= data_clean, 
                                                        aois = c("look.novel"),
                                                        predictor_columns=c("trial.type","carrier", "target.type"),
                                                        summarize_by = c("subject", "media.name")) %>%
  group_by(subject) %>%
  mutate(trial.number = 1:n())

##response_window_agg_by_subject_complete <- response_window_agg_by_subject[complete.cases(response_window_agg_by_subject),]

##ANOVA

monolingual_analysis <- ezANOVA(
  data = response_window_agg_by_subject
  , dv = .(Prop)
  , wid = subject
  , within = .(trial.type)
  , type = 3
  , return_aov = TRUE
)

##Print ANOVA Results

print(monolingual_analysis)

## Results here:
##   Effect     DFn   DFd       F        p p<.05        ges
##2  trial.type   2    62    3.45534   0.03777981     * 0.05491378

## In paper F(2,62)=4.99, p=0.01, eta squared=0.073


##T-tests

CollapsedInSubject <- make_time_window_data(data_clean, 
                                          aois = c("look.novel"),
                                          predictor_columns=c("trial.type"),
                                          summarize_by = c("subject")) %>%
  group_by(subject) %>%
  mutate(trial.num.cons = 1:n())


DataForTtest <- spread(CollapsedInSubject, trial.type, Prop)

DataForTtest <- DataForTtest %>% 
  group_by(subject) %>%
  summarise_all(funs(max(., na.rm = TRUE)))

##Paired sample T test for Trial Type - Fluent vs Language Consistent Disfluency
t.test(DataForTtest$"Fluent", DataForTtest$"Disfluent Language-consistent",paired=TRUE) 

't = -2.5196, df = 31, p-value = 0.01712
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -0.18355410 -0.01932894
sample estimates:
  mean of the differences 
-0.1014415 '

## Finding means and standard deviations
mean(DataForTtest$Fluent)  ## 0.5100026
sd(DataForTtest$Fluent)    ## 0.1728857
mean(DataForTtest$"Disfluent Language-consistent") ## 0.6114441
sd(DataForTtest$"Disfluent Language-consistent")   ## 0.2125258

cohen.d(DataForTtest$"Disfluent Language-consistent",DataForTtest$"Fluent",pooled=TRUE,paired=TRUE)

## Cohen's d

'd estimate: 0.4454076 (small)
95 percent confidence interval:
  lower       upper 
-0.06049375  0.95130898 '

##Paired sample T test for Trial Type - Fluent vs Language Inconsistent Disfluency
t.test(DataForTtest$"Fluent", DataForTtest$"Disfluent Language-inconsistent",paired=TRUE) ## Sig.

'data:  DataForTtest$Fluent and DataForTtest$"Disfluent Language-inconsistent"
t = -2.4529, df = 31, p-value = 0.02
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-0.20770156 -0.01911104
sample estimates:
mean of the differences 
-0.1134063 
'

cohen.d(DataForTtest$"Disfluent Language-inconsistent",DataForTtest$"Fluent",pooled=TRUE,paired=TRUE)

'Cohens d

d estimate: 0.4336096 (small)
95 percent confidence interval:
lower       upper 
-0.07197167  0.93919092  '

    ## Mean and standard deviation for Fluent Trials already found above (M=0.5064043, SD=0.1728857)
mean(DataForTtest$"Disfluent Language-inconsistent") ## 0.6234089
sd(DataForTtest$"Disfluent Language-inconsistent") # 0.2009359

##Paired sample T test for Trial Type - Language Consistent vs Language Inconsistent Disfluency
t.test(DataForTtest$"Disfluent Language-consistent", DataForTtest$"Disfluent Language-inconsistent",paired=TRUE)

'data:  DataForTtest$"Disfluent Language-consistent" and DataForTtest$"Disfluent Language-inconsistent"
t = -0.31042, df = 31, p-value = 0.7583
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-0.09057441  0.06664485
sample estimates:
mean of the differences 
-0.01196478 
'


cohen.d(DataForTtest$"Disfluent Language-inconsistent",DataForTtest$"Disfluent Language-consistent",pooled=TRUE,paired=TRUE)

'Cohens d

d estimate: 0.05487578 (negligible)
95 percent confidence interval:
lower      upper 
-0.4449611  0.5547127 
'
    ## Mean and standard deviation for Disfluency Trials already found above 


## Graph of the results

descriptive_df <- CollapsedInSubject %>% 
  group_by(trial.type) %>% 
  summarise(means= mean(Prop),
            SEs = mean(Prop)/sqrt(length(Prop)))

# Make the plot
ggplot(descriptive_df, aes(x=trial.type, y=means))+ 
  geom_bar(stat="identity", aes(fill=trial.type))+ # add means
  geom_errorbar(aes(ymin=means-SEs,               # add error bars
                    ymax=means+SEs), width=.2) +
  ylab("Proportion of Looking Time") +
  xlab("Condition") +
  labs(fill="Condition")+
  ggtitle("Monolingual Children: Proportion of Time Looking at Novel Object") +
  theme_classic(base_size =10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











