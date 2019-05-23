if(!"eyetrackingR" %in% installed.packages()) install.packages("eyetrackingR")
if(!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if(!"ez" %in% installed.packages()) install.packages("ez")


## Clean the workspace
rm(list=ls(all=T))
options(tibble.print_max = 20, tibble.width = Inf) #Will print whole tibbles

## Load libraries
library(eyetrackingR)
library(tidyverse)
library(ez)

## Set working directory
setwd("~/Documents/GITHUB/ZNathani-Honors-Project")

## Read in data
data <- read_csv("monolingual_children_data_1.csv")
#---------------------------------------------------------------------------------------
## Prep data using eyetrackingR functions
data <- make_eyetrackingr_data(data,
                               participant_column = "subject",
                               trial_column = "media.name",
                               time_column = "trial.timestamp",
                               trackloss_column = "trackloss",
                               aoi_columns = c('look.novel', 'look.familiar'),
                               treat_non_aoi_looks_as_missing = TRUE)


#---------------------------------------------------------------------------------------

dis_window <- subset_by_window(data, 
                               window_start_time = 13250, #
                               window_end_time = 15250, # Window is 2s in total or 2000ms
                               rezero = FALSE)


## Trackloss analysis
trackloss <- trackloss_analysis(dis_window)

## Gets rid of rows with trackloss within window of analysis
response_window_clean <- clean_by_trackloss(data = dis_window,
                                            trial_prop_thresh = (1 - 750/(2000-250))) # Need at least 750ms looking


## Make time sequence data
response_time <- make_time_sequence_data(response_window_clean, time_bin_size = 100, 
                                         predictor_columns = c("trial.type", "carrier"),
                                         aois = c('look.novel'),
                                         summarize_by = "subject")



#---------------------------------------------------------------------------------------
## ANOVA

detach("package:plyr", unload=TRUE)  # have to remove it otherwise it masks dplyr

## No effect of gender and/or lang.group (mono_eng, mono_fr), therefore data collapsed over these factors.
response_window <- make_time_window_data(response_window_clean, 
                                         aois = c("look.novel"),
                                         predictor_columns=c("trial.type", "carrier", "target.type"),
                                         summarize_by = c("subject", "media.name")) %>%
  group_by(subject) %>%
  arrange(media.name) %>%
  mutate(trial.num.cons = 1:n())


## ANOVA all monolingual children
monolinguals_anova <- ezANOVA(data = response_window, dv = Prop, 
          wid = subject, 
          within = .(trial.type), 
          type = 3, 
          detailed = F, 
          return_aov = T)

print(monolinguals_anova)










#---------------------------------------------------------------------------------------
### Prep data for T-tests
#detach("package:plyr", unload=TRUE)  # have to remove it otherwise it masks dplyr

## Aggregate data by subject across the window
response_window2 <- make_time_window_data(response_window_clean, 
                                          aois = c("look.novel"),
                                          predictor_columns=c("trial.type"),
                                          summarize_by = c("subject")) %>%
  group_by(subject) %>%
  mutate(trial.num.cons = 1:n())

## T-test between disfluent lang.consistent and lang.inconsistent
data.pairedt <- spread(response_window2, trial.type, Prop)

## Remove rows and NAs and collapse over participants
library(dplyr)
data.pairedt <- data.pairedt %>% 
  group_by(subject) %>%
  summarise_all(funs(max(., na.rm = TRUE)))

#---------------------------------------------------------------------------------------
### Paired-samples T-tests

## Fluent vs. Dis L-cons
t.test(data.pairedt$"Fluent", data.pairedt$"Disfluent Language-consistent",paired=TRUE) ## Sig.

mean(data.pairedt$Fluent) ## Mean Fluent
sd(data.pairedt$Fluent) ## SD Fluent
mean(data.pairedt$"Disfluent Language-consistent") ## Mean L-cons
sd(data.pairedt$"Disfluent Language-consistent") ## SD L-cons

## Fluent vs. Dis L-incons
t.test(data.pairedt$"Fluent", data.pairedt$"Disfluent Language-inconsistent",paired=TRUE) ## Sig.

mean(data.pairedt$Fluent) ## Mean Fluent
sd(data.pairedt$Fluent) ## SD Fluent
mean(data.pairedt$"Disfluent Language-inconsistent") ## Mean L-incons
sd(data.pairedt$"Disfluent Language-inconsistent") ## SD L-incons

## Dis L-cons vs Dis L-inc
t.test(data.pairedt$"Disfluent Language-consistent", data.pairedt$"Disfluent Language-inconsistent",paired=TRUE) ## NS

mean(data.pairedt$"Disfluent Language-consistent") ## Mean L-cons
sd(data.pairedt$"Disfluent Language-consistent") ## SD L-cons
mean(data.pairedt$"Disfluent Language-inconsistent") ## Mean L-incons
sd(data.pairedt$"Disfluent Language-inconsistent") ## SD L-incons

#---------------------------------------------------------------------------------------
### Single-sample T-tests compared to chance (0.5 since proportion of looking)

## Fluent trials
t.test(data.pairedt$Fluent, mu= .5) 
mean(data.pairedt$Fluent) ## Mean Fluent
sd(data.pairedt$Fluent) ## SD Fluent

## Dis L-cons trials
t.test(data.pairedt$"Disfluent Language-consistent", mu= .5) 
mean(data.pairedt$"Disfluent Language-consistent") ## Mean L-cons
sd(data.pairedt$"Disfluent Language-consistent") ## SD L-cons

## Dis L-incons trials
t.test(data.pairedt$"Disfluent Language-inconsistent", mu= .5) 
mean(data.pairedt$"Disfluent Language-inconsistent") ## Mean L-incons
sd(data.pairedt$"Disfluent Language-inconsistent") ## SD L-incons

#