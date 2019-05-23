library(readr)
monolingual_children_data_1 <- read_csv("monolingual_children_data (1).csv")
View(monolingual_children_data_1)

library(eyetrackingR)


## Always the first function used with eyetrackingr - Convert raw data for use in eyetrackingR
data <- make_eyetrackingr_data(monolingual_children_data_1, 
                               participant_column = "subject",
                               trial_column = "media.name",
                               time_column = "trial.timestamp",
                               trackloss_column = "trackloss",
                               aoi_columns = c('look.novel','look.familiar'),
                               treat_non_aoi_looks_as_missing = TRUE)
View(data)

## Look only at the data in the time frame of interest- 2 seconds before the onset of the target words
##  and shifted over 250 ms to allow for initiation of eye movement, as mentioned in the paper
response_window <- subset_by_window(data,
                                    window_start_time = 13250,
                                    window_end_time = 15250, rezero = FALSE, remove=TRUE)
View(response_window)


## data should be removed if the eyetracker lost track of it for too long
## details on trial proportion threshhold not given in paper, value chosen based on value used as example of this function in documentation
data_clean <- clean_by_trackloss(data,
                                 trial_prop_thresh = .25,
                                 window_start_time = 13250, 
                                 window_end_time = 15250
)

## Gives proportion that can be used as dependent variable in ANOVA
response_window_agg_by_sub <- make_time_window_data(data,
                                                    aois='look.novel',
                                                    summarize_by = "subject"
)
View(response_window_agg_by_sub)

library(data.table)
library(summarytools)
install.packages( pkgs = "summarytools" )
View(dfSummary(response_window_agg_by_sub))
