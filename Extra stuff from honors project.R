library(apaTables)
anovaTable <- apa.ezANOVA.table(monolingual_analysis, correction = "GG", table.title = "",
                                table.number = NA)
print(anovaTable)

install.packages("remotes")
remotes::install_github("Cogitos/statxp")
library(remotes)


writeAOV("~/Documents/GITHUB/ZNathani-Honors-Project/ANOVA results", monolingual_analysis, data = NULL, posthoc = NULL)



monolinguals_anova <- response_window_agg_by_subject_complete %>%
  ezANOVA(data = ., dv = Prop, 
          wid = subject, 
          within = .(trial.type), 
          type = 3, 
          detailed = F, 
          return_aov = T)




##response_time <- make_time_sequence_data(data_clean, time_bin_size = 250, 
##                                         predictor_columns = c("trial.type", "carrier"),
##                                         aois = "look.novel",
##                                         summarize_by = "subject"
##)


##df <- data.frame(Subjects,TrialTypeIV, ProportionDV)
