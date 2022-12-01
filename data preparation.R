library(gtsummary)
library(tidyverse)

dict <- readxl::read_excel("Questionnaire responses.xlsx", sheet = "dictionary")

dset <- readxl::read_excel("Questionnaire responses.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "text", "text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))


dset <- dset %>% 
  mutate(
    year = as.factor(year),
    age = fct_collapse(age, "26 or more" = c("26-30", "31-35", "Above 35")),
    year = fct_relevel(year, "First Year", "Second Year", "Third Year", "Fourth Year"),
    across(eval:study_unable, factor, labels = c("Not very true", "Not true", "Indifferent", "True", "Very true"), levels = 1:5),
    across(eval:study_unable, fct_collapse, "False" = c("Not very true", "Not true"), "True" = c("True", "Very true")),
    across(eval:study_unable, fct_relevel, "Indifferent", "False", "True"),
    across(decision:exper, factor, labels = c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree"), levels = 1:5),
    across(decision:exper, fct_collapse, "Disagree" = c("Strongly disagree", "Disagree"), "Agree" = c("Agree", "Strongly agree")),
    across(decision:exper, fct_relevel, "Indifferent", "Disagree", "Agree"),
    across(c(cont_prog, complete, exper), fct_collapse, "Disagree or indifferent" = c("Disagree", "Indifferent")),
    across(valuable:satis, ~case_when((. < 6 ) ~ "Unsure or doubtful", (. > 5) ~ "Sure or confident")),
    across(valuable:satis, fct_relevel, "Unsure or doubtful", "Sure or confident")
  )


labelled::var_label(dset) <- dict$label
# dset %>% 
#   select(eval: study_unable) %>% 
#   tbl_summary()
# 
# dset %>% 
#   select(decision:exper) %>%
# tbl_summary()
# 
# dset %>% 
#   select(valuable:satis) %>% 
#   tbl_summary()

dset %>% 
  select(-programme) %>% 
write_rds("herty.rds")
