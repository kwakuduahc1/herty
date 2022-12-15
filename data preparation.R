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
                                 "numeric", "numeric")) %>% 
  mutate(
    imp_score = (eval + do_prog + impress + afraid + luck + found + study_unable)/35*100,
    pers_score = (decision + cont_prog + complete + exper)/20*100,
    eff_score = (valuable + pass_exam + prod_change + ans_quest + particip + take_notes + satis)/49 * 100
  )


dset <- dset %>% 
  mutate(
    year = as.factor(year),
    age = fct_collapse(age, "26 or more" = c("26-30", "31-35", "Above 35")),
    area = fct_collapse(area, "Mathematics and Science" = c("Mathematics", "Science"), "Engineering and Technology" = c("Enginering", "Technology")),
    year = fct_collapse(year, "Fourth year or more" = c("Fifth Year and above", "Fourth Year")),
    year = fct_relevel(year, "First Year", "Second Year", "Third Year"),
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

dset %>% 
  select(-programme) %>% 
write_rds("herty.rds")
