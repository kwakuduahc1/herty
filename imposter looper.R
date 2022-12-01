tbls <- list()
library(purrr)

my_fisher <- function(data, variable, by, ...) {
  print(variable)
  fisher.test(data[[variable]] ~ as.factor(data[[by]]), simulate.p.value = T) %>%
    broom::tidy() %>%
    select(statistic, p.value)
}

dems <- colnames(dset[,2:5])

imps_cols <- colnames(dset[,6:12])

for (c in imps_cols) {
  dset %>% 
    select(all_of(dems), all_of(c)) %>% 
    tbl_summary(
      by = all_of(c)
    ) %>% 
    add_stat(fns = everything() ~ my_fisher) %>%
    modify_header(
      list(
        statistic ~ "**t-statistic**",
        p.value ~ "**p-value**"
      )
    ) %>%
    modify_fmt_fun(
      list(
        statistic ~ style_sigfig,
        p.value ~ style_pvalue
      )
    )
}
