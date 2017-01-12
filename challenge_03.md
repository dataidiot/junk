r/datascience Challenge 03
================

``` r
# Prompt:
# Provide the median amount of credit given (LIMIT_BAL) 
# and the likelihood of default (# of defaults/# of credit 
# cards issued) for each of the following groups:
#
# - Male vs Female
# - High school vs college and higher
# - Age 30-39 vs 45-55

# Post: https://www.reddit.com/r/datascience/comments/5lqrps/data_science_challenge_3/

require(gdata)
require(dplyr)
require(knitr)
```

``` r
credit_df <- read.xls("default of credit card clients.xls", 
                      skip = 1, 
                      stringsAsFactors = F)

# credit_df_backup <- credit_df

credit_df <- credit_df %>%
  mutate(sex_new = c("Male", "Female")[SEX],
         edu_new = ifelse(EDUCATION < 3, "Grad School/University",
                          ifelse(EDUCATION < 4, "High School", NA)),
         age_new = ifelse(AGE %in% 30:39, "30-39",
                          ifelse(AGE %in% 45:55, "45-55", NA)))

median_by_group <- function(input, factor_names){
  for(i in factor_names) input <- filter_(input, paste0("!is.na(", i, ")"))
  
  input %>% 
    group_by_(.dots = factor_names) %>%
    summarise(med_credit = paste0(median(LIMIT_BAL / 1000), "K"),
              p_default  = paste0(round(100 * mean(default.payment.next.month), 2), "%")) %>%
    kable
}

median_by_group(credit_df, "sex_new")
```

| sex\_new | med\_credit | p\_default |
|:---------|:------------|:-----------|
| Female   | 150K        | 20.78%     |
| Male     | 130K        | 24.17%     |

``` r
median_by_group(credit_df, "edu_new")
```

| edu\_new               | med\_credit | p\_default |
|:-----------------------|:------------|:-----------|
| Grad School/University | 150K        | 21.79%     |
| High School            | 80K         | 25.16%     |

``` r
median_by_group(credit_df, "age_new")
```

| age\_new | med\_credit | p\_default |
|:---------|:------------|:-----------|
| 30-39    | 180K        | 20.25%     |
| 45-55    | 130K        | 24.09%     |

``` r
median_by_group(credit_df, c("sex_new", "edu_new"))
```

| sex\_new | edu\_new               | med\_credit | p\_default |
|:---------|:-----------------------|:------------|:-----------|
| Female   | Grad School/University | 150K        | 20.49%     |
| Female   | High School            | 90K         | 23.64%     |
| Male     | Grad School/University | 140K        | 23.77%     |
| Male     | High School            | 70K         | 27.39%     |

``` r
median_by_group(credit_df, c("sex_new", "age_new"))
```

| sex\_new | age\_new | med\_credit | p\_default |
|:---------|:---------|:------------|:-----------|
| Female   | 30-39    | 190K        | 18.37%     |
| Female   | 45-55    | 130K        | 21.83%     |
| Male     | 30-39    | 170K        | 23%        |
| Male     | 45-55    | 130K        | 26.87%     |

``` r
median_by_group(credit_df, c("edu_new", "age_new"))
```

| edu\_new               | age\_new | med\_credit | p\_default |
|:-----------------------|:---------|:------------|:-----------|
| Grad School/University | 30-39    | 190K        | 20.17%     |
| Grad School/University | 45-55    | 170K        | 23.7%      |
| High School            | 30-39    | 120K        | 22.73%     |
| High School            | 45-55    | 80K         | 25.76%     |

``` r
median_by_group(credit_df, c("sex_new", "edu_new", "age_new"))
```

| sex\_new | edu\_new               | age\_new | med\_credit | p\_default |
|:---------|:-----------------------|:---------|:------------|:-----------|
| Female   | Grad School/University | 30-39    | 200K        | 18.5%      |
| Female   | Grad School/University | 45-55    | 170K        | 21.08%     |
| Female   | High School            | 30-39    | 130K        | 19.54%     |
| Female   | High School            | 45-55    | 80K         | 24.21%     |
| Male     | Grad School/University | 30-39    | 180K        | 22.61%     |
| Male     | Grad School/University | 45-55    | 160K        | 26.62%     |
| Male     | High School            | 30-39    | 100K        | 27.26%     |
| Male     | High School            | 45-55    | 70K         | 28.09%     |
