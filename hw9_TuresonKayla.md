hw9_TuresonKayla
================
Kayla Tureson
2022-04-18

-   [Research Question](#research-question)
-   [Variables](#variables)
-   [Import Data](#import-data)
-   [Data obtained from:
    https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/B9I1F8](#data-obtained-from-httpsdataverseharvardedudatasetxhtmlpersistentiddoi107910dvnb9i1f8)
-   [Study title: Effect of aerobic exercise on amyloid accumulation in
    preclinical Alzheimer’s: A 1-year randomized controlled
    trial](#study-title-effect-of-aerobic-exercise-on-amyloid-accumulation-in-preclinical-alzheimers-a-1-year-randomized-controlled-trial)
    -   [Variable Summary](#variable-summary)
-   [Model](#model)
-   [Run Stan](#run-stan)
-   [Results](#results)

``` r
#library(papaja) #package not compatible
library(psych)
library(ggplot2)  # for plots
library(tinytex)
library(here)
library(readxl)  # for reading excel files
library(modelsummary)  # for summarizing data
library(rstan)
library(magrittr)  # for `%>%` operator
rstan_options(auto_write = TRUE)  # save compiled STAN object
options(mc.cores = 2)  # use two cores
library(posterior)
library(bayesplot)
library(dplyr)
library(kableExtra)
theme_set(theme_classic() +
    theme(panel.grid.major.y = element_line(color = "grey92")))
```

## Research Question

-   Is there a difference in amyloid accumulation between individuals in
    the control condition and the exercise intervention condition?

# Variables

-   `amyloid_accum`: accumulation of amyloid from pre (baseline) to post
    (wk52) intervention
-   `apx_group`: AExSupport = aerobic exercise condition, NoSupport =
    control condition

# Import Data

# Data obtained from: <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/B9I1F8>

# Study title: Effect of aerobic exercise on amyloid accumulation in preclinical Alzheimer’s: A 1-year randomized controlled trial

``` r
# load the data
exercise <- read_excel(here("APEx_FinalData_20201021.xlsx"))

# subset only individuals who have baseline and follow up data
tt <- table(exercise$study_id)
exercise2 <- subset(exercise, study_id %in% names(tt[tt == 3]))

# create a new variable (amyloid_accum) for amyloid accumulation from baseline to week 52 (smaller values are better)
exercise3 <- 
exercise2 %>% 
  group_by(study_id)  %>% 
  mutate(amyloid_accum = amyloid_Mean6_CB[timept=="WK52"] -              amyloid_Mean6_CB[timept=="BL"])

# subset only the first entry of each participant 
exercise.subset <- 
  exercise3 %>% 
  group_by(study_id) %>% 
  filter(row_number()==1)

# change the condition variable to a factor
exercise.subset$apx_group <- as.factor(exercise.subset$apx_group)

# remove NAs so that STAN will run later
exercise.subset <- na.omit(exercise.subset)
```

## Variable Summary

``` r
## can't get N to work, not sure why 
datasummary(amyloid_accum * 
                (Mean + SD + Min + Max + Histogram) ~ 
                factor(apx_group),
            data = exercise.subset,
caption = "Summary Statistics") %>%
    # add table note
    add_footnote("SD = standard deviation", notation = "none")
```

|               |           | AExSupport |  NoSupport |
|:--------------|:----------|-----------:|-----------:|
| amyloid_accum | Mean      |       0.01 |       0.00 |
|               | SD        |       0.06 |       0.04 |
|               | Min       |      -0.15 |      -0.09 |
|               | Max       |       0.15 |       0.08 |
|               | Histogram | ▁ ▁▄▇▅▂▃▂▁ | ▁▂▁▂▃▆▇▆▂▂ |

Summary Statistics

**Note:** ^^ SD = standard deviation

# Model

Let *Y* = change in amyloid from baseline to week 52 (week 52 -
baseline; smaller values indicate less accumulation), *G* = treatment
condition Model:
$$
  \\begin{aligned}
    Y\_{i, G = 0} & \\sim N(\\mu_1, \\sigma_1) \\\\
    Y\_{i, G = 1} & \\sim N(\\mu_2, \\sigma_2)
  \\end{aligned}
$$

Prior:
$$
  \\begin{aligned}
    \\mu_1 & \\sim N(3, 2) \\\\
    \\mu_2 & \\sim N(3, 2) \\\\
    \\sigma_1 & \\sim N^+(0, 2) \\\\
    \\sigma_2 & \\sim N^+(0, 2)
  \\end{aligned}
$$

# Run Stan

We used 4 chains, each with 4,000 iterations (the first 2,000 used as
warm-ups).

``` r
# Rescale data so all are greater than 0 and STAN will run (chose .16 based on min values from summary data)
exercise.subset$amyloid_accum_rescale <- exercise.subset$amyloid_accum + .16

# 1. form the data list for Stan
stan_dat <- with(exercise.subset,
    list(N1 = sum(apx_group == "NoSupport"),
         N2 = sum(apx_group == "AExSupport"),
         y1 = amyloid_accum_rescale[which(apx_group == "NoSupport")],
         y2 = amyloid_accum_rescale[which(apx_group == "AExSupport")])
)
# 2. Run Stan
m1 <- stan(
    file = here("normal_2group.stan"),
    data = stan_dat,
    seed = 1234,  # for reproducibility
    iter = 4000
)
```

# Results

As shown in the graph below, the chains mixed well.

``` r
mcmc_rank_hist(m1, pars = c("mu1", "mu2", "sigma1", "sigma2"))
```

![](hw9_TuresonKayla_files/figure-gfm/rank-hist-m1-1.png)<!-- -->

The following table shows the posterior distributions of
*μ*<sub>1</sub>, *μ*<sub>2</sub>, *σ*<sub>1</sub>, *σ*<sub>2</sub>, and
*μ*<sub>2</sub> − *μ*<sub>1</sub>.

``` r
summ_m1 <- as_draws_df(m1) %>%
    subset_draws(variable = c("mu1", "mu2", "sigma1", "sigma2")) %>%
    mutate_variables(`mu2 - mu1` = mu2 - mu1) %>%
    summarise_draws()
knitr::kable(summ_m1, digits = 2)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
mad
</th>
<th style="text-align:right;">
q5
</th>
<th style="text-align:right;">
q95
</th>
<th style="text-align:right;">
rhat
</th>
<th style="text-align:right;">
ess_bulk
</th>
<th style="text-align:right;">
ess_tail
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
mu1
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8834.47
</td>
<td style="text-align:right;">
5696.68
</td>
</tr>
<tr>
<td style="text-align:left;">
mu2
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8343.40
</td>
<td style="text-align:right;">
5863.17
</td>
</tr>
<tr>
<td style="text-align:left;">
sigma1
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8137.71
</td>
<td style="text-align:right;">
5899.56
</td>
</tr>
<tr>
<td style="text-align:left;">
sigma2
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8486.36
</td>
<td style="text-align:right;">
6206.78
</td>
</tr>
<tr>
<td style="text-align:left;">
mu2 - mu1
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
8627.39
</td>
<td style="text-align:right;">
6308.51
</td>
</tr>
</tbody>
</table>

-   Based on our analysis, the results show there was no difference in
    amyloid accumulation from baseline to week 52 between the two groups
    (i.e., exercise intervention group and education control condition).
    The posterior mean in the exercise intervention group was 0.17,
    whereas the posterior mean in the education control group was 0.16.
    Additionally, the 90% CIs were the same in both the exercise
    intervention group and the education control group. Of note, data
    were transformed prior to running our model, and therefore the
    actual values are 0.16 less than those provided in the table above.
