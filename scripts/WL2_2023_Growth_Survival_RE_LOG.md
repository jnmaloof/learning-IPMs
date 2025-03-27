---
title: "WL2023_Size_Growth"
author: "Julin Maloof"
date: "2025-01-24"
output: 
  html_document: 
    keep_md: true
---



Intitial attempt at making a growth/survival matrix from WL2 2023, now using random effects


``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.0.4     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.3.0 ──
## ✔ broom        1.0.7     ✔ rsample      1.2.1
## ✔ dials        1.4.0     ✔ tune         1.3.0
## ✔ infer        1.0.7     ✔ workflows    1.2.0
## ✔ modeldata    1.4.0     ✔ workflowsets 1.1.0
## ✔ parsnip      1.3.0     ✔ yardstick    1.3.2
## ✔ recipes      1.1.1     
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ scales::discard() masks purrr::discard()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ recipes::fixed()  masks stringr::fixed()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ yardstick::spec() masks readr::spec()
## ✖ recipes::step()   masks stats::step()
```

``` r
library(multilevelmod)
library(lmerTest)
```

```
## Loading required package: lme4
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
## 
## 
## Attaching package: 'lmerTest'
## 
## The following object is masked from 'package:lme4':
## 
##     lmer
## 
## The following object is masked from 'package:recipes':
## 
##     step
## 
## The following object is masked from 'package:stats':
## 
##     step
```

``` r
library(broom.mixed)
library(doMC)
```

```
## Loading required package: foreach
## 
## Attaching package: 'foreach'
## 
## The following objects are masked from 'package:purrr':
## 
##     accumulate, when
## 
## Loading required package: iterators
## Loading required package: parallel
```

``` r
library(furrr)
```

```
## Loading required package: future
```

``` r
library(modelr)
```

```
## 
## Attaching package: 'modelr'
## 
## The following objects are masked from 'package:yardstick':
## 
##     mae, mape, rmse
## 
## The following object is masked from 'package:broom':
## 
##     bootstrap
```

``` r
registerDoMC(cores = 7)
conflicted::conflict_prefer("select", "dplyr")
```

```
## [conflicted] Will prefer dplyr::select over any other package.
```

``` r
conflicted::conflict_prefer("filter", "dplyr")
```

```
## [conflicted] Will prefer dplyr::filter over any other package.
```

``` r
conflicted::conflict_prefer("lag", "dplyr")
```

```
## [conflicted] Will prefer dplyr::lag over any other package.
```

## Import the data


``` r
growth <- read_csv("../input/WL2-2023_Size_Combined.csv")
```

```
## Rows: 17336 Columns: 10
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (5): block, Genotype, pop.mf, parent.pop, survey.notes
## dbl  (4): mf, rep, height.cm, long.leaf.cm
## date (1): survey_date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
growth
```

```
## # A tibble: 17,336 × 10
##    survey_date block Genotype pop.mf parent.pop    mf   rep height.cm
##    <date>      <chr> <chr>    <chr>  <chr>      <dbl> <dbl>     <dbl>
##  1 2023-07-03  <NA>  CP2_1_1  CP2_1  CP2            1     1       0.5
##  2 2023-07-03  <NA>  CP2_1_2  CP2_1  CP2            1     2       0.7
##  3 2023-07-03  <NA>  CP2_1_3  CP2_1  CP2            1     3       1.1
##  4 2023-07-03  <NA>  CP2_1_4  CP2_1  CP2            1     4       0.8
##  5 2023-07-03  <NA>  CP2_1_5  CP2_1  CP2            1     5       0.9
##  6 2023-07-03  <NA>  CP2_1_6  CP2_1  CP2            1     6       1  
##  7 2023-07-03  <NA>  CP2_1_7  CP2_1  CP2            1     7       1.5
##  8 2023-07-03  <NA>  CP2_1_8  CP2_1  CP2            1     8       1.1
##  9 2023-07-03  <NA>  CP2_1_9  CP2_1  CP2            1     9       0.5
## 10 2023-07-03  <NA>  CP2_1_10 CP2_1  CP2            1    10       0.7
## # ℹ 17,326 more rows
## # ℹ 2 more variables: long.leaf.cm <dbl>, survey.notes <chr>
```


``` r
survival <- read_csv("../input/WL2_Mortality.csv")
```

```
## Rows: 1575 Columns: 12
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (9): block, BedLoc, bed, bed.col, Genotype, pop.mf, parent.pop, death.da...
## dbl (3): bed.row, mf, rep
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
survival
```

```
## # A tibble: 1,575 × 12
##    block BedLoc bed   bed.row bed.col Genotype   pop.mf  parent.pop    mf   rep
##    <chr> <chr>  <chr>   <dbl> <chr>   <chr>      <chr>   <chr>      <dbl> <dbl>
##  1 A     A_1_A  A           1 A       TM2_6_11   TM2_6   TM2            6    11
##  2 A     A_1_B  A           1 B       LVTR1_7_1  LVTR1_7 LVTR1          7     1
##  3 A     A_2_A  A           2 A       SQ2_6_14   SQ2_6   SQ2            6    14
##  4 A     A_2_B  A           2 B       YO8_8_3    YO8_8   YO8            8     3
##  5 A     A_3_A  A           3 A       CC_2_3     CC_2    CC             2     3
##  6 A     A_3_B  A           3 B       YO11_5_14  YO11_5  YO11           5    14
##  7 A     A_4_A  A           4 A       BH_6_3     BH_6    BH             6     3
##  8 A     A_4_B  A           4 B       DPR_4_8    DPR_4   DPR            4     8
##  9 A     A_5_A  A           5 A       CP2_5_1    CP2_5   CP2            5     1
## 10 A     A_5_B  A           5 B       LVTR1_3_12 LVTR1_3 LVTR1          3    12
## # ℹ 1,565 more rows
## # ℹ 2 more variables: death.date <chr>, survey.notes <chr>
```

## Wrangle Growth

We want to predict size next from size.  In the Merow examples there was a column "size" and "size next" for each time step.  I guess we are going to need to do that here.

We also should compute the interval length 


``` r
growth2 <- growth %>% arrange(Genotype, survey_date) %>%
  select(-survey.notes, -long.leaf.cm) %>%
  group_by(Genotype) %>%
  mutate(l.height.cm = log(height.cm),
         l.height.cm.next = lead(l.height.cm),
         elapsed_days= lead(survey_date) - survey_date,
         elapsed_weeks = as.integer(elapsed_days)/7) %>%
  ungroup() %>%
  mutate(week = as.numeric(survey_date - ymd("2023-07-03")) / 7, # 7/03 = pre-transplant survey date
         week_char = as.character(week)) %>% 
  drop_na(block, l.height.cm, l.height.cm.next)

growth2 
```

```
## # A tibble: 5,894 × 14
##    survey_date block Genotype pop.mf parent.pop    mf   rep height.cm
##    <date>      <chr> <chr>    <chr>  <chr>      <dbl> <dbl>     <dbl>
##  1 2023-07-26  L     BH_1_1   BH_1   BH             1     1       4.7
##  2 2023-08-02  L     BH_1_1   BH_1   BH             1     1       4.8
##  3 2023-08-16  L     BH_1_1   BH_1   BH             1     1       4.9
##  4 2023-08-23  L     BH_1_1   BH_1   BH             1     1       5.7
##  5 2023-08-30  L     BH_1_1   BH_1   BH             1     1       4.9
##  6 2023-09-06  L     BH_1_1   BH_1   BH             1     1       3.6
##  7 2023-09-13  L     BH_1_1   BH_1   BH             1     1       4.4
##  8 2023-09-20  L     BH_1_1   BH_1   BH             1     1       5.1
##  9 2023-10-13  L     BH_1_1   BH_1   BH             1     1       5.6
## 10 2023-07-26  H     BH_1_10  BH_1   BH             1    10       3.1
## # ℹ 5,884 more rows
## # ℹ 6 more variables: l.height.cm <dbl>, l.height.cm.next <dbl>,
## #   elapsed_days <drtn>, elapsed_weeks <dbl>, week <dbl>, week_char <chr>
```

For easy visualization lets subset just to elapsed_days of 7


``` r
growth2 %>%
  filter(elapsed_days==7) %>%
  ggplot(aes(x=l.height.cm, y=l.height.cm.next)) +
  geom_point(aes(color=parent.pop), alpha = 0.3) +
  geom_abline() +
  geom_smooth(method="lm")
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Do some model comparison

Define the formula



``` r
minSize <- min(growth2$l.height.cm, na.rm = TRUE)
maxSize <- max(growth2$l.height.cm, na.rm = TRUE)
# newdata = tibble(l.height.cm=seq(minSize, maxSize, length.out=101), elapsed_weeks = 1)
```


In order to be able to use the tidymodel tools on mixed effect models, it works best to add variables and formulas in the following way.  This is a bit cumbersome, but it will help later.

elapsed weeks is probably important, it is the time between survey dates, so if it has been 2 weeks instead of 1, we expect more growth

``` r
lmer.spec <- linear_reg() %>%
  set_engine("lmer")

growth.rec <- growth2 %>%
  select(l.height.cm.next, l.height.cm, elapsed_weeks, parent.pop, mf, block) %>%
  recipe() %>%
  update_role(l.height.cm.next, new_role = "outcome") %>% 
  update_role(c(l.height.cm, elapsed_weeks, parent.pop, mf, block), new_role = "predictor")

growth.rec.poly <- growth.rec %>% 
  step_poly(l.height.cm, degree = 3, keep_original_cols = TRUE)  %>%
  step_rename_at(contains("poly"), fn = \(x) str_replace(x, "_poly_", ".p"))

growth.wflow <- workflow() %>%
  add_recipe(growth.rec)

growth.wflow.poly <- workflow() %>% add_recipe(growth.rec.poly)

growth.models <- tibble(wflow=list(
  
### Models with only Random Effects
  m1_block = {growth.wflow %>%
      add_model(lmer.spec,
                formula = l.height.cm.next ~ (1|block)) },
  
  m2_parent = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ (1|parent.pop) )},
  
  m3_block.parent = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ (1|parent.pop) + (1|block) )},
  
  m4_block.parent.mf = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ (1|parent.pop/mf) + (1|block) )},
  
### Models with a linear height component.  Compare also having random effects slope for height w.r.t. parent.pop and block.
  
  m5_linear.parent.mf.block = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm + (1|parent.pop/mf) + (1|block) )},
  
  m6_linear.parent.mf.block.slope = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm + (l.height.cm|parent.pop/mf) + (1|block) )},
  
  m7_linear.parent.mf.block.slope.slope = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm + (l.height.cm|parent.pop/mf) + (l.height.cm|block) )},
  
  # adding this after looking at m7...intercept and slope for mf have a correlation of -1.00.  Also, very little variation attributable to mf, so remove it all the way.
  m7s_linear.parent.block.slope.slope = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm + (l.height.cm|parent.pop) + (l.height.cm|block) )},

### Models with a linear height component and elapsed weeks.
  
  m8_linear_with_weeks_int.parent = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm*elapsed_weeks + (1|parent.pop/mf) + (1|block) )},
  
  m9_linear_with_weeks_int.parent.all.slope = { growth.wflow %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm*elapsed_weeks + (l.height.cm|parent.pop/mf) + (l.height.cm|block) )},

### Models with quadratic and cubic terms for height
  
  m10_quadratic.parent.mf.block = { growth.wflow.poly %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm.p1 + l.height.cm.p2 + (1|parent.pop/mf) + (1|block) )},
  
  m11_cubic.parent.mf.block = { growth.wflow.poly %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop/mf) + (1|block) )},

  # also try m11 without mf because m11 fits well but has no variance associated with mf.

  m11s_cubic.parent.block = { growth.wflow.poly %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop) + (1|block) )},

  m12_quadratic_weeks.parent.height.slope = { growth.wflow.poly %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm.p1*elapsed_weeks*parent.pop + l.height.cm.p2 + (l.height.cm.p1 + l.height.cm.p2 | parent.pop / mf) + (l.height.cm.p1 + l.height.cm.p2 | block) )}, 
  
  m13_cubic_weeks.parent.height.slope = { growth.wflow.poly %>% 
      add_model(lmer.spec,
                formula = l.height.cm.next ~ l.height.cm.p1*elapsed_weeks*parent.pop + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1 + l.height.cm.p2 | parent.pop) + (l.height.cm.p1 + l.height.cm.p2 | block) )}
),
name = names(wflow)
)
```

Fit the models


``` r
system.time( { # 190 seconds
  growth.models <- growth.models %>%
    mutate(fit = map(wflow, fit, data = growth2),
           glance = map(fit, glance)
    ) 
})
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

```
## Warning: There were 5 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `fit = map(wflow, fit, data = growth2)`.
## Caused by warning in `checkConv()`:
## ! Model failed to converge with max|grad| = 0.00239239 (tol = 0.002, component 1)
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.
```

```
##    user  system elapsed 
## 142.767   1.744 151.702
```

``` r
growth.models %>% select(-wflow, -fit) %>% unnest(glance) %>% arrange(BIC)
```

```
## # A tibble: 15 × 8
##    name                     nobs sigma logLik    AIC    BIC REMLcrit df.residual
##    <chr>                   <int> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>       <int>
##  1 m9_linear_with_weeks_i…  5894 0.342 -2153.  4334.  4427.    4306.        5880
##  2 m11s_cubic.parent.block  5894 0.349 -2199.  4412.  4459.    4398.        5887
##  3 m11_cubic.parent.mf.bl…  5894 0.349 -2199.  4414.  4467.    4398.        5886
##  4 m7s_linear.parent.bloc…  5894 0.347 -2202.  4421.  4482.    4403.        5885
##  5 m10_quadratic.parent.m…  5894 0.349 -2210.  4435.  4482.    4421.        5887
##  6 m7_linear.parent.mf.bl…  5894 0.345 -2197.  4418.  4498.    4394.        5882
##  7 m6_linear.parent.mf.bl…  5894 0.347 -2216.  4453.  4520.    4433.        5884
##  8 m8_linear_with_weeks_i…  5894 0.349 -2227.  4471.  4524.    4455.        5886
##  9 m5_linear.parent.mf.bl…  5894 0.353 -2278.  4568.  4608.    4556.        5888
## 10 m13_cubic_weeks.parent…  5894 0.332 -1884.  3975.  4663.    3769.        5791
## 11 m12_quadratic_weeks.pa…  5894 0.330 -1887.  3989.  4711.    3773.        5786
## 12 m4_block.parent.mf       5894 0.466 -4073.  8155.  8189.    8145.        5889
## 13 m3_block.parent          5894 0.496 -4319.  8647.  8674.    8639.        5890
## 14 m2_parent                5894 0.507 -4428.  8862.  8883.    8856.        5891
## 15 m1_block                 5894 0.813 -7156. 14317. 14337.   14311.        5891
```

Concentrate on the five best models.

``` r
growth.models.best <- growth.models %>%
  unnest(glance) %>%
  slice_min(BIC, n=6)

growth.models.best$fit
```

```
## $m9_linear_with_weeks_int.parent.all.slope
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 0 Recipe Steps
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear mixed model fit by REML ['lmerMod']
## Formula: l.height.cm.next ~ l.height.cm * elapsed_weeks + (l.height.cm |  
##     parent.pop/mf) + (l.height.cm | block)
##    Data: data
## REML criterion at convergence: 4305.735
## Random effects:
##  Groups        Name        Std.Dev. Corr 
##  mf:parent.pop (Intercept) 0.06274       
##                l.height.cm 0.03697  -1.00
##  parent.pop    (Intercept) 0.14093       
##                l.height.cm 0.12372  -0.35
##  block         (Intercept) 0.09624       
##                l.height.cm 0.05385  -0.98
##  Residual                  0.34249       
## Number of obs: 5894, groups:  mf:parent.pop, 140; parent.pop, 22; block, 13
## Fixed Effects:
##               (Intercept)                l.height.cm  
##                   0.38652                    0.70305  
##             elapsed_weeks  l.height.cm:elapsed_weeks  
##                  -0.13300                    0.05373  
## optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings 
## 
## $m11s_cubic.parent.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 2 Recipe Steps
## 
## • step_poly()
## • step_rename_at()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear mixed model fit by REML ['lmerMod']
## Formula: l.height.cm.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 +  
##     (1 | parent.pop) + (1 | block)
##    Data: data
## REML criterion at convergence: 4397.882
## Random effects:
##  Groups     Name        Std.Dev.
##  parent.pop (Intercept) 0.13718 
##  block      (Intercept) 0.02269 
##  Residual               0.34868 
## Number of obs: 5894, groups:  parent.pop, 22; block, 13
## Fixed Effects:
##    (Intercept)  l.height.cm.p1  l.height.cm.p2  l.height.cm.p3  
##          1.311          47.958           4.648          -1.915  
## 
## $m11_cubic.parent.mf.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 2 Recipe Steps
## 
## • step_poly()
## • step_rename_at()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear mixed model fit by REML ['lmerMod']
## Formula: l.height.cm.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 +  
##     (1 | parent.pop/mf) + (1 | block)
##    Data: data
## REML criterion at convergence: 4397.882
## Random effects:
##  Groups        Name        Std.Dev.
##  mf:parent.pop (Intercept) 0.00000 
##  parent.pop    (Intercept) 0.13718 
##  block         (Intercept) 0.02269 
##  Residual                  0.34868 
## Number of obs: 5894, groups:  mf:parent.pop, 140; parent.pop, 22; block, 13
## Fixed Effects:
##    (Intercept)  l.height.cm.p1  l.height.cm.p2  l.height.cm.p3  
##          1.311          47.958           4.648          -1.915  
## optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings 
## 
## $m7s_linear.parent.block.slope.slope
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 0 Recipe Steps
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear mixed model fit by REML ['lmerMod']
## Formula: l.height.cm.next ~ l.height.cm + (l.height.cm | parent.pop) +  
##     (l.height.cm | block)
##    Data: data
## REML criterion at convergence: 4403.413
## Random effects:
##  Groups     Name        Std.Dev. Corr 
##  parent.pop (Intercept) 0.13978       
##             l.height.cm 0.12450  -0.33
##  block      (Intercept) 0.09119       
##             l.height.cm 0.05116  -0.98
##  Residual               0.34670       
## Number of obs: 5894, groups:  parent.pop, 22; block, 13
## Fixed Effects:
## (Intercept)  l.height.cm  
##      0.2188       0.7689  
## optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings 
## 
## $m10_quadratic.parent.mf.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 2 Recipe Steps
## 
## • step_poly()
## • step_rename_at()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## l.height.cm.next ~ l.height.cm.p1 + l.height.cm.p2 + (1 | parent.pop/mf) +  
##     (1 | block)
##    Data: data
## REML criterion at convergence: 4420.878
## Random effects:
##  Groups        Name        Std.Dev.
##  mf:parent.pop (Intercept) 0.00000 
##  parent.pop    (Intercept) 0.14239 
##  block         (Intercept) 0.02287 
##  Residual                  0.34929 
## Number of obs: 5894, groups:  mf:parent.pop, 140; parent.pop, 22; block, 13
## Fixed Effects:
##    (Intercept)  l.height.cm.p1  l.height.cm.p2  
##          1.308          47.711           4.955  
## optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings 
## 
## $m7_linear.parent.mf.block.slope.slope
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 0 Recipe Steps
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear mixed model fit by REML ['lmerMod']
## Formula: l.height.cm.next ~ l.height.cm + (l.height.cm | parent.pop/mf) +  
##     (l.height.cm | block)
##    Data: data
## REML criterion at convergence: 4394.194
## Random effects:
##  Groups        Name        Std.Dev. Corr 
##  mf:parent.pop (Intercept) 0.06828       
##                l.height.cm 0.03980  -1.00
##  parent.pop    (Intercept) 0.14091       
##                l.height.cm 0.13082  -0.36
##  block         (Intercept) 0.09387       
##                l.height.cm 0.05275  -0.98
##  Residual                  0.34535       
## Number of obs: 5894, groups:  mf:parent.pop, 140; parent.pop, 22; block, 13
## Fixed Effects:
## (Intercept)  l.height.cm  
##      0.2248       0.7630  
## optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings
```


Do some cross-validation to better assess predictive power

``` r
set.seed(1001)
growth_folds <- vfold_cv(growth2, v = 10)

growth.models.best <- growth.models.best %>%
  mutate(resamples = map(wflow, fit_resamples, resamples = growth_folds, control = control_resamples(save_pred = TRUE), .progress = TRUE))
```

```
## ■■■■■■ 17% | ETA: 15s ■■■■■■■■■■■ 33% | ETA: 7s ■■■■■■■■■■■■■■■■ 50% | ETA: 4s
## ■■■■■■■■■■■■■■■■■■■■■ 67% | ETA: 3s ■■■■■■■■■■■■■■■■■■■■■■■■■■ 83% | ETA: 1s
```

```
## Warning: There were 6 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `resamples = map(...)`.
## Caused by warning:
## ! ! tune detected a parallel backend registered with foreach but no backend
##   registered with future.
## ℹ Support for parallel processing with foreach was soft-deprecated in tune
##   1.2.1.
## ℹ See ?parallelism (`?tune::parallelism()`) to learn more.
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.
```


``` r
growth.models.best %>% 
  mutate(metrics = map(resamples, collect_metrics, type = "wide")) %>%
  select(name, metrics) %>%
  unnest(metrics) %>%
  select(-.config) %>%
  arrange(rmse)
```

```
## # A tibble: 6 × 3
##   name                                       rmse   rsq
##   <chr>                                     <dbl> <dbl>
## 1 m11_cubic.parent.mf.block                 0.379 0.806
## 2 m11s_cubic.parent.block                   0.379 0.806
## 3 m10_quadratic.parent.mf.block             0.382 0.804
## 4 m9_linear_with_weeks_int.parent.all.slope 0.404 0.799
## 5 m7s_linear.parent.block.slope.slope       0.408 0.795
## 6 m7_linear.parent.mf.block.slope.slope     0.410 0.795
```
Interesting.  The rsq is very similar for all models, but m11 clearly wins on rmse.


``` r
growth.models.best %>% 
  mutate(metrics = map(resamples, collect_metrics, type = "long")) %>%
  select(name, metrics) %>%
  unnest(metrics) %>%
  arrange(.metric, mean) %>%
  select(name, .metric, mean, std_err)
```

```
## # A tibble: 12 × 4
##    name                                      .metric  mean std_err
##    <chr>                                     <chr>   <dbl>   <dbl>
##  1 m11_cubic.parent.mf.block                 rmse    0.379 0.00762
##  2 m11s_cubic.parent.block                   rmse    0.379 0.00762
##  3 m10_quadratic.parent.mf.block             rmse    0.382 0.00742
##  4 m9_linear_with_weeks_int.parent.all.slope rmse    0.404 0.00635
##  5 m7s_linear.parent.block.slope.slope       rmse    0.408 0.00641
##  6 m7_linear.parent.mf.block.slope.slope     rmse    0.410 0.00640
##  7 m7s_linear.parent.block.slope.slope       rsq     0.795 0.00758
##  8 m7_linear.parent.mf.block.slope.slope     rsq     0.795 0.00758
##  9 m9_linear_with_weeks_int.parent.all.slope rsq     0.799 0.00722
## 10 m10_quadratic.parent.mf.block             rsq     0.804 0.00667
## 11 m11s_cubic.parent.block                   rsq     0.806 0.00676
## 12 m11_cubic.parent.mf.block                 rsq     0.806 0.00676
```


``` r
growth.models.best %>% 
  mutate(metrics = map(resamples, collect_metrics, type = "long")) %>%
  select(name, metrics) %>%
  unnest(metrics) %>%
  arrange(.metric, mean) %>%
  select(name, .metric, mean, std_err) %>%
  ggplot(aes(x=name, y = mean, ymin=mean-std_err, ymax=mean+std_err, fill = name)) +
  geom_col() +
  geom_errorbar(width = 0.5) +
  facet_wrap(~.metric, scales = "free_y") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  ggtitle("10-fold cross validation") +
  scale_fill_viridis_d()
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

So, we go with simplified m11 (or possibly m12 if we decide we need weeks).

``` r
growth.model.final <- growth.models.best %>% filter(name == "m11s_cubic.parent.block") %>% pull(fit) %>% magrittr::extract2(1)

growth.model.final
```

```
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 2 Recipe Steps
## 
## • step_poly()
## • step_rename_at()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear mixed model fit by REML ['lmerMod']
## Formula: l.height.cm.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 +  
##     (1 | parent.pop) + (1 | block)
##    Data: data
## REML criterion at convergence: 4397.882
## Random effects:
##  Groups     Name        Std.Dev.
##  parent.pop (Intercept) 0.13718 
##  block      (Intercept) 0.02269 
##  Residual               0.34868 
## Number of obs: 5894, groups:  parent.pop, 22; block, 13
## Fixed Effects:
##    (Intercept)  l.height.cm.p1  l.height.cm.p2  l.height.cm.p3  
##          1.311          47.958           4.648          -1.915
```

For plotting the prediction curve, it looks like I can't combine `geom_function` and `facet_wrap` so I will compute the prediction curves myself and then combine in the plots


``` r
newdata.growth <- growth2 %>%
  group_by(parent.pop) %>%
  summarize(minSize = min(l.height.cm, na.rm = TRUE),
            maxSize = max(l.height.cm, na.rm = TRUE)) %>%
  
  # generate a "newdata" data frame for each parent.pop
  mutate(newdata = pmap(list(parent.pop, minSize, maxSize), \(p, x, y) tibble(parent.pop = p,
                                                                              elapsed_weeks = 1,
                                                                              l.height.cm = seq(x, y, length.out = 101)))) %>%
  select(newdata) %>%
  unnest(newdata)

newdata.growth <- growth.rec.poly %>% # These steps are necessary to add the polynomials to the new data
  update_role(mf, block, new_role = "unused") %>%
  update_role_requirements(role="unused", bake = FALSE) %>%
  prep() %>% bake(newdata.growth)

growth.predictions <- extract_fit_engine(growth.model.final) %>%
  predict(newdata.growth, re.form = ~ (1 | parent.pop)) %>%
  cbind(newdata.growth, l.height.cm.next=.)
```

Plot it.  Points are actual data (height.next vs height).  The blue line is the model prediction, the red line is slope = 1.

``` r
growth2 %>%
  ggplot(aes(x=l.height.cm, y = l.height.cm.next)) +
  geom_point(alpha=.25) +
  geom_line(data = growth.predictions, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", lty=2 ) +
  #geom_smooth(color = "red") +
  facet_wrap(~parent.pop, scales = "free")
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

## On to survival

Since we want to predict survival as a function of size, we need to combine the data sets


``` r
survival2 <- survival %>% select(Genotype, death.date) %>%
  right_join(growth) %>%
  mutate(l.height.cm = log(height.cm)) %>%
  arrange(Genotype, survey_date) %>%
  select(-survey.notes, -long.leaf.cm) %>%
  group_by(Genotype) %>%
  mutate(elapsed_days= lead(survey_date) - survey_date,
         elapsed_weeks = as.integer(elapsed_days)/7,
         block = ifelse(is.na(block), unique(na.omit(block)), block), # fill in missing block info 
         death.date = lubridate::mdy(death.date),
         surv.next = ifelse(is.na(death.date), 1, death.date > lead(survey_date))) %>% 
  ungroup() %>%
  mutate(week = as.numeric(survey_date - ymd("2023-07-03")) / 7, # 7/03 = pre-transplant survey date
         week_char = as.character(round(week, 2)),
         ppmf = str_c(parent.pop, "_", mf)) %>% # allows me to specify RE for pp and mf separately.
  drop_na(surv.next, parent.pop, l.height.cm, elapsed_days)
```

```
## Joining with `by = join_by(Genotype)`
```

```
## Warning in right_join(., growth): Detected an unexpected many-to-many relationship between `x` and `y`.
## ℹ Row 1 of `x` matches multiple rows in `y`.
## ℹ Row 1362 of `y` matches multiple rows in `x`.
## ℹ If a many-to-many relationship is expected, set `relationship =
##   "many-to-many"` to silence this warning.
```

``` r
# check it
survival2 %>% select(Genotype, death.date, survey_date, surv.next, l.height.cm)
```

```
## # A tibble: 8,070 × 5
##    Genotype death.date survey_date surv.next l.height.cm
##    <chr>    <date>     <date>          <dbl>       <dbl>
##  1 BH_1_1   NA         2023-07-03          1       0.693
##  2 BH_1_1   NA         2023-07-26          1       1.55 
##  3 BH_1_1   NA         2023-08-02          1       1.57 
##  4 BH_1_1   NA         2023-08-16          1       1.59 
##  5 BH_1_1   NA         2023-08-23          1       1.74 
##  6 BH_1_1   NA         2023-08-30          1       1.59 
##  7 BH_1_1   NA         2023-09-06          1       1.28 
##  8 BH_1_1   NA         2023-09-13          1       1.48 
##  9 BH_1_1   NA         2023-09-20          1       1.63 
## 10 BH_1_1   NA         2023-10-13          1       1.72 
## # ℹ 8,060 more rows
```
I SHOULD TRY DIFFERENT OPTIMIZERS TO SEE IF I CAN GET MORE OF THESE TO CONVERGE

``` r
surv.spec <- linear_reg() %>%
  set_engine("glmer", family = "binomial")

surv.rec <- survival2 %>%
  select(surv.next, l.height.cm, elapsed_weeks, week, parent.pop, mf, ppmf, block) %>%
  recipe() %>%
  update_role(surv.next, new_role = "outcome") %>% 
  update_role(c(l.height.cm, elapsed_weeks, week, parent.pop, mf, ppmf, block), new_role = "predictor")

surv.rec.poly <- surv.rec %>% 
  step_poly(l.height.cm, degree = 3, keep_original_cols = TRUE)  %>%
  step_poly(week, degree = 3, keep_original_cols = TRUE) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_rename_at(contains("poly"), fn = \(x) str_replace(x, "_poly_", ".p"))

surv.wflow <- workflow() %>%
  add_recipe(surv.rec)

surv.wflow.poly <- workflow() %>% add_recipe(surv.rec.poly)

surv.models <- tibble(wflow=list(
  m1_RE = {surv.wflow %>%
      add_model(surv.spec,
                formula = surv.next ~ (1|parent.pop/mf) + (1|block)) },
  
  m1b_RE2 = {surv.wflow %>% # reality check; this should be the same as M1
      add_model(surv.spec,
                formula = surv.next ~ (1|parent.pop) + (1|ppmf) + (1|block)) },
  
  m2_linear = {surv.wflow %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm + (1|parent.pop/mf) + (1|block)) },
  
  m2_linear.week_cont = {surv.wflow %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm + week + (1|parent.pop/mf) + (1|block)) },
  
  m2e_linear.elap_weeks = {surv.wflow %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm + elapsed_weeks + (1|parent.pop/mf) + (1|block)) },
  
  m2wne_linear.elap_weeks.week_cont = {surv.wflow %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm + week + elapsed_weeks + (1|parent.pop/mf) + (1|block)) },
  
  m3_linear.slope.parent.pop = {surv.wflow %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm + (l.height.cm|parent.pop) + (1|ppmf) + (1|block)) },
  
  m5_linear.slope.parent.pop.mf = {surv.wflow %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm + (l.height.cm|parent.pop/mf) + (1|block)) },
  
  m6_linear.slope.parent.pop.mf.block = {surv.wflow %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm + (l.height.cm|parent.pop/mf) + (l.height.cm|block)) },
  
  m7_quad = {surv.wflow.poly %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm + l.height.cm.p2 + (1|parent.pop/mf) + (1|block)) },
  
  m8_quad.slope.parent.pop = {surv.wflow.poly %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm.p1 + l.height.cm.p2 + (l.height.cm.p1 + l.height.cm.p2|parent.pop) + (1|ppmf) + (1|block)) },
  
  m11_cubic = {surv.wflow.poly %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop) + (1|block)) },

  m11e_cubic.elap_weeks = {surv.wflow.poly %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop) + (1|block)) },

  m11wne_cubic.date.elap_weeks.weeks_cont = {surv.wflow.poly %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + week + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop) + (1|block)) },

  m11wnec_cubic.date.elap_weeks.cubic.weeks_cont = {surv.wflow.poly %>%
      add_model(surv.spec,
                formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + week.p1 + week.p2 + week.p3 + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop) + (1|block)) }
  
),
name = names(wflow)
)

  
  #Too Slow when including date as a slope for RE, so remove that
  # Still does not converge
  # m6d_linear.date.slope.parent.pop.mf.block = {surv.wflow %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm + week_char + (l.height.cm|parent.pop/mf) + (l.height.cm|block)) },
  # 
  # m6e_linear.weeks.slope.parent.pop.mf.block = {surv.wflow %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm + elapsed_weeks + (elapsed_weeks + l.height.cm|parent.pop/mf) + (l.height.cm|block)) },
  
  # Does not converge
  #   m7_quad = {surv.wflow %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm + l.height.cm.p2 + (1|parent.pop/mf) + (1|block)) },
  # 
  # m7d_quad.date = {surv.wflow %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm + week_char + l.height.cm.p2 + (1|parent.pop/mf) + (1|block)) },
  # 
  # m7e_quad.weeks = {surv.wflow %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm + elapsed_weeks + l.height.cm.p2 + (1|parent.pop/mf) + (1|block)) },
  
 
  # Fails to converge
  # m9_quad.slope.parent.pop.mf = {surv.wflow %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm + l.height.cm.p2 + (l.height.cm + l.height.cm.p2|parent.pop/mf) + (1|block)) },
  

  
  # Keep date out of RE so it will fit
  # Still fails to converge
  # m10d_quad.date.slope.parent.pop.mf.blck = {surv.wflow %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm + week_char + l.height.cm.p2 + (l.height.cm + l.height.cm.p2|parent.pop/mf) + (l.height.cm + l.height.cm.p2|block)) },
  # 
  # m10e_quad.weeks.slope.parent.pop.mf.blck = {surv.wflow %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm + elapsed_weeks + l.height.cm.p2 + (l.height.cm + l.height.cm.p2 + elapsed_weeks|parent.pop/mf) + (l.height.cm + l.height.cm.p2 + elapsed_weeks|block)) },
  # 

 
  # m10_quad.slope.parent.pop.mf.blck = {surv.wflow.poly %>%
  #    add_model(surv.spec,
  #              formula = surv.next ~ l.height.cm.p1 + l.height.cm.p2 + (l.height.cm.p1 + l.height.cm.p2|parent.pop/mf) + (l.height.cm.p1 + l.height.cm.p2|block)) },
  
  # m11_cubic = {surv.wflow.poly %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop/mf) + (1|block)) },
  # 
  # m11e_cubic.elap_weeks = {surv.wflow.poly %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop/mf) + (1|block)) },
  # 
  # m11wne_cubic.date.elap_weeks.weeks_cont = {surv.wflow.poly %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + week + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop/mf) + (1|block)) },
  # 
  # m11wnec_cubic.date.elap_weeks.cubic.weeks_cont = {surv.wflow.poly %>%
  #     add_model(surv.spec,
  #               formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + week.p1 + week.p2 + week.p3 + l.height.cm.p2 + l.height.cm.p3 + (1|parent.pop/mf) + (1|block)) },
# 
#   m12_cubic.slope.parent.pop = {surv.wflow.poly %>%
#       add_model(surv.spec,
#                 formula = surv.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1|parent.pop) + (1|ppmf) + (1|block)) },
#   
#   m12wn_cubic.week_cont.slope.parent.pop = {surv.wflow.poly %>%
#       add_model(surv.spec,
#                 formula = surv.next ~ l.height.cm.p1 + week + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1|parent.pop) + (1|ppmf) + (1|block)) },
#   
#   m12e_cubic.elap_weeks.slope.parent.pop = {surv.wflow.poly %>%
#       add_model(surv.spec,
#                 formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1|parent.pop) + (1|ppmf) + (1|block)) },
#   
#   m12wne_cubic.date.elap_weeks.weeks_cont.slope.parent.pop = {surv.wflow.poly %>%
#       add_model(surv.spec,
#                 formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + week + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1|parent.pop) + (1|ppmf) + (1|block)) },
#   
#   m13_cubic.slope.parent.pop.blk = {surv.wflow.poly %>%
#       add_model(surv.spec,
#                 formula = surv.next ~ l.height.cm.p1 + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1|parent.pop) + (1|ppmf) + (l.height.cm|block)) },
#   
#   m13wn_cubic.week_cont.slope.parent.pop.blk = {surv.wflow.poly %>%
#       add_model(surv.spec,
#                 formula = surv.next ~ l.height.cm.p1 + week + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1|parent.pop) + (1|ppmf) + (l.height.cm|block)) },
#   
#   m13e_cubic.elap_weeks.slope.parent.pop.blk = {surv.wflow.poly %>%
#       add_model(surv.spec,
#                 formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1|parent.pop) + (1|ppmf) + (l.height.cm.p1|block)) },
#   
#   m13wne_cubic.date.elap_weeks.weeks_cont.slope.parent.pop.blk = {surv.wflow.poly %>%
#       add_model(surv.spec,
#                 formula = surv.next ~ l.height.cm.p1 + elapsed_weeks + week + l.height.cm.p2 + l.height.cm.p3 + (l.height.cm.p1|parent.pop) + (1|ppmf) + (l.height.cm.p1|block)) }
```

Fit the models


``` r
plan(multisession, workers = 7)

system.time( { # 110 seconds if using multisession; 
  surv.models <- surv.models %>%
        mutate(fit = future_map(wflow, fit, data = survival2, .progress = TRUE, .options = furrr_options(packages = c("workflows", "multilevelmod"))),
    #mutate(fit = map(wflow, fit, data = survival2, .progress = TRUE),
           glance = map(fit, glance)
    ) 
})
```

```
## boundary (singular) fit: see help('isSingular')
```

```
##    user  system elapsed 
##   1.914   0.258  17.632
```

``` r
surv.models %>% select(-wflow, -fit) %>% unnest(glance) %>% arrange(BIC)
```

```
## # A tibble: 15 × 8
##    name                       nobs sigma logLik   AIC   BIC deviance df.residual
##    <chr>                     <int> <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int>
##  1 m11wnec_cubic.date.elap_…  8070     1 -2552. 5123. 5193.    4998.        8060
##  2 m2_linear.week_cont        8070     1 -2656. 5325. 5367.    5184.        8064
##  3 m2wne_linear.elap_weeks.…  8070     1 -2653. 5320. 5369.    5182.        8063
##  4 m11wne_cubic.date.elap_w…  8070     1 -2651. 5319. 5375.    5203.        8062
##  5 m2e_linear.elap_weeks      8070     1 -2695. 5403. 5445.    5229.        8064
##  6 m11e_cubic.elap_weeks      8070     1 -2691. 5397. 5446.    5277.        8063
##  7 m7_quad                    8070     1 -2727. 5466. 5508.    5257.        8064
##  8 m2_linear                  8070     1 -2735. 5481. 5516.    5271.        8065
##  9 m11_cubic                  8070     1 -2732. 5476. 5518.    5349.        8064
## 10 m3_linear.slope.parent.p…  8070     1 -2732. 5478. 5526.    5235.        8063
## 11 m5_linear.slope.parent.p…  8070     1 -2728. 5474. 5537.    5192.        8061
## 12 m8_quad.slope.parent.pop   8070     1 -2723. 5467. 5544.    5216.        8059
## 13 m6_linear.slope.parent.p…  8070     1 -2723. 5468. 5545.    5164.        8059
## 14 m1_RE                      8070     1 -3016. 6040. 6068.    5803.        8066
## 15 m1b_RE2                    8070     1 -3016. 6040. 6068.    5803.        8066
```

``` r
plan(sequential)
```


``` r
surv.models <- surv.models %>% mutate(messages = map(fit, \(x) {x <- extract_fit_engine(x)
c(x@optinfo$message, unlist(x@optinfo$warnings), unlist(x@optinfo$conv$lme4$messages))
})
) 

surv.models %>% select(name, messages) %>% unnest(messages) %>% filter(str_detect(messages, "Model"))
```

```
## # A tibble: 0 × 2
## # ℹ 2 variables: name <chr>, messages <chr>
```




``` r
save.image(file="../output/WL2_2023_growth_survival_models_LOG.Rdata")
```

Take a look at some of the best models...

Concentrate on the three best models

``` r
surv.models.best <- surv.models %>%
  unnest(glance) %>%
  filter(rank(BIC) <= 5 ) %>%
  arrange(BIC)

surv.models.best
```

```
## # A tibble: 5 × 11
##   wflow        name           fit         nobs sigma logLik   AIC   BIC deviance
##   <named list> <chr>          <named li> <int> <dbl>  <dbl> <dbl> <dbl>    <dbl>
## 1 <workflow>   m11wnec_cubic… <workflow>  8070     1 -2552. 5123. 5193.    4998.
## 2 <workflow>   m2_linear.wee… <workflow>  8070     1 -2656. 5325. 5367.    5184.
## 3 <workflow>   m2wne_linear.… <workflow>  8070     1 -2653. 5320. 5369.    5182.
## 4 <workflow>   m11wne_cubic.… <workflow>  8070     1 -2651. 5319. 5375.    5203.
## 5 <workflow>   m2e_linear.el… <workflow>  8070     1 -2695. 5403. 5445.    5229.
## # ℹ 2 more variables: df.residual <int>, messages <named list>
```


``` r
surv.models.best %>%
  pull(fit) 
```

```
## $m11wnec_cubic.date.elap_weeks.cubic.weeks_cont
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 4 Recipe Steps
## 
## • step_poly()
## • step_poly()
## • step_normalize()
## • step_rename_at()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: surv.next ~ l.height.cm.p1 + elapsed_weeks + week.p1 + week.p2 +  
##     week.p3 + l.height.cm.p2 + l.height.cm.p3 + (1 | parent.pop) +  
##     (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  5123.473  5193.432 -2551.737  5103.473      8060 
## Random effects:
##  Groups     Name        Std.Dev.
##  parent.pop (Intercept) 0.4185  
##  block      (Intercept) 0.5023  
## Number of obs: 8070, groups:  parent.pop, 23; block, 13
## Fixed Effects:
##    (Intercept)  l.height.cm.p1   elapsed_weeks         week.p1         week.p2  
##       2.406790        1.003990       -0.654743        0.234028        0.183715  
##        week.p3  l.height.cm.p2  l.height.cm.p3  
##      -0.692899        0.122918        0.003734  
## 
## $m2_linear.week_cont
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 0 Recipe Steps
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: surv.next ~ l.height.cm + week + (1 | parent.pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  5324.599  5366.574 -2656.300  5312.599      8064 
## Random effects:
##  Groups        Name        Std.Dev.
##  mf:parent.pop (Intercept) 0.1583  
##  parent.pop    (Intercept) 0.3629  
##  block         (Intercept) 0.4726  
## Number of obs: 8070, groups:  mf:parent.pop, 148; parent.pop, 23; block, 13
## Fixed Effects:
## (Intercept)  l.height.cm         week  
##      0.4646       0.8807       0.1219  
## 
## $m2wne_linear.elap_weeks.week_cont
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 0 Recipe Steps
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## surv.next ~ l.height.cm + week + elapsed_weeks + (1 | parent.pop/mf) +  
##     (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  5320.223  5369.194 -2653.111  5306.223      8063 
## Random effects:
##  Groups        Name        Std.Dev.
##  mf:parent.pop (Intercept) 0.1497  
##  parent.pop    (Intercept) 0.3551  
##  block         (Intercept) 0.4734  
## Number of obs: 8070, groups:  mf:parent.pop, 148; parent.pop, 23; block, 13
## Fixed Effects:
##   (Intercept)    l.height.cm           week  elapsed_weeks  
##        0.8175         0.8328         0.1040        -0.1195  
## 
## $m11wne_cubic.date.elap_weeks.weeks_cont
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 4 Recipe Steps
## 
## • step_poly()
## • step_poly()
## • step_normalize()
## • step_rename_at()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: surv.next ~ l.height.cm.p1 + elapsed_weeks + week + l.height.cm.p2 +  
##     l.height.cm.p3 + (1 | parent.pop) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  5318.694  5374.662 -2651.347  5302.694      8062 
## Random effects:
##  Groups     Name        Std.Dev.
##  parent.pop (Intercept) 0.3711  
##  block      (Intercept) 0.4679  
## Number of obs: 8070, groups:  parent.pop, 23; block, 13
## Fixed Effects:
##    (Intercept)  l.height.cm.p1   elapsed_weeks            week  l.height.cm.p2  
##        2.20276         0.81778        -0.12569         0.42534         0.12394  
## l.height.cm.p3  
##        0.05737  
## 
## $m2e_linear.elap_weeks
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 0 Recipe Steps
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: surv.next ~ l.height.cm + elapsed_weeks + (1 | parent.pop/mf) +  
##     (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  5402.594  5444.570 -2695.297  5390.594      8064 
## Random effects:
##  Groups        Name        Std.Dev.
##  mf:parent.pop (Intercept) 0.2380  
##  parent.pop    (Intercept) 0.3818  
##  block         (Intercept) 0.4952  
## Number of obs: 8070, groups:  mf:parent.pop, 148; parent.pop, 23; block, 13
## Fixed Effects:
##   (Intercept)    l.height.cm  elapsed_weeks  
##        1.6365         0.9680        -0.3343
```



``` r
set.seed(1001)
surv_folds <- vfold_cv(survival2, v = 10)

system.time( { #127 seconds
  surv.models.best <- surv.models.best %>%
    mutate(resamples = map(wflow, fit_resamples, resamples = surv_folds, control = control_resamples(save_pred = TRUE)))
})
```

```
## Warning: There were 5 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `resamples = map(wflow, fit_resamples, resamples = surv_folds,
##   control = control_resamples(save_pred = TRUE))`.
## Caused by warning:
## ! ! tune detected a parallel backend registered with foreach but no backend
##   registered with future.
## ℹ Support for parallel processing with foreach was soft-deprecated in tune
##   1.2.1.
## ℹ See ?parallelism (`?tune::parallelism()`) to learn more.
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.
```

```
##    user  system elapsed 
##  93.276   4.095  21.913
```


``` r
surv.models.best %>% 
  mutate(metrics = map(resamples, collect_metrics, type = "wide")) %>%
  select(name, metrics) %>%
  unnest(metrics) %>%
  select(-.config) %>%
  arrange(desc(rsq))
```

```
## # A tibble: 5 × 3
##   name                                            rmse   rsq
##   <chr>                                          <dbl> <dbl>
## 1 m11wnec_cubic.date.elap_weeks.cubic.weeks_cont 0.319 0.150
## 2 m2wne_linear.elap_weeks.week_cont              0.323 0.130
## 3 m11wne_cubic.date.elap_weeks.weeks_cont        0.323 0.130
## 4 m2_linear.week_cont                            0.323 0.130
## 5 m2e_linear.elap_weeks                          0.324 0.125
```
cubic for both weeks and size is the best


``` r
surv.models.best %>% 
  mutate(metrics = map(resamples, collect_metrics, type = "long")) %>%
  select(name, metrics) %>%
  unnest(metrics) %>%
  arrange(.metric, mean) %>%
  select(name, .metric, mean, std_err)
```

```
## # A tibble: 10 × 4
##    name                                           .metric  mean std_err
##    <chr>                                          <chr>   <dbl>   <dbl>
##  1 m11wnec_cubic.date.elap_weeks.cubic.weeks_cont rmse    0.319 0.00353
##  2 m2wne_linear.elap_weeks.week_cont              rmse    0.323 0.00361
##  3 m11wne_cubic.date.elap_weeks.weeks_cont        rmse    0.323 0.00363
##  4 m2_linear.week_cont                            rmse    0.323 0.00356
##  5 m2e_linear.elap_weeks                          rmse    0.324 0.00373
##  6 m2e_linear.elap_weeks                          rsq     0.125 0.0113 
##  7 m2_linear.week_cont                            rsq     0.130 0.0106 
##  8 m11wne_cubic.date.elap_weeks.weeks_cont        rsq     0.130 0.0105 
##  9 m2wne_linear.elap_weeks.week_cont              rsq     0.130 0.0109 
## 10 m11wnec_cubic.date.elap_weeks.cubic.weeks_cont rsq     0.150 0.00981
```


``` r
surv.models.best %>% 
  mutate(metrics = map(resamples, collect_metrics, type = "long")) %>%
  select(name, metrics) %>%
  unnest(metrics) %>%
  arrange(.metric, mean) %>%
  select(name, .metric, mean, std_err) %>%
  ggplot(aes(x=reorder(name, mean), y = mean, ymin=mean-std_err, ymax=mean+std_err, fill = reorder(name, mean))) +
  geom_col() +
  geom_errorbar(width = 0.5) +
  facet_wrap(~.metric, scales = "free_y") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  ggtitle("10-fold cross validation") +
  scale_fill_viridis_d()
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-27-1.png)<!-- -->


``` r
surv.model.final <- surv.models.best %>% slice_min(BIC) %>% pull(fit) %>% magrittr::extract2(1)
```

For plotting the prediction curve, it looks like I can't combine `geom_function` and `facet_wrap` so I will compute the prediction curves myself and then combine in the plots.


``` r
newdata.survival <- survival2 %>%
  group_by(parent.pop, week) %>%
  reframe(l.height.cm = seq(min(l.height.cm, na.rm = TRUE),
                               max(l.height.cm, na.rm = TRUE),
                               length.out = 101),
          elapsed_weeks = 1) %>%
  filter(parent.pop != "WV")

newdata.survival <-  surv.rec.poly %>% 
  update_role(mf, ppmf, block, new_role = "unused") %>%
  update_role_requirements(role="unused", bake = FALSE) %>%
  prep() %>% 
  bake(newdata.survival)

surv.predictions <- surv.model.final %>% 
  extract_fit_engine() %>%
  predict(newdata.survival, type = "response", re.form = ~ (1 | parent.pop)) %>%
  cbind(newdata.survival, surv.next=.)

surv.predictions %>%
  ungroup() %>%
  nest(.by = parent.pop) %>% 
  mutate(plot = map2(data, parent.pop, \(x, pp) {
  ggplot(x, aes(x=l.height.cm, y = surv.next)) +
      geom_smooth(color = "grey50", lty=3, se = FALSE, method = "gam", data = {survival2 %>% filter(parent.pop==pp)}) + 
      geom_line() +
      facet_wrap(~week, scale = "free_x") +
      geom_point(alpha = 0.3, data = {survival2 %>% filter(parent.pop==pp)}) + 
      ggtitle(pp) +
      scale_color_brewer(type = "qual", palette = "Accent")
  })) %>%
  pull(plot) %>% walk(print)
```

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-2.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-3.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-4.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-5.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-6.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-7.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-8.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-9.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-10.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-11.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-12.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-13.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-14.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-15.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-16.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-17.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-18.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-19.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-20.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `gam.reparam()`:
## ! NA/NaN/Inf in foreign function call (arg 3)
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-21.png)<!-- -->

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Failed to fit group -1.
## Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

```
## Warning: Failed to fit group -1.
## Caused by error in `smooth.construct.cr.smooth.spec()`:
## ! x has insufficient unique values to support 10 knots: reduce k.
```

![](WL2_2023_Growth_Survival_RE_LOG_files/figure-html/unnamed-chunk-29-22.png)<!-- -->


``` r
surv.model.final <- surv.model.final %>% extract_fit_engine()
growth.model.final <- growth.model.final %>% extract_fit_engine()
```


``` r
save.image(file="../output/WL2_2023_growth_survival_models_LOG.Rdata")
```

Save just the pertinent stuff

``` r
save(surv.model.final, growth.model.final, survival2, growth2, growth.rec.poly, surv.rec.poly, file = "../output/WL2_2023_growth_survival_models_FINAL_LOG.Rdata")
```



