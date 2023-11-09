
```r
knitr::kable(simulations_res)
```



| sample_size| dif| true_power| measmean_power| allval_power| lme_power|
|-----------:|---:|----------:|--------------:|------------:|---------:|
|          10| 0.0|      0.044|          0.050|        0.600|     0.050|
|          20| 0.0|      0.053|          0.052|        0.595|     0.052|
|          40| 0.0|      0.052|          0.066|        0.596|     0.066|
|          80| 0.0|      0.054|          0.055|        0.616|     0.055|
|          10| 0.5|      0.181|          0.186|        0.595|     0.186|
|          20| 0.5|      0.323|          0.304|        0.620|     0.304|
|          40| 0.5|      0.605|          0.580|        0.610|     0.580|
|          80| 0.5|      0.880|          0.855|        0.630|     0.855|

## Plot


```r
simulations_res %>% 
  gather(Outcome, Value, -sample_size, -dif) %>% 
  filter(grepl("_power", Outcome)) %>% 
  mutate(Outcome = case_when(
    Outcome=="lme_power" ~ "LME: All measured data",
    Outcome=="allval_power" ~ "FE Regression: All measured data",
    Outcome=="true_power" ~ "T-test: Mean true values",
    Outcome=="measmean_power" ~ "T-test: Mean measured values",
  )) %>% 
  mutate(Test = ifelse(dif == 0, 
                       yes = "False Positives",
                       no = "Power")) %>% 
  mutate(Outcome = fct_inorder(Outcome)) %>% 
  ggplot(aes(x=sample_size, y=Value, colour=Outcome)) + 
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5) +
  labs(x="Sample Size (Individuals)", y="Power",
       title="Power for calculating group means",
       subtitle=paste0("Different methods for calculating group means\n",
                       "when each individual is measured 5 times")) +
  facet_wrap(~Test)
```
