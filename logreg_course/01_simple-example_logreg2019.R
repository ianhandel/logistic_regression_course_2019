
library(tidyverse)
library(skimr)
dat <- read_csv("logreg_data_01_20190530.csv")
head(dat)
skim(dat)

dat %>% 
  count(sex, status)

dat %>% 
  group_by(treatment) %>% 
  summarise(proportion_diseased = mean(status == "diseased"))

with(dat,
  fisher.test(status=="diseased", treatment))


mod <- glm(status=="diseased" ~ treatment,
           family = binomial,
           data = dat)

summary(mod)

exp(coef(mod))

