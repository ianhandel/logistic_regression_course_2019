
library(tidyverse)
library(skimr)
dat <- read_csv("logreg_data_01_20190530.csv")
head(dat)
skim(dat)


mod1 <- glm(status=="diseased" ~ treatment,
           family = binomial,
           data = dat)

summary(mod1)


exp(coef(mod1))

round(exp(coef(mod1)), 3)

mod2 <- glm(status=="diseased" ~ treatment + age,
           family = binomial,
           data = dat)

summary(mod2)

round(exp(coef(mod2)), 3)

