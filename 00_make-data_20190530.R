# Create dataset for LR course

library(tidyverse)
library(here)
library(boot) # for logit functions

set.seed(4815)

N <- 500
days_in_month <- 30

# make some data
# males are more likely to be treated

dat <- tibble(ID = paste0("A", str_pad(1:N, 4, "left", 0)),
              treatment = sample(c("treated", "control"), N, TRUE),
              age = runif(N, 6 * days_in_month, 8 * days_in_month),
              region = sample(LETTERS[1:4], N, TRUE),
              supp = round(runif(N, 0, 10),3),
              sex = sample(c("male", "female"), N, TRUE),
              weight = 50 +
                age * 0.1 +
                (sex == "male") * 1.2 +
                + age * 0.1 * (sex == "male") +
                (region == "C") * 0.2 +
                rnorm(N, 0, 1),
              logit_p_disease = -1 +
                (treatment == "treated") * -1.9 +
                (sex == "male") * 0.3 +
                (age - 220) * 0.02 +
                (supp - 5) ^ 2,
              p_disease = inv.logit(logit_p_disease),
              status = c("healthy", "diseased")[rbernoulli(N, p_disease) + 1]) %>% 
  mutate(age = round(age),
         weight = round(weight, 1)) %>% 
  mutate(treatment = if_else(sex == "male" & runif(N) > 0.80, "treated", treatment))

dat %>% 
  count(sex, treatment) %>% 
  spread(sex, n)

dat %>% 
  count(sex, treatment, status) %>% 
  spread(status, n)

fisher.test(dat$status == "diseased", dat$sex)

fisher.test(dat$status == "diseased", dat$treatment)


glm(status == "diseased" ~ age + sex + treatment + supp, data = dat, family = "binomial") %>% 
  summary()

glm(status == "diseased" ~ age  + treatment, data = dat, family = "binomial") %>% 
  summary()

dat <- dat %>% 
  select(-logit_p_disease, -p_disease)


write_csv(dat, here("logreg_course", "logreg_data_01_20190530.csv"))
write_csv(dat, here("exercises", "logreg_data_01_20190530.csv"))

