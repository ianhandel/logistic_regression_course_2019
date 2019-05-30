# Create dataset for LR course

library(tidyverse)
library(boot) # for logit functions

set.seed(4815)

N <- 1000
days_in_month <- 30

# make some data
# males are more likely to be treated

dat <- tibble(ID = paste("A", str_pad(1:N, 4, "left", 0)),
              treatment = sample(c("treated", "control"), N, TRUE),
              age = runif(N, 6 * days_in_month, 8 * days_in_month),
              region = sample(LETTERS[1:4], N, TRUE),
              sex = sample(c("male", "female"), N, TRUE),
              weight = 50 +
                age * 0.15 +
                (sex == "male") * 0.7 +
                + age * 0.05 * (sex == "male") +
                (region == "C") * 0.2 +
                rnorm(N, 0, 1),
              logit_p_disease = -2 +
                (treatment == "treated") * -0.4 +
                (sex == "male") * 0.2 +
                age * 0.005,
              p_disease = inv.logit(logit_p_disease),
              status = c("healthy", "diseased")[rbernoulli(N, p_disease) + 1]) %>% 
  mutate(age = round(age),
         weight = round(weight, 1)) %>% 
  mutate(treatment = if_else(sex == "male" & runif(N) > 0.80, "treated", treatment))


# ggplot(dat) +
#   aes(x = age, y = weight, colour = sex) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)

dat %>% 
  count(sex, treatment) %>% 
  spread(sex, n)

dat %>% 
  count(sex, treatment, status) %>% 
  spread(status, n)

fisher.test(dat$status == "diseased", dat$sex)

glm(status == "diseased" ~ age + sex + treatment, data = dat, family = "binomial") %>% 
  summary()

dat <- dat %>% 
  select(-logit_p_disease, -p_disease)
