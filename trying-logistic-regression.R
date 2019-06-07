head(dat)

dat <- dat %>%
  mutate(status01 = as.numeric(status == "diseased"))

mod1 <- glm(status01 ~ age + treatment, family = binomial, data = dat)
summary(mod1)

# make results table

mod1 %>% 
  tidy() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(OR = exp(estimate))

#Age
#There was a significant relationship between age and disease status (p value = 0.0122). For every unit increase in age the odds of being disease increase by 1.02 times (95% CI ?).

#Treated animals
#The odds of treated animals becoming ill are 0.58 times the odds of untreated animals becoming ill

#Categorical variable (>2 categories)
#Ian needs to show this!

library(sjPlot)
get_model_data(mod1)
plot_model(mod1)



dat <- dat %>%
  mutate(region2 = fct_relevel(region, "D"))

mod2 <- glm(status01 ~ age + treatment + region2,
            family = binomial, data = dat)

get_model_data(mod2) %>%
  select(term:conf.high) %>% 
  View()

#saving

get_model_data(mod2) %>% 
  write_csv("model_2.csv")
