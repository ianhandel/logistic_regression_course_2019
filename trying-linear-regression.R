head(dat)

mod <- lm(weight ~ age + sex, data = dat)

summary(mod)
inv
