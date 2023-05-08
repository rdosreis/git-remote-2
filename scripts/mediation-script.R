### mediation analysis ###

install.packages("mediation")
pacman::p_load(mediation, tidyverse, ggplot2, dplyr)

## Model-based causal mediation analysis ##

# The model-based causal mediation analysis proceeds in two steps. 
# First, the researcher specifies two statistical models, the mediator model for the conditional distribution of the mediator
# Mi given the treatment Ti and a set of the observed pre-treatment covariates Xi and the outcome model for the 
# conditional distribution of the outcome Yi given Ti, Mi, and Xi. 
# These models are fitted separately and then their fitted objects comprise the main inputs to the 
# <"mediate" function>, which computes the estimated ACME and other quantities of interest under these models 
# and the sequential ignorability assumption. 
# Since the sequential ignorability assumption is untestable, we recommend that the researchers conduct 
# a sensitivity analysis via the <"medsens" function>, which can be applied to certain statistical models. 
# We now illustrate these functionalities with an empirical example.

# 3.1. Estimation of the average causal mediation effects

library("mediation")
set.seed(2014)
dt <- data("framing", package = "mediation")

# fitting mediator model #
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
summary(med.fit)

# fitting outcome model #
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
                data = framing, family = binomial("probit"))
summary(out.fit)

# mediating #

med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                    robustSE = TRUE, sims = 100)
summary(med.out)

med.out <- mediate(med.fit, out.fit, boot = TRUE, treat = "treat",
                    mediator = "emo", sims = 100)
summary(med.out)

# with interaction #

med.fit <- lm(emo ~ treat + age + educ + gender + income, data=framing)
out.fit <- glm(cong_mesg ~ emo * treat + age + educ + gender + income,
                data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                    robustSE = TRUE, sims = 100)
summary(med.out)
plot(med.out)

## moderated mediation ##

med.fit <- lm(emo ~ treat * age + educ + gender + income, data=framing)
out.fit <- glm(cong_mesg ~ emo + treat * age + emo * age + educ + gender +
                  income, data = framing, family = binomial("probit"))

med.age20 <- mediate(med.fit, out.fit, treat = "treat",
                      mediator = "emo", covariates = list(age = 20), sims = 100)
med.age60 <- mediate(med.fit, out.fit, treat = "treat",
                      mediator = "emo", covariates = list(age = 60), sims = 100)
summary(med.age20)
summary(med.age60)

med.init <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo", sims=2)
dif60x20 <- test.modmed(med.init, covariates.1 = list(age = 20),
              covariates.2 = list(age = 60), sims = 100)
dif60x20

##
pacman::p_load(tidyverse, ggplot2, lme4, effects, carData)
n <- 100
subject <- seq(1:n)
time <- 0:10
netflix <- rbinom(n, 1, .5)
grid <- data.frame(expand.grid(subject = subject, time= time))
multi <- data.frame(cbind(subject, netflix)) %>%
  dplyr::left_join(., grid, by = 'subject')


# Simulation params
groupIntercept <- 50
sdIntercept <- 10
grouptimeBeta <- 4
sdtimeBeta <- 2
withinSd <- 1


# generate an intercept + slope for each subject
multi_subs <- multi %>% 
  group_by(subject) %>%
  summarise(subIntercept = rnorm(1, groupIntercept, sdIntercept), 
            subSlope = rnorm(1, grouptimeBeta, sdtimeBeta))
# join intercepts to main frame
multi <- left_join(multi, multi_subs)

# get values for each subject
multi <- dplyr::mutate(multi, happy = subIntercept + time*subSlope  + 
                       rnorm(nrow(multi), 0, withinSd) + rnorm(nrow(multi), netflix*20, 5),
                       bin = ((happy-rnorm(nrow(multi), 100, 30))/100),
                       boughtIceCream = ifelse(bin > .5, 1, 0),
                       netflix = as.factor(netflix))



# Now that we have our 'data', we won't need to pay attention to the 'subintecept' and 'subslope' columns of the dataframe unless we want to specifically see the effects of these values later

theme_set(theme_bw())
ggplot(multi, aes(x = time, y = happy, color = netflix)) +
  geom_point(alpha = .5, size = 1) +
  geom_line(aes(group = subject)) +
  labs(x = 'Time', y = 'Happiness', title = 'Does Netflix Predict Happiness Over Time?', color = 'Netflix User') +
  scale_x_continuous(breaks = 0:10) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = 'Set1')

multiBin <- mutate(multi,
                   happyBin=cut(happy, seq(0,250, 10), labels = seq(5,250, 10))) %>%
  group_by(happyBin, netflix) %>%
  summarize(n = n(), propBoughtIceCream = sum(boughtIceCream/n)) %>%
  mutate(happyNum = as.numeric(levels(happyBin)[happyBin]))

ggplot(multiBin, aes(x = happyNum, y = propBoughtIceCream, color = netflix)) +
  geom_point()  +
  geom_line() +
  labs(y = 'P(Bought Ice Cream)', x = 'Happiness') +
  scale_color_brewer(palette = 'Set1') +
  ylim(0,1)

ggplot(multi, aes(x = happy, y = boughtIceCream, color = netflix)) +
  geom_jitter(width = 0, height = .01) +
  geom_line(data = multiBin, aes(x = happyNum, y = propBoughtIceCream, color = netflix)) +
  geom_point(data = multiBin, aes(x = happyNum, y = propBoughtIceCream, color = netflix)) +
  scale_color_brewer(palette = 'Set1')

p_load(lme4)

mod_lme4 <- lmer(data = multi, happy ~ time*netflix + (time|subject))
mod_rstanarm <- stan_glmer(data = multi, happy ~ time*netflix + (time|subject), chains =4, cores = 4)
mod_brms <- brm(data = multi, happy ~ time*netflix + (time|subject), chains = 4, cores = 4)

effect_time <- as.data.frame(effect('time:netflix', mod_lme4, confint=list(alpha = .95)), 
                             xlevels = list(time = 0:10, netflix = c('0','1')))

head(effect_time)

fitLmePlot <- ggplot(data = effect_time, aes(x = time, y = fit)) +
  geom_line(aes(color = netflix), lwd = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = netflix), alpha = .5) +
  scale_x_continuous(breaks = 0:10) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = 'Set1') +
  scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Time', y = 'Happiness', title = 'Fitted Line - Lme4') +
  ylim(30,130)

ggplot(data = effect_time, aes(x = time, y = fit, color = netflix)) +
  geom_point() +
  geom_line(lwd = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = netflix), alpha = .7, colour = NA) +
  scale_x_continuous(breaks = 0:10) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = 'Time', y = 'Happiness', title = 'Does Netflix Predict Happiness Over Time?') +
  geom_point(data = multi, aes(x = time, y = happy), alpha = .5, size = 1) +
  geom_line(data = multi, aes(x = time, y = happy, group = subject), alpha = .2) 
