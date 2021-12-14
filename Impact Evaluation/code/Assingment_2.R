# Assignment 2


# Women as policymakers

# 1. Import data

setwd('D:/Mukhammed/CEU/Impact Evaluation/')

library('tidyverse')
women = read.csv('data/women.csv')

# 2. Summary statistics
glimpse(women)
summary(women)

# 3. Check for balance
library(cobalt)
love.plot(reserved ~ irrigation + water, data = women, stars = 'std')

# 4. Boxplot to compare water availability since treatment
ggplot(women,aes(y = water, x = factor(reserved))) +
  geom_boxplot(fill = "green", color="black") + coord_flip() +
  labs(x = "Reserved", y = "Water") +
  ggtitle("Water availability since treatment") +
  theme(plot.title = element_text(hjust = 0.5))

# 5. Regression analysis
mod_irr = lm(irrigation ~ reserved, data = women)
mod_water = lm(water ~ reserved, data = women)
library(stargazer)
stargazer(mod_irr, mod_water, title="The treatment effect",
          type = "text", out='regression.rtf')



# STAR experiment

# 1. Import data
star = read.csv('data/star.csv')

# 2. Mean score by small VS regular class
library('dplyr')

star %>%
  filter(small == 1) %>%
  select(totalscore) %>%
  summary(star)

star %>%
  filter(regular == 1) %>%
  select(totalscore) %>%
  summary(star)

# 3. Boxplot
star = star[!(star$aide == 1),]

ggplot(star,aes(y = totalscore, x = factor(small))) +
  geom_boxplot(fill = "green", color="black") + coord_flip() +
  labs(x = "0 = regular class, 1 = small class", y = "Total score") +
  ggtitle("Total score by class size") +
  theme(plot.title = element_text(hjust = 0.5))

# 4. Check balance for covariates
love.plot(small ~ boy + freelunch + white_asian + tchexper, data = star, stars = 'std')

# 5. Formal test of balance
mod_star_check <- lm(small ~ boy + white_asian + tchexper + freelunch, data = star)
summary(mod_star_check)

# 6. The effect of treatment
mod1 = lm(totalscore ~ small, data = star)
mod2 = lm(totalscore ~ small + white_asian + freelunch, data = star)
mod3 = lm(totalscore ~ small + white_asian + tchexper + freelunch + schurban, data = star)
stargazer(mod1, mod2, mod3, title="Effect of small classes on test scores",
          type = "text", out='regression.rtf')


# Labor training

l = read.csv('data/lalonde.csv')

# Regression analysis
mod4 = lm(re78 ~ treat, data = l)
mod5 = lm(re78 ~ treat + age + education + married, data = l)
mod6 = lm(re78 ~ treat + age + education + married +
            black + nodegree + re74 + re75, data = l)
stargazer(mod4, mod5, mod6, title="Effect of labor training program on earnings",
          type = "text", out='regression.rtf')

# Grouping


# Matching
library(cem)
agecut = c(0, 20.5, 25.5, 40.5, 55.5)
educut = c(0, 6.5, 8.5, 12.5, 16.5)
mat = cem(treatment = "treat", data = l, drop = "re78",
            cutpoints = list(education=educut, age=agecut))
est1 = att(mat, re78 ~ treat, data = l)
est2 = att(mat, re78 ~ treat + age + education + married, data = l)
est3 = att(mat, re78 ~ treat + age + education + married +
             black + nodegree + re74 + re75, data = l)