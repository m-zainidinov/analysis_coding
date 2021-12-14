# Assignment 4


# Violence and insurgency

setwd('D:/Mukhammed/CEU/Impact Evaluation/')

chechen = read.csv('data/chechen.csv')

# 1
table(chechen$fire)

# 2
# Compare by mean
library(tidyverse)
chechen %>%
  group_by(groznyy) %>%
  summarize(mean(deaths, na.rm = TRUE))

# Compare by median
chechen %>%
  group_by(groznyy) %>%
  summarize(median(deaths, na.rm = TRUE))

# 3
# Compare pre- and post-attack period
chechen %>%
  group_by(fire)%>%
  summarise(mean(preattack, na.rm = TRUE))

chechen %>%
  group_by(fire)%>%
  summarise(mean(postattack, na.rm = TRUE))

# Compare quartiles
chechen$fire = as.factor(chechen$fire)

chechen %>%
  ggplot(aes(x = fire, y = preattack)) + theme_light()+
  geom_boxplot(fill = 'lightblue') + 
  ggtitle('Insurgent attacks')

chechen %>%
  ggplot(aes(x = fire, y = postattack)) + theme_light()+
  geom_boxplot(fill = 'lightblue') + 
  ggtitle('Insurgent attacks')

# 4
shelled = chechen %>%
  subset(fire == '1') %>%
  summarise(mean(preattack, na.rm = TRUE))

not_shelled = chechen %>%
  subset(fire == '0') %>%
  summarise(mean(preattack, na.rm = TRUE))

pre_diff = shelled - not_shelled
pre_diff

# 5
chechen$diffattack = chechen$postattack - chechen$preattack
chechen %>%
  subset(fire == '1') %>%
  summarise(mean(diffattack, na.rm = TRUE))

# 6
chechen %>%
  group_by(fire) %>%
  summarise(mean(diffattack, na.rm = TRUE))


# Exploiting variation in policy changes

# 7

# 8
library(wooldridge)
data("traffic1")

# 9
m1 = lm(dthrte85 ~ open85, data = traffic1)
summary(m1)

# 10
m2 = lm(dthrte90 ~ open90, data = traffic1)
summary(m2)

# 11
library(texreg)
screenreg(list(m1,m2))

#12
# Reshape
library(reshape2)

traffic = traffic1
traffic$cdthrte = NULL
traffic$cadmn = NULL
traffic$copen = NULL
traffic$cspeed = NULL

traffic = traffic %>% 
  rename(admn_1990 = admn90, admn_1985 = admn85,
         open_1990 = open90, open_1985 = open85,
         dthrte_1990 = dthrte90, dthrte_1985 = dthrte85,
         speed_1990 = speed90, speed_1985 = speed85)

long = reshape(traffic, varying = c('admn_1990', 'admn_1985',
                                    'open_1990', 'open_1985',
                                    'dthrte_1990', 'dthrte_1985', 
                                    'speed_1990', 'speed_1985'),
               direction = 'long', idvar = 'state', sep = '_')

# Regression
long$year = ifelse(long$time == '1990', 1, 0)
m3 = lm(dthrte ~ open + year + open * year, data = long)
screenreg(list(m3))

# 13
m4 = lm(dthrte85 ~ admn85, data = traffic1)
m5 = lm(dthrte90 ~ admn90, data = traffic1)
m6 = lm(dthrte ~ admn + year + admn*year, data = long)
screenreg(list(m4,m5,m6))

# 14
m7 = lm(dthrte ~ admn + open + year + open*year + admn * year, data = long)
screenreg(list(m7))
