# Assignment 3

# Regression Discontinuity Design

# Government Transfer and Poverty Reduction in Brazil

setwd('D:/Mukhammed/CEU/Impact Evaluation/')

rm(list = ls())

tr = read.csv('data/transfer.csv')

# 2
# Cutoffs
cut1 = 10188
cut2 = 13584
cut3 = 16980

# Midpoints
mp1 = 11886
mp2 = 15282

# Closeness
tr$closeness = ifelse(tr$pop82 <= mp1, (tr$pop82 - cut1)/cut1*100,
               ifelse((tr$pop82 > mp1) & (tr$pop82 <= mp2),
                      (tr$pop82 - cut2)/cut2*100,
                      (tr$pop82 - cut3)/cut3*100))

# 3
# Filtering obervations
library(tidyverse)
tr3 = tr %>%
  dplyr::filter(closeness >= -3 & closeness <= 3)

# RDD data
library(rddtools)

lit3 = rdd_data(literate91, closeness, data = tr3, cutpoint = 0)
edu3 = rdd_data(educ91, closeness, data = tr3, cutpoint = 0)
pov3 = rdd_data(poverty91, closeness, data = tr3, cutpoint = 0)

# Regressions
lit3_para = rdd_reg_lm(lit3, order = 1)
edu3_para = rdd_reg_lm(edu3, order = 1)
pov3_para = rdd_reg_lm(pov3, order = 1)

library(stargazer)
stargazer(lit3_para, edu3_para, pov3_para,
          dep.var.labels = 'Outcome variable',
          column.labels = c('literacy', 'education', 'poverty'),
          title = 'Parametric regressions to estimate the treatment effect',
          type = "text", out = 'output/rdd_parametric.rtf')

# 4
# Plotting
plot(lit3_para, ylim = c(0.75, 0.85)) + title('Literacy')
plot(edu3_para, ylim = c(4.2, 5)) + title('Education')
plot(pov3_para, ylim = c(0.5, 0.7)) + title('Poverty')

#5

# 1 percentage
tr1 = tr %>%
  dplyr::filter(closeness >= -1 & closeness <= 1)
lit1 = rdd_data(literate91, closeness, data = tr1, cutpoint = 0)
edu1 = rdd_data(educ91, closeness, data = tr1, cutpoint = 0)
pov1 = rdd_data(poverty91, closeness, data = tr1, cutpoint = 0)
lit1_para = rdd_reg_lm(lit1, order = 1)
edu1_para = rdd_reg_lm(edu1, order = 1)
pov1_para = rdd_reg_lm(pov1, order = 1)

# 2 percentage
tr2 = tr %>%
  dplyr::filter(closeness >= -2 & closeness <= 2)
lit2 = rdd_data(literate91, closeness, data = tr2, cutpoint = 0)
edu2 = rdd_data(educ91, closeness, data = tr2, cutpoint = 0)
pov2 = rdd_data(poverty91, closeness, data = tr2, cutpoint = 0)
lit2_para = rdd_reg_lm(lit2, order = 1)
edu2_para = rdd_reg_lm(edu2, order = 1)
pov2_para = rdd_reg_lm(pov2, order = 1)

# 4 percentage
tr4 = tr %>%
  dplyr::filter(closeness >= -4 & closeness <= 4)
lit4 = rdd_data(literate91, closeness, data = tr4, cutpoint = 0)
edu4 = rdd_data(educ91, closeness, data = tr4, cutpoint = 0)
pov4 = rdd_data(poverty91, closeness, data = tr4, cutpoint = 0)
lit4_para = rdd_reg_lm(lit4, order = 1)
edu4_para = rdd_reg_lm(edu4, order = 1)
pov4_para = rdd_reg_lm(pov4, order = 1)

# 5 percentage
tr5 = tr %>%
  dplyr::filter(closeness >= -5 & closeness <= 5)
lit5 = rdd_data(literate91, closeness, data = tr5, cutpoint = 0)
edu5 = rdd_data(educ91, closeness, data = tr5, cutpoint = 0)
pov5 = rdd_data(poverty91, closeness, data = tr5, cutpoint = 0)
lit5_para = rdd_reg_lm(lit5, order = 1)
edu5_para = rdd_reg_lm(edu5, order = 1)
pov5_para = rdd_reg_lm(pov5, order = 1)

# Results

# Literacy
stargazer(lit1_para, lit2_para, lit3_para, lit4_para, lit5_para,
          dep.var.labels = 'literate91',
          column.labels = c('-1% <= X <= 1%', '-2% <= X <= 2%',
                            '-3% <= X <= 3%',
                            '-4% <= X <= 4%', '-5% <= X <= 5%'),
          title = 'Parametric regressions to estimate treatment effect on literacy rate',
          type = "text", out = 'output/rdd_lit_para.rtf')

# Education
stargazer(edu1_para, edu2_para, edu3_para, edu4_para, edu5_para,
          dep.var.labels = 'educ91',
          column.labels = c('-1% <= X <= 1%', '-2% <= X <= 2%',
                            '-3% <= X <= 3%',
                            '-4% <= X <= 4%', '-5% <= X <= 5%'),
          title = 'Parametric regressions to estimate treatment effect on years of education',
          type = "text", out = 'output/rdd_edu_para.rtf')

# Poverty
stargazer(pov1_para, pov2_para, pov3_para, pov4_para, pov5_para,
          dep.var.labels = 'poverty91',
          column.labels = c('-1% <= X <= 1%', '-2% <= X <= 2%',
                            '-3% <= X <= 3%',
                            '-4% <= X <= 4%', '-5% <= X <= 5%'),
          title = 'Parametric regressions to estimate treatment effect on the poverty rate',
          type = "text", out = 'output/rdd_pov_para.rtf')

# Sensitivity analysis
bw_lit5 = rdd_bw_ik(lit5)
lit5_nonpara = rdd_reg_np(rdd_object = lit5, bw = bw_lit5)
plotSensi(lit5_nonpara, from = 0.5, to = 10, by = 0.5)

bw_edu5 = rdd_bw_ik(edu5)
edu5_nonpara = rdd_reg_np(rdd_object = edu5, bw = bw_edu5)
plotSensi(edu5_nonpara, from = 0.5, to = 10, by = 0.5)

bw_pov5 = rdd_bw_ik(pov5)
pov5_nonpara = rdd_reg_np(rdd_object = pov5, bw = bw_pov5)
plotSensi(pov5_nonpara, from = 0.5, to = 10, by = 0.5)

# 6
edu3_before = rdd_data(educ80, closeness, data = tr3, cutpoint = 0)
pov3_before = rdd_data(poverty80, closeness, data = tr3, cutpoint = 0)

edu3_before_para = rdd_reg_lm(edu3_before, order = 1)
pov3_before_para = rdd_reg_lm(pov3_before, order = 1)

stargazer(edu3_before_para, pov3_before_para,
          dep.var.labels = 'Outcome variable',
          column.labels = c('education', 'poverty'),
          title = 'Parametric regressions to estimate the treatment effect',
          type = "text", out = 'output/rdd_parametric_before.rtf')



# Difference in Differences

# Banks in Business

# 7. Import data
library(readr)
banks = read.csv('data/banks.csv')

# 8. Averages by year
banks_mean = banks %>%
  group_by(year) %>%
  summarize_at(vars(bib6, bib8), mean)

# 9. Gather in one column
banks_gather = gather(banks_mean, 'bty', 'num', -year)

# 10. Filter for the years 1930 and 1931 and plot the banks
banks_30_31 = banks_gather%>%
  filter(year == '1930' | year == '1931')

ggplot(banks_30_31) + geom_line(aes(x = year, y = num, color = bty))

# 11. Parallel trend assumption
ggplot(banks_gather, aes(x=year, y=log(num), colour=bty)) + geom_line()



# Alternative
# 9. Gather in one column
banks_gather = gather(banks, 'bty', 'num', 'bib6', 'bib8')

# 10. Filter for the years 1930 and 1931 and plot the banks
banks_30_31 = banks_gather%>%
  filter(year == '1930' | year == '1931')