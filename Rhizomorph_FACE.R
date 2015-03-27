library(dplyr)
library(car)

FACE = read.csv(file = '~//Documents//Rhizo/rhizomorphs_lenth production and stdcrp RICH (1).csv',
                na.strings = ".")
names(FACE)
FACE$end.date = as.Date(FACE$end.date, "%m/%d/%y")

head(FACE)=
boxplot(rhiz_prod_len_frame.mm ~ CO2trt, data=FACE)
# What is the significance of the CO2 treatment on rhizomorph production length?

mod_co2 = lm(rhiz_prod_len_frame.mm ~ CO2trt + depth + block + year, data=FACE)
summary(mod_co2)
aov(mod_co2, type=3)

residuals(mod_co2)

mod_co2 = gls(rhiz_prod_len_frame.mm ~ CO2trt, data=FACE, na.action=na.omit)
mod_co2_ar1 = update(mod_co2, correlation=corAR1(0.2, form= ~ end.date | depth * block))

mod_co2 = lme(rhiz_prod_len_frame.mm ~ CO2trt, data=FACE, na.action=na.omit,
              random= ~ 1 | block)
mod_co2_ar1 = lme(rhiz_prod_len_frame.mm ~ CO2trt, data=FACE, na.action=na.omit,
                  random= ~ 1 | block, correlation=corAR1(0.2, form= ~ end.date | depth))

avg_rhizo_prod_length_CO2trt = tapply(FACE1$rhiz_prod_len_frame.mm, FACE1$CO2trt, mean)

# Do i need to add an index?
# Warning messages:
#    1: In mean.default(X[[1L]], ...) :
#    argument is not numeric or logical: returning NA
# 2: In mean.default(X[[2L]], ...) :
#   argument is not numeric or logical: returning NA
summary(avg_rhizo_prod_length_CO2trt)
# How can I fix this
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     NA      NA      NA     NaN      NA      NA       2 

# What is the significance of the nitrogen treatment on rhizomorph production length?
avg_rhizo_prod_length_nitrogen = tapply(FACE$rhiz_prod_len_frame.mm, FACE$nitrogen, mean)
# same message
# What is the significance of the soil horizon on rhizomorph production length?


# What is the significance of the DOY on rhizomorph production length?


# Ask the same questions and while coupling the data, ex: CO2 X Horizon

# What are the avg rhizomorph production lengths given 
# CO2 treatment, nitrogen treatment, and soil horizon?
# Using dplyr
Face_trt_means= FACE1 %>%
                group_by(CO2trt,nitrogen, depth)%>%
                summarise_each(funs(mean), rhiz_prod_len_frame.mm)


t.test(FACE$rhiz_prod_len_frame.mm ~ rhizo$CO2trt))


fit = aov(FACE$rhiz_prod_len_frame.mm ~ FACE$CO2trt)
fit = aov(rhiz_prod_len_frame.mm ~ CO2trt, data=FACE)
summary(fit)
