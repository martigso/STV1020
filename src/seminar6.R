#### ####
#### ####
library(tidyverse)
library(stargazer)


#### ####

# Bytt ut det som står i "" med din egen filbane:
anes96 <- read.csv("../data/ANES1996small.csv")



#### ####
names(anes96)


#### ####
#### ####
## View(anes96)


#### ####
head(anes96)


#### ####
table(anes96$v960066, useNA = "always")


#### ####
anes96 <- anes96 %>% 
  rename(hillary_thermo = v960281,
         income = v960701,
         womenmvmt_thermo = v961039,
         gender = v960066,
         age = v960605)


#### ####
names(anes96)


#### ####
#### ####
## nydata <- data %>%
##   select(var1, var2)


#### ####
#### ####
## nydata <- data %>%
##   select(-var2)


#### ####
anes962 <- anes96 %>% 
  select(hillary_thermo,
         income,
         womenmvmt_thermo,
         gender,
         age)


#### ####
anes962 <- anes962 %>%
  mutate(female = ifelse(gender == 1, 0, 1))


#### ####
table(anes962$female, anes962$gender, useNA = "always")


#### ####
summary(anes96$hillary_thermo)


#### ####
ggplot(anes96, aes(x = hillary_thermo)) +
  geom_histogram(binwidth = 10) +
  theme_bw()



#### ####
#### ####
## model <- lm(av ~ uv1 + uv2, data = data, na.action = "na.exclude")


#### ####
thermo_mult <- lm(hillary_thermo ~ income + female, 
                  data = anes962, 
                  na.action = "na.exclude")


#### ####
summary(thermo_mult)


#### ####
stargazer(thermo_mult, type = "text")


#### ####
thermo_biv <- lm(hillary_thermo ~ income, 
                 data = anes962, 
                 na.action = "na.exclude")


#### ####
stargazer(thermo_biv, thermo_mult, 
          type = "text")


#### ####
#### ####
stargazer(thermo_biv, thermo_mult, 
          type = "html",
          title = c("Results from regression analysis"),
          covariate.labels = c("Income",
                               "Female",
                               "Intercept"),
          dep.var.labels = "Hillary Clinton Thermometer score")


#### ####
anes962 <- anes962 %>% 
  mutate(thermo_fit = fitted(thermo_mult),
         thermo_resid = resid(thermo_mult))


#### ####
thermo_additiv <- lm(hillary_thermo ~ female + womenmvmt_thermo, 
                     data = anes962, 
                     na.action = "na.exclude")



#### ####
thermo_samspill <- lm(hillary_thermo ~ female * womenmvmt_thermo, 
                      data = anes962, 
                      na.action = "na.exclude")

summary(thermo_additiv)
summary(thermo_samspill)



#### ####
#### ####
stargazer(thermo_additiv, thermo_samspill, 
          type = "html",
          title = c("Tabell fra Kellstedt og Whitten s. 257"),
          covariate.labels = c("Female",
                               "Women's Movement Thermometer",
                               "WMT x Female",
                               "Intercept"),
          dep.var.labels = "Hillary Clinton Thermometer score")


#### ####
#### ####
#### ####
ggplot(anes962, aes(x = thermo_resid)) +
  geom_histogram() +
  theme_bw()


#### ####
#### ####
#### ####
ggplot(anes962, aes(x = thermo_fit, y = thermo_resid)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()


#### ####
#### ####
## plot(thermo_mult)
