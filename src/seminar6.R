#### ####
#### ####
library(tidyverse)
library(stargazer)


#### Laste inn data ####

# Bytt ut det som står i "" med din egen filbane:
anes96 <- read.csv("../data/ANES1996small.csv")



#### Se toppen av datasettet ####
head(anes96)


#### Lage tabell ####
table(anes96$v960066, useNA = "always")


#### Skifte variabelnavn ####
anes96 <- anes96 %>% 
  rename(hillary_thermo = v960281,
         income = v960701,
         womenmvmt_thermo = v961039,
         gender = v960066,
         age = v960605)

#### Se toppen av datasettet ####
head(anes96)


#### Lage nytt datasett ####
anes_sub <- anes96 %>% 
  select(hillary_thermo,
         income,
         womenmvmt_thermo,
         gender,
         age)


#### Omkode variabel ####
anes_sub <- anes_sub %>%
  mutate(female = ifelse(gender == 1, 0, 1))


#### Lage krysstabell ####
table(anes_sub$female, anes_sub$gender, useNA = "always")


#### Deskriptiv stats ####
summary(anes96$hillary_thermo)


#### Histogram ####
ggplot(anes96, aes(x = hillary_thermo)) +
  geom_histogram(binwidth = 10) +
  theme_bw()


#### Bivariat regresjon ####
thermo_biv <- lm(hillary_thermo ~ income, 
                 data = anes_sub, 
                 na.action = "na.exclude")


#### Regresjonstabell ####
stargazer(thermo_biv, 
          type = "text")


#### Multivariat regresjon ####
thermo_mult <- lm(hillary_thermo ~ income + female, 
                  data = anes_sub, 
                  na.action = "na.exclude")

#### Summary regresjon ####
summary(thermo_mult)


#### Stargazer 2 regresjoner ####
stargazer(thermo_biv, thermo_mult, 
          type = "text")


#### Lage litt finere stargazer ####
stargazer(thermo_biv, thermo_mult, 
          type = "html",
          title = c("Results from regression analysis"),
          covariate.labels = c("Income",
                               "Female",
                               "Intercept"),
          star.cutoffs = c(0.05, 0.01, 0.001), # viktig
          dep.var.labels = "Hillary Clinton Thermometer score")


#### Legge inn resid og fit ####
anes_sub <- anes_sub %>% 
  mutate(thermo_fit = fitted(thermo_mult),
         thermo_resid = resid(thermo_mult))


#### Additiv modell ####
thermo_additiv <- lm(hillary_thermo ~ female + womenmvmt_thermo, 
                     data = anes_sub, 
                     na.action = "na.exclude")




#### Samspillsmodell ####
thermo_samspill <- lm(hillary_thermo ~ female * womenmvmt_thermo, 
                      data = anes_sub, 
                      na.action = "na.exclude")

summary(thermo_additiv)
summary(thermo_samspill)



#### Stargazer ####
stargazer(thermo_additiv, thermo_samspill, 
          type = "text",
          title = c("Tabell fra Kellstedt og Whitten s. 257"),
          covariate.labels = c("Female",
                               "Women's Movement Thermometer",
                               "WMT x Female",
                               "Intercept"),
          star.cutoffs = c(0.05, 0.01, 0.001), # viktig
          dep.var.labels = "Hillary Clinton Thermometer score")


#### Diagnostikk (residfordeling) ####
ggplot(anes_sub, aes(x = thermo_resid)) +
  geom_histogram() +
  theme_bw()


#### Diagnostikk (ikke korrelasjon mellom resid og fit) ####
ggplot(anes_sub, aes(x = thermo_fit, y = thermo_resid)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()


#### ####
#### ####
## plot(thermo_mult)


#### Visualisere samspill ####

mock_data <- data.frame(female = sort(rep_along(1:22, c(0, 1))),
                        womenmvmt_thermo = seq(0, 100, 10))


preds <- predict(thermo_samspill, newdata = mock_data, se.fit = TRUE)

mock_data$fit <- preds$fit
mock_data$se.fit <- preds$se.fit

mock_data$upr_conf <- mock_data$fit + 1.96 * mock_data$se.fit
mock_data$lwr_conf <- mock_data$fit - 1.96 * mock_data$se.fit


ggplot(mock_data, aes(x = womenmvmt_thermo, y = fit)) +
  geom_line(aes(color = factor(female))) +
  geom_ribbon(aes(ymin = lwr_conf, ymax = upr_conf, 
                  fill = factor(female)), 
              alpha = .2) +
  theme_bw()




