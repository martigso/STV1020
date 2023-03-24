#### Oppsett ####

# Laster inn pakker
library(tidyverse)
library(stargazer)

# Laster inn datasettet
# Bytt ut det som står mellom "" til å passe din filbane:
load("../data/FairFPSR3.RData")




# Et alternativ til str()
glimpse(FairFPSR3)

# Printer variabelnavnene
names(FairFPSR3)

# Henter ut oppsummerende statistikk:
summary(FairFPSR3)



# Sjekker hvor mange observasjoner som vi har informasjon på alle variablene om:
table(complete.cases(FairFPSR3))



# Sjekker hvor mange observasjoner som har missing på variabelen inflation
table(is.na(FairFPSR3$inflation))




FairFPSR3 <- FairFPSR3 %>% 
  mutate(complete = complete.cases(.),
         inf_na = is.na(inflation))



# Oppretter den nye variabelen og endrer referansekategori
FairFPSR3 <- FairFPSR3 %>% 
  mutate(growth_dich = ifelse(growth > 0, "Growth", "No growth"),
         growth_dich = factor(growth_dich, levels = c("No growth", "Growth")))



# Sjekker at det ser ok ut: 
class(FairFPSR3$growth_dich)
table(FairFPSR3$growth_dich, useNA = "always")


#### Plot ####
ggplot(data = FairFPSR3) +
  geom_histogram(aes(x = growth, fill = growth_dich),
               binwidth = 1)


#### Finere plot ####
ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita")



#### Regresjon ####

## lm(avhengig_variabel ~ uavhengig_variabel, data = mitt_datasett)
## # På mac får du ~ med alt + k + space
## # På win får du ~ med Alt Gr + ^
## # ^ er knappen til venstre for toppen av enter på de fleste tastatur



model <- lm(inc_vote ~ growth, 
            data = FairFPSR3,
            na.action = "na.exclude")



summary(model)



stargazer(model, 
          type = "text")



## stargazer(model,
##           type = "html",
##           out = "model1_tab.htm")


#### Plot ####
ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita")


#### Finere plot ####
ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita") +
  geom_smooth(aes(x = growth, y = inc_vote),
              method = "lm", color = "goldenrod3")


#### Enda finere plot ####
ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita") +
  geom_smooth(aes(x = growth, y = inc_vote),
              method = "lm", color = "goldenrod3") +
  geom_hline(yintercept = mean(FairFPSR3$inc_vote), linetype = "dashed") +
  geom_vline(xintercept = mean(FairFPSR3$growth), linetype = "dashed")



FairFPSR3 <- FairFPSR3 %>% 
  mutate(fitted = fitted(model), 
         residuals = resid(model))


#### Plot ####
ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  geom_line(aes(x = growth, y = fitted)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita")


#### Finere plot ####
ggplot(data = FairFPSR3) +
  geom_point(aes(x = growth, y = inc_vote)) +
  theme_bw() +
  labs(y = "Incumbent-Party Vote Percentage",
       x = "Percentage change in Real GDP Per Capita") +
  geom_line(aes(x = growth, y = fitted)) +
  geom_hline(yintercept = mean(FairFPSR3$inc_vote), linetype = "dashed") +
  geom_vline(xintercept = mean(FairFPSR3$growth), linetype = "dashed")



confint(model)


#### Regresjon ####
# Lagrer modellen
model_dich <- lm(inc_vote ~ growth_dich, 
                 data = FairFPSR3,
                 na.action = "na.exclude")

# Undersøker resultatene
stargazer(model_dich, 
          type = "text")

