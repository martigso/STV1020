
##### Installerer nye pakker #####
# install.packages("stargazer")
# install.packages("gmodels")
# install.packages("scales")

##### Laster inn pakker #####
library(tidyverse)
library(stargazer)
library(gmodels)
library(scales)

##### Laste inn .csv-filer #####
ess <- read.csv("./data/internett.csv")

##### Lagre .csv-filer #####
# write.csv(ess, file = "der/du/vil/lagre")


##### Se på data #####

# View(ess)
head(ess)
mean(c(3,2,9,NA), na.rm = TRUE)


tail(ess)
glimpse(ess)

summary(ess) 


##### Telle NA-verdier #####
library(tidyverse)
ess %>% 
  slice_head(n = 6) %>% 
  is.na()

ess_nona <- ess %>% 
  filter(is.na(tillit) == FALSE)

sum(2, 2, 6)

sum(TRUE, FALSE, TRUE, TRUE, FALSE)


##### Logisk NA på 1 variabel #####
ess %>% 
  slice_head(n = 6) %>%  # Beholder bare de seks første observasjonene
  select(tillit) %>%     # Beholder bare variabelen tillit
  is.na()                # Finner ut om disse observasjonene har missing


##### Teller antall NA #####
ess %>% 
  slice_head(n = 6) %>% 
  select(tillit) %>% 
  is.na() %>% 
  sum()


##### Antall NA i hele datasettet #####
ess %>% 
  is.na() %>% 
  sum()

200 / 2745

##### Antall NA på 1 variabel #####
ess$internettbruk %>% 
  is.na() %>% 
  sum() # Viser hvor mange missing det er på en variabel


##### Enheter uten NA på noen variabler #####
ess %>% 
  slice_head(n = 6) %>% 
  filter(complete.cases(.))

ess$internettbruk %>% 
  mean(na.rm = TRUE)


##### Antall uten NA på noen variabler #####
ess %>% 
  complete.cases() %>% 
  sum()
2562 / 2745

##### Summaerende stats #####
# For alle variabler i datasettet
summary(ess)
# kjonn	
# 1	Mann
# 2	Kvinne


# For variabelen internettbrukt
summary(ess$internettbruk)


##### Fjerne NA #####
# Fjerne alle observajoner med minst en missing  
ess_nona <- ess %>% 
  drop_na() 

# Fjerne alle observasjoner med missing på en variabel (eller fler) 
ess_nona_internet <- ess %>% 
  drop_na(internettbruk) # Du kan legge til flere variable med komma

# Vi skal ikke bruke data1 og data2 mer så jeg fjerner dem fra environment
rm(ess_nona, ess_nona_internet)

##### Standardavvik #####

sd(ess$internettbruk, na.rm = TRUE)


##### Varians #####
# Lagrer variansen i et eget objekt
varians <- var(ess$internettbruk, na.rm = TRUE)

varians



##### Sjekke om std == var#####
# Lagrer først standardavviket i et objekt: 
stdavvik <- sd(ess$internettbruk, na.rm = TRUE)

# Bruker logisk test for å spørre R om standardavviket er det samme som kvadrat-
# roten (sqrt) av variansen
stdavvik == sqrt(varians)

rm(stdavvik, varians)



##### Lage tabeller #####

# install.packages("stargazer")
library(stargazer)

class(ess)

ess <- data.frame(ess)
class(ess)
ess %>% 
  data.frame() %>% 
  stargazer(., type = "text")

stargazer(ess, type = "text", summary = TRUE)


stargazer(ess, type = "html",
          out = "./tabell.html",
          covariate.labels = c("Internettbruk", "Kjønn", 
                               "Alder", "Utdanning", 
                               "Tillit til parlamentet"))



##### Laste inn ANES som .RData #####
# Bytt ut det som står i hermetegn med filbanen og filnavnet på din maskin:
load("./data/ANES2016small.RData")

summary(ANES2016small)

##### Omkode vote og female #####
ANES2016small <- ANES2016small %>% 
  mutate(vote = case_match(V2Trump,
                           1 ~ "Trump",
                           0 ~ "Clinton"), 
         gender = case_match(female, 
                             0 ~ "Male", 
                             1 ~ "Female"))
table(ANES2016small$V2Trump, ANES2016small$vote)
######
# Sjekker at omkodingen ble riktig:
table(ANES2016small$female, ANES2016small$gender, useNA = "always")

# Sjekker at omkodingen ble riktig:
table(ANES2016small$V2Trump, ANES2016small$vote, useNA = "always")



##### Krysstabell #####
krysstabell <- table(ANES2016small$vote, ANES2016small$gender)

krysstabell



##### Proporsjontabell #####
prop.table(krysstabell, margin = 2)
prop.table(krysstabell, margin = 1)

##### Kji-kvadrattest#####
chisq.test(krysstabell)


CrossTable(ANES2016small$vote, ANES2016small$gender, chisq = T)



##### Visualisering #####

# Absolutte tall:
ggplot(ANES2016small, aes(x = vote,
                          fill = gender)) + 
  geom_bar(position = "dodge") +
  labs(x = element_blank(),
       y = "Antall") +
  theme(legend.title = element_blank()) +
  theme_bw()



# Andeler:
ggplot(ANES2016small, aes(x = vote,
                          group = gender)) + 
  geom_bar(aes(y = after_stat(prop)),
           position = "dodge") +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Stemmegivning og kjønn") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  facet_wrap(~ gender)



##### T-test #####

ggplot(ess, aes(x = utdanning, fill = as.factor(kjonn))) +
  geom_boxplot()



t.test(utdanning ~ as.factor(kjonn), data = ess, var.equal = TRUE)




# Differanse mellom snittene
round(11.74582 - 11.28754, digits = 2)

# Midtpunktet mellom nedre og øvre konfidensintervall 
round(sum(0.1288003, 0.7877442) / 2, digits = 2)




##### Forskjellige typer t-test #####
# Enhalet test
# Tester om menn (verdi 1) har signifikant mindre utdanning enn kvinner:
t.test(utdanning ~ as.factor(kjonn), 
       data = ess, 
       alternative = "less",
       var.equal = TRUE)

# Enhalet test
# Tester om menn (verdi 1) har signifikant mer utdanning enn kvinner:
t.test(utdanning ~ as.factor(kjonn), 
       data = ess, 
       alternative = "greater",
       var.equal = TRUE)


# Tohalet test
# Tester om menn (verdi 1) har signifikant forskjellig utdanning fra kvinner:
t.test(utdanning ~ as.factor(kjonn), 
       data = ess, 
       alternative = "two.sided",
       var.equal = TRUE)



##### Laste inn FairFPSR3 #####
load("../data/FairFPSR3.RData")



##### Kovarians #####
cov(x = FairFPSR3$growth,
    y = FairFPSR3$inc_vote, 
    use = "pairwise.complete.obs")



cov(FairFPSR3,
    use = "pairwise.complete.obs")



##### Pearson's r #####
# Korrelasjon mellom vekst og stemmeandel
cor(x = FairFPSR3$growth,
    y = FairFPSR3$inc_vote, 
    use = "pairwise.complete.obs")




##### Pearson's r med signifikans #####
# tester om korrelasojnen er statistisk signifikant
cor.test(FairFPSR3$inc_vote, 
         FairFPSR3$growth, 
         use = "pairwise.complete.obs")



##### Korrelasjonsmatrise #####
cor(FairFPSR3, use = "pairwise.complete.obs")



##### Visualisere sammenhengen (2 kontinuerlige vars) #####
ggplot(data = FairFPSR3, aes(x = growth, y = inc_vote)) +
   geom_point()


# Fikkse litt på plottet
ggplot(FairFPSR3, aes(x = growth, y = inc_vote)) +
  geom_point(shape = 1) +
  theme_bw() +
  labs(x = "Percentage Change in Real DGP Per Capita",
       y = "Incumbent Party Vote Percentage")



ggplot(FairFPSR3, aes(x = growth, y = inc_vote)) +
  geom_point(shape = 1) +
  theme_bw() +
  labs(x = "Percentage Change in Real DGP Per Capita",
       y = "Incumbent Party Vote Percentage") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black")



