#############################
######## Seminar 2 ##########
#############################

## Installere pakke ##########
## install.packages("tidyverse")


## Laste inn pakke ##########
library(tidyverse)

## Sette arbeidsområde ##########
## setwd("~/Dokumenter/STV1020")

## Se på arbeidsområde ##########
getwd()
list.files()


## Laste inn datasett generelt ##########
## # Laster inn og lagrer datasettet som et objekt:
## datasett <- read_filtype("filbane/filnavn.filtype")


## Haven-pakken ##########
## install.packages("haven")


## Laste inn haven ##########
## library(haven)
## 
## datanavn <- read_dta("data/filnavn.dta")


## Laste inn csv ##########
## # For csv-format:
## datanavn <- read_csv("data/filnavn.csv")
## 

## Laste inn R-data ##########
# For filer i Rdata-format:
## load("data/filnavn.Rdata")
## load("data/filnavn.rda")


## Laste inn Excel-filer ##########
## install.packages("readxl")
## 
## library(readxl)
## 
## datanavn <- read_excel("data/filnavn.xlsx")


## Laste inn ESS ##########
# https://github.com/martigso/STV1020/blob/gh-pages/data/ess.csv
ess <- read.csv("../data/ess.csv", encoding = "UTF-8")



## Sjekke dimensjoner ##########

dim(ess)


## Subsette data ##########
ess_subset <- ess %>% 
  select(nwspol, polintr, vote, yrbrn) %>% 
  rename(news = nwspol,
         interest = polintr, 
         year_born = yrbrn)



## Subsette data ##########
ess_subset <- ess %>% 
  select(vote,
         news = nwspol,
         interest = polintr, 
         year_born = yrbrn)


## Regne ut alder ##########
ess_subset$age <- 2018 - ess_subset$year_born 


## Sjekke klassen til vote ##########
# ../bilder/vote_skjermdump.JPG
class(ess_subset$vote)


## Tabell over vote ##########
table(ess_subset$vote)


## Lage faktor ##########
# Lager en ny variabel i datasettet som heter vote_factor
ess_subset$vote_factor <- as.factor(ess_subset$vote)


## Sjekker faktornivåene ##########
levels(ess_subset$vote_factor)

## Sjekker klasse ##########
class(ess_subset$vote_factor)


## Sjekker klasser på interest ##########
# ../bilder/interest_skjermdump.jpg
class(ess_subset$interest)


## Gjør om til faktor ##########
ess_subset$interest_factor <- as.factor(ess_subset$interest)


## Sjekker faktornivåene ##########

levels(ess_subset$interest_factor)


## Sjekker klasse ##########
class(ess_subset$interest_factor)

## Sjekker klasse på news ##########
# ../bilder/news_skjermdump.JPG
class(ess_subset$news)

# Er den numerisk?
is.numeric(ess_subset$news)


## Oversikt over datasett ##########
summary(ess_subset)
str(ess_subset)
glimpse(ess_subset)

## Hode, skulder, kne og tå ##########
head(ess_subset)
tail(ess_subset)


## Tabeller ##########
table(ess_subset$vote, useNA = "always")

prop.table(table(ess_subset$vote))

prop.table(table(ess_subset$vote, useNA = "always"))


## Deskriptiv statistikk ##########

# Finner minimumsverdi (det laveste antall minutter brukt på nyheter)
min(ess_subset$news, na.rm = TRUE) # na.rm = TRUE sier at missing skal droppes i beregningen

# Finner maksimumsveriden (den høyeste antall minutter brukt på nyheter)
max(ess_subset$news, na.rm = TRUE)

# Finner gjennomsnittlig antall minutter
mean(ess_subset$news, na.rm = TRUE)

# Finner median
median(ess_subset$news, na.rm = TRUE)

# Finner standardavviket
sd(ess_subset$news, na.rm = TRUE)

# Finner varians
var(ess_subset$news, na.rm = TRUE)

# Finner kvantilverdiene
quantile(ess_subset$news, na.rm = TRUE)

# Finner forskjellig deskriptiv statistikk for en variabel
summary(ess_subset$news)

# For spesielt interesserte, modus:
ess_subset$news %>% table() %>% .[which.max(.)]

## Bar plot ##########
ggplot(ess_subset, aes(x = interest_factor)) + 
  geom_bar()


## Fjerne NA ##########
ess_subset %>% 
  filter(is.na(interest_factor) == FALSE) %>% 
  ggplot(., aes(x = interest_factor)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


## Pie chart ##########
ess_subset %>% 
  filter(is.na(interest_factor) == FALSE) %>% 
  ggplot(., aes(x = "", y = interest, fill = interest_factor)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_grey()


## Variabelfarger ##########
ess_subset %>% 
  filter(is.na(interest_factor) == FALSE & 
           is.na(vote_factor) == FALSE) %>% 
  ggplot(., aes(x = interest_factor)) + 
  geom_bar(aes(fill = vote_factor),
           position = "dodge") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


## Histogram  (bins) ##########
ggplot(ess_subset, aes(x = news)) +
  geom_histogram(bins = 5) +
  ggtitle("Histogram med fem søyler (bins) og frekvens")


## Histogram (binwidth) ##########
ggplot(ess_subset, aes(x = news)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Histogram med søylebredde (binwidth) på 10 og frekvens")


## Histogram (bins, density) ##########
ggplot(ess_subset, aes(x = news, y = ..density..)) +
  geom_histogram(bins = 5) +
  ggtitle("Histogram med fem søyler (bins) og density")


## Histogram (binwidth, density) ##########
ggplot(ess_subset, aes(x = news, y = ..density..)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Histogram med søylebredde (binwidth) 10 og density")

## Boxplot ##########
ggplot(ess_subset, aes(x = news, y = vote_factor)) +
  geom_boxplot() +
  theme_minimal()


ggplot(ess_subset, aes(x = interest_factor, y = age, color = age)) +
  geom_jitter() +
  geom_hline(yintercept = mean(ess_sub$alder, na.rm = T), 
             color = "white", linetype = "dashed") +
  scale_color_continuous(low = "orange", high = "darkcyan") +
  ggdark::dark_theme_classic() +
  labs(x = "", y = "") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")