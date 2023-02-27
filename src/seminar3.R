#### Installere gapminder-pakken ####
# install.packages("gapminder")
library(gapminder)


#### Laste inn gapminder-data ####
data(gapminder, package = "gapminder")


#### Lagre data i .RData/.rda-format ####
## save(gapminder, file = "../data/gapminder.RData")


#### Fjerne objekter fra environment####
# Denne koden fjerner alt i environment
rm(list = ls())

# Denne koden fjerner et objekt
rm(objektnavn)

# Denne koden fjerner flere objekter
rm(objektnavn1, objektnavn2)

#### Fjerne gapminder og laste inn igjen ####
rm(gapminder)

load("../data/gapminder.RData")


#### Datasettoversikt####
names(gapminder)    # Variabelnavnene i datasettet
head(gapminder)     # Første 6 radene i dataseettet
summary(gapminder)  # Summerende statistikk på alle variablene i datasettet

#### Omkoding ####

# Sjekker summary for opprinnelige variabel
summary(gapminder$year)

# spennet er 2007-1952, dvs. 55 år
2007 - 1952

# Legger til ny variabel i datasettet
gapminder$year_1952 <- gapminder$year - 1952

#### Kontrollere resultat ####

# Sjekker summary for ny variabel
summary(gapminder$year_1952)

# Lager en tabell med opprinnelig og ny variabel:
table(gapminder$year_1952, gapminder$year, useNA = "always")

#### Endre retning på variabel ####

# Endrer retning på variabelen:
gapminder$year_2007 <- (gapminder$year_1952 * -1) + 55

# Alternativt:
gapminder$year_2007 <- 55 - gapminder$year_1952

# Sjekker at det ble riktig med table():
table(gapminder$year_1952, gapminder$year_2007, useNA = "always")



#### ifelse() ####

# data$nyvar <- ifelse(test = data$variabel == "some logical condition",
#                      yes  = "what to return if 'some condition' is TRUE",
#                      no   = "what to return if 'some condition' is FALSE")


#### omkode lifeExp til dummy ####
gapminder$lifeExp_num <- ifelse(test = gapminder$lifeExp > mean(gapminder$lifeExp, na.rm = TRUE),
                                yes = 1,
                                no = 0)


#### case_match() ####
# data %>%
#   mutate(nyvariabel = case_match(gammelvariabel,
#                                  gammel_verdi1 ~ ny_verdi1,
#                                  gammel_verdi2 ~ ny_verdi2))
#

library(tidyverse)

#### case_match() lifeExp ####
gapminder <- gapminder %>% 
  mutate(lifeExp_char = case_match(lifeExp_num, 
                               1  ~ "high", 
                               0  ~ "low"))


#### Sjekke resultat ####
table(gapminder$lifeExp_char, gapminder$lifeExp_num)

#### Repitisjon av select/filter ####

gapminder_pop <- gapminder %>% 
  select(pop)

gapminder_sub <- gapminder %>%
  select(pop, country, year)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

gapminder_1952pop <- gapminder %>% 
  filter(year == 1952, 
         pop > mean(pop))

rm(gapminder_pop, gapminder_sub, gapminder_1952, gapminder_1952pop)


#### ggplot tomt ####
ggplot(data = gapminder)



#### ggplot fundament ####
ggplot(data = gapminder, mapping = aes(x = continent))


#### ggplot bar plot ####
ggplot(gapminder, aes(x = continent)) + 
  geom_bar()

summary(gapminder$continent)


#### ggplot farge bars ####
ggplot(gapminder, aes(x = continent, fill = continent)) + 
  geom_bar()



#### legge til count ####
gapminder <- gapminder %>% 
  add_count(continent)


#### ggplot snittlinje ####
ggplot(gapminder, aes(x = continent, fill = continent)) + 
  geom_bar() +
  geom_hline(aes(yintercept = mean(n)))




#### ggplot aksenavn ####
ggplot(gapminder, aes(x = continent, fill = continent)) + 
  geom_bar() +
  geom_hline(aes(yintercept = mean(n))) +
  labs(x = "", 
       y = "Antall land-år-observasjoner", 
       title = "Antall observasjoner per kontinent",
       caption = "Gjennomsnittlig antall observasjoner er gitt ved den horisontale linjen") +
  theme_bw() +
  theme(legend.position = "none")



#### ggplot som objekt ####
plot_le <- ggplot(gapminder, aes(x = lifeExp))


#### ggplot histogram ####
plot_le +
  geom_histogram()

#### ggplot binwidth ####
plot_le +
  geom_histogram(binwidth = 1)



#### ggplot themes ####
plot_le +
  geom_histogram(aes(fill = continent),
                 binwidth = 1, 
                 alpha = 0.5) +
  theme_bw()

#### ggplot boxplot ####

plot_le + 
  geom_boxplot(aes(y = continent))



#### ggplot  boxplot ####

plot_le + 
  geom_boxplot(aes(y = continent)) +
  theme_bw() +
  labs(title = "Boxplot for forventet levealder per kontinent",
       x = "Forventet levealder",
       y = "") +
  theme(legend.position = "none")


#### ggplot density ####
plot_le + 
  geom_density()

#### ggplot density ####
plot_le + 
  geom_density(linewidth = 1.5, fill = "pink", alpha = 0.3)

#### ggplot density ####
plot_le + 
  geom_density(linewidth = 0.5, fill = "pink", alpha = 1)

#### ggplot facets ####
plot_le + 
  geom_density(linewidth = 0.5, fill = "pink", alpha = 1) +
  facet_wrap(vars(continent)) +
  theme_bw()

#### ggplot spredningsplot ####
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap)) + 
  geom_point()



#### ggplot smooth ####
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap)) +
  geom_point() + 
  geom_smooth()



#### ggplot smooth pr gruppe ####
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap, col = continent)) +
  geom_point() + 
  geom_smooth()



#### ggplot density ####
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap, col = continent)) +
  geom_point() + 
  geom_smooth(colour = "black") + 
  facet_wrap(vars(continent)) +
  labs(x = "Forventet levealder", 
       y = "GDP per capita", 
       title = "Et plot med Gapminderdata") +
  theme_bw() +
  theme(legend.position = "none")



#### ggsave -- lagre plot ####

# For png-format:
ggsave(filename = "gdplevealder.png")

# For pdf-format:
ggsave(filename = "gdplevealder.pdf")

# For jpeg-format:
ggsave(filename = "gdplevealder.jpeg")

# Du trenger egentlig ikke skrive "filename" så lenge du bruker ggsave:
ggsave("gdplevealder.png")



# For eksempel:
ggsave("plot_le.png", plot = plot_le)


