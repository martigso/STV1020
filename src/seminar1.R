#############################
######## Seminar 1 ##########
#############################

100/(2+4)

#### Hjelpefil #########################

?mean

??vif

#### Objekter #########################

To <- 2

3 + To

To  * To

#### Tallrekker #########################

en_til_hundre <- 1:100

en_til_hundre

tall <- c(1, 4, 56, 8, 4, 2, 4)
tall

tall[3]


#### Funksjoner (gj.snitt) #########################

mean(en_til_hundre)


mean(tall)
median(tall)

#### Indeksering #########################

tall[5]

tall[3:6]

c(3, 5, 3, 6)

tall[c(3,5,3,6)]


mean(tall[c(3,5,3,6)])

ny_vektor <- tall[c(3,5,3,6)]

ny_vektor

#### Klasser (tall) #########################

is.numeric(en_til_hundre)
# Her ser vi at vi får opp "TRUE" som betyr at en_til_hundre er et numerisk objekt

class(en_til_hundre)
class(95.62)


#### Klasser (tekst) #########################

Tekst <- "Hei, jeg elsker R! <3"
Tekst

# Denne klassen kan inneholde tekst, men vil f.eks. ikke kunne brukes til matte. 

mean(Tekst)


#### Bytte klasse #########################
tall

tall_ch <- as.character(tall)

tall_ch

mean(tall_ch)


#### Klasser (faktor) #########################

# Lager en faktorvariabel uten å sette nivåer
skolenivaer <- factor(c("Barneskole", 
                        "Ungdomskole", 
                        "Videregaende", 
                        "Videregaende", 
                        "Universitet",
                        "Ungdomskole",
                        "Universitet"))

# Printer alle verdiene
skolenivaer 

# Printer alle nivåene
levels(skolenivaer)

# Er det noe som skurrer her?


#### Klasser (faktor) #########################

# Endrer faktornivåene
skolenivaer <- factor(skolenivaer,
                      levels = c("Barneskole",
                                 "Ungdomskole",
                                 "Videregaende",
                                 "Universitet"))
skolenivaer

levels(skolenivaer)


#### Klasser (blandet) #########################
teksttall <- c(1, 4, 0, 4, "Bamse", "R", "R Seminarer er de BESTE seminarer", 42, "the answer")

class(teksttall)
mean(teksttall[1:4])


#### Lage data #########################

navn <- c("Beate", "Tobias", "Smara",
          "Alexander", "Avelina", "Markus")

alder <- c(21, 22, 20, 20, 20, 71)

per_tusen <- c(45, 40, 37, 50, 48, 120)

kjonn <- c("jente", "gutt", "jente",
           "gutt", "jente", "gutt")

by <- c("Ål", "Lørenskog", "Oshlo",
        "Fredrikstad", "Porsgrunn",
        "Kautokeino")

#### Vektorlengde #########################

length(navn)
length(by)

#### Lage datasett #########################

navnestat <- data.frame(navn, alder, per_tusen, kjonn, by)


#### Se på datasett #########################
View(navnestat)


#### Indeksering i datasett #########################

# Med klammeparanteser kan vi velge rad og kolonne. Rad kommer først, og så kolonnen:
# `datasett[rader, kolonner]`

navnestat[2, 1] 
navnestat[4, 3]

# Skriver vi en tom får vi alle kollonene/radene 
navnestat[, 2] 
navnestat[2, ]



#### Trekke ut variabler fra datasett #########################

#Her skriver vi først navnet på dataframen, og så variabelen: 
navnestat$per_tusen
navnestat$alder


#### Gjennomsnitt (med NA) #########################

# La oss prøve å få ut gjennomsnittlet til antall 
mean(navnestat$alder)

#### Fjerne NA ####

mean(navnestat$alder, na.rm = TRUE) 
median(navnestat$alder, na.rm = TRUE)


#### Summerende statestikk #########################

# Variabel
summary(navnestat$alder)

# Datasett
summary(navnestat)




#### Klasser i datasett #########################

class(navnestat$navn)
class(navnestat$alder)
class(navnestat$by)


#### Endre klasser i datasett #########################

# Her lager vi en ny variabel antall2 der vi ber R lagre alder som character
navnestat$antall2 <- as.character(navnestat$antall)


#### Logiske operasjoner #########################

1 == 2                                # tester om 1 er lik 2
2 == 2                                # tester om 2 er lik 2
"Statsvitenskap" == "statsvitenskap"  # Logiske tester kan også brukes på tekst
"statsvitenskap" == "statsvitenskap"  # R er imidlertid sensitivt til store og små bokstaver
1 <= 2                                # Tester om 1 er mindre enn eller lik 2
1 >= 2                                # Tester om 1 er større enn eller lik 2
1 != 2                                # Tester om 1 er ulik 2
1 == 2 | 1 == 1                       # Tester om en av de to påstandene 1 er lik 2 eller 1 er lik 1 er sanne
1 == 2 & 1 == 1                       # Tester om begge de to påstandene 1 er lik 2 og 1 er lik 1 er sanne


#### Installere pakker #########################

# Installere pakke
# install.packages("tidyverse")

# Laste inn / åpne pakke
library(tidyverse)


#### Lage figurer #########################

# I første argument spesifiserer vi datasettet og hvilken variabel vi vil plotte
# Vi legger til en + på slutten av linjen for å fortelle R at vi vil legge til flere lag
# Etter + skrive vi hva slags plot vi vil ha. 

ggplot(data = navnestat, aes(x = per_tusen)) + 
  geom_bar(bins = 30) 
