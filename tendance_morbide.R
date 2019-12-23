# reproduit la figure du tweet de Baptiste Coulmont
# https://twitter.com/coulmont/status/1206549946176098304?s=19
# sur les deces en France

# donnees disponibles depuis
# https://www.data.gouv.fr/en/datasets/fichier-des-personnes-decedees/
# voir dans les Community contributions pour recuperer un csv
# via le lien suivant 
# https://www.data.gouv.fr/fr/datasets/r/f5465d95-e0f3-42b4-9bed-9db2c0d40261

library(tidyverse) # manipulation et visualisation
library(lubridate) # manipulation donnees date

# lit et inspecte donnees
deces_brut <- read_csv('insee_deces.csv')
glimpse(deces_brut)
deces_brut

# deces_brut <- deces_brut[sample(10000),] # decommenter pour continuer sur un sous-echantillon

# formate donnees (prend qqs minutes)
deces_formate <- deces_brut %>% 
  filter(date_deces >= 1970) %>% # focalise sur les deces de 1970 a aujourd hui
  mutate(date_naissance = ymd(date_naissance), # format date
         date_deces = ymd(date_deces), # format date
         age_deces = round(as.numeric(date_deces - date_naissance)/365), # calcule age en annees a la louche (ignore annees bissextiles)
         classe_age = case_when(
           between(age_deces,0,9)  ~ "0 - 9 ans",
           between(age_deces,10,19)  ~ "10 - 19 ans",
           between(age_deces,20,29)  ~ "20 - 29 ans",
           between(age_deces,30,39)  ~ "30 - 39 ans",
           between(age_deces,40,49)  ~ "40 - 49 ans",
           between(age_deces,50,59)  ~ "50 - 59 ans",
           between(age_deces,60,69)  ~ "60 - 69 ans",
           between(age_deces,70,79)  ~ "70 - 79 ans",
           age_deces > 79  ~ "80 ans et plus"), # cree variable int_age pour periode d age
         decade_deces = case_when(
           between(year(date_deces),1970,1979)  ~ "1970",
           between(year(date_deces),1980,1989)  ~ "1980",
           between(year(date_deces),1990,1999)  ~ "1990",
           between(year(date_deces),2000,2009)  ~ "2000",
           between(year(date_deces),2010,2019)  ~ "2010"), # cree variable pour periode de temps
         jour_deces = as_factor(weekdays(date_deces))) %>% # jour du deces dans la semaine (1 = lundi, 2 = mardi, ...)
  filter(age_deces>=0) # ne prend que les ages >= 0

# jette un coup d oeil
glimpse(deces_formate)
deces_formate

# combien de deces par sexe?
deces_formate %>% 
  count(sexe) # 2 = femmes, 1 = homme

# combien de deces par jour et periode de temps?
deces_formate %>% 
  count(jour_deces, decade_deces)

# combien de deces par jour et classe d age?
deces_formate %>% 
  count(jour_deces, classe_age)

# compte et visualise
deces_formate %>% 
  count(jour_deces, classe_age, decade_deces) %>% 
  group_by(classe_age, decade_deces) %>%
  mutate(pct = n / sum(n)) %>% # calcule proportion de deces par jour par classe d age
  ggplot() +
  aes(x = factor(jour_deces,levels = c('Lundi','Mardi','Mercredi','Jeudi','Vendredi','Samedi','Dimanche')), # reordonne les jours de la semaine
      y = pct, 
      group = decade_deces) +
  geom_line(aes(color = decade_deces)) + # lignes
  geom_point(aes(color = decade_deces)) + # points
  scale_y_continuous(labels = scales::percent) + # represente des pourcentage
  guides(color=guide_legend(title="Période")) + # change titre de la legende
  facet_wrap(vars(classe_age)) + # une figure par classe d age
  theme_minimal() + # theme minimal
  scale_x_discrete(labels=c("Lundi" = "Lun", 
                            "Mardi" = "Mar",
                            "Mercredi" = "Mer",
                            "Jeudi" = "Jeu",
                            "Vendredi" = "Ven",
                            "Samedi" = "Sam",
                            "Dimanche" = "Dim")) + # change labels de l axe des abscisses
  theme(axis.text.x = element_text(angle=45)) + # incline le nom des jours
  theme(strip.text = element_text(hjust = 0)) + # justifie a gauche titre des figures
  xlab('') + # pas de titre aux X
  ylab('') + # pas de titre aux Y
  labs(title = 'Le jour de la mort, en fonction de l\'âge au décès') + # titre
  labs(subtitle = 'Proportion des décès qui ont lieu tel jour de la semaine, en fonction de l\'âge') + # sou-titre
  labs(caption = 'Source : INSEE, fichier des décès. Calculs : B. Coulmont') # note

# sauve la figure
ggsave('deces_coulmont.png', width = 6, height = 6)


