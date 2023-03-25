# =============================================================================#
# Préparation de la base de données pour les analyses portées 
# Création de base de données secondaires utilisées dans les fonctions ui et server
# =============================================================================#

# ------------- Récupération de la base de données brutes ------------- # 
data1 = read.csv2("data/Final database.csv", sep = ",", dec = ".")

# ------------- Récupération du nom des colonnes ------------- #
## Pas forcément nécessaire dans cette étude mais toujours intéressant d'observer le contenu d'une base de données

#columnsnames = colnames(data1)
#columnsnames

# ------------- Retrait des lignes contenat des anomalies ------------- #
data1 = data1 %>%
  filter(Uri != "#", danceability != "n")

# ------------- Retrait des colonnes non nécessaires pour notre étude ------------- #

data1 = data1[,c(1:56,113:151)]

# ------------- Recodage des variables numériques ------------- #
data1$Popularity = as.numeric(data1$Popularity)
data1[,14:25] = lapply(data1[,14:25],FUN = as.numeric)
data1 = na.omit(data1)

data1$duration_s = data1$duration_ms / 1000

# ------------- Regrouper les valeurs mondiales ------------- #
dataglob = data1 %>%
  filter(Country == "Global")

# ------------- Retirer les valeurs mondiales ------------- # 
data1 = data1 %>%
  filter(Country != "Global") %>%
  arrange(Artist)

# Détermination du nombre de musiques par genre musicaux
# Détermination des genres les plus produits
datagenre1 = data1 %>%
  group_by(Genre = Genre_new) %>%
  summarise(Popularite = sum(Popularity)) %>%
  top_n(5, wt = Popularite) %>%
  arrange(desc(Popularite))
# datagenre1

datagenre2 = data1 %>%
  group_by(Genre = Genre) %>%
  summarise(Popularite = sum(Popularity)) %>%
  top_n(20, wt = Popularite) %>%
  arrange(desc(Popularite))
#datagenre2

plus_ecoutes = data1 %>% 
  group_by(Artist = Artist) %>%
  summarise(Popularite = sum(Popularity)) %>%
  top_n(20, wt = Popularite) %>%
  arrange(desc(Popularite)) 
  

## Plus écoutés par pays
### On demande à l'utilisateur de rentrer un artiste 
### et on lui renvoit les pays où l'artiste est le plus écoutés dans ce pays

plus_ecoutes_pays = data1 %>%
  group_by(Country = Country, Artist = Artist) %>%
  summarise(Popularite = sum(Popularity), .groups = "drop") %>%
#  filter(Artist == "The Weeknd") %>%
  top_n(20, wt = Popularite) %>% 
  arrange(desc(Popularite)) 

## Données de l'artiste le plus écouté : Ed Sheeran
# Artiste le plus écouté : Ed Sheeran
edshe = data1 %>% 
  group_by(Title, Artist) %>%
  filter(str_detect(Artist, "Ed Sheeran") == TRUE) %>%
  select(Title, Album, Popularity, Artist)
  
pop_edsher = edshe %>%
  summarise(Popularite = sum(Popularity), .groups = "drop") 

nb_edshe = pop_edsher %>%
  summarise(nb = n())

max = max(pop_edsher$Popularite)
best_edshe = pop_edsher[pop_edsher$Popularite == max, c(1,3)]


edshe_al = data1 %>% 
  group_by(Album ) %>%
  filter(str_detect(Artist, "Ed Sheeran") == TRUE) %>%
  select(Title, Album, Popularity, Artist) %>%
  summarise(Popularite = sum(Popularity), .groups = "drop") 

nb_al = edshe_al %>%
  summarise(nb = n())

max_al = max(edshe_al$Popularite)
best_al = edshe_al[edshe_al$Popularite == max_al,]

datatitre = 
  data1 %>%
  group_by(Title = Title, Artist = Artist, Album = Album, Genre = Genre_new) %>%
  summarise(Danceability = mean(danceability), 
            Popularity = sum(Popularity),
            Energy = mean(energy), 
            Loudness = mean(loudness), 
            Speechiness = mean(speechiness), 
            Tempo = mean(tempo), 
            Duration = mean(duration_s))
