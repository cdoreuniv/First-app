# Définition des palettes de couleurs
source("nettoyage.R")

palette1 = brewer.pal(8, "Set2")
palette2 = brewer.pal(12, "Set3")

my_palette = c(palette1, palette2)

server = function(input, output, session ) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("sparkles"))
    )
  })
  
### -------------------   PAGE 1  : Top on the flop -------------- ###   
  ### Graphique des 20 artistes les plus écoutés ###
  output$plus_ecoutes_20 = renderPlotly({
    ggplot(data = plus_ecoutes, 
           aes(
             x = fct_reorder(Artist, Popularite), 
             y = Popularite, 
             fill = Artist)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = my_palette) + 
      coord_flip() + 
      labs(
        title = "Les 20 artistes les plus écoutés", 
        x = "Artistes",
        y = "Popularité"
      ) + 
      theme_classic() +
      guides(fill = "none")
  })
  

  # Graphique des genres les + écoutés
  
  datagenre11 = renderUI({
    datagenre1 %>%
    top_n(input$slgenre, wt = Popularite) 
  })
  datagenre21 = renderUI({
    datagenre2 %>%
    top_n(input$slgenre, wt = Popularite)
  })
  
  output$top_genres = renderPlotly({
    if (input$level == "Genre_new") {
      ggplot(datagenre1, 
             aes(
               x = fct_reorder(Genre, Popularite),
               y = Popularite, 
               fill = Genre)) +
        geom_bar(stat = "identity") + 
        scale_fill_manual(values = my_palette) + 
        coord_flip() + 
        labs(
          title = "Les 5 genres les plus écoutés", 
          x = "Popularité",
          y = "Genre"
        ) + 
        theme_classic() +
        guides(fill = "none")
    } else {
      ggplot(datagenre2, 
             aes(
               x = fct_reorder(Genre, Popularite),
               y = Popularite, 
               fill = Genre)) +
        geom_bar(stat = "identity") + 
        scale_fill_manual(values = my_palette) + 
        coord_flip() + 
        labs(
          title = "Les 20 genres les plus écoutés", 
          x = "Popularité",
          y = "Genre"
        ) + 
        theme_classic() +
        guides(fill = "none")
    }
  })
  
  ## ValueBox du nombre de chanson du genre 1
  output$nb_titre_genre = renderValueBox({
    if (input$level == "Genre_new") {
      df = data1 %>%
        group_by(Genre = Genre_new) %>%
        filter(Genre == "pop") %>%
        summarise(nb = n(), .groups = "drop") 
      valueBox(
        tagList(tags$sup("Nombre de chansons du genre 1 dans les charts", style="font-size: 18px")),
        df$nb,
        icon = icon("arrow-up"),
        color = "green", 
        width = 5)
    }else{
      df = data1 %>%
        group_by(Genre = Genre) %>%
        filter(Genre == "dance pop") %>%
        summarise(nb = n(), .groups = "drop") 
      valueBox(
        tagList(tags$sup("Nombre de chansons du genre 1 dans les charts", style="font-size: 18px")),
        df$nb, 
        icon = icon("arrow-up"),
        color = "green",
        width = 5)
    }
  })
  
  ## ValueBox de la chanson du genre la plus écoutée
  output$chanson_genre = renderValueBox({
    if (input$level == "Genre_new") {
      df = data1 %>%
        group_by(Genre = Genre_new, Titre = Title) %>%
        filter(Genre == "pop") %>%
        summarise(Popularite = sum(Popularity), .groups = "drop") 
      max = max(df$Popularite)
      chan = df[df$Popularite == max, 2]
      valueBox(
        tagList(tags$sup("Chanson du genre #1 la plus écoutée", style="font-size: 18px")),
        paste0(chan$Titre,"(",data1[data1$Title == chan$Titre, 5][1],")"), 

        icon = icon("repeat"),
        color = "yellow", 
        width = 12)
    }else{
      df = data1 %>%
        group_by(Genre = Genre, Titre = Title) %>%
        filter(Genre == "dance pop") %>%
        summarise(Popularite = sum(Popularity), .groups = "drop") 
      max = max(df$Popularite)
      chan = df[df$Popularite == max, 2]
      valueBox(
        tagList(tags$sup("Chanson du genre #1 la plus écoutée", style="font-size: 18px")),
        paste0(chan$Titre, "(", data1[data1$Title == chan$Titre, 5][1] ,")"),
        icon = icon("repeat"),
        color = "yellow", 
        width = 12)
    }
  })
    
    #### Nuage de mots
    output$cloud_words = renderPlot({
      text = data1$Title
      # Create a corpus  
      docs = Corpus(VectorSource(text))
      docs = docs %>%
        tm_map(removeNumbers) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace)
      docs = tm_map(docs, content_transformer(tolower))
      docs = tm_map(docs, removeWords, stopwords("english"))
      docs = tm_map(docs, removeWords,c("feat","edit","remix","remastered","remaster","radio","version","original","mix"))
      dtm = TermDocumentMatrix(docs) 
      matrix = as.matrix(dtm) 
      words = sort(rowSums(matrix),decreasing=TRUE) 
      df = data.frame(word = names(words),freq=words)
      wordcloud(words = df$word, freq = df$freq, scale=c(5,0.75),min.freq = 1,
                max.words=150, random.order=FALSE, rot.per=0.25, 
                colors=my_palette)
    })
  


### -------------------   PAGE 2  -------------- ###    
  ### Filtrer les données en fonction de l'artiste sélectionné ###
  plus_ecoutes_pays = reactive({
    data1 %>%
      group_by(Country = Country, Artist = Artist) %>%
      summarise(Popularite = sum(Popularity), .groups = "drop") %>%
      filter(Artist == input$artiste) %>%
      top_n(20, wt = Popularite) %>% 
      arrange(desc(Popularite))
  })
  
  choix_artiste = reactive({
    data1 %>%
      group_by(Artist = Artist) %>%
      filter(Artist == input$artiste) 
  })
  
  ### Créer le graphique représentant la popularité de l'artiste sélectionné dans chaque pays ###
  output$popularite_artiste_pays = renderPlotly({
    ggplot(plus_ecoutes_pays(), aes(x = fct_reorder(Country, Popularite), y = Popularite, fill = Country)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = my_palette) +
      labs(title = paste0("Popularité de l'artiste ", input$artiste, " dans chaque pays"),
           x = "Pays",
           y = "Popularité") +
      theme_classic() +
      guides(fill = "none")
  })
  
  ### Caractéristiques musicales de l'artiste sélectionné ###
  perform_art = reactive({
      data1 %>%
        group_by(Title = Title) %>%
        filter(Artist == input$artiste) %>%
        select(Title, danceability, energy, loudness, speechiness, tempo, duration_s) %>%
        distinct()
  })
  
  create_histo = function(df,variable){
    ggplot(df, aes(variable)) +
      geom_histogram(color = my_palette, bins = 10) +
      theme_classic()
  }
  #### Graphique de la densité de "danceability" de l'artiste sur l'ensemble des albums des charts ####
  output$dance_art = renderPlotly({
    ggplot(perform_art(), aes(danceability)) +
      geom_histogram(bins = 10, color = "#FFFFFF", fill = "#57d53b") +
      #geom_density()+
      labs(
        title = "Danceability") +
      theme(panel.background = element_rect(fill = "#000000"),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            plot.title = element_text(color = "#57d53b", size = 12, face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "#000000")
      )
  })
  #### Graphique de la densité de "energy" de l'artiste sur l'ensemble des albums des charts ####
  output$energy_art = renderPlotly({
    ggplot(perform_art(), aes(energy)) +
      geom_histogram( bins = 10, fill = "#57d53b", color = "#FFFFFF") +
      #geom_density() +
      labs(
      title = "Energy") + 
      theme(panel.background = element_rect(fill = "#000000"),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            plot.title = element_text(color = "#57d53b", size = 12, face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "#000000")
            )
  })
  #### Graphique de la densité de "loudness" de l'artiste sur l'ensemble des albums des charts ####
  output$loudness_art = renderPlotly({
    ggplot(perform_art(), aes(loudness)) +
      geom_histogram( bins = 10, fill = "#57d53b", color = "#FFFFFF") +
      #geom_density()+
      labs(
        title = "Loudness"
      )+
      theme(panel.background = element_rect(fill = "#000000"),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            plot.title = element_text(color = "#57d53b", size = 12, face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "#000000")
      )
  })
  #### Graphique de la densité de "speechiness" de l'artiste sur l'ensemble des albums des charts ####
  output$speechiness_art = renderPlotly({
    ggplot(perform_art(), aes(speechiness)) +
      geom_histogram( bins = 10, fill = "#57d53b", color = "#FFFFFF") +
      #geom_density()+
      labs(
        title = "Speechiness"
      )+
      theme(panel.background = element_rect(fill = "#000000"),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            plot.title = element_text(color = "#57d53b", size = 12, face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "#000000")
      )
  })
  #### Graphique de la densité de "tempo" de l'artiste sur l'ensemble des albums des charts ####
  output$tempo_art = renderPlotly({
    ggplot(perform_art(), aes(tempo)) +
      geom_histogram( bins = 10, fill = "#57d53b", color = "#FFFFFF") +
      #geom_density()+
      labs(
        title = "Tempo"
      )+
      theme(panel.background = element_rect(fill = "#000000"),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            axis.title.y = element_blank(),
            plot.title = element_text(color = "#57d53b", size = 12, face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "#000000")
      )
  })
  
  #### Graphique de la densité de "duration_ms" de l'artiste sur l'ensemble des albums des charts ####
  output$duration_art = renderPlotly({
    ggplot(perform_art(), aes(duration_s)) +
      geom_histogram(bins = 6, fill = "#57d53b", color = "#FFFFFF") +
      #geom_density()+
      labs(
        title = "Duration"
      )+
      theme(axis.title.y = element_blank()) + 
      theme(panel.background = element_rect(fill = "#000000"),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            plot.title = element_text(color = "#57d53b", size = 12, face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "#000000")
      )
  })
  
  ### Sélection du titre de l'artiste
  observeEvent(input$artiste, {
    titre_select = data1 %>% 
      filter(Artist == input$artiste) %>% 
      select(Title)
    titre_art = unique(titre_select)
    updateSelectInput(session, "titre_input", choices = titre_art$Title)
  })
  
  carac_chanson = reactive(
    data1 %>%
      filter(Title == input$titre_input, Artist == input$artiste) %>%
      group_by(Title = Title) %>%
      summarise(Danceability = mean(danceability), 
                Popularity = sum(Popularity),
                Energy = mean(energy), 
                Loudness = mean(loudness), 
                Speechiness = mean(speechiness), 
                Tempo = mean(tempo), 
                Duration = mean(duration_s))
    )
  
  output$caract_titre = renderDataTable(carac_chanson(),
                                        options = list(
                                          searching = FALSE,
                                          ordering = FALSE,
                                          paging = FALSE,
                                          pageLength = 1,
                                          stripe = FALSE,
                                        style = list(
                                          'background-color' = '#000000', # Fond noir
                                          'color' = '#FFFFFF' # Texte blanc
                                        )))

  
  
euclidean_distance = reactive({
    m = length(datatitre[,5:11])
    n = nrow(datatitre)
    res = datatitre[,1:4] 
    data = matrix(0, ncol = m, nrow = n)
    vartot = datatitre[, 5:11]
    varsel = carac_chanson()[1,2:8]
    for(j in 1:m){
      for(i in 1:n){
        data[i,j] = sqrt(sum((vartot[i,j] - varsel[1,j])^2))
      }
    }
    data = as.data.frame(data)
    colnames(data) = c("Danceability", "Popularity", "Energy", "Loudness", "Speechiness", "Tempo", "Duration")
    res = cbind(res, data)
    res =  res %>%
      arrange(Danceability, Energy, Loudness, Speechiness, Tempo, Duration)
    return(res)
  })



output$recommend = renderDataTable(euclidean_distance()[2:6,],
                                      options = list(
                                        searching = FALSE,
                                        ordering = FALSE,
                                        paging = FALSE,
                                        pageLength = 1,
                                        stripe = FALSE,
                                        #"page" = list("background-color" = "#000000"),
                                        # Set font color to white
                                       # "search" = list("color" = "#FFFFFF"),
                                        #"length_menu" = list("color" = "#FFFFFF"),
                                        "info" = list("color" = "#FFFFFF"),
                                        "paginate" = list("color" = "#FFFFFF", "background-color" = "#000000")
                                     )
)
}



