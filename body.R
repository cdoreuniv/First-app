body = function() {
    dashboardBody(
      tags$head(
        tags$script(src = "howler.js/dist/howler.min.js"), 
        tags$style(HTML(
          '.skin-blue .main-header .logo {
            background-color: #57d53b ;
            font-family: "Gotham Black", sans-serif;
            font-weight: bold;
            font-size: 24px;
                                }
                        
                /* logo when hovered */
        .skin-blue .main-header .logo:hover {
            background-color: #57d53b;
            font-family: "Gotham Black", sans-serif;
            font-weight: bold;
            font-size: 24px;
                                }
                                
                /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
             background-color: #57d53b;
                                }
                                
                /* main sidebar */
        .skin-blue .main-sidebar {
              background-color: #303030;
                                }
                                
                 /* active selected tab in the sidebarmenu */
         .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
               background-color: #303030;
                                }
                                
                /* other links in the sidebarmenu */
          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                background-color: #303030;
                color: #FFFFFF;
                                }
                                
                /* other links in the sidebarmenu when hovered */
           .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
               background-color: #303030;
                                }
                /* toggle button when hovered  */
            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                background-color: #303030;
                                }

                /* body */
            .content-wrapper, .right-side {
                 background-color: #000000;
            }
                                
                                /* DataTable */
#caract_titre tbody tr.even {
    background-color: #000000;
    color: #000000;
}

#caract_titre tbody {
    background-color: #000000;
    color: #000000;
}
 
#recommend tbody tr.even {
    background-color: #000000;
    color: #000000;
}

#recommend tbody {
    background-color: #000000;
    color: #000000;
}
                                '
        )
        )),
      
      tabItems(
        ### -------------------   PAGE 1  : Top on the flop -------------- ###  
        tabItem( 
          tabName = "top20",
          fluidRow(
            h1(
              "Les 20 artistes les plus populaires entre 2017 et 2020",
              style = "text-align: center; color: #81b71a;" ),
            box(# Graphique pour afficher les 20 artistes les plus écoutés
               #title = "Top 20 artistes",
               solidHeader = TRUE,
               plotlyOutput(outputId = "plus_ecoutes_20"),
               width = 7,
               background = "black"
                ),
            valueBox( nb_edshe$nb,
                      "Nombre de chansons dans les charts de l'artiste #1 ", 
                       width = 5,
                      icon = icon("arrow-up"), 
                      color = "light-blue"
            ),
            valueBox( best_edshe$Title,
                      "Titre le plus écouté de l'artiste #1",
                      width = 5, 
                      icon = icon("music"),
                      color = "blue"
            ),
            valueBox( best_al$Album,
                      "Album le plus écouté de l'artiste #1",
                      width = 5, 
                      icon = icon("album-collection"),
                      color = "purple"
            )
           
            
          ),
            
          h1(
            "Les genres les plus populaires entre 2017 et 2020",
            style = "text-align: center; color: #81b71a;" ),
          fluidRow(# Liste déroulante du niveau de précision des genres à choisir
            column(width = 5,
              box(
               width = 12,
                selectInput(inputId = "level", 
                          label = "Niveau de précision ",
                          choices = c("Niveau 1" = "Genre_new", # 21 genres de musiques
                                      "Niveau 2" = "Genre") # Niveau de précision supérieur (1120 genres)
                          ),
               background = "black"),
              valueBoxOutput("nb_titre_genre", width = 12),
              valueBoxOutput("chanson_genre", width = 12)),
             column( width = 7,
              box(#Graphique pour afficher les 20 genres les + écoutés
                #title = "Top 20 genres",
                solidHeader = TRUE,
                plotlyOutput(outputId = "top_genres"), 
                width = 12,
                background = "black"
            ))
            
          ),
          fluidRow(
            h1(
              "Les mots les plus fréquents dans les titres de chanson",
              style = "text-align: center; color: #81b71a;" ),
            box(
              style = "height:500px;",
              plotOutput(outputId = "cloud_words"),
              background = "black")
          )
          ),
        
        ### -------------------   PAGE 2 : Bathroom Singer -------------- ###   
          tabItem( 
            tabName = "artist",
            fluidRow(
            box(# Liste déroulante pour sélectionner l'artiste
                   width = 4,
                   selectInput(inputId = "artiste",
                               label = "Sélectionnez un artiste:",
                               choices = unique(data1$Artist)),
                   solidHeader = FALSE,
                   background = "black"
                        ),
            box(# Graphique pour afficher la popularité de l'artiste dans chaque pays
                   width = 8,
                   plotlyOutput(outputId = "popularite_artiste_pays"),
                   solidHeader = FALSE,
                   background = "black" 
                        )
                       ),
            fluidRow(
              style = "height:200px;",
              column( 
                width = 2,
                plotlyOutput(outputId = "dance_art", height = "200px")),
              #tags$hr(style = "margin-left: 5px; margin-right: 5px;")),
                column( 
                  width = 2,
                  plotlyOutput(outputId = "energy_art", height = "200px")),
                column( 
                  width = 2,
                  plotlyOutput(outputId = "loudness_art", height = "200px")),
              column( 
                width = 2,
                plotlyOutput(outputId = "speechiness_art", height = "200px")),
              column( 
                width = 2,
                plotlyOutput(outputId = "tempo_art", height = "200px")),
              column( 
                width = 2,
                plotlyOutput(outputId = "duration_art", height = "200px")),
              
                            ),
            tags$hr(style="margin-top: 20px; margin-bottom: 20px;"),
            
            fluidRow(
              box(# Liste déroulante pour sélectionner le titre de l'artiste
                width = 3,
                selectInput(inputId = "titre_input",
                            label = "Sélectionnez un titre :",
                            choices = NULL),
                background = "black"
                ),
              box(# Afficher les caractéristiques du titre
                width = 9,
                dataTableOutput(outputId = "caract_titre")
                #background = "black"
              )
            ),
            
            fluidRow(
              h3(
                "Recommendations de titres proches de celui sélectionné",
                style = "text-align: center; color: #81b71a;" ),
              box(# Afficher les recommendations
                width = 12,
                dataTableOutput(outputId = "recommend")
                #background = "black"
                
            )
           )
           
)
)
)
}