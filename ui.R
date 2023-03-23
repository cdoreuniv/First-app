source("librairies.R")
source("nettoyage.R")
source("header.R")
source("sidebar.R")
source("body.R")

ui = dashboardPage(
    header(),
    sidebar(),
    body()
  )
