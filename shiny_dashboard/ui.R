
shinyUI(
  dashboardPage(
      dashboardHeader(title="MOVADO"),
      dashboardSidebar(
          sidebarMenu(
            menuItem("Movado Insight", tabName = "movado", icon = icon("info")),
            menuItem("Retailers",
              menuSubItem("Macy's", tabName = "macys"),
              menuSubItem("Nordstrom", tabName = "nordstrom"),
              menuSubItem("Amazon", tabName = "amazon")
                    ),
            menuItem("Competitors",
                     menuSubItem("Tag Heuer", tabName = "tag_heuer"),
                     menuSubItem("Other", tabName = "other")
                     )
          )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "movado",
                  fluidRow(plotOutput("movado_price") )
                  ),
          tabItem(tabName = "macys",
                  fluidRow(plotOutput("macys_stars")),
                  fluidRow(plotOutput("macys_review_count")),
                  fluidRow(plotOutput("macys_price")),
                  ),
          tabItem(tabName = "nordstrom",
                  fluidRow(plotOutput("nordstrom_stars")),
                  fluidRow(plotOutput("nordstrom_review_count")),
                  fluidRow(plotOutput("nordstrom_price")),
                  ),
          tabItem(tabName = "amazon", "AMAZON PAGE"
                  ),
          tabItem(tabName = "tag_heuer", "competition 1"
                  ),
          tabItem(tabName = "other", "competition 2")
        )
      )
  )
)