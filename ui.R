library(shinydashboard)
library(DT)

header <- dashboardHeader(
            title = "Life Insurance"
          )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Insurees", tabName = "insurees", icon = icon("group")),
    menuItem("Benefit Reserve", tabName = "benefit", icon = icon("money"),
      menuSubItem("Assumptions", tabName = "assumptions", icon = icon("tasks")),
      menuSubItem("Simulation", tabName = "simulation", icon = icon("fighter-jet"))
    )
  )
)  

body <- dashboardBody(  
  tabItems(
    # dashboard tab
    tabItem(tabName = "dashboard"),
    # insurees table tab
    tabItem(tabName = "insurees",
      fluidRow(
        box(width = 12,
          DT::dataTableOutput("insuree_table")
        )
      )
    ),
    # benefit reserve tab
    tabItem(tabName = "simulation",
      fluidRow( 
        fluidRow(
          box(width = 12,
              DT::dataTableOutput("sorter")
          )
        )
        
      )
    ),
    tabItem(tabName = "assumptions",
      box(width = 3,
          sliderInput("obs", "# of Observations", min = 1000, max = 10000, 
                      value = 1000, step = 1000, ticks = FALSE),
          sliderInput("i", 
                      "Interest Rate", 
                      min = 0,
                      max = 1,
                      step = 0.01,
                      value = 0.04,
                      ticks = FALSE),
          actionButton("run_sim", "Run Simulation")
      ),
      box(width = 9, 
          p("Mortality is simulated in accordance with the ", 
            a(href = "http://www.ssa.gov/oact/STATS/table4c6.html", 
              "Official US Social Secuity Actuarial Table"))
      )
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)