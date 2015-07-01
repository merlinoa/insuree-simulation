library(shinydashboard)
library(DT)

header <- dashboardHeader(
            title = "Life Insurance"
          )

sidebar <- dashboardSidebar(
  dateInput("date", "Reserve Evaluation Date", value = Sys.Date(), 
            min = as.Date("2015-01-01"), 
            max = Sys.Date()
            ),
  sliderInput("i", 
              "Interest Rate", 
              min = 0,
              max = 0.10,
              step = 0.005,
              value = 0.04,
              ticks = TRUE),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Insurees", tabName = "insurees", icon = icon("group")),
    menuItem("Reserve Simulation", tabName = "simulation", icon = icon("money"),
      menuSubItem("Assumptions", tabName = "assumptions", icon = icon("tasks")),
      menuSubItem("Results", tabName = "results", icon = icon("fighter-jet"))
    )
  )
)  

body <- dashboardBody(  
  tabItems(
    # dashboard tab
    tabItem(tabName = "dashboard",
      fluidRow(
        column(width = 6,
          box(width = 12,
              title = "Reserve Confidence Level",
              solidHeader = TRUE,
              status = "success",
            sliderInput(label = "",
                        inputId = "ci",
                        value = 0.75,
                        min = 0.25,
                        max = 1.0,
                        step = 0.01
            )
          ),
          infoBoxOutput("reserve", width = 12)
        ),
        column(width = 6,
          tabBox(width = 12,
                 height = 200,
            tabPanel(title = "Histogram",
                     plotOutput("hist_plot",
                                height = 200)
            ),
            tabPanel(title = "CDF",
                     plotOutput("cdf",
                                height = 200)
            )
          )
        )
      ),
      fluidRow(
        box(width = 12,
            height = 175,
        title = "Number of Policies",
        valueBoxOutput("n_insurees", width = 4),
        valueBoxOutput("n_deferral", width = 4),
        valueBoxOutput("n_effective", width = 4)
        )
      ),
      fluidRow(
        box(width = 12,
            height = 175,
            title = "Undiscounted Benefits",
            valueBoxOutput("b_insurees", width = 4),
            valueBoxOutput("b_deferral", width = 4),
            valueBoxOutput("b_effective", width = 4)
        )
      ),
      fluidRow(
        valueBoxOutput("avg_age", width = 6)
      )
    ),
    # insurees table tab
    tabItem(tabName = "insurees",
      fluidRow(
        box(width = 12,
          DT::dataTableOutput("insuree_table")
        )
      )
    ),
    # simulation results tab
    tabItem(tabName = "results", 
      fluidRow(
        tabBox(width = 12,
          tabPanel(title = "Table",
                   DT::dataTableOutput("sorter")
          ),
          tabPanel(title = "Download"
          )
        )     
      )
    ),
    tabItem(tabName = "assumptions",
      fluidRow(
        box(width = 12, 
          h2("Mortality"),
          p("Mortality is simulated in accordance with the ", 
            a(href = "http://www.ssa.gov/oact/STATS/table4c6.html", 
            "Official US Social Secuity Actuarial Table"),".  The
            probability of death at future time periods depends on
            the age and gender of each individual insuree. The age of each
            insuree is recorded to the day, so the simulation uses different
            ages depending on the day it is run."),
          h2("Benefit / Policy"),
          p("The benefit payment for each policy is paid if the insuree dies
            within the term specified by the insuree's policy.  The insuree can 
            choose to have a deferral period before the term period begins.  If the
            insuree dies during the deferral period, no beath benefit will be paid.
            All death benefits are paid at the midpoint of the term year of death, unless the
            insuree dies during a partial year in which case the benefit is paid at the midpoint
            of the partial year period. (e.g. if the insuree is 75.5 at the reserve evaluation date,
            and the simulation has her dying between age 75.5 and age 76, the benefit benefit will be discounted for 0.25
            years.)"),
          h3("Simulation General"),
          p("The future life of each insuree is simulated 5,000 times.  The reserve confidence 
            levels are established for all insurees grouped together.  The ", code("insuree"), " 
            R package is used to run the simulation.  To learn more about the ", code("insuree"), 
            "package see ", a(href = "https://github.com/merlinoa/insuree", "the GitHub repository.")
          )
        )
      )
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)