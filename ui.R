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
        box(width = 6,
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
        valueBoxOutput("reserve", width = 6)
      ),
      fluidRow(
        valueBoxOutput("n_insurees", width = 6),
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
          tabPanel(title = "Histogram",
                   plotOutput("hist_plot")
          ),
          tabPanel(title = "CDF",
                   plotOutput("cdf")
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
            the age and gender of each individual insuree.  Curtate age is used for
            the insueer's age (i.e. the age is rounded down to the nearest whole year) (e.g 
            if the insurees is 50 and 3 months, 50 will be used for that insuree's age.)"),
          h2("Benefit / Policy"),
          p("The benefit payment for each policy is paid if the insuree dies
            within the term specified by the insuree's policy.  The insuree can 
            choose to have a deferral period before the term period begins.  If the
            insuree dies during the deferral period, no beath benefit will be paid.
            All death benefits are paid at the end of the year of death.  For simplification
            all policies are assume to be effective today"),
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