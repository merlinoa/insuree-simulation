library(shinythemes)
library(DT)

shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  includeCSS("ractuary-style.css"),
  
  fluidRow(  
    headerPanel(tags$div(a(img(src = "ractuary-logo.png", width = 50), 
                         href = "http://www.ractuary.com/shiny/"),
                       h1("Life Insurance Simulation")
      ), 
    windowTitle = "Life Insurance Simulation"
    )
  ),
  
  br(),
  
  fluidRow(
    column(3, 
      
      wellPanel(
        sliderInput("obs", "# of Observations", min = 1000, max = 10000, 
          value = 1000, step = 1000, ticks = FALSE),
        numericInput("n_insurees", "Number of Insurees", value = 5),
        sliderInput("i", 
                    "Interest Rate", 
                    min = 0,
                    max = 1,
                    step = 0.01,
                    value = 0.04,
                    ticks = FALSE)
      ),
        
      wellPanel( 
        numericInput("x_", "Age",
          value = 50
        ),
        selectInput("gender", "Gender",
                     choices = c("Male", "Female"),
                    selected = "Male"
        ),
        numericInput("m_", "Deferral",
                     value = 0
        ),
        numericInput("t_", "Term",
          value = 5
        ),
        numericInput("benefit", "Death Benefit",
                     value = 500000
        )
      ),
      
      wellPanel(
        actionButton("run_sim", "Run Simulation")
      )
    ),
    
    column(9, 
      mainPanel(
        p("Mortality is simulated in accordance with the "
        , a(href = "http://www.ssa.gov/oact/STATS/table4c6.html", 
        "Official US Social Secuity Actuarial Table")),
        
        tabsetPanel(
          tabPanel("Histogram", 
            dataTableOutput("test")
          ),
          #tabPanel("CDF", 
          #  plotOutput("cdf")
          #),
          tabPanel("VaR Table", 
            DT::dataTableOutput("sorter")
          )
          #tabPanel("Download", 
          #  wellPanel(
          #    h5("Download Summary Table"),
          #      downloadButton("download_summary", "Download Summary")
          #  )
          #)
        )
      )
    )
  )
))