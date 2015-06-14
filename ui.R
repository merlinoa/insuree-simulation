library(shinythemes)

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
        numericInput("n_insurees", "Number of Insurees", value = 5)
      ),
        
      wellPanel( 
        numericInput("x_", "Age",
          value = 50
        ),
        selectInput("gender", "Gender",
                     choices = c("Male", "Female")
        ),
        numericInput("m_", "Deferral",
                     value = 0
        ),
        numericInput("t_", "Term",
          value = 5
        )
      ),
      
      wellPanel(
        h5("Frequency Parameters"),
        uiOutput("freq_param_boxes"),
        br(),
        h5("Severity Parameters"),
        uiOutput("sev_param_boxes"),
        actionButton("run_freq", "Run Simulation")
      )
    ),
    
    column(9, 
      mainPanel(
        p("Mortality is simulated in accordance with the "
        , a(href = "http://www.ssa.gov/oact/STATS/table4c6.html", 
        "Official US Social Secuity Actuarial Table")),
        
        tabsetPanel(
          tabPanel("Histogram", 
            plotOutput("hist_plot"),
            plotOutput("hist_plot_total")
          ),
          tabPanel("CDF", 
            plotOutput("cdf"),
            plotOutput("cdf_total")
          ),
          tabPanel("VaR Table", 
            dataTableOutput("sorter")
          ),
          tabPanel("Download", 
            wellPanel(
              h5("Download Summary Table"),
                downloadButton("download_summary", "Download Summary")
            )
          )
        )
      )
    )
  )
))