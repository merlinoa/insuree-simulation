library(shiny)
library(ggplot2)
library(scales)
library(insuree)
library(lubridate)

shinyServer(function(input, output) {
  
  insurees_data <- reactive({
    df <- read.csv(file = "data/policies.csv")
    df$dob <- as.Date(df$dob, format = "%m/%d/%Y")
    df
  })
  
  # record number of observations
  n <- reactive({
    input$obs
  })
  
  # male actuarial tables
  male_qx <- reactive({
    qx_male <- insuree::LifeTable(x = qx_data$x, q_x = qx_data$male_qx)
    qx_male <- insuree::ActuarialTable(i = rep(input$i, 
                                    times = length(qx_male@x)), qx_male)
  })
  
  # female actuarial table
  female_qx <- reactive({
    qx_female <- insuree::LifeTable(x = qx_data$x, q_x = qx_data$male_qx)
    insuree::ActuarialTable(i = rep(input$i, times = length(qx_female@x)), qx_female)
  })
  
  # find curtate age (could improve this)
  curtate_x_ <- reactive({
    as.numeric(year(Sys.Date()) - year(insurees_data()$dob)) 
  })
  
  # create insuree objects
  insurees <- reactive({
    my_table <- insurees_data()
    k <- curtate_x_()
    holder <- list()
    for (j in 1:nrow(my_table)) {
      if (my_table$gender[j] == "male") {
        holder[[j]] <- insuree::Insuree(x_ = k[j],
                                        t_ = my_table$term[j],
                                        m_ = my_table$deferral[j],
                                        benefit = rep(my_table$benefit[j],
                                                      times = my_table$term[j]),
                                        male_qx()
                                        )
      } else {
        holder[[j]] <- insuree::Insuree(x_ = k[j],
                                        t_ = my_table$term[j],
                                        m_ = my_table$deferral[j],
                                        benefit = rep(my_table$benefit[j],
                                                      times = my_table$term[j]),
                                        female_qx()
                                       )
      }
    }
    holder
  })
  
  # run simulation
  benefit <- reactive({
    out <-lapply(insurees(), function(k) insuree::rpv(k, n = input$obs)$pv)
    out <- matrix(unlist(out), ncol = input$obs, byrow = TRUE)
    apply(out, 2, sum)
  })
  
  # start of output ----------------------------------------------------------
  
  # insuree table ---------------------------------
  output$insuree_table <- DT::renderDataTable({
    insurees_data() 
  })
  
  #output$hist_plot <- renderPlot({
  #  ggplot(benefit(), aes(x = agg_loss)) +
  #    geom_histogram(fill = "white", colour = "black") +
  #    scale_x_continuous(labels = dollar) +
  #    xlab("Benefit per Observation") +
  #    ylab("Count of Observations") +
  #    ggtitle("Death Benefit Present Value")
  #})
  
  #output$cdf <- renderPlot({
  #  ggplot(benefit(), aes(agg_loss)) +
  #    stat_ecdf() +
  #    xlab("Benefit per Observation") +
  #    ylab("P(benefit <= x)") +
  #    scale_x_continuous(labels = dollar) +
  #    ggtitle("Death Benefit Present Value")
  #})
  
  data_table <- reactive({
    table_values <- function(obs) {
      percentile <- c(.999, 0.995, seq(0.99, 0.9, -0.01), seq(0.85, 0.05, by = -0.05))
      points <- quantile(obs, percentile)
      obs_mean <- mean(obs)
      c(obs_mean, points)
    }
    
    out <- table_values(benefit())
      
    cbind("Value At Risk" = c("mean", names(out)[-1]), out)
  })
  
  output$sorter <- DT::renderDataTable({
    data_table()
  })
  
  # create downloadable table
  #output$download_summary <- downloadHandler(
  #  filename = function() {paste0("ractuary-insuree-sim-", Sys.Date(), ".csv")},
  #  content = function(file) {
  #    write.csv(data_table(), 
  #      file = file,
  #      row.names = FALSE
  #    )
  #  }
  #)
})