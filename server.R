library(shiny)
library(ggplot2)
library(scales)
library(insuree)
library(lubridate)

# number of observations
n <- 5000

shinyServer(function(input, output) {
  
  insurees_data <- reactive({
    df <- read.csv(file = "data/policies.csv")
    df$dob <- as.Date(df$dob, format = "%m/%d/%Y")
    df
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
    out <-lapply(insurees(), function(k) insuree::rpv(k, n = n)$pv)
    out <- matrix(unlist(out), ncol = n, byrow = TRUE)
    apply(out, 2, sum)
  })
  
  # start of display ----------------------------------------------------------
  # dashboard -----------------------------
  output$n_insurees <- renderValueBox({
    total_insurees <- nrow(insurees_data())
    valueBox(
      value = total_insurees,
      subtitle = "In Force Policies",
      icon = icon("group")
    )
  })
  
  output$avg_age <- renderValueBox({
    a_age <- mean(curtate_x_())
    valueBox(
      value = a_age,
      subtitle = "Average Insuree Age",
      icon = icon("heartbeat"),
      color = "yellow"
    )
  })
  
  output$reserve <- renderText({
    format(round(quantile(benefit(), input$ci),0), big.mark = ",")
  })
  
  # insuree table ---------------------------------
  output$insuree_table <- DT::renderDataTable({
    datatable(insurees_data(),
              rownames = FALSE
              ) %>%
      formatCurrency("benefit")
  })
  
  output$hist_plot <- renderPlot({
    ggplot(data.frame(loss = benefit()), aes(x = loss)) +
      geom_histogram(fill = "white", colour = "black") +
      scale_x_continuous(labels = dollar) +
      xlab("Benefit Payments") +
      ylab("Count of Observations") +
      ggtitle("Present Value of Death Benefit for all Insurees")
  })
  
  output$cdf <- renderPlot({
    ggplot(data.frame(loss = benefit()), aes(x = loss)) +
      stat_ecdf() +
      xlab("Benefit Payments") +
      ylab("P(benefit <= x)") +
      scale_x_continuous(labels = dollar) +
      ggtitle("Present Value of Death Benefit for all Insurees")
  })
  
  data_table <- reactive({
    table_values <- function(obs) {
      percentile <- c(0.995, 0.99, seq(0.95, 0.05, by = -0.05))
      points <- quantile(obs, percentile)
      obs_mean <- mean(obs)
      c(obs_mean, points)
    }
    
    out <- table_values(benefit())
      
    datatable(cbind("Confidence Level" = c("mean", names(out)[-1]), "Value" = round(out, 0)),
                    rownames = FALSE) %>%
                formatCurrency(2)
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