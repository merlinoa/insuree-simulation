library(shiny)
library(ggplot2)
library(scales)
library(insuree)

shinyServer(function(input, output) {
  
  # record number of observations
  n <- reactive({
    input$obs
  })
  
  # number of insurees
  #n_insuree <- reactive({
  #  input$n_insuree
  #})
  
  # create input boxes for n_insurees
  #output$insuree_boxes <- renderUI({
    #lapply(1:n_insuree(), function(i) {
    #  numericInput(inputId = paste0("insuree", i), 
    #              label = "Age", 
    #              value = "Term"
    #              )
    #})
  #})
  
  # actuarial table
  a_table <- reactive({
    if (input$gender == "Male") {
      qx_male <- insuree::LifeTable(x = qx_data$x, q_x = qx_data$male_qx)
      insuree::ActuarialTable(i = rep(input$i, times = length(qx_male@x)), qx_male)
    } else {
      qx_female <- insuree::LifeTable(x = qx_data$x, q_x = qx_data$male_qx)
      insuree::ActuarialTable(i = rep(input$i, times = length(qx_female@x)), qx_female)
    }
  })
  
  # create insuree objects
  insurees <- reactive({
    #isolate({
    #  lapply(1:n_insuree(), function(x){
        insuree::Insuree(x_ = input$x_,
                         t_ = input$t_,
                         m_ = input$m_,
                         benefit = rep(input$benefit,
                                       times = input$t_),
                         a_table())
    #  })
    #})
  })
  
  # run simulation
  benefit <- reactive({
    insuree::rpv(insurees(), n = input$obs)
  })
  
  # start of output ----------------------------------------------------------
  output$test <- renderDataTable({
    as.data.frame(quantile(benefit(), seq(0.5, 0.95, by = 0.05)))
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
  
  #data_table <- reactive({
  #  table_values <- function(obs) {
  #    percentile <- c(.999, 0.995, seq(0.99, 0.9, -0.01), seq(0.85, 0.05, by = -0.05))
  #    points <- quantile(obs, percentile)
  #    obs_mean <- mean(obs)
  #    c(obs_mean, points)
  #  }
  #  
  #  benefit <- table_values(benefit())
  #  
  #  out <- data.frame(Net, Gross)
  #    
  #  cbind("Value At Risk" = c("mean", rownames(out)[-1]), out)
  #})
  
  #output$sorter <- renderDataTable({
  #  format(data_table(), 
  #    digits = 0, 
  #    big.mark = ",", 
  #    scientific = FALSE, 
  #    justify = "right"
  #  )
  #})
  
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