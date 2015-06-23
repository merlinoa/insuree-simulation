library(shiny)
library(ggplot2)
library(scales)
library(insuree)
library(lubridate)

# some data
n <- 5000
df <- read.csv(file = "data/policies.csv")

# convert dates to YYYY-MM-DD format
df$dob <- as.Date(df$dob, format = "%m/%d/%Y")
df$issue_date <- as.Date(df$issue_date, format = "%m/%d/%Y")

# find other dates and intervals of interest
df$effective <- df$issue_date + years(df$deferral)
df$expiration <- df$effective + years(df$term)


shinyServer(function(input, output) {
  # find insuree exact age
  age <- reactive({
    inter <-  interval(df$dob, input$date)
    inter / dyears(1)
  })
  
  # unexpired deferral period
  unearned_m_ <- reactive({
    inter <- interval(input$date, df$effective)
    pmax(inter / dyears(1), 0)
  })
  
  # unexpired term period
  unearned_t_ <- reactive({
    inter <- interval(input$date, df$expiration)
    pmin(inter / dyears(1), df$term)
  })
  
  # benefit length
  benefit_length <- reactive({
    ceiling(age() + unearned_t_() + unearned_m_()) - floor(age() + unearned_m_())
  })
  
  # data frame to display
  insurees_data <- reactive({
    l <- length(names(df))
    df[, -c(l, l - 1)]
  })
  
  # male actuarial tables
  male_qx <- reactive({
    qx_male <- insuree::LifeTable(x = qx_data$x,
                                  t = rep(1, length(qx_data$x)),
                                  q_x = qx_data$male_qx)
    qx_male <- insuree::ActuarialTable(i = rep(input$i, 
                                    times = length(qx_male@x)), qx_male)
  })
  
  # female actuarial table
  female_qx <- reactive({
    qx_female <- insuree::LifeTable(x = qx_data$x,
                                    t = rep(1, length(qx_data$x)),
                                    q_x = qx_data$female_qx)
    insuree::ActuarialTable(i = rep(input$i, times = length(qx_female@x)), qx_female)
  })
  
  # create insuree objects
  insurees <- reactive({
    my_table <- insurees_data()
    k <- age()
    holder <- list()
    for (j in 1:nrow(my_table)) {
      if (my_table$gender[j] == "male") {
        holder[[j]] <- insuree::Insuree(x_ = k[j],
                                        t_ = unearned_t_()[j],
                                        m_ = unearned_m_()[j],
                                        benefit = rep(my_table$benefit[j],
                                                      times = benefit_length()[j]),
                                        male_qx()
                                        )
      } else {
        holder[[j]] <- insuree::Insuree(x_ = k[j],
                                        t_ = unearned_t_()[j],
                                        m_ = unearned_m_()[j],
                                        benefit = rep(my_table$benefit[j],
                                                      times = benefit_length()[j]),
                                        female_qx()
                                       )
      }
    }
    holder
  })
  
  # run simulation
  benefit <- reactive({
    set.seed(12345)
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
    a_age <- round(mean(age()),2)
    valueBox(
      value = a_age,
      subtitle = "Average Insuree Age",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$reserve <- renderValueBox({
    reserve_ci <- format(round(quantile(benefit(), input$ci) ,0), big.mark = ",")
    valueBox(
      value = reserve_ci,
      subtitle = "Reserve at Variable Confidence Level",
      icon = icon("money"),
      color = "green"
    )
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
})