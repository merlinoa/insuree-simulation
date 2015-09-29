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
  
  # data frame to display
  insurees_data <- reactive({
    l <- length(names(df))
    df[, -c(l, l - 1)]
  })
  
  # male actuarial tables
  male_qx <- reactive({
    insuree::LifeTable(x = c(qx_data$x, max(qx_data$x) + 1),
                       q_x = c(qx_data$male_qx, NA))
  })
  
  # female actuarial table
  female_qx <- reactive({
    insuree::LifeTable(x = c(qx_data$x, max(qx_data$x) + 1),
                       q_x = c(qx_data$female_qx, NA))
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
                                        benefit_t = unearned_t_()[j],
                                        benefit_value = my_table$benefit[j],
                                        male_qx()
                                        )
      } else {
        holder[[j]] <- insuree::Insuree(x_ = k[j],
                                        t_ = unearned_t_()[j],
                                        m_ = unearned_m_()[j],
                                        benefit_t = unearned_t_()[j],
                                        benefit_value = my_table$benefit[j],
                                        female_qx()
                                       )
      }
    }
    holder
  })
  
  # run simulation
  benefit <- reactive({
    pool <- Pool(insurees = insurees())
    set.seed(12345)
    out <- insuree::rpv(pool, n = n, interest = input$i)
    apply(summary(out), 1, sum)
  })
  
  # start of display ----------------------------------------------------------
  # dashboard -----------------------------
  output$n_insurees <- renderValueBox({
    total_insurees <- nrow(insurees_data()[df$issue_date <= input$date, ])
    valueBox(
      value = total_insurees,
      subtitle = "Total Non Expired",
      icon = icon("group")
    )
  })
  
  output$n_deferral <- renderValueBox({
    n_deferral <- length(unearned_m_()[unearned_m_() > 0 & df$issue_date <= input$date])
    valueBox(
      value = n_deferral,
      subtitle = "In Deferral Period",
      icon = icon("user")
    )
  })
  
  output$n_effective <- renderValueBox({
    n_effective <- length(unearned_t_()[unearned_m_() == 0 & unearned_t_() > 0 & df$issue_date <= input$date])
    valueBox(
      value = n_effective,
      subtitle = "In Term Period",
      icon = icon("male")
    )
  })
  
  output$b_insurees <- renderValueBox({
    total_insurees <- sum(insurees_data()$benefit[df$issue_date <= input$date])
    valueBox(
      value = format(total_insurees, big.mark = ","),
      subtitle = "Total Non Expired",
      icon = icon("group"),
      color = "yellow"
    )
  })
  
  output$b_deferral <- renderValueBox({
    b_deferral <- sum(insurees_data()$benefit[unearned_m_() > 0 & df$issue_date <= input$date])
    valueBox(
      value = format(b_deferral, big.mark = ","),
      subtitle = "In Deferral Period",
      icon = icon("user"),
      color = "yellow"
    )
  })
  
  output$b_effective <- renderValueBox({
    b_effective <- sum(insurees_data()$benefit[unearned_m_() == 0 & unearned_t_() > 0 & df$issue_date <= input$date])
    valueBox(
      value = format(b_effective, big.mark = ","),
      subtitle = "In Term Period",
      icon = icon("male"),
      color = "yellow"
    )
  })
  
  # May want to move to Insurees tab
  #output$avg_age <- renderValueBox({
  #  a_age <- round(mean(age()),2)
  #  valueBox(
  #    value = a_age,
  #    subtitle = "Average Insuree Age",
  #    icon = icon("calendar"),
  #    color = "purple"
  #  )
  #})
  
  reserve_n <- reactive({
    quantile(benefit(), input$ci)
  })
  
  output$reserve <- renderInfoBox({
    reserve_ci <- format(round(reserve_n() ,0), big.mark = ",")
    infoBox(
      title = "Reserve",
      value = reserve_ci,
      subtitle = "Discounted for mortality and interest",
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
      xlab("Reserve: Present Value of Benefits") +
      ylab("Observations") +
      ggtitle("Simulated Death Benefits") +
      geom_vline(xintercept = mean(benefit()), colour="blue", size = 1) +
      geom_vline(xintercept = reserve_n(), colour="green4", size = 2) +
      geom_text(data = NULL, x = mean(benefit()), y = 0, label = "mean", angle = 90, 
                vjust = -0.4, size = 5, hjust = -2, family = "Courier") +
      geom_text(data = NULL, x = reserve_n(), y = 0, label = input$ci, 
                angle = 90, vjust = -0.4, size = 5, hjust = -2, family = "Courier")
  },
  height = 200)
  
  output$cdf <- renderPlot({
    ggplot(data.frame(loss = benefit()), aes(x = loss)) +
      stat_ecdf() +
      xlab("Reserve: Present Value of Benefits") +
      ylab("P(benefit <= x)") +
      scale_x_continuous(labels = dollar) +
      ggtitle("Simulated Death Benefits") +
      geom_vline(xintercept = mean(benefit()), colour="blue", size = 1) +
      geom_vline(xintercept = reserve_n(), colour="green4", size = 2) +
      geom_text(data = NULL, x = mean(benefit()), y = 0, label = "mean", angle = 90, 
                vjust = -0.4, size = 5, hjust = -2, family = "Courier") +
      geom_text(data = NULL, x = reserve_n(), y = 0, label = input$ci, 
                angle = 90, vjust = -0.4, size = 5, hjust = -2, family = "Courier")
  },
  height = 200)
  
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