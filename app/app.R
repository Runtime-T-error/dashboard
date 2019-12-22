# Title     : TODO
# Objective : TODO
# Created by: luka
# Created on: 12/21/19

library(shiny)
library(RPostgreSQL)
library(ggplot2)
library(dplyr)
library(magrittr)
library(modelr)
library(eeptools)
library(lubridate)
library(ggfortify)
library(tcltk2)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="raf-hackaton", host="192.168.11.32", port=5431, user="admin", password="adminRAF")

init <- function() {
df_invs <- dbGetQuery(con, "SELECT * from invitations")
df_inv_act <- dbGetQuery(con, "SELECT * from invitation_activities")
df_users <<- dbGetQuery(con, "SELECT * from users")
df_users %<>% rename(user_id=id, user_created_at=created_at)
df_invs %<>% rename(inv_id=id, inv_created_at=created_at, inv_status=status)
df_inv_act %<>% rename(act_id=id, act_created_at=created_at)
df_users %<>% inner_join(df_invs, by=c("user_id"="user_id")) %>% inner_join(df_inv_act, by=c("inv_id"="invitation_id"))
df_users$sent_at_weekday <- weekdays(as.Date(df_users$sent_at))
df_users$age <- floor(age_calc(df_users$date_of_birth, today(), units="years"))
df_users %<>% mutate_at(vars(country, signup_type, gender, inv_status, email_notification_type, type, source_type), factor)
df_users$type_str <- df_users$type
df_users$gender_str <- df_users$gender
df_users$inv_status_str <- df_users$inv_status
df_users$is_selected_hour <- factor(!is.na(df_users$selected_hour))
df_users$is_phone_number_set <- df_users$phone_number != ""
df_users <<- mutate_at(df_users, vars(type, gender, inv_status), as.numeric)
users_varlist <<- names(df_users)
users_numeric_varlist <<- names(select_if(df_users, is.numeric))
users_factor_varlist <<- names(select_if(df_users, is.factor))
}

init()

tclTaskSchedule(30 * 60 * 1000, init(), id="refresh", redo=T)

eval_text <- function(text, context) {
  eval(parse(text=text), context)
}

server <- function(input, output, session) {

  get_data <- reactive(
    {
    tryCatch(
      {
      df_users %>% filter(if(input$base_filter=="") TRUE else eval_text(input$base_filter, df_users))
    },
      warning = function(w) {
        data.frame()
      }, error = function(e) {
        data.frame()
      }
    )
  })
  get_model <- reactive(
    {
    tryCatch(
      {
      lm(reformulate(input$lm_rhs, input$lm_lhs), get_data())
    },
      warning = function(w) {
        print(w)
        NULL
      }, error = function(e) {
        print(e)
        NULL
      }
    )
  })

  get_pca <- reactive(
    {
    tryCatch(
      {
      return(prcomp(subset(get_data(), select=input$pca_factors), center=TRUE))
    },
      warning = function(w) {
        print(w)
        NULL
      }, error = function(e) {
        print(e)
        NULL
      }
    )
  })

  get_data %<>% debounce(1000)

  output$basic_plot <- renderPlot(
    {
    data <- get_data()
    if(count(data) == 0) return(ggplot(data)) # if data is empty (e.g. invalid filter), return early

    pos <- if(input$p1_jitter) "jitter" else "identity"
    x <- eval_text((input$var_x), data)
    y <- eval_text((input$var_y), data)
    if(!input$p1_col) {
      ggplot(data, aes(x, y)) + geom_point(alpha=input$p1_alpha, position=pos) +
        (if(input$p1_facet) facet_grid(rows=reformulate(".", input$p1_facet_var)) else xlab("placeholder")) +
        xlab(input$var_x) + ylab(input$var_y)
    }
    else {
      eval_col <- eval_text(input$p1_col_var, data)
      ggplot(data, aes(x, y, color=eval_col)) +
        geom_point(alpha=input$p1_alpha, position=pos) +
        (if(input$p1_facet) facet_grid(rows=reformulate(".", input$p1_facet_var)) else xlab("placeholder")) +
        (if(is.numeric(eval_col)) scale_colour_gradient(name=input$p1_col_var, low="red", high="blue") else xlab("placeholder")) +
        xlab(input$var_x) + ylab(input$var_y)
    }
  })
  output$basic_plot.ui <- renderUI(
    {
    plotOutput("basic_plot", height=input$plot_height)
  })

  output$lmOutput <- renderTable(
    {
    if(input$tabs == "lm") { # We'll build a model only if tab 2 is showing
      model <- get_model()
      print(paste('model len', length(model)))
      if(length(model) > 0) {
        summ <- summary(model)
        table <- cbind(terms=rownames(summ$coefficients), round(summ$coefficients, 4))
        return(table)
      }
    }
    data.frame()
  })

  output$anovaOutput <- renderTable(
    {
    if(input$tabs == "lm") { # We'll build a model only if tab 2 is showing
      model <- get_model()
      if(length(model) > 0) {
        summ <- anova(model)
        table <- cbind(terms=rownames(summ), round(summ, 4))
        return(table)
      }
    }
    data.frame()
  }
  )

  output$freqpoly <- renderPlot(
    {
    model <- get_model()
    if(is.null(model)) return(ggplot())
    data <- get_data()
    data %<>% add_residuals(model)
    ggplot(data, aes(resid)) + geom_freqpoly(bins=input$freqpoly_bins)
  }
  )
  output$freqpoly.ui <- renderUI(plotOutput("freqpoly", height=input$plot_height))

  output$pca_table <- renderTable(
    {
    model <- get_pca()
    if(length(model) > 0) {
      return(cbind(terms=rownames(model$rotation), round(model$rotation, 4)))
    }
    data.frame()
  }
  )

  output$pca_plot <- renderPlot(
    {
    model <- get_pca()
    if(is.null(model)) return(ggplot())
    if(!input$pca_col) {
      autoplot(model)
    } else {
      autoplot(model, data=get_data(), colour=input$pca_col_var)
    }
  }
  )

  output$pca_plot.ui <- renderUI(plotOutput("pca_plot", height = input$plot_height))

  observe(
    {
    if(is.null(input$p1_col)) {
      updateSelectInput(session, "p1_col_var", choices = character(0))
    } else if(!input$p1_col) {
      updateSelectInput(session, "p1_col_var", choices = character(0))
    } else {
      updateSelectInput(session, "p1_col_var", choices = users_varlist)
    }
  })

  observe(
    {
    if(is.null(input$pca_col)) {
      updateSelectInput(session, "pca_col_var", choices = character(0))
    } else if(!input$pca_col) {
      updateSelectInput(session, "pca_col_var", choices = character(0))
    } else {
      updateSelectInput(session, "pca_col_var", choices = users_varlist)
    }
  })

  observe(
    {
    if(is.null(input$p1_facet)) {
      updateSelectInput(session, "p1_facet_var", choices = character(0))
    } else if(!input$p1_facet) {
      updateSelectInput(session, "p1_facet_var", choices = character(0))
    } else {
      updateSelectInput(session, "p1_facet_var", choices = users_factor_varlist)
    }
  })

  observeEvent(input$btn_swap, {
    selx <- isolate(input$var_x) # if we isolate input variable, this block won't react on it
    sely <- isolate(input$var_y)
    updateSelectInput(session, "var_x", choices = users_varlist, selected = sely)
    updateSelectInput(session, "var_y", choices = users_varlist, selected = selx)
  })
}


ui <- fluidPage(

  titlePanel("Runtime <T>error"),

fluidRow(
  column(12, textAreaInput("base_filter", "Filter", width="400%", rows=1))
),

tabsetPanel(id="tabs",
            tabPanel("Basic plots", value="basic",
                     h3("Scatter plot"),
                     fluidRow(
                       column(5, selectInput("var_x", "X", choices = users_varlist, selected = "total_amount")),
                       column(2, actionButton("btn_swap", "Swap", style="margin-top: 26px")),
                       column(5, selectInput("var_y", "Y", choices = users_varlist, selected = "length_of_interview"))
                     ),
                     fluidRow(
                       column(9, uiOutput("basic_plot.ui")),
                       column(3, wellPanel(
                         sliderInput("p1_alpha", "Transparency (alpha):", min = 0, max = 1, value = 0.2),
                         checkboxInput("p1_jitter", "Jitter", value = T),
                         checkboxInput("p1_col", "Color"),
                         selectInput("p1_col_var", "Color", choices = character(0)),
                         checkboxInput("p1_facet", "Facet"),
                         selectInput("p1_facet_var", "Facet", choices = character(0))
                         )
                       )
                     )
            ),
            tabPanel("Linear models", value="lm",
                     h3("Linear models"),
                     fluidRow(
                       column(5, textAreaInput("lm_lhs", "Dependent variables", rows=1, width="250%", value="inv_status")),
                       column(2),
                       column(5, textAreaInput("lm_rhs", "Independent variables", rows=1, width="250%", value="(gender + total_amount + length_of_interview)^2"))
                     ),
                     fluidRow(
                       column(4, h4("Regression table"), tableOutput("lmOutput")),
                       column(5, h4("ANOVA table"), tableOutput("anovaOutput"))
                     ),
                     fluidRow(
                     column(9, uiOutput("freqpoly.ui")),
                     column(3, wellPanel(
                       sliderInput("freqpoly_bins", "Bins", min = 5, max = 60, value = 30)
                     ))
                     )
            ),
            tabPanel("PCA", value="pca",
            h3("Principal component analysis"),
                     fluidRow(
                       column(4, selectInput("pca_factors", "PCA factors", choices = users_numeric_varlist, multiple = T, width="250%"))),
                     fluidRow(column(8, tableOutput("pca_table"))),
                     fluidRow(
                       column(9, uiOutput("pca_plot.ui")),
                       column(3, wellPanel(
                         checkboxInput("pca_col", "Color"),
                         selectInput("pca_col_var", "Color", choices = character(0)),
                       ))
                     )
            )
),

sliderInput("plot_height", "Global plot height", 100, 2400, 600)
)


app <- shinyApp(ui = ui, server = server)
runApp(app, 6660)