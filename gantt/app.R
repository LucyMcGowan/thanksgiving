library("shiny")
library("plotly")
library("shinydashboard")
library("googlesheets")

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  fluidRow(
    infoBox("Number of guests", 
            17, icon = icon("users"), color = "purple"),
    valueBox("2:30PM", "Eating time!", icon = icon("cutlery"), color = "orange"),
    infoBox("Allergies", 
            "all nuts (especially peanuts), gluten", icon = icon("ban"), color = "purple")
  ),
  fluidRow(
    box(title = "Thanksgiving Cooking times",
        width = 12,
        status = "primary",
        plotlyOutput("plot")
    )
  ),
  fluidRow(
    box(title = "Have another dish idea? Enter it in our spreadsheet then click the button below to rerun the chart!",
        width = 12,
        status = "warning",
        HTML('<iframe frameborder="0"
             height="500" width = "1000" 
             src="https://docs.google.com/spreadsheets/d/1-Ttr5MBqHvLgakC6ihhprx_nsNRhl5ED_kunbYjdyHE/edit?output=html"></iframe>'
        ),
        br(),
        actionButton("run", "Click to re-run the chart")
    )
  )
)

header = dashboardHeader(title = "Thanksgiving!")
ui <- dashboardPage(
  title = "D'Agostino-McGowan Thanksgiving!",
  skin = "yellow",
  header,
  sidebar,
  body
)
server <- function(input, output) {
  
  output$plot <- renderPlotly({
    input$run
    dat <- gs_url("https://docs.google.com/spreadsheets/d/1-Ttr5MBqHvLgakC6ihhprx_nsNRhl5ED_kunbYjdyHE/edit?usp=sharing",
                  lookup = FALSE,
                  visibility = "public") %>%
      gs_read()
    
    dat$start <- lubridate::as_datetime(dat$start, tz = "America/Chicago")
    dat$finish <- lubridate::as_datetime(dat$finish, tz = "America/Chicago")
    dat$duration <- dat$finish - dat$start
    dat$duration
    
    cols      <- RColorBrewer::brewer.pal(length(unique(dat$where)), name = "Set3")
    dat$color  <- factor(dat$where, labels = cols)
    dat <- dat[order(dat$start, decreasing = TRUE), ]
    
    p <- plot_ly()
    for(i in 1:(nrow(dat))){
      p <- add_trace(p,
                     type = "scatter",
                     x = c(dat$start[i], dat$start[i] + dat$duration[i]), 
                     y = c(i, i), 
                     mode = "lines",
                     line = list(color = dat$color[i], width = 20),
                     showlegend = FALSE,
                     hoverinfo = "text",
                     text = paste("Dish: ", dat$dish[i], "<br>",
                                  "Duration: ", dat$duration[i], "minutes<br>",
                                  "Where: ", dat$where[i])
      )
    }
    
    p <- layout(p,
                
                xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
                
                yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                             tickmode = "array", tickvals = 1:nrow(dat), ticktext = unique(dat$dish)),
                
                annotations = 
                  list(xref = "paper", yref = "paper",
                       x = 0.1, y = 1, xanchor = "left",
                       text = paste0("McGowan-D'Agostino Thanksgiving"),
                       font = list(color = "#f2f2f2", size = 20),
                       ax = 0, ay = 0,
                       align = "left"),
                
                plot_bgcolor = "#333333",  
                paper_bgcolor = "#333333",
                margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))  
    
    p
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

