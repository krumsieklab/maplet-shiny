rm(list=ls())

################################################################################
####################load packages, functions and result SE object ##############
################################################################################

# load packages
# devtools::install_github(repo="krumsieklab/maplet@v1.0.1", subdir="maplet")
library(shiny)
library(shinyWidgets)
library(maplet)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(DT)
library(plotly)
library(openxlsx)
library(readxl)
library(RColorBrewer)
# refer help functions
source("help_functions.R")

# load SE with fixed name
load("SE.Rdata")


################################################################################
########################## Define UI for Shiny application #####################
################################################################################

ui <- fluidPage(
    
    # set appearance customization -------------------------------------------------
    
    theme = "bootstrap.css",
    includeCSS("www/style.css"),
    setBackgroundColor("#FFFFFF"),# set canvas background color
    div(style = "padding: 1px 0px; width: '100%'",
        titlePanel(
            title = "",
            windowTitle = "Maplet"
        )
    ),
    # remove shiny "red" warning messages on GUI
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    # adjust tab height
    tags$head(
        tags$style(HTML(' .navbar {
                          height: 80px;
                          min-height:25px !important;
                        }
                      .navbar-nav > li > a, .navbar-brand {
                            padding-top:1px !important; 
                            padding-bottom:1px !important;
                            height: 80px;
                            }'))),
    navbarPage(
        # embed Maplet logo and title
        title = div(img(src='logo.png',
                        style="float:left; margin-top: 5px; padding-right:20px;padding-bottom:5px",
                        height = 60),
                    tags$a("Krumsiek Lab", href="https://github.com/krumsieklab/maplet-shiny", style="color: White"),
                    tags$script(HTML("var header = $('.navbar > .container-fluid');header.append('<div style=\"float:right\"><a href=\"https://weill.cornell.edu\"><img src=\"wcm2.png\" alt=\"logo\" style=\"float:right;height:50px;margin-top: 10px; padding-right:1px; \"> </a></div>');console.log(header)")),
                    windowTitle = "Maplet"),
        # sticky tabs while scrolling main panel
        position = c("fixed-top"), 
        
        # Define layout of Module-Annotations Explorer(coded as mod5) ----------------------------------------------------
        tabPanel(HTML(paste("Annotations", "Explorer", sep = "<br/>")), 
                 sidebarLayout(
                     sidebarPanel(id = "mod5_panel1",
                                  # sidebar autoscroll with main panel
                                  style = "margin-left: -25px; margin-top: 45px; margin-bottom: 5px; position:fixed; width: 20%; height: 100%;",
                                  tags$p(
                                      HTML("<b>Annotations Explorer Module</b> creates tables, distribution plots, or other graphics to explore the SE object."
                                      )),
                                  radioButtons("mod5_dimension", "Select one dimension:", 
                                               choices = list("Column Data" = "col", 
                                                              "Row Data" = "row")
                                  ),
                                  br(),
                                  uiOutput("mod5_dimension_ui"),
                                  br(),
                                  tags$p(
                                      HTML("<b>Hint:<br></b>Outputs are delayed untill you click 'UPDATE' button after selection."
                                      )),
                                  br(),
                                  # delay the output
                                  actionButton("mod5_go", "Update")
                     ), 
                     mainPanel(id = "mod5_panel2", 
                               br(), 
                               br(), 
                               br(), 
                               style = "overflow-y: auto; position: absolute; left: 25%",
                               uiOutput("mod5_output_ui")
                     )
                 )
        )
    )
)

################################################################################
################ Define server logic required to draw outputs ##################
################################################################################

server <- function(input, output) {
    ## download button
    session_store <- reactiveValues()
    
    # Define rendering logic of control widgets in Module-Annotations Explorer(coded as mod5) ----------------------
    output$mod5_dimension_ui <- renderUI({
        switch(input$mod5_dimension,
               "col"=list(selectInput("mod5_var1_select", 
                                      "Select the primary variable:", 
                                      choices = names(colData(D)),
                                      selected = "Age",
                                      width = "220px"),
                          checkboxInput("mod5_var1_type", 
                                        "Continuous", 
                                        value = TRUE),
                          tags$hr(),
                          selectInput("mod5_var2_select", 
                                      "Select the secondary variable:", 
                                      choices = names(colData(D)),
                                      selected = "sample",
                                      width = "220px"),
                          checkboxInput("mod5_var2_type", 
                                        "Continuous", 
                                        value = TRUE),
                          tags$hr(),
                          selectInput("mod5_select_hover", 
                                      "Select hovering text:", 
                                      choices = names(colData(D)),
                                      selected = names(colData(D))[1],
                                      width = "220px",
                                      multiple=TRUE)
               ),
               "row"=selectInput("mod5_rowdata_plot", 
                                 "Select one plot for row data:", 
                                 choices = c("SUPER_PATHWAY"),
                                 width = "220px")
        )
    })
    
    output$mod5_output_ui <- renderUI({
        switch(input$mod5_dimension,
               "col"=list(downloadButton("mod5_download_plotly", "download plotly"),
                          plotlyOutput('mod5_plot', height = 600)),
               "row"=list(fluidRow(
                   splitLayout(style = "border: 1px", cellWidths = c(1000, 1000), 
                               downloadButton("mod5_download_plotly", "download plotly"), 
                               downloadButton("mod5_download_plotly2", "download plotly")
                   )
               ),
               fluidRow(
                   splitLayout(style = "height:600px; border: 1px", cellWidths = c(1000, 1000), 
                               plotlyOutput('mod5_plot', height = 600), 
                               plotlyOutput('mod5_plot2', height = 600)
                   )
               ))
        )
    })
    
    # Define rendering logic of outputs in Module-Annotations Explorer(coded as mod5) ------------------------------
    mod5_input <- eventReactive(input$mod5_go,{
        c(input$mod5_var1_select,
          input$mod5_var1_type,
          input$mod5_var2_select,
          input$mod5_var2_type,
          input$mod5_rowdata_plot)
    })
    
    output$mod5_plot <- renderPlotly({
        session_store$mod5_plotly <- switch(input$mod5_dimension,
                                            "col"=
                                                if(mod5_input()[2]==TRUE & mod5_input()[4]==TRUE){
                                                    mod5_scatter(D, x=mod5_input()[3], 
                                                                 y=mod5_input()[1], 
                                                                 hover = input$mod5_select_hover)
                                                } else if(mod5_input()[2]==TRUE & mod5_input()[4]==FALSE) {
                                                    mod5_boxplot(D, x=mod5_input()[3], 
                                                                 x_cate = FALSE,
                                                                 y=mod5_input()[1],
                                                                 y_cate = TRUE,
                                                                 fill=mod5_input()[3], 
                                                                 hover=input$mod5_select_hover)
                                                } else if(mod5_input()[2]==FALSE & mod5_input()[4]==TRUE) {
                                                    mod5_boxplot(D, x=mod5_input()[1], 
                                                                 x_cate = FALSE,
                                                                 y=mod5_input()[3],
                                                                 y_cate = TRUE,
                                                                 fill=mod5_input()[1], 
                                                                 hover=input$mod5_select_hover)
                                                } else {
                                                    mod5_barplot(D, x=mod5_input()[3], 
                                                                 fill=mod5_input()[1], 
                                                                 hover = input$mod5_select_hover)
                                                },
                                            "row"=
                                                rowData(D) %>%
                                                data.frame %>%
                                                dplyr::rename(var=mod5_input()[5]) %>%
                                                dplyr::group_by(var) %>%
                                                dplyr::summarise(count=n()) %>%
                                                plot_ly(labels = ~var, 
                                                        values = ~count, 
                                                        type = 'pie',
                                                        textposition = 'inside',
                                                        source="mod5-click",
                                                        title="<b>Distribution of Super Pathway</b>") %>% 
                                                layout(autosize = F, width = 1000, height = 500,
                                                       uniformtext=list(minsize=12, mode='hide'),
                                                       legend = list(x = 1,
                                                                     y = .5,
                                                                     tracegroupgap = 5)
                                                )
        )
        session_store$mod5_plotly
    }
    )
    # download button
    output$mod5_download_plotly <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
            saveWidget(as_widget(session_store$mod5_plotly), file, selfcontained = TRUE)
        }
    )
    
    ## to see the stored data of clicking
    # output$info <- renderPrint({
    #   d5 <- event_data("plotly_click", source = "mod5-click")
    #   if(!is.null(d5)){
    #     d5
    #   }
    # })
    
    output$mod5_plot2 <- renderPlotly({
        d5 <- event_data("plotly_click", source = "mod5-click")
        pie_dat <- as.data.frame(rowData(D))
        
        if (!is.null(d5)){
            lvls <- rev(pie_dat$SUPER_PATHWAY)
            label <- lvls[round(as.numeric(d5$pointNumber))+1]
            
            session_store$mod5_plot2 <- 
                pie_dat[pie_dat$SUPER_PATHWAY == label, ] %>%
                dplyr::rename(var="SUB_PATHWAY") %>%
                dplyr::group_by(var) %>%
                dplyr::summarise(count=n()) %>%
                plot_ly(labels = ~var, 
                        values = ~count, 
                        type = 'pie', 
                        textposition = 'inside',
                        title=paste0("<b>Distribution of Sub Pathway in Specified Super Pathway - </b>", label)
                ) %>%
                layout(autosize = F, width = 1000, height = 500,
                       uniformtext=list(minsize=12, mode='hide'),
                       legend = list(x = 1,
                                     y = .5,
                                     tracegroupgap = 5)
                )
            session_store$mod5_plot2
        }
    })
    # download button
    output$mod5_download_plotly2 <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
            saveWidget(as_widget(session_store$mod5_plotly2), file, selfcontained = TRUE)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)