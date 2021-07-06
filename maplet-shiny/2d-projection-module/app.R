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
        
        # Define layout of Module-2D Projection(coded as mod3) ----------------------------------------------------
        
        tabPanel(HTML(paste("2D", "Projection", sep = "<br/>")), 
                 sidebarLayout(
                     sidebarPanel(id = "mod3_panel1",
                                  # sidebar autoscroll with main panel
                                  style = "margin-left: -25px; margin-top: 45px; margin-bottom: 5px; position:fixed; width: 20%; height: 100%;",
                                  tags$p(
                                      HTML("<b>2D Projection Module</b> generates an interactive 2D projection of PCA/UMAP."
                                      )),
                                  tags$p(
                                      HTML("It displays a drop-down menu of all colData columns for coloring."
                                      )),
                                  # select one plot type
                                  radioButtons("mod3_select_plot", "Select one plot type:", 
                                               choices = list("PCA" = "pca", 
                                                              "UMAP" = "umap")
                                  ),
                                  # function argument
                                  uiOutput("mod3_pca_data"),
                                  # select coloring colData and factor it
                                  uiOutput("mod3_plot_argument"),
                                  br(),
                                  tags$p(
                                      HTML("<b>Hint:<br></b>Outputs are delayed untill you click 'UPDATE' button after selection."
                                      )),
                                  br(),
                                  # delay the output
                                  actionButton("mod3_go", "Update")
                     ), 
                     mainPanel(id = "mod3_panel2", 
                               br(), 
                               br(), 
                               br(), 
                               style = "overflow-y: auto; position: absolute; left: 25%",
                               # plotly
                               downloadButton("mod3_download_plotly", "download plotly"),
                               plotlyOutput('mod3_plot', height = 700)
                     )
                 )
        )
    )
)

################################################################################
################ Define server logic required to draw outputs ##################
################################################################################

server <- function(input, output) {
    
    # Define rendering logic of control widgets in Module-2D projection(coded as mod3) ------------------------
    output$mod3_pca_data <- renderUI({
        if(input$mod3_select_plot=="pca"){
            selectInput("mod3_pca_data_type", "Select data type for PCA:",
                        width = "220px",
                        choices = c("scores", "loadings"),
                        selected = "scores"
            )
        } else {
            NULL
        }
    })
    
    # create intermediate var to indicate coloring widgets
    inter_var <- reactive({
        if (input$mod3_select_plot=="pca" & input$mod3_pca_data_type=="scores") {
            "pca-scores"
        } else if(input$mod3_select_plot=="pca" & input$mod3_pca_data_type=="loadings"){
            "pca-loadings"
        } else {
            "umap"
        }
    })
    # create reactive plotting argument for PCA/UMAP
    output$mod3_plot_argument <- renderUI({
        switch(
            inter_var(),
            "pca-scores"=list(
                checkboxInput("mod3_scale_data", "Scaled data", 
                              value = TRUE
                ),
                selectInput("mod3_select_colData", 
                            "Select one coloring variable:", 
                            choices = names(colData(D)),
                            selected = "BOX.NUMBER",
                            width = "220px"
                ),
                checkboxInput("mod3_checkbox_factor", 
                              "Categorical Coloring", 
                              value = FALSE
                ),
                selectInput("mod3_select_hover", 
                            "Select hovering text:", 
                            # selectInput coerces its output to character
                            # https://github.com/rstudio/shiny/issues/2367
                            # choices = setNames(seq_along(colData(D)), names(colData(D))),
                            choices = names(colData(D)),
                            selected = "sample",
                            width = "220px",
                            multiple=TRUE
                )
            ),
            "pca-loadings"=list(
                checkboxInput("mod3_scale_data", "Scaled data", 
                              value = TRUE
                ),
                selectInput("mod3_select_colData", 
                            "Select one coloring variable:", 
                            choices = names(rowData(D)),
                            selected = "SUPER_PATHWAY",
                            width = "220px"
                ),
                checkboxInput("mod3_checkbox_factor", 
                              "Categorical Coloring", 
                              value = FALSE
                ),
                selectInput("mod3_select_hover", 
                            "Select hovering text:", 
                            # choices = setNames(seq_along(rowData(D)), names(rowData(D))),
                            choices = names(rowData(D)),
                            selected = "name",
                            width = "220px",
                            multiple=TRUE
                )
            ),
            "umap"=list(numericInput("mod3_umap_n_neighbors", 
                                     "Number of neighbors for UMAP:", 
                                     value = 15,
                                     width = "220px"
            ),
            checkboxInput("mod3_scale_data", "Scaled data", 
                          value = TRUE
            ),
            selectInput("mod3_select_colData", 
                        "Select one coloring variable:", 
                        choices = names(colData(D)),
                        selected = "BOX.NUMBER",
                        width = "220px"
            ),
            checkboxInput("mod3_checkbox_factor", 
                          "Categorical Coloring", 
                          value = FALSE
            ),
            selectInput("mod3_select_hover", 
                        "Select hovering text:", 
                        # choices = setNames(seq_along(colData(D)), names(colData(D))),
                        choices = names(colData(D)),
                        selected = "sample",
                        width = "220px",
                        multiple=TRUE
            )
            )
        )
    })
    
    # create reactive inputs list
    mod3_input_object <- eventReactive(input$mod3_go, 
                                       {c(input$mod3_select_plot, 
                                          input$mod3_select_colData,
                                          input$mod3_scale_data,
                                          input$mod3_checkbox_factor,
                                          input$mod3_pca_data_type,
                                          input$mod3_umap_n_neighbors)}
    )
    
    # Define rendering logic of outputs in Module-2D projection(coded as mod3) --------------------------------
    ## download button
    session_store <- reactiveValues()
    
    # render pca/umap of mod3
    output$mod3_plot <- renderPlotly({
        session_store$mod3_plotly <- if (mod3_input_object()[1]=="pca"){
            mod3_plots_pca(D = D,
                           scale_data = mod3_input_object()[3],
                           color = mod3_input_object()[2],
                           categorizing=mod3_input_object()[4],
                           data_type = mod3_input_object()[5],
                           hover = input$mod3_select_hover
            )
        } else {
            mod3_plots_umap(D = D,
                            scale_data = mod3_input_object()[3],  
                            color = mod3_input_object()[2],
                            categorizing=mod3_input_object()[4],
                            n_neighbors = as.numeric(mod3_input_object()[6]),
                            hover = input$mod3_select_hover
            )
        } 
        session_store$mod3_plotly
    })
    
    # download button
    output$mod3_download_plotly <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
            saveWidget(as_widget(session_store$mod3_plotly), file, selfcontained = TRUE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)