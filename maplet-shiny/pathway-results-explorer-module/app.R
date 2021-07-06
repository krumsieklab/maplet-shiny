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
# extract object names from result SE 'D'
obj_name <- get_obj_name(D)
# define pathway annotation column (extracted from corresponding stat_bar
pwvar <- mtm_res_get_entries(D, c("plots", "stats"))[[1]]$args$group_col
# define threshold for significance (extracted from corresponding stat_bar plot)
alpha <- mtm_res_get_entries(D, c("plots", "stats"))[[1]]$args$feat_filter[[3]]
# get pathway annotations
rd <- get_pathway_annotations(D, pwvar)


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

        # Define layout of Module-Pathway Results Explorer(coded as mod2) ----------------------------------------------------
        
        tabPanel(HTML(paste("Pathway Results", "Explorer", sep = "<br/>")),
                 sidebarLayout(
                     sidebarPanel(
                         id = "mod2_panel1",
                         # sidebar auto-scrolling with main panel
                         style = "margin-left: -25px; margin-top: 45px; margin-bottom: 5px; position:fixed; width: 20%; height: 100%;",
                         tags$p(
                             HTML(
                                 "<b>Pathway Results Explorer:</b> Displays a series of interactive plots at different granularities given a SE and a statname."
                             )
                         ),
                         tags$p(
                             HTML(
                                 "StatsBar plot -> Equalizer/Volcano plot -> Box/Scatter plot."
                             )
                         ),
                         br(),
                         selectInput(
                             "mod2.stat",
                             "Select one stat name:",
                             choices = distinct(obj_name[obj_name$V1 == "plots" &
                                                             obj_name$V2 == "stats",],
                                                stat_name)$stat_name
                         ),
                         br(),
                         radioButtons(
                             "mod2.plot1",
                             "Select plot1:",
                             choices = list("Barplot" = "bar",
                                            "No Barplot" = "null"),
                             selected  = "bar"
                         ),
                         br(),
                         radioButtons(
                             "mod2.plot2",
                             "Select plot2 type:",
                             choices = list("Equalizer" = "equalizer",
                                            "Volcano" = "volcano"),
                             selected  = "volcano"
                         ),
                         tags$hr(),
                         radioButtons(
                             "mod2.plot3",
                             "Select plot3 type:",
                             choices = list("Box" = "box",
                                            "Scatter" = "scatter"),
                             selected  = "scatter"
                         ),
                         checkboxInput("mod2.categorical", 
                                       "Treat as categorical", 
                                       value = FALSE
                         ),
                         br(),
                         tags$p(
                             HTML("<b>Hint:<br></b>Outputs are delayed untill you click 'UPDATE' button after selection."
                             )),
                         actionButton("mod2_go", "Update")
                     ),
                     mainPanel(
                         id = "mod2_panel2",
                         style = "overflow-y: auto; position: absolute; left: 25%",
                         br(),
                         br(),
                         br(),
                         # Bar plot or not
                         uiOutput("mod2.p1"),
                         br(),
                         # equalizer or volcano
                         uiOutput("mod2.p2"),
                         br(),
                         # box or scatter
                         uiOutput("mod2.p3"),
                         br()
                     )
                 ))
    )
)

################################################################################
################ Define server logic required to draw outputs ##################
################################################################################

server <- function(input, output) {

    # Define rendering logic of outputs in Module-Pathway Results Explorer(coded as mod2) --------------------------------
    # Module 2: create reactive inputs list
    mod2_input_object <- eventReactive(input$mod2_go, 
                                       {c(input$mod2.stat,
                                          input$mod2.plot1,
                                          input$mod2.plot2,
                                          input$mod2.plot3)}
    )
    
    # Module 2: store reactive output plots
    session_store <- reactiveValues()
    
    # Module 2: plot 1
    output$mod2.p1 <- renderUI({
        inputs <- mod2_input_object()
        switch(
            inputs[2],
            "bar" = list(downloadButton("download_plotly_bar",
                                        "download bar plot"),
                         plotlyOutput("mod2.bar", height = 600)),
            "null" = NULL
        )
    })
    
    # Module 2: plot 1 - bar plot
    output$mod2.bar <- renderPlotly({
        inputs <- mod2_input_object()
        plots <- mtm_res_get_entries(D, c("plots", "stats"))
        for (i in seq_along(plots)) {
            if (plots[[i]]$args$stat_list == inputs[1]) {
                plot <- plots[[i]]$output[[1]]
            }
        }
        session_store$mod2.bar <- ggplotly(plot, source = "sub_bar") %>%
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.3))
        # render plotly graph
        session_store$mod2.bar
    })
    
    # Module 2: plot 1 - bar plot - html file
    output$download_plotly_bar <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
            # export plotly html widget as a temp file to download.
            saveWidget(as_widget(session_store$mod2.bar), file, selfcontained = TRUE)
        }
    )
    
    # Module 2: plot 2
    output$mod2.p2 <- renderUI({
        inputs <- mod2_input_object()
        d <- event_data("plotly_click", source = "sub_bar")
        
        vol_list <- list(
            downloadButton("download_plotly_volcano",
                           "download volcano plot"),
            plotlyOutput("mod2.vol", height = 600)
        )
        
        # equalizer/bar -> bar/null -> plot
        plot2 <- switch(inputs[3],
                        "equalizer" = switch(
                            inputs[2],
                            "bar" = if (!is.null(d)) {
                                list(
                                    downloadButton("download_plotly_eq",
                                                   "download equalizer plot"),
                                    plotlyOutput("mod2.equal", height = 600)
                                )
                            },
                            "null" = list(
                                downloadButton("download_plotly_eq",
                                               "download equalizer plot"),
                                uiOutput("mod2.equal.ui"),
                                plotlyOutput("mod2.equal", height = 600)
                            )
                        ),
                        "volcano" = switch(inputs[2],
                                           "bar" = if (!is.null(d)) {
                                               vol_list
                                           },
                                           "null" = vol_list))
    })
    
    # Module 2: plot 2 - volcano plot
    output$mod2.vol <- renderPlotly({
        inputs <- mod2_input_object()
        d <- event_data("plotly_click", source = "sub_bar")
        
        # get the threshold for significance
        alpha <- get_threshold_for_p_adj(D, inputs[1])
        legend_name <- paste0("p.adj < ", alpha)
        
        if (!is.null(d)) {
            # D:SE object, inputs: sidebar value, legend_name: legend name
            # d: click info for bar plot, pwvar: SUB_PATWAY/PATTHWAY, alpha: significant value (ex. p.adj < 0.1)
            plot <- mod2_plot_vol(D, inputs, legend_name, d, pwvar, alpha)
        } else {
            plot <- mod2_plot_vol(D, inputs, legend_name, NULL, pwvar, alpha)
        }
        
        session_store$mod2.vol <- ggplotly(plot, source = "sub_vol") %>%
            layout(legend = list(orientation = 'h',
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.2,
                                 title = list(text=paste0('<b> ', legend_name, ' </b>'))))
        session_store$mod2.vol
    })
    
    # Module 2: plot 2 - volcano plot - html file
    output$download_plotly_volcano <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
            saveWidget(as_widget(session_store$mod2.vol), file, selfcontained = TRUE)
        }
    )
    
    # Module 2: plot 2 - equalizer plot - not bar
    output$mod2.equal.ui <- renderUI({
        inputs <- mod2_input_object()
        data_bar <- get_data_by_name(D, "stat_list", "stats", inputs[1])
        subpathways <- data_bar$name
        selectInput(
            "mod2.equal.path",
            "Select one pathway name:",
            choices = c(unique(unlist(subpathways))),
            selected = ""
        )
    })
    
    # Module 2: plot 2 - equalizer plot
    output$mod2.equal <- renderPlotly({
        inputs <- mod2_input_object()
        # get click info for bar plot
        d <- event_data("plotly_click", source = "sub_bar")
        # get the threshold for significance
        alpha <- get_threshold_for_p_adj(D, inputs[1])
        
        if (inputs[2] == "null") {
            # D:SE object, inputs: sidebar value, rd: pathway annotations
            # alpha: significant value (ex. p.adj < 0.1), pwvar: SUB_PATWAY/PATTHWAY,
            # path_name: pathway name for equalizer plot, d: click info for bar plot
            plot <- mod2_plot_eq(D, inputs, rd, alpha, pwvar, input$mod2.equal.path, NULL)
        } else {
            plot <- mod2_plot_eq(D, inputs, rd, alpha, pwvar, NULL, d)
        }
        
        session_store$mod2.eq <- if (is.null(plot)) plotly_empty() else ggplotly(plot, source = "sub_eq")
        session_store$mod2.eq
    })
    
    # Module 2: plot 2 - equalizer plot - html file
    output$download_plotly_eq <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
            saveWidget(as_widget(session_store$mod2.eq), file, selfcontained = TRUE)
        }
    )
    
    # Module 2: plot 3 - box/scatter plot
    output$mod2.p3 <- renderUI({
        inputs <- mod2_input_object()
        d.eq <- event_data("plotly_click", source = "sub_eq")
        d.vol <- event_data("plotly_click", source = "sub_vol")
        
        download.name <- ifelse(inputs[4]=="box", "download box plot", "download scatter plot")
        plot.list <- list(
            downloadButton("download_plotly_box_scatter", download.name),
            plotOutput("mod2.box.scatter", height = 600)
        )
        
        if (!is.null(d.eq) | !is.null(d.vol))  {
            plot.list
        }
    })
    
    # Module 2: plot 3 - box/scatter plot
    output$mod2.box.scatter <- renderPlot({
        inputs <- mod2_input_object()
        # Get the data set
        data <- D %>%
            maplet:::mti_format_se_samplewise() %>%
            tidyr::gather(var, value, dplyr::one_of(rownames(D)))
        # get the click info for bar/equalizer/volcano if available
        d.bar <- event_data("plotly_click", source = "sub_bar")
        d.eq <- event_data("plotly_click", source = "sub_eq")
        d.vol <- event_data("plotly_click", source = "sub_vol")
        
        # get the threshold for significance
        alpha <- get_threshold_for_p_adj(D, inputs[1])
        
        plot <- mod2_plot_box_scatter(D, # SE object
                                      inputs, # sidebar inputs
                                      d.bar, # click info for bar plot
                                      d.eq, # click info for equalizer plot
                                      d.vol, # click info for volcano plot
                                      rd, # pathway annotations
                                      pwvar, # pathway annotation column 
                                      input$mod2.equal.path, # pathway name if plot2 is "equalizer"
                                      alpha, # significant value (ex. p.adj < 0.1)
                                      input$mod2.categorical, # if treated categorical
                                      data) # data for box/scatter plot
        
        session_store$mod2.box.scatter <- if (is.null(plot)) NULL else plot
        session_store$mod2.box.scatter
    })
    
    # Module 2: plot 3 - scatter/box plot - html file
    output$download_plotly_box_scatter <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = session_store$mod2.box.scatter, device = device)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)