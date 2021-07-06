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
        
        # Define layout of Module-All Results Explorer(coded as mod1) ----------------------------------------------------
        
        tabPanel(HTML(paste("All Results", "Explorer", sep = "<br/>")), 
                 sidebarLayout(
                     sidebarPanel(id = "mod1_panel1",
                                  # sidebar auto-scrolling with main panel
                                  style = "margin-left: -25px; margin-top: 45px; margin-bottom: 5px; position:fixed; width: 20%; height: 100%;",
                                  tags$p(
                                      HTML("<b>All Results Explorer Module</b> extracts all the result objects one at a time."
                                      )),
                                  tags$p(
                                      HTML("Users can assess results in a drop-down menu that offers a list of a stat_name and a plot type (e.g. “missingness”, “pval”)."
                                      )),
                                  br(),   
                                  # select plot type or stats table
                                  radioButtons("mod1_radio", "Select output type:",
                                               choices = list("Plot" = "plots", 
                                                              "Table" = "stats"),
                                               selected = "stats"
                                  ),
                                  br(),   
                                  # define one UI object to select stat_name
                                  uiOutput("mod1_select_statname_ui"),
                                  br(),   
                                  # define one UI object to select output type
                                  uiOutput("mod1_select_object_ui"),
                                  br(),   
                                  tags$p(
                                      HTML("<b>Hint:<br></b>Outputs are delayed untill you click 'UPDATE' button after selection. Some plots such as box plot or multiple plots may cost dozens of seconds to show up."
                                      )),
                                  # delay the output
                                  actionButton("mod1_go", "Update")
                     ), 
                     mainPanel(id = "mod1_panel2", 
                               # scrollable panel
                               style = "overflow-y: auto; position: absolute; left: 25%",
                               br(), 
                               br(), 
                               br(), 
                               # dynamic number of plots
                               uiOutput('mod1_output')
                     )
                 )
        )
    )
)

################################################################################
################ Define server logic required to draw outputs ##################
################################################################################

server <- function(input, output) {
    
    # Define rendering logic of control widgets in Module-All Results Explorer(coded as mod1) ------------------------
    
    # create stat_name list dependent on radio button
    output$mod1_select_statname_ui <- renderUI({
        selectInput("mod1_select_statname", "Select one stat name:",
                    width = "220px",
                    choices = dplyr::distinct(obj_name[obj_name$V1==input$mod1_radio, ], stat_name)$stat_name
        )
    })
    
    # create object list dependent on radio button and stat_name
    output$mod1_select_object_ui <- renderUI({
        if (input$mod1_radio=="stats"){
            NULL
        } else {
            selectInput("mod1_select_object", "Select one object:",
                        width = "220px",
                        choices = dplyr::distinct(obj_name[obj_name$stat_name==input$mod1_select_statname&obj_name$V1==input$mod1_radio, ], V2)$V2
            )
        }
        
    })
    
    # create indicator of box plot output
    box_switch <- reactive({
        if (input$mod1_select_object=="box"){
            "box_plot"
        } else {
            "non_box_plot"
        }
    })
    
    ## get the order of selected stat_name
    ord <- reactive({
        # assign a data frame of all the object names of box plots
        # filter() cannot run in Shiny, use subset() instead
        box_obj_name <- subset(obj_name, V1=="plots"&V2=="box")
        box_output_order <- box_obj_name %>%
            dplyr::mutate(order=seq(from=1, to=n()))
        
        if(input$mod1_select_statname %in% box_output_order$stat_name){
            box_output_order[box_output_order$stat_name==input$mod1_select_statname, ]$order
        } else {
            1
        }
    })
    
    # create reactive inputs list
    mod1_input_object <- eventReactive(input$mod1_go, ## delayed output
                                       {c(input$mod1_radio,
                                          input$mod1_select_statname,
                                          input$mod1_select_object)}
    )
    
    # Define rendering logic of outputs in Module-All Results Explorer(coded as mod1) --------------------------------
    
    # Insert the right number of plot output objects into UI
    output$mod1_output_plot <- renderUI({
        ## limit plots to specified stat_name
        obj_name <- subset(obj_name, V1==mod1_input_object()[1])
        obj_name <- subset(obj_name, V2==mod1_input_object()[3])
        output_order <- obj_name %>%
            dplyr::mutate(order=seq(from=1, to=n()))
        output_order <- subset(output_order, stat_name==mod1_input_object()[2])
        plots <- list()
        for(plot_i in seq_along(output_order$order)){
            plots[[plot_i]] <- mtm_res_get_entries(D, c(mod1_input_object()[1], mod1_input_object()[3]))[[output_order$order[plot_i]]]
        }
        # there are multiple plots
        len_i <- length(plots)
        # some plots have multiple objects
        len_j <- length(plots[[1]]$output)
        # name every plot object in UI
        mod1_plot_output_list <- lapply(1:(len_i*len_j), function(i) {
            plotname <- paste("Plot", i, sep="")
            # locate the row in the `plots`
            row_n <- ceiling(i/len_j)
            ## set dynamic height of box scatter plots based on output2
            height <- if(plots[[1]]$fun[2]=="box"&plots[[1]]$fun[3]=="scatter"&!is.null(plots[[row_n]]$output2)){
                as.numeric(plots[[row_n]]$output2)*150
            } else {
                560
            }
            plotOutput(plotname, height = height, width = 850)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, mod1_plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    # get the max number of objects
    num_df <- subset(obj_name, V1=="plots")
    num_df <- num_df %>%
        dplyr::group_by(V2, stat_name) %>%
        dplyr::summarise(cnt_sum=sum(cnt))
    max_plot <- max(num_df$cnt_sum)
    
    for (i in 1:max_plot) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
            my_i <- i
            plotname <- paste("Plot", my_i, sep="")
            output[[plotname]] <- renderPlot({
                ## limit plots to specified stat_name
                obj_name <- subset(obj_name, V1==mod1_input_object()[1])
                obj_name <- subset(obj_name, V2==mod1_input_object()[3])
                output_order <- obj_name %>%
                    dplyr::mutate(order=seq(from=1, to=n()))
                output_order <- subset(output_order, stat_name==mod1_input_object()[2])
                plots <- list()
                for(plot_i in seq_along(output_order$order)){
                    plots[[plot_i]] <- mtm_res_get_entries(D, c(mod1_input_object()[1], mod1_input_object()[3]))[[output_order$order[plot_i]]]$output
                }
                # there are multiple plots
                len_i <- length(plots)
                # some plots have multiple objects
                len_j <- length(plots[[1]])
                # locate the row in the `plots`
                row_n <- ceiling(my_i/len_j)
                # locate the column in the `plots`
                col_n <- ifelse((my_i %% len_j)==0, len_j, (my_i %% len_j))
                # render the plot object in each loop
                plots[[row_n]][col_n]
                
            })
        })
    }
    # render stats table of Mod1
    output$mod1_output_table <- renderDataTable({
        table <- data.frame(var=row.names(rowData(D)), rowData(D)) %>%
            left_join(mtm_get_stat_by_name(D, mod1_input_object()[2]), 
                      by=c("var"="var")
            ) %>%
            dplyr::select(c(2, 20:26))
        
        ## put interested columns ahead
        table <- if ('term' %in% names(table)) {
            table %>%
                dplyr::select(name, statistic, p.value, p.adj, term, dplyr::everything()) %>%
                ## scientific notation
                dplyr::mutate(statistic=formatC(statistic, format = "E", digits = 2),
                              p.value=formatC(p.value, format = "E", digits = 2),
                              p.adj=formatC(p.adj, format = "E", digits = 2),
                              estimate=formatC(estimate, format = "E", digits = 2),
                              std.error=formatC(std.error, format = "E", digits = 2)
                )
        } else {
            table %>%
                dplyr::select(name, statistic, p.value, p.adj, dplyr::everything())
        }
        datatable(table,
                  options = list(
                      # limit number of rows
                      pageLength =  10,
                      lengthMenu = c(10, 20, 50),
                      ## set column width
                      autoWidth = TRUE,
                      columnDefs = list(list(width = '100px', targets = c(2:4))),
                      scrollX = TRUE
                  ))
    })
    # render plots or table
    output$mod1_output <- renderUI({
        switch(
            mod1_input_object()[1],
            "plots" = uiOutput("mod1_output_plot"),
            "stats" = dataTableOutput("mod1_output_table")
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)