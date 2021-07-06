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
        
        # Define layout of Module-Feature Results Explorer(coded as mod4) --------------------------------------------------
        
        tabPanel(HTML(paste("Feature Results", "Explorer", sep = "<br/>")),
                 sidebarLayout(
                     sidebarPanel(
                         id = "mod4_panel1",
                         # sidebar autoscroll with main panel
                         style = "margin-left: -25px; margin-top: 45px; margin-bottom: 5px; position:fixed; width: 20%; height: 100%;",
                         tags$p(
                             HTML(
                                 "<b>Feature Results Explorer</b> requires collection on all statistical results in a table given one metabolite name."
                             )
                         ),
                         tags$p(
                             HTML(
                                 "When clicking on one row, it should display interactive plots following the same orders in Module 2."
                             )
                         ),
                         # select one metabolite
                         selectInput(
                             "mod4_metabolite",
                             "Select one metabolite:",
                             width = "220px",
                             choices = arrange(mtm_res_get_entries(D, c("stats", "univ"))[[1]]$output$table, var)$var,
                             selected = ""
                         ),
                         br(),
                         checkboxInput("mod4.categorical", 
                                       "Treat as categorical", 
                                       value = FALSE
                         ),
                         br(),
                         tags$p(
                             HTML("<b>Hint:<br></b>Outputs are delayed untill you click 'UPDATE' button after selection."
                             )),
                         # delay the output
                         actionButton("mod4_go", "Update")
                     ),
                     mainPanel(
                         id = "mod4_panel2",
                         br(),
                         br(),
                         br(),
                         style = "overflow-y: auto; position: absolute; left: 25%",
                         # stats table
                         dataTableOutput('mod4_table'),
                         br(),
                         br(),
                         # volcano plotly
                         uiOutput("mod4.p1"),
                         br(),
                         br(),
                         # box/scatter plotly
                         uiOutput("mod4.p.ui"),
                         uiOutput("mod4.p2")
                     )
                 )
        )
    )
)

################################################################################
################ Define server logic required to draw outputs ##################
################################################################################

server <- function(input, output) {
    # store reactive output plots
    session_store <- reactiveValues()
    
    # Define rendering logic of outputs in Module-Feature Results Explorer(coded as mod4) --------------------------------
    
    # Module 4: general reactive stats table
    mod4_metabolite_table <-
        eventReactive(input$mod4_go,
                      {
                          table <- data.frame()
                          # assign an object of all stats tables
                          table_stats <- mtm_res_get_entries(D, c("stats", "univ"))
                          # assign an object of all stats plots
                          plot_stats <- mtm_res_get_entries(D, c("plots", "stats"))
                          for (i in 2:length(table_stats)) {
                              tab <- table_stats[[i]]$output$table %>%
                                  dplyr::mutate(`stat name` = plot_stats[[i - 1]]$args$stat_list)
                              table <- rbind(table, tab)
                          }
                          table <- table %>%
                              dplyr::select(var, statistic, p.value, p.adj, `stat name`, estimate, std.error) %>%
                              dplyr::mutate(
                                  statistic = formatC(statistic, format = "E", digits = 2),
                                  p.value = formatC(p.value, format = "E", digits = 2),
                                  p.adj = formatC(p.adj, format = "E", digits = 2),
                                  estimate = formatC(estimate, format = "E", digits = 2),
                                  std.error = formatC(std.error, format = "E", digits = 2)
                              ) %>%
                              dplyr::filter(var == input$mod4_metabolite) %>%
                              dplyr::rename("name" = var)
                      })
    
    # Module 4: output the stats table
    output$mod4_table <- renderDataTable({
        datatable(mod4_metabolite_table(),
                  selection = "single",
                  options = list(
                      dom = 't',
                      # limit number of rows
                      pageLength =  10,
                      lengthMenu = c(10, 20, 50)
                  )
        )
    })
    
    observe({
        if (!is.null(input$mod4_table_rows_selected)) {
            session_store$mod4.tb.row <- input$mod4_table_rows_selected
        }
    })
    
    # mod4: extract the stat_name
    stat_name_selected <- reactive({
        mod4_metabolite_table() %>% 
            dplyr::slice(round(as.numeric(session_store$mod4.tb.row))) %>%
            dplyr::select(`stat name`)
    })
    
    # Module 4: volcano plot
    output$mod4.p1 <- renderUI({
        if (!is.null(session_store$mod4.tb.row)) {
            list(
                downloadButton("mod4_download_plotly_volcano", "download volcano plot"),
                plotlyOutput('mod4_volcano', height = 800)
            )
        }
    })
    
    # Module 4: volcano plot by using stat_name
    output$mod4_volcano <- renderPlotly({
        # Get volcano data set
        data_vol <- get_data_by_name(D, "stat_name", "volcano", stat_name_selected())
        isSelected <- input$mod4_metabolite
        
        # Set the legend color column
        data_vol[, "isSelected"] <- ifelse(data_vol$var==isSelected, TRUE, FALSE)
        highlight_point <- data_vol[data_vol$isSelected==TRUE, ]
        
        plot <- data_vol %>%
            ggplot(aes(x = statistic, y = p.value, color = isSelected, label = name)) +
            geom_point() +
            geom_point(data=highlight_point, size = 3) +
            scale_y_continuous(trans = reverselog_trans(10),
                               breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(y = "p-value (10^(-y))") +
            ggtitle(paste0(stat_name_selected(), "-", isSelected)) +
            scale_color_manual(values=c("#999999", "red"))
        
        session_store$mod4.vol <- ggplotly(plot, source = "mod4_sub_vol") %>%
            layout(legend = list(orientation = 'h',
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.2,
                                 title = list(text='<b> isSelected </b>')))
        session_store$mod4.vol
    })
    
    # Module 4: volcano plot - html file
    output$mod4_download_plotly_volcano <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
            saveWidget(as_widget(session_store$mod4.vol), file, selfcontained = TRUE)
        }
    )
    
    # Module 4: box/scatter plot
    output$mod4.p2 <- renderUI({
        d <- event_data("plotly_click", source = "mod4_sub_vol")
        
        if (!is.null(d)) {
            download.name <- ifelse(
                input$mod4.box.or.scatter == "box",
                "download box plot",
                "download scatter plot"
            )
            list(
                downloadButton("mod4_download_box_scatter", download.name),
                plotOutput("mod4.box.scatter", height = 600)
            )
        }
    })
    
    # Module 4: box/scatter - ui
    output$mod4.p.ui <- renderUI({
        d <- event_data("plotly_click", source = "mod4_sub_vol")
        if (!is.null(d)) {
            radioButtons(
                "mod4.box.or.scatter",
                "Select plot type:",
                choices = list("Box" = "box",
                               "Scatter" = "scatter"),
                selected  = "scatter"
            )
        }
    })
    
    # Module 4: box/scatter plot
    output$mod4.box.scatter <- renderPlot({
        # Get the data set
        data <- D %>%
            maplet:::mti_format_se_samplewise() %>%
            tidyr::gather(var, value, dplyr::one_of(rownames(D)))
        
        d <- event_data("plotly_click", source = "mod4_sub_vol")
        
        if (!is.null(d)) {
            data_vol <- get_data_by_name(D, "stat_name", "volcano", stat_name_selected())
            
            # set the column curveNumber by color legend
            isSelected <- input$mod4_metabolite
            data_vol[, "curveNumber"] <- ifelse(data_vol$var==isSelected, 1, 0)
            data_vol_true <- data_vol[data_vol$curveNumber==1, ]
            data_vol_false <- data_vol[data_vol$curveNumber==0, ]
            
            # By using click info (curveNumber & ponitNumber) to get the metabolite name
            metabolite <- ifelse(d$curveNumber == 1,
                                 data_vol_true[d$pointNumber + 1, ]$var[1],
                                 data_vol_false[d$pointNumber + 1, ]$var[1])
            term <- data_vol$term[1]
            
            # Filter the data by metabolite name
            data <- data[data$var == metabolite, ]
            
            # Treat as categorical or not?
            if (input$mod4.categorical) {
                data[, term] <- factor(data[, term])
            } else {
                data[, term] <- as.numeric(data[, term])
            }
            
            # Draw the plot
            if (input$mod4.box.or.scatter == "scatter") {
                plot <- data %>%
                    ggplot(aes(x = !!sym(term), y = value)) +
                    geom_point(size = 3) +
                    geom_smooth(method = "lm", se = T, color = "black") + 
                    ggtitle(metabolite)
            } else {
                plot <- data %>%
                    ggplot(aes(x = !!sym(term), y = value)) +
                    geom_boxplot() +
                    geom_jitter(size = 3, width = 0.2) +
                    ggtitle(metabolite)
            }
        }
        
        session_store$mod4.box.scatter <- if (is.null(plot)) NULL else plot
        session_store$mod4.box.scatter
    })
    
    # Module 4: scatter/box plot - png file
    output$mod4_download_box_scatter <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = session_store$mod4.box.scatter, device = device)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)