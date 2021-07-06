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
        
        # Define layout of Module-Real-Time Pipeline(coded as mod6) ----------------------------------------------------
        tabPanel(HTML(paste("Real-Time", "Pipeline", sep = "<br/>")), 
                 # Sidebar layout with input and output definitions ----
                 dashboardPage(
                     dashboardHeader(disable = TRUE),
                     dashboardSidebar(disable = TRUE),
                     dashboardBody(
                         sidebarLayout(
                             # Sidebar panel for inputs ----
                             sidebarPanel(
                                 id = "mod6_panel1",
                                 style = "overflow-y: scroll; max-height: 700px; width: 80%; position:relative; margin-left: -5px; margin-top: 45px; margin-bottom: 5px;",
                                 tags$p(
                                     HTML("<b>Real-Time Pipeline Module</b> starts with original data, creates a pipeline and download it to local."
                                     )),
                                 tags$p(
                                     HTML("Pipeline is constrained to run <b>Data Loading->Preprocessing->Differential Analysis</b> and no section should be skipped."
                                     )),
                                 tags$p(
                                     HTML("The result SE object is dependent on the <b>instant parameters</b>."
                                     )),
                                 # Input: Select a file ----
                                 fileInput("file1", "Uploading File",
                                           multiple = FALSE,
                                           accept = c(".xlsx"),
                                           width = "300px"),
                                 # Input: Checkbox if file has header ----
                                 checkboxInput("header", "Header", TRUE),
                                 
                                 tags$hr(),
                                 
                                 box(solidHeader = T, collapsible = T, collapsed = T,
                                     title="Sheets for Dimensions", width = "220px",
                                     checkboxInput("mod6_assay_in_row", "Samples in rows?", FALSE),
                                     tags$p(HTML("Assay sheet:")),
                                     uiOutput("mod6_assay_sheet"),
                                     tags$p(HTML("rowData sheet:")),
                                     uiOutput("mod6_rowdata_sheet"),
                                     tags$p(HTML("colData sheet:")),
                                     uiOutput("mod6_coldata_sheet"),
                                     tags$p(HTML("Click to show the original data, but this investigation is not necessary for pipeline.")),
                                     actionButton("mod6_go", "Investigate")
                                 ),
                                 
                                 tags$hr(),
                                 
                                 box(solidHeader = T, collapsible = T, collapsed = T,
                                     title="Data Loading", width = "220px",
                                     tags$p(HTML("ID column in assay:")),
                                     uiOutput("mod6_assay_id_column"),
                                     tags$p(HTML("ID column in rowData:")),
                                     uiOutput("mod6_rowdata_id_column"),
                                     tags$p(HTML("ID column in colData:")),
                                     uiOutput("mod6_coldata_id_column"),
                                     tags$p(HTML("Run to see log text of data loading.")),
                                     actionButton("mod6_go_load", "Run", width = "110px")
                                 ),
                                 
                                 tags$hr(),
                                 
                                 box(solidHeader = T, collapsible = T, collapsed = T,
                                     title="Preprocessing", width = "220px",
                                     tags$p(HTML("Max % missingness per feature:")),
                                     numericInput("mod6_filter_feat_max", label = NULL,
                                                  value = 100,
                                                  min = 0,
                                                  max = 100,
                                                  step = 5,
                                                  width = "220px"),
                                     tags$p(HTML("Max % missingness per feature (normalization):")),
                                     numericInput("mod6_feat_max_norm", label = NULL,
                                                  value = 100,
                                                  min = 0,
                                                  max = 100,
                                                  step = 5,
                                                  width = "220px"),
                                     tags$p(HTML("Max % missingness per sample:")),
                                     numericInput("mod6_filter_sample_max", label = NULL,
                                                  value = 100,
                                                  min = 0,
                                                  max = 100,
                                                  step = 5,
                                                  width = "220px"),
                                     tags$p(HTML("Sample coloring column:")),
                                     uiOutput("mod6_pre_sample_color_column"),
                                     tags$p(HTML("Batch column:")),
                                     uiOutput("mod6_pre_batch_column"),
                                     tags$p(HTML("PCA/UMAP coloring column:")),
                                     uiOutput("mod6_pre_pca_color_column"),
                                     tags$p(HTML("Heatmap annotation column:")),
                                     uiOutput("mod6_pre_heatmap_anno_column"),
                                     tags$p(HTML("Heatmap annotation row:")),
                                     uiOutput("mod6_pre_heatmap_anno_row"),
                                     tags$p(HTML("Run to see log text of data loading and preprocessing. This step may cost a few seconds to run.")),
                                     actionButton("mod6_go_preprocess", "Run", width = "110px")
                                 ),
                                 
                                 tags$hr(),
                                 
                                 box(solidHeader = T, collapsible = T, collapsed = T,
                                     title="Differential Analysis", width = "220px",
                                     tags$p(HTML("Outcome variable:")),
                                     uiOutput("mod6_outcome"),
                                     checkboxInput("mod6_outcome_binary", "Binary outcome?", FALSE),
                                     tags$p(HTML("Type of analysis:")),
                                     selectInput("mod6_analysis_type", label = NULL,
                                                 width = "220px",
                                                 choices = c("lm","pearson","spearman","kendall"),
                                                 selected = "lm"),
                                     tags$p(HTML("Multiple testing correction:")),
                                     selectInput("mod6_mult_test_method", label = NULL,
                                                 width = "220px",
                                                 choices = c("BH","bonferroni","BY"),
                                                 selected = "BH"),
                                     tags$p(HTML("Significance threshold:")),
                                     numericInput("mod6_sig_threshold", label = NULL,
                                                  value = 0.05,
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.01,
                                                  width = "220px"),
                                     tags$p(HTML("Pathway aggregation in barplot:")),
                                     uiOutput("mod6_group_col_barplot"),
                                     tags$p(HTML("Barplot coloring column:")),
                                     uiOutput("mod6_color_col_barplot"),
                                     tags$p(HTML("Run to see log text of data loading, preprocessing and differential analysis. This step may cost a few seconds to run.")),
                                     actionButton("mod6_go_differ", "Run", width = "110px")
                                 )
                             ),
                             
                             # Main panel for displaying outputs ----
                             mainPanel(
                                 id = "mod6_panel2", 
                                 style = "overflow-y: auto; max-height: 85vh; position: absolute; left: 28%",
                                 br(), 
                                 br(), 
                                 br(), 
                                 # Output: Data file ----
                                 tags$p(HTML("Downloading SE.Rdata may cost more than one minute. Please wait for the prompt.")),
                                 downloadButton("download_se", "Download result SE .Rdata"),
                                 br(),
                                 br(),
                                 uiOutput("mod6_main_panel")
                             )
                         )
                     )
                 )
        )
    )
)

################################################################################
################ Define server logic required to draw outputs ##################
################################################################################

server <- function(input, output) {
    
    # Define rendering logic of control widgets in Module-Real-Time Pipeline (coded as mod6)----------------------
    # control widget of selecting file
    output$mod6_assay_sheet <- renderUI({
        req(input$file1)
        selectInput("assay_sheet", label = NULL,
                    width = "220px",
                    choices = getSheetNames(as.character(input$file1$datapath))
        )
    })
    # control widget of dimensions
    df_assay <- reactive({
        read_excel(as.character(input$file1$datapath),
                   col_names = input$header,
                   sheet=input$assay_sheet)
    })
    
    output$mod6_rowdata_sheet <- renderUI({
        req(input$file1)
        selectInput("rowdata_sheet", label = NULL,
                    width = "220px",
                    choices = getSheetNames(as.character(input$file1$datapath))
        )
    })
    df_rowdata <- reactive({
        read_excel(as.character(input$file1$datapath),
                   col_names = input$header,
                   sheet=input$rowdata_sheet)
    })
    
    output$mod6_coldata_sheet <- renderUI({
        req(input$file1)
        selectInput("coldata_sheet", label = NULL,
                    width = "220px",
                    choices = getSheetNames(as.character(input$file1$datapath))
        )
    })
    df_coldata <- reactive({
        read_excel(as.character(input$file1$datapath),
                   col_names = input$header,
                   sheet=input$coldata_sheet)
    })
    # control widget of data loading
    output$mod6_assay_id_column <- renderUI({
        selectInput("assay_id_column", label = NULL,
                    width = "220px",
                    choices = colnames(df_assay())
        )
    })
    
    output$mod6_rowdata_id_column <- renderUI({
        selectInput("rowdata_id_column", label = NULL,
                    width = "220px",
                    choices = colnames(df_rowdata())
        )
    })
    
    output$mod6_coldata_id_column <- renderUI({
        selectInput("coldata_id_column", label = NULL,
                    width = "220px",
                    choices = colnames(df_coldata())
        )
    })
    # control widget of preprocessing
    output$mod6_pre_sample_color_column <- renderUI({
        selectInput("pre_sample_color_column", label = NULL,
                    width = "220px",
                    choices = colnames(df_coldata())
        )
    })
    
    output$mod6_pre_batch_column <- renderUI({
        selectInput("pre_batch_column", label = NULL,
                    width = "220px",
                    selected=NULL,
                    choices = colnames(df_coldata())
        )
    })
    
    output$mod6_pre_pca_color_column <- renderUI({
        selectInput("pre_pca_color_column", label = NULL,
                    width = "220px",
                    multiple=TRUE,
                    selected=NULL,
                    choices = colnames(df_coldata())
        )
    })
    
    output$mod6_pre_heatmap_anno_column <- renderUI({
        selectInput("pre_heatmap_anno_column", label = NULL,
                    width = "220px",
                    multiple=TRUE,
                    selected=NULL,
                    choices = colnames(df_coldata())
        )
    })
    
    output$mod6_pre_heatmap_anno_row <- renderUI({
        selectInput("pre_heatmap_anno_row", label = NULL,
                    width = "220px",
                    multiple=TRUE,
                    selected=NULL,
                    choices = colnames(df_rowdata())
        )
    })
    # control widget of differential analysis
    output$mod6_outcome <- renderUI({
        selectInput("outcome", label = NULL,
                    width = "220px",
                    choices = colnames(df_coldata())
        )
    })
    
    output$mod6_group_col_barplot <- renderUI({
        selectInput("group_col_barplot", label = NULL,
                    width = "220px",
                    selected=NULL,
                    choices = colnames(df_rowdata())
        )
    })
    
    output$mod6_color_col_barplot <- renderUI({
        selectInput("color_col_barplot", label = NULL,
                    width = "220px",
                    selected=NULL,
                    choices = colnames(df_rowdata())
        )
    })
    
    # Define rendering logic of outputs in Module-Real-Time Pipeline(coded as mod6) ------------------------------
    # record the file path of selected file
    mod6_filepath <- 
        eventReactive(input$mod6_go, ## delayed output
                      {c(input$file1$datapath)
                      })
    
    # print table when clicking "investigate" button
    observeEvent(input$mod6_go,{
        output$mod6_main_panel <- renderUI({
            list(dataTableOutput("mod6_assay"),
                 br(),br(),
                 dataTableOutput("mod6_rowdata"),
                 br(),br(),
                 dataTableOutput("mod6_coldata"))
        })
    })
    
    # render logic of the table
    output$mod6_assay <- renderDataTable({
        table <- read_excel(as.character(mod6_filepath()),
                            col_names = input$header,
                            sheet=input$assay_sheet)
        datatable(table,
                  caption="Original Assay Data",
                  options = list(
                      # limit number of rows
                      pageLength =  10,
                      lengthMenu = c(10, 20, 50),
                      autoWidth = TRUE
                  ))
    })
    
    output$mod6_rowdata <- renderDataTable({
        table <- read_excel(as.character(mod6_filepath()),
                            col_names = input$header,
                            sheet=input$rowdata_sheet)
        datatable(table,
                  caption="Original rowData",
                  options = list(
                      # limit number of rows
                      pageLength =  10,
                      lengthMenu = c(10, 20, 50),
                      autoWidth = TRUE
                  ))
    })
    
    output$mod6_coldata <- renderDataTable({
        table <- read_excel(as.character(mod6_filepath()),
                            col_names = input$header,
                            sheet=input$coldata_sheet)
        datatable(table,
                  caption="Original colData",
                  options = list(
                      # limit number of rows
                      pageLength =  10,
                      lengthMenu = c(10, 20, 50),
                      autoWidth = TRUE
                  ))
    })
    
    observeEvent(input$mod6_go_load,{
        # define main panel for loading section
        output$mod6_main_panel <- renderUI({
            tagAppendAttributes(verbatimTextOutput("log_load"), 
                                style="white-space:pre-wrap;")
        })
    })
    
    observeEvent(input$mod6_go_preprocess,{
        # define main panel for preprocessing section
        output$mod6_main_panel <- renderUI({
            tagAppendAttributes(verbatimTextOutput("log_preprocess"), 
                                style="white-space:pre-wrap;")
        })
    })
    
    observeEvent(input$mod6_go_differ,{
        # define main panel of differential analysis
        output$mod6_main_panel <- renderUI({
            tagAppendAttributes(verbatimTextOutput("log_differ"), 
                                style="white-space:pre-wrap;")
        })
    })
    
    # get loading SE
    D_load <- reactive({
        ## loading D
        file_data <- as.character(input$file1$datapath)
        D <-
            mt_load_xls(file=file_data, 
                        sheet=input$assay_sheet, 
                        samples_in_row=input$mod6_assay_in_row, 
                        id_col=input$assay_id_column) %>%
            mt_anno_xls(file=file_data, 
                        sheet=input$rowdata_sheet,
                        anno_type="features", 
                        anno_id_col=input$rowdata_id_column, 
                        data_id_col = "name") %>%
            mt_anno_xls(file=file_data, 
                        sheet=input$coldata_sheet, 
                        anno_type="samples", 
                        anno_id_col =input$coldata_id_column, 
                        data_id_col ="sample") %>%
            mt_reporting_data() %>%
            {.}
        ## return D
        D
    })
    
    # get proprocessing SE
    D_preprocess <- reactive({
        ## preprocessing D
        D <- D_load() %>%
            mt_reporting_heading(heading = "Preprocessing", lvl=1) %>%
            mt_reporting_heading(heading = "Filtering", lvl = 2) %>%
            mt_plots_missingness(feat_max=(input$mod6_filter_feat_max)/100,samp_max = (input$mod6_filter_sample_max)/100) %>%
            mt_pre_filter_missingness(feat_max = (input$mod6_filter_feat_max)/100, samp_max = (input$mod6_filter_sample_max)/100) %>%
            mt_plots_missingness(feat_max=(input$mod6_filter_feat_max)/100, samp_max = (input$mod6_filter_sample_max)/100) %>%
            mt_anno_missingness(anno_type = "samples", out_col = "missing") %>%
            mt_anno_missingness(anno_type = "features", out_col = "missing") %>%
            mt_reporting_heading(heading = "Normalization", lvl = 2) %>%
            mt_plots_sample_boxplot(color=!!sym(input$pre_sample_color_column), title = "Original", plot_logged = T) %>%
            {.}
        if(!is.null(input$pre_batch_column)){
            D %<>%
                mt_pre_batch_median(batch_col = input$pre_batch_column)
        }
        D <- D %>%
            mt_plots_sample_boxplot(color=!!sym(input$pre_sample_color_column), title = "After batch correction", plot_logged = T) %>%
            mt_pre_norm_quot(feat_max = (input$mod6_feat_max_norm)/100) %>%
            mt_plots_dilution_factor(in_col=input$pre_sample_color_column) %>%
            mt_plots_sample_boxplot(color=!!sym(input$pre_sample_color_column), title = "After normalization", plot_logged = T) %>%
            mt_pre_trans_log() %>%
            mt_pre_impute_knn() %>%
            mt_plots_sample_boxplot(color=!!sym(input$pre_sample_color_column), title = "After imputation", plot_logged = T) %>%
            mt_pre_outlier_detection_univariate() %>%
            mt_reporting_data() %>%
            mt_reporting_heading(heading = "Global Statistics", lvl = 1) %>%
            {.}
        ## add PCA/UMAP plots
        lapply(input$pre_pca_color_column, function(x){
            D <<- D %>%
                mt_plots_pca(scale_data = T, title = sprintf("scaled PCA - %s",x), color=!!sym(x), size=2.5, ggadd=scale_size_identity()) %>%
                mt_plots_umap(scale_data = T, title = sprintf("scaled UMAP - %s",x), color=!!sym(x), size=2.5, ggadd=scale_size_identity()) %>%
                {.}
        }) %>% invisible
        ## add heatmap
        D %<>%
            mt_plots_heatmap(scale_data = T, annotation_col = input$pre_heatmap_anno_column, annotation_row = input$pre_heatmap_anno_row,
                             clustering_method = "ward.D2", fontsize = 5, cutree_rows = 3, cutree_cols = 3, color=gplots::bluered(101)) %>%
            {.}
        ## return D
        D
    })
    
    ## get differential analysis SE
    D_differ <- reactive({
        # Differential analysis D
        D <- D_preprocess() %>%
            mt_reporting_heading(heading = "Statistical Analysis", lvl = 1) %>%
            diff_analysis_func(var=input$outcome,
                               binary=input$mod6_outcome_binary,
                               analysis_type=input$mod6_analysis_type, 
                               mult_test_method=input$mod6_mult_test_method,
                               alpha=input$mod6_sig_threshold,
                               group_col_barplot=input$group_col_barplot,
                               color_col_barplot=input$color_col_barplot) %>%
            {.}
        ## return D
        D
    })
    
    # render logic of the log text of data loading
    output$log_load <- renderPrint({
        get_log_text(D_load())
    })
    # render logic of the log text of preprocessing
    output$log_preprocess <- renderPrint({
        # loading log
        text_load <- get_log_text(D_load())
        # preprocessing log
        text_preprocess <- get_log_text(D_preprocess())
        # paste log text
        str <- paste(text_load, text_preprocess, sep = "\n")
        cat(str)
    })
    
    # render logic of the log text of differential analysis
    output$log_differ <- renderPrint({
        # loading log
        text_load <- get_log_text(D_load())
        # preprocessing log
        text_preprocess <- get_log_text(D_preprocess())
        # differential analysis log
        text_differ <- get_log_text(D_differ())
        # paste log text
        str <- paste(text_load, text_preprocess, text_differ, sep = "\n")
        cat(str)
    })
    
    # download SE button
    # https://mastering-shiny.org/action-transfer.html
    output$download_se <- downloadHandler(
        filename = function() {
            paste0("SE_", Sys.Date(), ".Rdata")
        },
        content = function(fname) {
            ## loading D
            file_data <- as.character(input$file1$datapath)
            D <-
                mt_load_xls(file=file_data, 
                            sheet=input$assay_sheet, 
                            samples_in_row=input$mod6_assay_in_row, 
                            id_col=input$assay_id_column) %>%
                mt_anno_xls(file=file_data, 
                            sheet=input$rowdata_sheet,
                            anno_type="features", 
                            anno_id_col=input$rowdata_id_column, 
                            data_id_col = "name") %>%
                mt_anno_xls(file=file_data, 
                            sheet=input$coldata_sheet, 
                            anno_type="samples", 
                            anno_id_col =input$coldata_id_column, 
                            data_id_col ="sample") %>%
                mt_reporting_data() %>%
                {.}
            ## preprocessing D
            D <- D %>%
                mt_reporting_heading(heading = "Preprocessing", lvl=1) %>%
                mt_reporting_heading(heading = "Filtering", lvl = 2) %>%
                mt_plots_missingness(feat_max=(input$mod6_filter_feat_max)/100,samp_max = (input$mod6_filter_sample_max)/100) %>%
                mt_pre_filter_missingness(feat_max = (input$mod6_filter_feat_max)/100, samp_max = (input$mod6_filter_sample_max)/100) %>%
                mt_plots_missingness(feat_max=(input$mod6_filter_feat_max)/100, samp_max = (input$mod6_filter_sample_max)/100) %>%
                mt_anno_missingness(anno_type = "samples", out_col = "missing") %>%
                mt_anno_missingness(anno_type = "features", out_col = "missing") %>%
                mt_reporting_heading(heading = "Normalization", lvl = 2) %>%
                mt_plots_sample_boxplot(color=!!sym(input$pre_sample_color_column), title = "Original", plot_logged = T) %>%
                {.}
            if(!is.null(input$pre_batch_column)){
                D %<>%
                    mt_pre_batch_median(batch_col = input$pre_batch_column)
            }
            D <- D %>%
                mt_plots_sample_boxplot(color=!!sym(input$pre_sample_color_column), title = "After batch correction", plot_logged = T) %>%
                mt_pre_norm_quot(feat_max = (input$mod6_feat_max_norm)/100) %>%
                mt_plots_dilution_factor(in_col=input$pre_sample_color_column) %>%
                mt_plots_sample_boxplot(color=!!sym(input$pre_sample_color_column), title = "After normalization", plot_logged = T) %>%
                mt_pre_trans_log() %>%
                mt_pre_impute_knn() %>%
                mt_plots_sample_boxplot(color=!!sym(input$pre_sample_color_column), title = "After imputation", plot_logged = T) %>%
                mt_pre_outlier_detection_univariate() %>%
                mt_reporting_data() %>%
                mt_reporting_heading(heading = "Global Statistics", lvl = 1) %>%
                {.}
            ## add PCA/UMAP plots
            lapply(input$pre_pca_color_column, function(x){
                D <<- D %>%
                    mt_plots_pca(scale_data = T, title = sprintf("scaled PCA - %s",x), color=!!sym(x), size=2.5, ggadd=scale_size_identity()) %>%
                    mt_plots_umap(scale_data = T, title = sprintf("scaled UMAP - %s",x), color=!!sym(x), size=2.5, ggadd=scale_size_identity()) %>%
                    {.}
            }) %>% invisible
            ## add heatmap
            D %<>%
                mt_plots_heatmap(scale_data = T, annotation_col = input$pre_heatmap_anno_column, annotation_row = input$pre_heatmap_anno_row,
                                 clustering_method = "ward.D2", fontsize = 5, cutree_rows = 3, cutree_cols = 3, color=gplots::bluered(101)) %>%
                {.}
            # Differential analysis D
            D <- D %>%
                mt_reporting_heading(heading = "Statistical Analysis", lvl = 1) %>%
                diff_analysis_func(var=input$outcome,
                                   binary=input$mod6_outcome_binary,
                                   analysis_type=input$mod6_analysis_type, 
                                   mult_test_method=input$mod6_mult_test_method,
                                   alpha=input$mod6_sig_threshold,
                                   group_col_barplot=input$group_col_barplot,
                                   color_col_barplot=input$color_col_barplot) %>%
                {.}
            # write Rdata to local
            save(D, file=fname)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)