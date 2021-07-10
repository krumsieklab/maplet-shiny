## What is maplet-Shiny?

This paper presents the user guide and developer guide for a R Shiny-based, interactive interface prototype for maplet. Maplet is an extensive R toolbox for modular and reproducible statistical pipelines for omics data analysis. Shiny is a powerful R package that enables the developers to build elegant interactive web applications. Our maplet Shiny interface is based on the SummarizedExperiment data structure created by the maplet pipelines. The application is constituted by six modules with a variety of functionalities, including data uploading, exploration and visualization. The concise and user-friendly page design facilitates exploratory analysis, statistical analysis, result reporting, and visualization for researchers. In addition, the interface allows the user to download all interactive plots and to customize the plot annotations, which efficiently helps users to better inspect the data and gain insights into the statistical results. The developer guide describes in detail the structure of the interface for future revision and development. 

### Resoure:

- **User guide & developer guide**: Read the file "CapstoneFinalPaper_JinfengLu_YifanWu_XiangZhu.pdf"

- **Poster**: Read the file "CapstonePoster_JinfengLu_YifanWu_XiangZhu.pdf"

## How to start maplet-Shiny?

- **Step One**: Download the project from github including app.R, help_functions.R and www (logo & CSS file)

- **Step Two**: Make sure your interested SE object with analysis pipeline stored in metadata be located in the same local repository with app.R.

- **Step Three**: Click the '.Rproj' file to open this project and then open the 'app.R' file. In the up-right corner of the source panel of 'app.R', click the 'Run App' to run this Shiny application.

> **_NOTE:_** The control widgets for user inputs of maplet-Shiny is SE-dependent thus you have to prepare a result SE object for this application to run.

## What's the features of maplet-Shiny?

The data flow of maplet-Shiny is **data uploading** -> **data exploration** -> **data visulization**. The first module is for data uploading where you can start with an excel of raw data, input parameters to run a pipeline and investigate the log text. The second module is for exploring the annotations of features and samples. The last four modules are to visualize the plots & stat tables stored in the metadata of SE object either by printing them exactly the same way as they are or by re-creating them in an interactive way before downloading the new plots.

- **Real-Time Pipeline Module**: Starts with original data, creates a pipeline and download it to local.

- **Annotations Explorer Module**: Creates tables, distribution plots, or other graphics to explore the SE object.

- **2D Projection Module**: Generates an interactive 2D projection of PCA/UMAP.

- **All Results Explorer Module**: Extracts all the result objects one at a time.

- **Feature Results Explorer Module**: Collects all statistical results in a table given one metabolite name. When clicking on one row, it should display interactive plots following
the order of StatsBar plot -> Equalizer/Volcano plot -> Box/Scatter plot.

- **Pathway Results Explorer Module**: Displays a series of interactive plots at different granularities given a SE and a statname following the order of StatsBar plot -> Equalizer/Volcano plot -> Box/Scatter plot.

## Contributors

The first version of maplet-Shiny is a capstone project of Weill Cornell Medicine.

- Team members: Jinfeng Lu, Xiang Zhu, Yifan Wu

- Instructors: Elisa Benedetti, Kelsey Chetnik, Jan Krumsiek, Mustafa Buyukozkan, Elizabeth Sweeney
