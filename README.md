## What is maplet-Shiny?


aaaaa
## How to start maplet-Shiny?

- **Step One**: Download the project from github including app.R, help_functions.R and www(logo & CSS file)

- **Step Two**: Make sure your interested SE object with analysis pipeline stored in metadata be located in the same local repository with app.R.

- **Step Three**: Click the '.Rproj' file to open this project and then open the 'app.R' file. In the up-right corner of the source panel of 'app.R', click the 'Run App' to run this Shiny application.

> **_NOTE:_** The control widgets for user inputs of maplet-Shiny is SE-dependent thus you have to prepare a result SE object for this application to run.

## What's the features of maplet-Shiny?

The data flow of maplet-Shiny is **data uploading** -> **data exploration** -> **data visulization**. The first module is for data uploading where you can start with an excel of raw data, input parameters to run a pipeline and investigate the log text. The second module is for exploring the annotations of features and samples. The last four modules are to visualize the plots & stat tables stored in the metadata of SE object either by printing them exactly the same way as they are or by re-creating them in an interactive way before downloading the new plots.

## Contributors

The first version of maplet-Shiny is a capstone project of Weill Cornell Medicine.

- Team members: Jinfeng Lu, Xiang Zhu, Yifan Wu

- Instructors: Elisa Benedetti, Kelsey Chetnik, Jan Krumsiek, Mustafa Buyukozkan, Elizabeth Sweeney
