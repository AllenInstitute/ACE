# annotation_comparison

R Shiny app for comparison of annotations such as (i) cell type assignments (e.g., from different mapping/clustering algorithms), (ii) donor metadata (e.g., donor, sex, age), and (iii) cell metadata (e.g., anatomic location, QC metrics).  This tool is now **[hosted on shinyapps.io](https://sea-ad.shinyapps.io/ACEapp/)**.

![image](https://github.com/AllenInstitute/ACE/assets/25486679/9bc32bb6-8245-4c5e-8ea0-b26be27fcab0)
![image](https://github.com/AllenInstitute/ACE/assets/25486679/aef25d6d-f842-42d9-8b72-a65604b875b0)


Additionally, this tool can compare results across more than two taxonomies as once. The two primary use cases are (1) annotating a novel taxonomy with information from multiple existing taxonomies and (2) comparison of data from multiple studies of Alzheimer's disease (like in the example below).

![multi_comparison](https://github.com/AllenInstitute/annotation_comparison/assets/25486679/c55dac6e-99f6-4fd6-9203-70e136cef9a7)

## How to use 

Either visit the tool **[hosted on shinyapps.io](https://sea-ad.shinyapps.io/ACEapp/)** or follow the instructions below to run locally in R Studio.  Currently, you need to run it locally to point to external files stored locally, but for most use cases, the hosted version is recommended.

### Setup RStudio
(You only need do this step once).
1. Install R
2. Install RStudio
3. Install the following packages in R Studio:
```
options(install.packages.compile.from.source = "always")
packages <- setdiff(c("remotes","dplyr","data.table","DT","feather","ggplot2","ggbeeswarm","shiny","UpSetR","anndata"), installed.packages()[,"Package"])
if(length(packages)>0) install.packages(packages, type = "both")
remotes::install_github("AllenInstitute/scrattch.io")
remotes::install_github("bokeh/rbokeh")
```
4. Download the files from this repo (you may want to repeat this step occasionally to check for updates!) 

### Run the app
1. In RStudio, open the file `ui.R` and click the "Run App" button in the upper right corner of the screen
2. Enter the location of your annotation file in the "Location of annotation information" box (it *should* be pointing to some example data by default)
3. Explore!

## Reporting issues or suggestions

Please share any comments, suggestions, bugs, or any other thoughts using the button in the app, or by [submitting an issue](https://github.com/AllenInstitute/ACE/issues).
![image](https://github.com/AllenInstitute/ACE/assets/25486679/a0e2ee8d-5315-400d-a723-b9bb9719c4bd)

