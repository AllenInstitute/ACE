# Annotation Comparison Explorer (ACE)

R Shiny and [web-based](https://sea-ad.shinyapps.io/ACEapp/) tool for comparison of annotations such as (i) cell type assignments, (ii) donor metadata (e.g., donor, sex, age), and (iii) cell metadata (e.g., anatomic location, QC metrics).  Two primary use cases are (1) comparing user-defined clustering results and mapping results (e.g., after running [MapMyCells](https://portal.brain-map.org/atlases-and-data/bkp/mapmycells)) for the same set of cells and (2) comparing cell type names between different taxonomies on [Allen Brain Map](https://portal.brain-map.org/cell-types).  Additionally, ACE includes a data set comparing cell type names and associated abudance changes between multiple published studies of Alzheimer's disease in dorsolateral prefrontal cortex (see visualizations far below).

**ACE is now [hosted on shinyapps.io](https://sea-ad.shinyapps.io/ACEapp/)**.

![main_ACE_app_page_top](https://github.com/user-attachments/assets/51375516-0203-45d7-a207-6f91a08140df)

## How to use 

### Visit the website

**We recommend using the version of this tool [hosted on shinyapps.io](https://sea-ad.shinyapps.io/ACEapp/)**. 

### Run locally

For power users, this tool can also be run locally in R Studio by following the steps below.

#### Setup RStudio
(You only need do this step once).
1. Install R
2. Install RStudio
3. Install the following packages in R Studio**:
```
options(install.packages.compile.from.source = "always")
packages <- setdiff(c("remotes","dplyr","data.table","DT","feather","ggplot2","ggbeeswarm","shiny","UpSetR","anndata"), installed.packages()[,"Package"])
if(length(packages)>0) install.packages(packages, type = "both")
remotes::install_github("AllenInstitute/scrattch.io")
remotes::install_github("bokeh/rbokeh")
```
4. Download the files from this repo (you may want to repeat this step occasionally to check for updates!) 

**If there are issues with setup in step #3, note that the complete R environment is saved [in the renv folder](https://github.com/AllenInstitute/ACE/tree/main/renv).

#### Run the app
1. In RStudio, open the file `ui.R` and click the "Run App" button in the upper right corner of the screen
2. Explore!

The only difference between local and web versions is that the local version can point to locations on your local computer (which in most cases is unnecessary as these can be uploaded on the web version).

## Reporting issues or suggestions

Please share any comments, suggestions, bugs, or any other thoughts using the button in the app, or by [submitting an issue](https://github.com/AllenInstitute/ACE/issues).
![image](https://github.com/AllenInstitute/ACE/assets/25486679/a0e2ee8d-5315-400d-a723-b9bb9719c4bd)

## License

The license for this package is available on Github at: https://github.com/AllenInstitute/ACE/blob/master/LICENSE

## Level of Support

We are actively updating this tool. **Community involvement is encouraged through both issues and pull requests.**

## Example visualizations and Acknowledgements

### Confusion matrices

![main_ACE_app_page_bottom](https://github.com/user-attachments/assets/4fd8eb94-5700-4c79-8729-e2bf6cfb34f3)

### Comparing matched cell types across studies of AD

![multi_study_comparison_ACE_panel](https://github.com/user-attachments/assets/31c68130-3159-4d00-88d1-dd0a253bc5a2)
