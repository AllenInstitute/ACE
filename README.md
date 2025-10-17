[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17382357.svg)](https://doi.org/10.5281/zenodo.17382357)

# Annotation Comparison Explorer (ACE)

R Shiny and [web-based](https://sea-ad.shinyapps.io/ACEapp/) tool for comparison of annotations such as (i) cell type assignments, (ii) donor metadata (e.g., donor, sex, age), and (iii) cell metadata (e.g., anatomic location, QC metrics).  Two primary use cases are (1) comparing user-defined clustering results and mapping results (e.g., after running [MapMyCells](https://portal.brain-map.org/atlases-and-data/bkp/mapmycells)) for the same set of cells and (2) comparing cell type names between different taxonomies on [Allen Brain Map](https://portal.brain-map.org/cell-types).  Additionally, ACE includes a data set comparing cell type names and associated abudance changes between multiple published studies of Alzheimer's disease in dorsolateral prefrontal cortex (see visualizations far below).

### Quick start: **[try the web-based version of ACE](https://sea-ad.shinyapps.io/ACEapp/)** or **[read the ACE preprint on bioRxiv](https://doi.org/10.1101/2025.02.11.637559)**.
<br>

<img width="1273" height="924" alt="main_ACE_app_page_top" src="https://github.com/user-attachments/assets/83804ed3-c9a4-4e89-805d-d927003b0afc" />

## How to use 

### Visit the website

**We recommend using the version of this tool [hosted on shinyapps.io](https://sea-ad.shinyapps.io/ACEapp/)**. 

### Run locally

For power users, this tool can also be run locally in R Studio by following the steps below.

#### Setup RStudio
(You only need do this step once).
1. Install R (ideally version 4.2.2)
2. Install RStudio
3. Download the files from this repo in a working directory
4. Set up your R environment to align with the [renv.lock file](https://github.com/AllenInstitute/ACE/blob/main/renv.lock)

We recommend using [rig](https://github.com/r-lib/rig) for R installation management along with [renv](https://rstudio.github.io/renv/) for R environment management.  After installing rig, you should be able to run these rig commands in a terminal from your working directory to tackle steps #1 and #4:
```
rig add 4.2.2
rig add rtools42
rig rstudio renv.lock
```
This should set up and then launch RStudio with the correct package configurations. (The first two lines of code only need to be run once).

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

<img width="1154" height="739" alt="confusion matrix" src="https://github.com/user-attachments/assets/310afba8-c50e-4890-bf3d-b2e8e3ba8424" />

### Comparing matched cell types across studies of AD

<img width="1220" height="856" alt="multi_study_comparison_ACE_panel" src="https://github.com/user-attachments/assets/8d15e984-9040-4ae6-8c21-f581f9726b9c" />

*ACE is supported by the National Institute of Neurological Disorders and Stroke under Award Number U24NS133077 and by the National Institute On Aging under Award Number U19AG060909. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.*
