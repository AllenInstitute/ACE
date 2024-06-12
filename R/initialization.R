# For each table...

#... what is is called in the main page?
table_names <- c(
  "Basal Ganglia example data",
  "SEA-AD: Alzheimer's cell type mapping",
  "Whole mouse brain (AIT21) MERFISH",
  "Human MTG 10x (SEA-AD, GA, CA)",
  "Human MTG SSv4 (Hodge, SEA-AD)",
  "Mouse Cortex + Hippocampus to Whole mouse brain (AIT21)"
)

#... what category each table is included in on the main page?
categories <- factor(c(
  "Other",
  "Disease studies",
  "Mouse cell types taxonomies",
  "Human cell types taxonomies",
  "Human cell types taxonomies",
  "Mouse cell types taxonomies"
  ),
# This is the order they will show up on in the list. **MAKE SURE THERE ARE NO TYPOS!**
levels = c("Mouse cell types taxonomies", "Human cell types taxonomies", "Disease studies", "Other")) # Keep "Other" as last


#... where is the cell x annotation table?
# Note that this can be a csv file, a feather directory, or a scrattch.taxonomy h5ad file.  It can also be local or on the web.
table_locations <- c(
  "//allen/programs/celltypes/workgroups/humancelltypes/JeremyM/github/annotation_comparison/example",
  "https://raw.githubusercontent.com/AllenInstitute/annotation_comparison/dev/data/DLPFC_SEAAD_cell_annotations_for_app.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/annotation_comparison/dev/data/WMB_MERFISH_subset.csv.gz", 
  "https://raw.githubusercontent.com/AllenInstitute/annotation_comparison/dev/data/MTG_cell_metadata.csv.gz",  
  "https://raw.githubusercontent.com/AllenInstitute/annotation_comparison/dev/data/hodge_to_seaad.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/annotation_comparison/dev/data/CtxHip_WMB_translation.csv.gz"
)

#... where is the annotation information table (e.g., information about each specific cell type.)?  
# The goal is that this part will eventually be able to point to a cell type annotation table in TDT format.
metadata_locations <- c(
  " ", # No metadata yet
  "https://raw.githubusercontent.com/AllenInstitute/annotation_comparison/dev/data/AD_study_cell_types_for_app.csv",
  "https://raw.githubusercontent.com/AllenInstitute/annotation_comparison/dev/data/WMB_cluster_annotations.csv.gz",
  " ", # No metadata yet
  " ", # No metadata yet
  "https://raw.githubusercontent.com/AllenInstitute/annotation_comparison/dev/data/WMB_cluster_annotations.csv.gz"
)

#... this is the description that will show up on ACE below the name. Should be relatively short, but also extremely informative as to what this is for.
descriptions   <- c(
  
  "Small example file pointing at subsampled data from NHP basal ganglia. Note that if you are runnign this from a non-Allen Institute computer, this example will not work.",
                    
  "Data and associated cell type assignments from multiple studies of Alzheimer's disease.  All data sets were mapped to SEA-AD data and their mappings as well as original cluster assignments are included in the tables.  In addition, each cell type's change in abundance in AD from the original study, as well as some basic information about the cell types are included.  Data is subsampled to 100 cells per SEA-AD supertype.  The way data is encoded, comparisons between each data set an SEA-AD are valid, but comparsisons CANNOT be accurately made between external data sets.",
                    
  "Data about brain cell types AND brain regions from mouse whole brain (Yao et al 2023).  This includes cell type assignments and spatial positions for MERFISH data, subsampled to 50 cells per cluster + 5000 cells per section. Cells have been assigned to CCF parcillations to allow direct matching between cell types and anatomic structures. I'm particularly interested if this table works properly.  Right now this file is loading from on prem, but an identical file is on GitHub (which crashes for some reason...).",
                    
  "MTG data set comparisons! Cluster assignments from the human great ape (GA) and cross-areal (CA) study alongside SEA-AD calls for each level of the taxonomy in MTG.",

  "MTG data set comparisons! Cluster assignments from the original SSv4 study ('Hodge' et al, 2019) alongside SEA-AD calls for each level of the taxonomy in MTG. Cortical layers are also also included per cell for cell type by layer comparisons, along with an example set of UMAP coordinates.",
                      
  "Data about brain cell types from mouse whole brain (Yao et al 2023; AIT21) and their mapping to mouse cortex + hippocampus (Yao et al 2021).  This includes data from AIT21 downsampled to only include 100 cells per cluster from cortex+hippocampus.  Any clusters from whole brain that are NOT listed are either rare or absent in mouse cortex + hippocampus.  We recommend using this table to translate forward (e.g., from the older to the newer taxonomy)."
  
)


############################################
## DO NOT EDIT ANYTHING BELOW THIS POINT! ##
############################################

categories = factor(c(as.character(categories),"Other"),levels = unique(c(levels(categories),"Other")))

# Convert above into a data frame
table_info <- data.frame(table_name   = table_names,
                         table_loc    = table_locations,
                         metadata_loc = metadata_locations,
                         description  = descriptions
)

# Convert table names into a nested list
table_names <- c(table_names,"Enter your own location")
table_name <- list()
for (cat in levels(categories)){
  table_name[[cat]] <- table_names[categories==cat]
    if(cat!="Other") 
      table_name[[cat]] <- c("Select comparison table...", table_names[categories==cat])
}
#table_name[["Enter your own location"]] = c("Enter your own location")
category = "Enter your own location"


