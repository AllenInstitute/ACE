# For each table...

#... what is is called in the main page?
table_names <- c(
  "SEA-AD: Alzheimer's cell type mapping",
  "Spatial localization of whole mouse brain types (AIT21)",
  "Human MTG 10x (SEA-AD vs. GA vs. CA)",
  "Human MTG SSv4 (Hodge vs. SEA-AD)",
  "Mouse Cortex + Hippocampus vs. Whole mouse brain (AIT21)",
  "Mouse MOp and VISp/ALM vs. Cortex + Hippocampus (SSv4)",
  "Mouse MOp vs. Whole mouse brain (AIT21) (10x)",
  "Mouse T vs. MET types in visual cortex (Patch-seq)"
)

#... what category each table is included in on the main page?
categories <- factor(c(
  "Disease studies",
  "Mouse cell types taxonomies",
  "Human cell types taxonomies",
  "Human cell types taxonomies",
  "Mouse cell types taxonomies",
  "Mouse cell types taxonomies",
  "Mouse cell types taxonomies",
  "Mouse cell types taxonomies"
  ),
# This is the order they will show up on in the list. **MAKE SURE THERE ARE NO TYPOS!**
levels = c("Disease studies", "Mouse cell types taxonomies", "Human cell types taxonomies")) 


#... where is the cell x annotation table?
# Note that this can be a csv file, a feather directory, or a scrattch.taxonomy h5ad file.  It can also be local or on the web.
table_locations <- c(
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/DLPFC_SEAAD_cell_annotations_for_app.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_MERFISH_subset.csv.gz", 
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/MTG_cell_metadata.csv.gz",  
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/hodge_to_seaad.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/CtxHip_WMB_translation.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/CTX_Hip_cellInfoM1VISp.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/M1_to_WMB_cell_info.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/gouwens_tasic_byCell.csv"
)

#... where is the annotation information table (e.g., information about each specific cell type.)?  
# The goal is that this part will eventually be able to point to a cell type annotation table in TDT format.
metadata_locations <- c(
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/AD_study_cell_types_for_app.csv",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_cluster_annotations.csv.gz",
  " ", # No metadata yet
  " ", # No metadata yet
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_cluster_annotations.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/CTX_Hip_cellTypeInfo.csv",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_cluster_annotations.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/gouwens_tasic_byCellType.csv"
)

#... this is the description that will show up on ACE below the name. Should be relatively short, but also extremely informative as to what this is for.
descriptions   <- c(
  
  "Data and associated cell type assignments from ten studies of Alzheimer's disease.  All data sets were mapped to SEA-AD data, and both the 10 data sets and the details of the data integration and mapping are described in Gabitto, Travaglini, et al 2023 (DOI:10.1101/2023.05.08.539485). For each data set, their mappings to SEA-AD as well as original cluster assignments are included in the tables.  In addition, each cell type's change in abundance in AD from the original study, as well as some basic information about the cell types are included.  Data is subsampled to 100 cells per SEA-AD supertype.  The way data is encoded, comparisons between each data set an SEA-AD are valid, but comparsisons CANNOT be accurately made between external data sets.  If you have additional data sets you'd like to see included in this study, please reach out!",
                    
  "[This data set may be slow to load--please be patient!]  Data about brain cell types AND brain regions using MERFISH data collected from mouse whole brain (Yao et al 2023; DOI: 10.1038/s41586-023-06812-z).  This includes cell type assignments and spatial positions for MERFISH data, subsampled to 50 cells per cluster + 5000 cells per section. Cells have been assigned to CCF parcillations to allow direct matching between cell types and anatomic structures. The current version of this table may have rotated CCF coordinates, making visualization challenging using the scatterplot view--new coordinates are in process, but this will not impact any other analysis.",
                    
  "Human MTG data set comparisons! Cluster assignments from two previous studies alongside SEA-AD calls for each level of the taxonomy in MTG. These studies are the human 'great ape' (GA) study (Jorstad, Song, Exposito-Alonso, et al 2023, DOI:10.1126/science.ade9516), which assesses cell types in MTG across species, and the human 'cross-areal' (CA) study (Jorstad et al 2023, DOI:10.1126/science.adf6812), which assesses cell types across multiple neocortical areas in human (including MTG).  The clusters in the CA study represented the starting point for analysis in generation of the SEA-AD supertypes in Gabitto, Travaglini, et al 2023 (DOI:10.1101/2023.05.08.539485).  All three data sets in this table were generated using a nearly identical set of cells collected using 10X genomics single nucleus RNA-seq technologies.",

  "Human MTG data set comparisons! Cluster assignments from the original study of cell types in human MTG from the Allen Institute ('Hodge', Bakken, et al 2019: DOI:10.1038/s41586-019-1506-7) alongside SEA-AD calls for each level of the taxonomy in MTG from Gabitto, Travaglini, et al 2023 (DOI:10.1101/2023.05.08.539485). Cortical layers of the initial dissections are also included per cell to allow for cell type by layer comparisons. We also include some example UMAP coordinates for visualization of metadata.",
                      
  "Data about brain cell types from mouse whole brain (Yao et al 2023, AIT21, DOI:10.1038/s41586-023-06812-z) and their mapping to mouse cortex + hippocampus (Yao et al 2021, DOI:10.1038/s41586-023-06812-z).  This includes data from AIT21 downsampled to only include 100 cells per cluster from cortex + hippocampus.  Any clusters from the whole brain that are NOT listed are either rare or absent in mouse cortex + hippocampus.  We recommend using this table to translate forward (e.g., from the older to the newer taxonomy).",
  
  "Convert cell types from mouse primary visual (VISp), Anterior Lateral Motor area (ALM), and primary motor (MOp) cortex to mouse cortex + hippocampus.  This is done using SmartSetV4 cells that were included in multiple studies and their associated cell type annotations: (1) VISp and ALM cells from Tasic et al 2018 (DOI:10.1038/s41586-018-0654-5), (2) MOp cells used in the Cell Type Knowledge Explorer (Yao, Liu, Xie, Fischer, et al 2021, DOI:10.1038/s41586-021-03500-8), and overlapping cells from mouse cortex + hippocampus (Yao et al 2021, DOI:10.1038/s41586-023-06812-z). No subsampling was done for this table. Any clusters from mouse Ctx + hipp that are NOT listed are either rare or absent in mouse VISp and mouse MOp.  We recommend using this table to translate forward (e.g., from the older to the newer taxonomy).  Finally, note that MOp and VISp/ALM cannot be directly compared because different cells were included in these two studies.",
  
  "Convert mouse cell type assignments from primary motor cortex (MOp) (Yao, Liu, Xie, Fischer, et al 2021, DOI:10.1038/s41586-021-03500-8) to whole mouse brain (Yao et al 2023; AIT21, DOI:10.1038/s41586-023-06812-z).  This is done using 10X v2 and v3 cells that were included in both studies, but with no subsampling. Any clusters from mouse whole brain that are NOT listed are either rare or absent in mouse MOp.  We recommend using this table to translate forward (e.g., from the older to the newer taxonomy).",
  
  "Mouse Patch-seq data from primary visual cortex (VISp). These tables compare matched transctiptomic-type (T-Type) and morphoelectric transcriptomic type (MET-type) cell type assignments for the same cells from Gouwens, Sorensen, et al 2020 (DOI:10.1016/j.cell.2020.09.057). The cell type annotations for MET types was taken directly from supplemental materials in the manuscript. T-types were assigned through comparison with Tasic et al 2018 (DOI:10.1038/s41586-018-0654-5)."
  
)


############################################
## DO NOT EDIT ANYTHING BELOW THIS POINT! ##
############################################

categories = factor(c(as.character(categories)),levels = unique(c(levels(categories))))

# Convert above into a data frame
table_info <- data.frame(table_name   = table_names,
                         table_loc    = table_locations,
                         metadata_loc = metadata_locations,
                         description  = descriptions
)

# Convert table names into a nested list
table_name <- list()
for (cat in levels(categories)){
  table_name[[cat]] <- c("Select comparison table...", table_names[categories==cat])
}
table_name[["Enter your own location"]] = c("Enter your own location")
category = "Enter your own location"


