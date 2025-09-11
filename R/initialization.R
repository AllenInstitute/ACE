# For each table...

#... what is is called in the main page?
table_names <- c(
  "Alzheimer's disease (SEA-AD vs. community)",
  "Parkinson's disease (ASAP-PMDBS project)",
  "Spatial localization of brain cell types",
  "Middle temporal gyrus (recent studies)",
  "Middle temporal gyrus (initial studies)",
  "Cortex + Hippocampus vs. whole brain",
  "Initial cortex and hippocampus studies",
  "Motor cortex vs. whole brain",
  "Mouse visual cortex (GABAergic)",
  "Mouse motor cortex",
  "Mouse aging study",
  "Mammalian Basal Ganglia Consensus Cell Type Atlas",
  "PsychENCODE ('brainSCOPE' data sets)"
)

#... what category each table is included in on the main page?
categories <- factor(c(
  "Disease studies",
  "Disease studies",
  "Mouse cell type classification",
  "Human/NHP cell type classification",
  "Human/NHP cell type classification",
  "Mouse cell type classification",
  "Mouse cell type classification",
  "Mouse cell type classification",
  "Patch-seq (shape + function + genes)",
  "Patch-seq (shape + function + genes)",
  "Mouse cell type classification",
  "Human/NHP cell type classification",
  "Disease studies"
  ),
# This is the order they will show up on in the list. **MAKE SURE THERE ARE NO TYPOS!**
levels = c("Disease studies", "Mouse cell type classification", "Human/NHP cell type classification","Patch-seq (shape + function + genes)")) 


#... where is the cell x annotation table?
# Note that this can be a csv file, a feather directory, or a scrattch.taxonomy h5ad file.  It can also be local or on the web.
table_locations <- c(
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/DLPFC_SEAAD_cell_annotations_for_app.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/ASAP_CRN_cell_info_subsample.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_MERFISH_subset.csv.gz", 
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/MTG_cell_metadata.csv.gz",  
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/hodge_to_seaad.csv.gz",   
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/CtxHip_WMB_translation.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/CTX_Hip_cellInfoM1VISp.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/M1_to_WMB_cell_info.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/gouwens_tasic_byCell.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/ToliasCTKE_byCell.csv",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/aging_mouse_brain_subsample.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/BG_cross_species_metadata.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/brainSCOPE_cell_info.csv.gz"
)

#... where is the annotation information table (e.g., information about each specific cell type.)?  
# The goal is that this part will eventually be able to point to a cell type annotation table in TDT format.
metadata_locations <- c(
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/AD_study_cell_types_for_app.csv",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/AD_study_cell_types_for_app.csv",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_cluster_annotations.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/mtg_whb_annotations.csv.gz", 
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/mtg_whb_annotations.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_cluster_annotations.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/CTX_Hip_cellTypeInfo.csv",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_cluster_annotations.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/gouwens_tasic_byCellType.csv",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/CTX_Hip_cellTypeInfo.csv", # Only include for cell type ordering.
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/WMB_cluster_annotations.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/BG_cross_species_annotation_information.csv.gz",
  "https://raw.githubusercontent.com/AllenInstitute/ACE/main/data/brainSCOPE_cell_types_for_app.csv"
)

#... which panels to omit (e.g., because relevant data for that panel is not included)?
omit_panels <- list(
  "scatterplot",
  "none",
  "none",
  "scatterplot",
  "none",
  "scatterplot",
  "scatterplot",
  "scatterplot",
  "individual",
  "individual",
  "none",
  "none",
  "scatterplot"
)
# Options used are: "confusion", "river", "individual", or "scatterplot". "none" is used by default but is just a placeholder for showing everything. Multiple options can be selected using a vector [e.g., c("one","two")]

# MAKE SURE THESE NAMES MATCH THE TAB NAMES IN ui.R!
tab_names <- setNames(c("Compare pairs of annotations", 
                        "Link 2+ annotations (river plots)", 
                        "Explore individual annotations", 
                        "Compare numeric annotations"),
                      c("confusion", 
                        "river", 
                        "individual", 
                        "scatterplot")
                      )

#... this is the description that will show up on ACE below the name. Should be relatively short, but also extremely informative as to what this is for.
descriptions   <- c(
  
  "Data and associated cell type assignments from ten studies of Alzheimer's disease.  All data sets were mapped to SEA-AD data, and both the 10 data sets and the details of the data integration and mapping are described in Gabitto, Travaglini, et al 2023 (DOI:10.1101/2023.05.08.539485). For each data set, their mappings to SEA-AD as well as original cluster assignments are included in the tables.  In addition, each cell type's change in abundance in AD from the original study, as well as some basic information about the cell types are included.  Data is subsampled to 100 cells per SEA-AD supertype.  The way data is encoded, comparisons between each data set an SEA-AD are valid, but comparsisons CANNOT be accurately made between external data sets.  If you have additional data sets you'd like to see included in this study, please reach out!",
  
  "Integration of five Parkinson's disease data sets focusing on the Human Postmortem-derived Brain Sequencing Collection (PMDBS) from Aligning Science Across Parkinson’s (ASAP) (see https://portal.brain-map.org/atlases-and-data/bkp/abc-atlas/asap-parkinsons for details). All cells include cluster assignments and are mapped to middle temporal gyrus (MTG) from the SEA-AD project (Gabitto, Travaglini, et al 2023; DOI:10.1101/2023.05.08.539485) and to whole human brain (WHB; Siletti et al 2023; DOI:10.1126/science.add7046). Also includes other cell metadata, such as brain region and data set.",
                    
  "[This data set may be slow to load--please be patient!]  Data about brain cell types AND brain regions using MERFISH data collected from mouse whole brain (Yao et al 2023; DOI:10.1038/s41586-023-06812-z; AIT21).  This includes cell type assignments and spatial positions for MERFISH data, subsampled to 50 cells per cluster + 5000 cells per section. Cells have been assigned to CCF parcillations to allow direct matching between cell types and anatomic structures. The current version of this table may have rotated CCF coordinates, making visualization challenging using the scatterplot view--new coordinates are in process, but this will not impact any other exploration components.",
                    
  "Human MTG data set comparisons (recent). Cluster and other assignments for the SAME set of cells collected from MTG that were used in four published studies, including in SEA-AD and the whole human brain. These studies are (1) the human 'great ape' (GA) study (Jorstad, Song, Exposito-Alonso, et al 2023, DOI:10.1126/science.ade9516), which assesses cell types in MTG across species; (2) the human 'cross-areal' (CA) study (Jorstad et al 2023, DOI:10.1126/science.adf6812), which assesses cell types across multiple neocortical areas in human (including MTG); (3) 'SEA-AD' calls for each level of the taxonomy in MTG (Gabitto, Travaglini, et al 2023; DOI:10.1101/2023.05.08.539485)' and (4) a draft atlas of all cell types in the human brain (Siletti et al 2023; DOI:10.1126/science.add7046). The latter two of these studies are currently available on both MapMyCells and the ABC Atlas. Note: the clusters in the GA study represented the starting point for analysis in generation of the SEA-AD supertypes.  All three MTG-focused data sets in this table were generated using a nearly identical set of cells collected using 10X genomics single nucleus RNA-seq technologies, and only the matching subset of cells from the whole human brain study are included. Clusters from the GA and CA studies are appended with '_GA' and '_CA', respectively, for ACE functionality.",

  "Human MTG data set comparisons (historical). Cluster assignments from the original study of cell types in human MTG from the Allen Institute ('Hodge', Bakken, et al 2019: DOI:10.1038/s41586-019-1506-7) alongside SEA-AD calls for each level of the taxonomy in MTG from Gabitto, Travaglini, et al 2023 (DOI:10.1101/2023.05.08.539485). Cortical layers of the initial dissections are also included per cell to allow for cell type by layer comparisons. We also include some example UMAP coordinates for visualization of metadata. Labels were transfered from the SEA-AD taxonomy to Hodge et al data using MapMyCells with deep generative model in October 2024.",
                      
  "Data about brain cell types from mouse whole brain (Yao et al 2023; DOI:10.1038/s41586-023-06812-z; AIT21) and their mapping to mouse cortex + hippocampus (Yao et al 2021, DOI:10.1038/s41586-023-06812-z).  This includes data from AIT21 downsampled to only include 100 cells per cluster from cortex + hippocampus.  Any clusters from the whole brain that are NOT listed are either rare or absent in mouse cortex + hippocampus.  We recommend using this table to translate forward (e.g., from the older to the newer taxonomy).",
  
  "Convert cell types from mouse primary visual (VISp), Anterior Lateral Motor area (ALM), and primary motor (MOp) cortex to mouse cortex + hippocampus.  This is done using SmartSetV4 cells that were included in multiple studies and their associated cell type annotations: (1) VISp and ALM cells from Tasic et al 2018 (DOI:10.1038/s41586-018-0654-5), (2) MOp cells used in the Cell Type Knowledge Explorer (Yao, Liu, Xie, Fischer, et al 2021, DOI:10.1038/s41586-021-03500-8), and overlapping cells from mouse cortex + hippocampus (Yao et al 2023; DOI:10.1038/s41586-023-06812-z; AIT21). No subsampling was done for this table. Any clusters from mouse Ctx + hipp that are NOT listed are either rare or absent in mouse VISp and mouse MOp.  We recommend using this table to translate forward (e.g., from the older to the newer taxonomy).  Finally, note that MOp and VISp/ALM cannot be directly compared because different cells were included in these two studies.",
  
  "Convert mouse cell type assignments from primary motor cortex (MOp) (Yao, Liu, Xie, Fischer, et al 2021, DOI:10.1038/s41586-021-03500-8) to whole mouse brain (Yao et al 2023; AIT21, DOI:10.1038/s41586-023-06812-z).  This is done using 10X v2 and v3 cells that were included in both studies, but with no subsampling. Any clusters from mouse whole brain that are NOT listed are either rare or absent in mouse MOp.  We recommend using this table to translate forward (e.g., from the older to the newer taxonomy).",
  
  "Mouse Patch-seq data from primary visual cortex (VISp). This table compares matched transcriptomic-type (T-Type) and morphoelectric transcriptomic type (MET-type) cell type assignments for the same cells from Gouwens, Sorensen, et al 2020 (DOI:10.1016/j.cell.2020.09.057). The cell type annotations for MET types was taken directly from supplemental materials in the manuscript. T-types were assigned through comparison with Tasic et al 2018 (DOI:10.1038/s41586-018-0654-5). In addition to cell type assigments, cell metadata and electrophysiological properties for each patch-seq cell is included for interactive visualization of electrophysiological properties.",
  
  "Mouse Patch-seq data from primary motor cortex (MOp). Data from Scala, Kobak, et al 2020 (DOI:10.1038/s41586-020-2907-3). This table include transcriptomic types (T-Types) defined through comparison with MOp cell types linked in the Cell Type Knowledge Explorer (Yao, Liu, Xie, Fischer, et al 2021, DOI:10.1038/s41586-021-03500-8) and through comparison with primary visual cortex (VISp) types from Tasic et al 2018 (DOI:10.1038/s41586-018-0654-5). In addition to cell type assigments, cell metadata and electrophysiological properties for each patch-seq cell is included for interactive visualization of electrophysiological properties.",
  
  "Explore mouse cell type assignments from the recent study of the aging mouse brain ('AGING_'; Jin, et al 2025, DOI:10.1038/s41586-024-08350-8) in the context of the reported cell type hierarchy, donor metric (age, sex), cell metadata, and changes with age.  Importantly also allows for conversion to cell type classifications from the whole mouse brain study ('WMB_'; Yao et al 2023; AIT21, DOI:10.1038/s41586-023-06812-z).  Data from the aging study were subsampled to retain a random 25% of the cells and to omit cells with 2 or fewer cells from a whole mouse brain study cluster. Any clusters from mouse whole brain that are NOT listed are either rare or absent in aging mouse brain study.  Both studies are currently available for further exploration on the Allen Brain Cell Atlas.",
  
  "The Human and Mammalian Brain Atlas (HMBA) consortia has created a unified taxonomy of the mammalian basal ganglia, with >2 million cells collected from humans, macaques, and marmosets, and linkages to existing mouse brain data. More details are available here: https://alleninstitute.github.io/HMBA_BasalGanglia_Consensus_Taxonomy/.  The data table here includes 296,671 cells (random downsampling to 15% of cells + 10 random cells per cluster after omitting cells not mapping to a 'group') along with information about cell type assignments (cluster, group, subclass, class, and neighborhood), dissected brain region, and species, and includes mapping results to existing whole human brain and whole mouse brain taxonomies available in MapMyCells and the ABC Atlas.",
  
  "In 2024 the PsychENCODE Consortium released a multi-omics resource focused on doroslateral prefrontal cortex (DLPFC) in humans with multiple disease conditions (Emani et al 2024; https://doi.org/10.1126/science.adi5199). The data set here includes RNA-seq data from seven single-nucleus RNA-seq data sets included in this study, and spans four brain disorders (autism spectrum disorder (ASD), major depressive disorder (MDD), post-traumatic stress disorder (PTSD), and William's syndrome) and controls. We include a downsampled set of 300,046 cells from 130 donors along with donor demographics, cluster assignments reported in the study ('brainSCOPE_') and results mapped to the SEA-AD MTG data set using the MapMyCells GUI in September 2025 using deep generative mapping ('SEAAD_')."
  
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
table_name[["Upload or enter your own location"]] = c("Upload or enter your own location")
category = "Upload or enter your own location"

names(omit_panels) <- table_names
omit_panels[["Upload or enter your own location"]] = "none"

# Reorder to put the upload option first
table_name <- table_name[c("Upload or enter your own location",setdiff(names(table_name),"Upload or enter your own location"))]