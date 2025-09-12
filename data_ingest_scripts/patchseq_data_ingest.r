##############################################################
##   This file includes scripts and explanations for how    ##
##   data was collected for all of the patchSeq data sets.  ##
##############################################################

##############################################################
#######    "Mouse visual cortex (GABAergic neurons)"

# These data are from Gouwens, Sorensen, et al 2020 (DOI:10.1016/j.cell.2020.09.057). T-types were assigned through comparison with Tasic et al 2018 (DOI:10.1038/s41586-018-0654-5). In addition to cell type assigments, cell metadata and electrophysiological properties for each patch-seq cell is included for interactive visualization of electrophysiological properties.

# Previously patch-seq data from this study had been collected as part of the MapMySpikes data challenge (https://alleninstitute.org/events/mapmyspikes/). Data for this patch-seq study was directly taken from the "VISp_Viewer" tab of the data challenge data set here:  https://alleninstitute.org/wp-content/uploads/2024/04/MapMySpikes_data_COMPLETE_final.xlsx

# The cell type annotations for MET types was taken directly from supplemental materials in the manuscript.  


##############################################################
#######    "Mouse motor cortex"

# Mouse Patch-seq data in this data set is from Scala, Kobak, et al 2020 (DOI:10.1038/s41586-020-2907-3). This table include transcriptomic types (T-Types) defined through comparison with MOp cell types linked in the Cell Type Knowledge Explorer (Yao, Liu, Xie, Fischer, et al 2021, DOI:10.1038/s41586-021-03500-8) and through comparison with primary visual cortex (VISp) types from Tasic et al 2018 (DOI:10.1038/s41586-018-0654-5). In addition to cell type assigments, cell metadata and electrophysiological properties for each patch-seq cell is included for interactive visualization of electrophysiological properties.

# Previously patch-seq data from this study had been collected as part of the MapMySpikes data challenge (https://alleninstitute.org/events/mapmyspikes/) and is also included on the Cell Type Knowledge Explorer (https://knowledge.brain-map.org/celltypes). Data for this patch-seq study was directly taken from the "CTKE_M1" tab of the data challenge data set here:  https://alleninstitute.org/wp-content/uploads/2024/04/MapMySpikes_data_COMPLETE_final.xlsx

# The cell type annotation table here is only included for cell type ordering, but does not include additional information about the cell types from this paper.


##############################################################
#######    Human neocortex (L2-L3 glutamatergic neurons)

# This study characterized the morpho-electric properties of five glutamatergic neuron types in layers 2-3 of human neocortex (Berg, Sorensen, Ting, Miller, et al. 2021; DOI:10.1038/s41586-021-03813-8). Annotations for cell type assigments, QC metrics, cell metadata, morphological features, and electrophysiological properties for each patch-seq cell are all included for interactive visualization.

# The cell information for this data set is taken directly from the Multimodal Characterization of Individual Neurons website on Allen Brain Map (https://portal.brain-map.org/cell-types/classes/multimodal-characterization), from this file: https://brainmapportal-live-4cc80a57cd6e400d854-f7fdcae.divio-media.net/filer_public/bb/50/bb505fc4-eae4-4047-a971-46567570ec0a/20200625_patchseq_metadata_human.csv, which is renamed as "berg_byCell.csv" 

# All cells from this table are included. The following columns were removed: project, cell_specimen_name, full_genotype, transcriptomics_sample_id, corresponding_AIT2.3.1_ID

# In addition, morphological information for the cells were merged into the table from the GitHub associated with this manuscript here (https://github.com/AllenInstitute/patchseq_human_L23); specific file: https://raw.githubusercontent.com/AllenInstitute/patchseq_human_L23/refs/heads/master/data/All_L23_Lockdown_all_raw_features.csv. All electrophysiological information was collected from: https://raw.githubusercontent.com/AllenInstitute/patchseq_human_L23/refs/heads/master/data/human_mouse_ephys_all_0127.csv. Similarly Mapping results, QC metrics and assignment to deep vs. superficial FREM3 was collected from https://raw.githubusercontent.com/AllenInstitute/patchseq_human_L23/refs/heads/master/data/human_IVSCC_excitatory_L23_consolidated_0131.csv
# Joining of relevant columns from both tables was done in Excel using the "vlookup" command and joining on cell_specimen_id/SpecimenID/specimen_id. All columns from All_L23_Lockdown_all_raw_features.csv were included, and the last 9 columns from human_IVSCC_excitatory_L23_consolidated_0131.csv are joined in.
# Renamed: SeuratMapping --> MTG_cluster
# We've also added MTG_cluster_plus which merges MTG_cluster with Is_deep_FREM3

# The cell type annotation table here is only included for cell type ordering, but does not include additional information about the cell types from this paper.  It adds "deep" and "superficial" FREM3 to the standard taxonomy.

##############################################################
#######    Human neocortex (L1 GABAergic neurons)

# This table includes data from a study of GABAergic neurons in layer 1 of human neocortex (Chartrand et al 2023; DOI:10.1126/science.adf0805). Annotations for cell type assigments, QC metrics, cell metadata, morphological features, and electrophysiological properties for each patch-seq cell are all included for interactive visualization.

# The cell information files include the following (with everything joined on cell specimen ID):

# File names: https://raw.githubusercontent.com/AllenInstitute/patchseq_human_L1/refs/heads/main/data/[...]
# Transcriptomic information (clusters, QC, etc.): human_l1_dataset_2023_02_06.csv
# electrophysiological information from L1: aibs_features_E.csv, tamas_features_E.csv, and mansvelder_features_E.csv
# morphological features: RawFeatureWide_human+derivatives.csv
# Cell soma layer depths: human_layer_depths_2023_02_06.csv

# Merge the three ephys files
e1 <- read.csv("aibs_features_E.csv")
e2 <- read.csv("tamas_features_E.csv")
e3 <- read.csv("mansvelder_features_E.csv")
kpCol <- intersect(colnames(e1),intersect(colnames(e2),colnames(e3)))
e_all <- rbind(e1[,kpCol],e2[,kpCol],e3[,kpCol])
write.csv(e_all,"All_ephys_features.csv")
# NOTE: the specimen names are different for Tamas and Mansfelder so I need to manually join using human_l1_dataset_2023_02_06 in excel to get the new names

# Note that cell type assignments are renamed to match what is was used in Hodge et al 2019.  Some names had been updated in this manuscript for clarity.

# The cell type annotation table here is only included for cell type ordering, but does not include additional information about the cell types from this paper.  


##############################################################
#######    Human neocortex (L2-L6 GABAergic neurons)

# This table includes data from a study of GABAergic neurons in layers 2-6 of human neocortex (Lee, Dalley et al, 2023; DOI:10.1126/science.adf6484). Annotations for cell type assigments, QC metrics, cell metadata, morphological features, and electrophysiological properties for each patch-seq cell are all included for interactive visualization.  While this study technically covers all remaining cortical layers, the deeper layers are relatively undersampled.

# The cell information files include the following (with everything joined on cell specimen ID):

# From layer 1: File names: https://raw.githubusercontent.com/AllenInstitute/human_patchseq_gaba/refs/heads/main/data/[...]
# Transcriptomic information (clusters, QC, etc.): LeeDalley_manuscript_metadata_v2.csv
# Additional metadata is available in: complete_patchseq_data_sets2.RData (see code below)
# electrophysiological information from L1: LeeDalley_ephys_fx.csv
# morphological features: LeeDalley_morpho_features.csv
# Donor information: DataS1_Donor_table.csv

# Getting extra metadata from the R object
load("complete_patchseq_data_sets2.RData")
cn_meta <- c("spec_id","postPatch","rna_amplification_pass_fail","marker_sum_norm_label","quality_score_label","topLeafValue")
cn_anno <- c("contam_sum_label","contaminationType_label")
out <- cbind(metaPatch[,cn_meta],annoPatch[,cn_anno])
write.csv(out,"extra_GABA_metadata.csv")

# Note that cell type assignments are renamed to match what is was used in Hodge et al 2019.  Some names had been updated in this manuscript for historical reasons.

# The cell type annotation table here is only included for cell type ordering, but does not include additional information about the cell types from this paper.  