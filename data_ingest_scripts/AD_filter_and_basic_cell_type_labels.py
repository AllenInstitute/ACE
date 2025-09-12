# This script performs basic QC on all of the data sets for the "Alzheimer's disease (SEA-AD vs. community)" studies.
# More specifically, we QCed single-cell RNA-seq datasets from eight publicly available studies by applying consistent filtering. For each dataset, raw count matrices and metadata were obtained directly from the official repositories linked in the corresponding primary studies, as described in the respective publications. After loading, all matrices were stored as sparse int32 objects to standardize data types. Cells were filtered to remove those with >5% mitochondrial reads and fewer than 200 detected genes (for studies not already pre-filtered). Quality control metrics such as total counts and number of genes per cell were calculated, and cell type annotations were mapped to a standard set of broad categories (Astro, Endo, GABA, Glut, Micro, OPC, Oligo). Non-brain and ultra-rare cell types (e.g., immune cells, fibroblasts, pericytes) were excluded. The resulting QCed data were then stratified by broad cell type and saved per study for downstream integration.

import anndata as ad
import gc
import numpy as np
import os
import pandas as pd
import scanpy as sc
import torch

from scipy.sparse import csr_matrix
from utils import rsparse_to_pysparse, rvector_to_index, use_font

# Set GPU or CPU
gpu = True if torch.cuda.is_available() else False

# Studies and broad cell types we'll be integrating

study_names = 'p400', 'p400_mit', 'sea_ad_dlpfc', 'cain', 'lau', \
              'mathys', 'morabito', 'zhou'
broad_cell_types = 'Astro', 'Endo', 'GABA', 'Glut', 'Micro', 'OPC', 'Oligo'

# QC all studies and save per broad cell type

for study_name in study_names:
    print(study_name)
    if study_name == 'sea_ad':
        study = sc.read_h5ad('ad_crossdatasets/data/sea_ad.h5ad')
        # Deal with NAs in covariates
        study.obs = study.obs.assign(
            **{'APOE4 status': lambda df: df['APOE4 status'].eq('Y'),
               'Metadata: PMI': lambda df: df['Metadata: PMI'].fillna(
                   df['Metadata: PMI'].median()),
               'Study: ACT': lambda df: df[
                   'Metadata: Primary Study Name'].eq('ACT'),
               'Study: ADRC Clinical Core': lambda df: df[
                   'Metadata: Primary Study Name'].eq('ADRC Clinical Core')})
    elif study_name in ['p400', 'p400_mit', 'sea_ad_dlpfc']:
        study = sc.read_h5ad(f'ad_crossdatasets/data/{study_name}.h5ad')
    else:
        from rpy2.robjects import r
        r.load(f'ad_crossdatasets/data/{study_name}_counts.rda')
        study = ad.AnnData(
            X=rsparse_to_pysparse(r.fullmat).T,
            obs=pd.read_csv(f'ad_crossdatasets/data/{study_name}_metadata2.csv',
                            dtype={'donor': 'category', 'diagnosis': 'category',
                                   'age': float, 'sex': int, 'braak': int,
                                   'broadcelltype': 'category',
                                   'subcluster': 'category', 'pmi': float,
                                   'apoe': 'category', 'plaque': 'category',
                                   'cogdx': 'category', 'apoe4': 'category',
                                   'trem2': 'category'})
                .astype({'sex': 'category', 'braak': 'category'})
                .set_index('cellid'),
            var=pd.DataFrame(index=rvector_to_index(
                r.rownames(r.fullmat))), dtype='int32')
    assert type(study.X) == csr_matrix, type(study.X)
    assert study.X.dtype == 'int32', study.X.dtype
    print(f'{study_name}, initial size: {study.shape}')
    # Filter to cells with <= 5% mitochondrial reads
    # (SEA-AD is already filtered to these)
    if study_name != 'sea_ad':
        study = study[study.X[:, study.var_names.str.startswith('MT-')]
                      .sum(axis=1).A1 / study.X.sum(axis=1).A1 <= 0.05]
        print(f'{study_name}, <5% mitochondrial reads: {len(study)} cells')
    # Calculate QC metrics
    study.obs['n_genes_by_counts'] = study.X.getnnz(axis=1)
    study.obs['total_counts'] = study.X.sum(axis=1).A1
    # Filter to cells with >= 200 genes detected
    # (SEA-AD is already filtered to these)
    if study_name != 'sea_ad':
        study = study[study.obs.n_genes_by_counts >= 200]
        print(f'{study_name}, >= 200 genes detected: {len(study)} cells')
    # Save, stratified by broad cell type
    if study_name == 'p400':
        broad_cell_type = study.obs.state.str.split('.').str[0].replace({
            'Arteriole': 'Endo', 'Ast': 'Astro', 'CD8+ T Cells': pd.NA,
            'COP': 'OPC', 'End': 'Endo', 'Erythrocytes': pd.NA, 'Exc': 'Glut',
            'Fib': pd.NA, 'Inh': 'GABA', 'Macrophages': pd.NA, 'Mic': 'Micro',
            'Monocytes': pd.NA, 'NFOL/MOL': 'OPC', 'NK Cells': pd.NA,
            'Neutrophils': pd.NA, 'OPC': 'OPC', 'Oli': 'Oligo', 'Peri': pd.NA,
            'SMC': pd.NA, 'Venule': 'Endo'})
    elif study_name == 'p400_mit':
        broad_cell_type = study.obs.broad_cell_type.astype(str).replace({
            'TCell': pd.NA, 'CAM': pd.NA, 'Fibro': pd.NA, 'Peri': pd.NA,
            'SMC': pd.NA})
    elif study_name == 'sea_ad':
        broad_cell_type = study.obs.broad_cell_type.astype(str).replace({
            'Astrocyte': 'Astro', 'Endothelial': 'Endo', 'Excitatory': 'Glut',
            'Inhibitory': 'GABA', 'Microglia-PVM': 'Micro',
            'Oligodendrocyte': 'Oligo', 'OPC': 'OPC', 'VLMC': pd.NA})
    elif study_name == 'sea_ad_dlpfc':
        broad_cell_type = study.obs.broad_cell_type.astype(str)\
            .replace({'VLMC': pd.NA})
    else:
        broad_cell_type = study.obs.broadcelltype.astype(str).replace({
            'Peri': pd.NA, 'VLMC': pd.NA, 'Exclude': pd.NA})
    assert tuple(np.unique(broad_cell_type.dropna())) == broad_cell_types
    os.makedirs(f'/mnt/mfs/ctcn/team/tl3087/ad_crossdatasets/QCed_shared_genes/'
                f'{study_name}', exist_ok=True)
    for cell_type in broad_cell_types:
        print(f'{study_name}: saving {cell_type}...')
        # noinspection PyTypeChecker
        study[broad_cell_type == cell_type].write(
            f'/mnt/mfs/ctcn/team/tl3087/ad_crossdatasets/QCed_shared_genes/'
            f'{study_name}/{cell_type}.h5ad')
    # Free memory
    del study
    _ = gc.collect()
