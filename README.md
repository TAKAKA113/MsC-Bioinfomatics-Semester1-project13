# East African Wheat Breeding: Phenotypes and GWAS

This repository contains a semester research project examining one century of
breeding in East African bread wheat. It combines multi-location phenotype
records with GBS-derived SNP data to study trait change over time and identify
trait-associated loci.

## Project overview

- **Samples:** approximately 185 wheat varieties released between 1920 and 2020
- **Phenotypes:** 14 agronomic and grain-size traits
- **Genotypes:** approximately 30,000 SNPs
- **Main analyses:** phenotype preprocessing, linear mixed models, correlation,
  PCA, temporal regression, DAPC, GAPIT GWAS, and GWASpoly

The phenotype results indicate decreasing plant height and earlier heading and
maturity, alongside increasing grain weight and size. The report identifies the
strongest GWAS signals for days to heading, days to maturity, and plant height.

## Repository structure

```text
.
├── datasets/                 # Raw phenotype and release-year data
├── data_processed/           # Processed phenotype data and genotype QC output
│   ├── alternative_exports/  # Additional filtered exports retained separately
│   └── genotype/             # Genotype-level QC metrics
├── scripts/                  # Phenotype preprocessing, LMM, diagnostics, and plots
├── figures/                  # Phenotype figures and diagnostics
├── GWASpoly/
│   ├── neighbour/            # K-nearest-neighbour-imputed GWASpoly run
│   └── mode/                 # Mode-imputed GWASpoly comparison run
├── reports/                  # Final project report
└── notes/                    # LMM learning and interpretation notes
```

## Analysis flow

1. Combine and clean field and grain-size phenotype datasets.
2. Estimate genotype-level trait values with linear mixed models.
3. Merge phenotype estimates with year of release.
4. Examine trait correlations, PCA structure, and temporal trends.
5. Evaluate genotype imputation methods and infer population structure with DAPC.
6. Run GAPIT and GWASpoly association analyses.
7. Compare significant loci and annotate candidate genes.

## GWAS and GWASpoly status

The final report selected **K-nearest-neighbour imputation** because it had the
highest imputation accuracy in the method comparison. Mode imputation was also
run as a comparison.

Both GWASpoly runs used 184 matched samples, DAPC covariates, a LOCO kinship
matrix, the additive model, and a Bonferroni threshold of 0.05. Both supported
four DAPC clusters. The neighbour-imputed run produced three DTH QTL and one PHT
QTL; the mode-imputed run produced three DTH QTL. Two DTH markers on chromosome
5A were shared between the two runs.

The report mentions GWASpoly in the methods, but its main GWAS figures and table
are based on the GAPIT FarmCPU and BLINK results. The SNPs in the stored GWASpoly
CSV files do not match the report's main significant-SNP table, so the GWASpoly
results were not directly incorporated into the final reported loci. GAPIT code
and standalone output files are not currently present in this repository.

## Reproducibility notes

- `scripts/02_merge_field_grain.R` is currently an empty placeholder.
- GWASpoly scripts contain HPC-specific working directories and require the
  external genotype, map, phenotype, and DAPC input files used during analysis.
- The phenotype scripts treat genotype as a random effect and calculate an
  intercept-plus-genotype BLUP, although some files call this value a BLUE. The
  report describes genotype as a fixed effect, so this model specification
  should be reconciled before reproducing the analysis from scratch.
