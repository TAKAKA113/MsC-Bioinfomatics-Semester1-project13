loc <- readxl::read_excel("data_processed/location_filtered.xlsx")

head(loc)
colnames(loc)

##
loc$genotype <- as.factor(loc$genotype)
loc$site     <- as.factor(loc$site)
loc$rep      <- as.factor(loc$rep)

##
str(loc$genotype)
str(loc$site)

##
formula_spno <- spno ~ 1 + (1 | genotype) + (1 | site)
formula_spno

##
model_spno <- lmer(
  spno ~ 1 + (1 | genotype) + (1 | site),
  data = loc,
  REML = TRUE
)