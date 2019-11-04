# -----------------------------
# PROTEIN ABUNDANCE CONVERSIONS
# -----------------------------
#  
# Protein quantity was aggregated from MS-based label-free peptide measurements using the sum
# of all peptide abundances. This relative number was divided by the sum of all protein abundances
# resulting in protein mass fraction (not mol fraction), the main metric used in Jahn et al., Cell
# Reports, 2018. However this metric can be used, together with some 'helper' parameters to convert
# protein mass in to mol fraction, molecules per cell, molecules oper volume, and others.
# 
# Columns to be added:
# --------------------
#
# - Protein mass in g/gDCW. This is simply the protein mass fraction (g/g) multiplied by
#   protein content per DCW (on average 65%, Touloupakis et al., Biotechnology for Biofuels, 2015)
mass_g_per_gDCW <- function(mass_frac) {
  mass_frac <- replace(mass_frac, mass_frac < 0, 0)
  mass_frac * 0.65 
  # unit: g/gDCW
}

# - mol fraction of proteins (number of protein as fraction of total protein number).
#   Simply divide mass fraction by molar mass (g/mol, convert from kDA), and divide results by sum of mol
mol_fraction <- function(mass_frac, MW) {
  mass_frac <- replace(mass_frac, mass_frac < 0, 0)
  mol_frac <- mass_frac / (MW*1000)
  mol_frac/sum(mol_frac, na.rm = TRUE)
  # unit: mol/mol
}

# - g protein mass/L cell volume. Here we need an extra parameter, the
#   cell volume in L/gDCW (NB: not culture volume!).
#   
#   Conversion factors needed for transformation:
#   1 gDCW/L culture = OD 4 (in-house measurement, Anfelt et al., Mircob Cell Fact, 2015)
#   OD 4 = 25 * 10^10 cells/L (µ = 0.08; Du et al., Algal Research, 2016), so that
#   
#   N (1 gDCW) = 25 * 10^10 cells
#   V (1 cell) = 9 * 10^-15 L (µ = 0.08; Du et al., Algal Research, 2016)
#   N (1 L cell volume) = 1/(9 * 10^-15) = 1.11111 * 10^14 cells
#   V (1 gDCW cells) = 25*10^10 cells * 9*10^-15 L/cell = 0.00225 L/gDCW = 2.25*10^-3 L/gDCW
conc_g_per_L_cell_vol <- function(mass_g_per_gDCW) {
  mass_g_per_gDCW / 2.25*10^-3 
  # unit: g/L cell volume
}

# - N (mol protein / cell). That is protein mass fraction in g/gDCW multiplied with 
#   gDCW/cell, divided by molar mass in g/mol
conc_mol_per_cell <- function(mass_g_per_gDCW, MW) {
  mass_g_per_gDCW / (25 * 10^10) / (MW*1000)
  # unit: mol/cell
}

# - N (mol protein / L cell volume). Multiply mol protein per cell with N cells per L
conc_mol_per_L_cell_vol <- function(conc_mol_per_cell) {
  conc_mol_per_cell * 1.11111 * 10^14
  # unit: mol/L cell volume
}

# - N (copies protein / cell). Simply multiply mol/cell by Avogadro constant (n/mol)
conc_copies_per_cell <- function(conc_mol_per_cell) {
  conc_mol_per_cell * (6.02214086 * 10^23)
  # unit: copies/cell
}

# not related to all above: 95 % confidence interval, relative to mean
rel_CI = function(x) {
  if (sum(is.na(x) > 2)) NA else {
    ci = Rmisc::CI(x)
    (ci[1]-ci[2])/ci[2]
  }
}


# Modifications to csv
# --------------------

# load packages
library(tidyverse)

# load original data
df <- read_csv("data/Jahn-et-al-2018_Pertubations.csv") %>% 
  group_by(condition)

# check that grouping works, i.e. all mass fractions per condition sum to unity
summarise(df, sum(mean_mass_fraction_norm, na.rm = TRUE))

# apply transformations
df_new <- df %>% 
  
  mutate(
    mass_g_per_gDCW = mass_g_per_gDCW(mean_mass_fraction_norm), 
    mol_fraction = mol_fraction(mean_mass_fraction_norm, MolWeight), 
    conc_g_per_L_cell_vol = conc_g_per_L_cell_vol(mass_g_per_gDCW),
    conc_mol_per_cell = conc_mol_per_cell(mass_g_per_gDCW, MolWeight),
    conc_mol_per_L_cell_vol = conc_mol_per_L_cell_vol(conc_mol_per_cell),
    conc_copies_per_cell = conc_copies_per_cell(conc_mol_per_cell)
  )
  
# Quality control
# --------------------

# check that mol fractions sum to unity
summarise(df_new, sum(mol_fraction, na.rm = TRUE))

# check that g/gDCW sum to ~0.65
summarise(df_new, sum(mass_g_per_gDCW, na.rm = TRUE))

# check min, max and sum of protein copies per condition
summarise(df_new, 
  min = min(conc_copies_per_cell, na.rm = TRUE),
  max = max(conc_copies_per_cell, na.rm = TRUE),
  sum = sum(conc_copies_per_cell, na.rm = TRUE)
)

# Save results
# --------------------
#write_csv(df_new, "data/Jahn-et-al-2018_Pertubations.csv")