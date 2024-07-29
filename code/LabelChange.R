#################################################################################

# LabelChange.R
# Merge labels together from original CPP_ROV to Elliott Bay classifications

#################################################################################
rm(list = ls())

## list out current path
getwd()


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"


## invoke relative file path 
setwd(data_input)

## check working directory
getwd()

#################################################################################
# Load libraries

library(dplyr)
library(readr)
#################################################################################

#################################################################################
# Load required data

data = read.csv("./annotations.csv", stringsAsFactors = FALSE)
#################################################################################

# Define the mapping of old values to new values
replacements <- list(
  SU_silt = "SS_SU",
  SU_shell = c("shell_SU", "lgshell_SU"),
  SU_peb = "peb_SU",
  SU_cob = "cob_SU",
  SU_bould = c("bould_SU", "reef_SU", "concr_SU"),
  SU_wood = "wood_SU",
  SU_anth = c("glass_SU", "metal_SU", "poly_SU", "anth_SU"),
  SG = c("eel_SG", "surf_SG"),
  AL_green = c("ulva_GR"),
  KE_text = c("5rib_KE", "cabb_KE", "cord_KE", "coland_KE", "cord_KE", "DiRet_KE"),
  KE_smooth = c("ribbn_KE", "3rib_KE", "bullBL_KE", "stalk_KE", "sugar_KE", "broad_KE"),
  KE_stipe = "bullST_KE",
  KE_holdfas = "holdfas_BR",
  BR_fucus = "fucus_BR",
  BR_acid = c("FlatAci_BR", "StriAci_BR"),
  BR_filam = "filam_BR",
  BR_sarg = "sargass_BR",
  AL_red = c("bushy_RE", "branch_RE", "leaf_RE", "filam_RE", "unk_RE", "art_CA", "senes_RE"),
  AL_CCA = "crust_CA",
  SI_kelpBry = "KelpBry_SI",
  SI = c("anem_SI", "HydBry_SI", "cup_SI", "CucEmb_SI", "mussel_SI", "scallop_SI", "SponSol_SI", 
         "TunSol_SI", "encru_SI"),
  MS = c("CaCuc_MS", "crab_MS", "gastro_MS", "fish_MS", "SStar_MS", "urchin_MS")
)
  
  
# Function to replace labels based on the mapping
replace_labels <- function(label) {
  for (replacement in names(replacements)) {
    if (label %in% replacements[[replacement]]) {
      return(replacement)
    }
  }
  return(label)
}

# Apply the replacement function to the "Label" column
data$Label <- sapply(data$Label, replace_labels)

# Remove rows where the "Label" column has values "unk_BR" or "senes_AL"
data_filtered <- data %>%
  filter(!(Label %in% c("unk_BR", "senes_AL", "UNIdent", "undaria_BR")))

# Write the updated data to a new CSV file
write.csv(data_filtered, "NewLabel_annotations.csv", row.names = FALSE)
