## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CoralNet data processing and category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(reshape)


## list out current path
getwd()


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"


## invoke relative file path 
setwd(data_input)


## read in csv file 
dat <- read.csv("combined_csv.csv")
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## process metadata ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## function to remove characters from columns
remove_characters <- function(df, column_name, direction, num_chars) {
  trim_chars <- function(x, direction, num_chars) {
    if (direction == "left") {
      return(substr(x, num_chars + 1, nchar(x)))
    } else if (direction == "right") {
      return(substr(x, 1, nchar(x) - num_chars))
    } else {
      stop("Invalid direction. Use 'left' or 'right'.")
    }
  }
  
  df[[column_name]] <- sapply(df[[column_name]], trim_chars, direction = direction, num_chars = num_chars)
  return(df)
}


## function to bring new column to the [1] position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
}


## function to create a unique key identifier combining site and transect info
create.key <- function(data){
  data$key <- data$site
  data$key <- with(data, paste0(key,"_",transect))
  data <- front.ofthe.line(data)
  return(data)
}


## function to create 1:nrow SU - sample unit - variable 
create.SU <- function(data){
  data$SU <- 1:nrow(data)
  data <- front.ofthe.line(data)
  return(data)
}


## copy columns to new dataframe (create metadata df)
split_dataframe <- function(df, num_cols){
  new.df <- df[, 1:num_cols]
  return(new.df)
}


## delete columns, but preserve some
delete_columns <- function(df, cols_to_delete, cols_to_preserve) {
  cols_to_delete_names <- names(df)[cols_to_delete]   # Convert column indices to names
  cols_to_delete_names <- setdiff(cols_to_delete_names, cols_to_preserve)    # Remove the columns to be preserved from the delete list
  df <- df[, !names(df) %in% cols_to_delete_names]    # Select columns that are not in the delete list
  return(df)
}


## combine columns - add together the CoralNet counts from multiple categories
combine_columns <- function(df, columns, new_column_name) {
  df <- df %>%
    mutate(!!sym(new_column_name) := rowSums(select(., all_of(columns))))
  return(df)
}


## delete columns once they have been combined
remove_columns <- function(df, columns_to_delete) {
  df <- df %>% select(-all_of(columns_to_delete))
  return(df)
}


## function that uses previous two functions to edit categories
revise_categories <- function(data, category_list, new_column){
  new_category <- category_list
  data <- combine_columns(data, new_category, new_column)
  data <- remove_columns(data, new_category)
  return(data)
}


## function to rename columns
rename_columns <- function(data, old, new) {
  names(data)[names(data) %in% old] <- new
  return(data)
}
## END function definition ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## invoke functions - data cleaning / processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## remove "S" and "T" labels from site columns 
dat <- remove_characters(dat, "site", "left", 1)
dat <- remove_characters(dat, "transect", "left", 1)


## create key and SU columns, sort data appropriately by site
dat <- create.key(dat)
dat <- arrange(dat, site)
dat <- create.SU(dat)


## split off metadata
metadata <- split_dataframe(dat, 31)


## columns to delete (most metadata)
cols_to_delete <- 1:31 


## but preserve these specific columns 
cols_to_preserve <- c("SU", "key", "site", "transect", "img_name") # Columns to preserve


## invoke function
dat <- delete_columns(dat, cols_to_delete, cols_to_preserve) # Delete columns and get the updated dataframe
## END data processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## revise CoralNet categories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## smooth brown algae categories
dat <- revise_categories(dat, c("coland_KE", "X5rib_KE", "cabb_KE"), "textured_kelp")

## smooth brown algae categories
dat <- revise_categories(dat, c("X3rib_KE", "broad_KE", "split_KE", "strap_KE"), "smooth_kelp")

## other brown kelp categories
dat <- revise_categories(dat, c("cord_KE", "DiRet_KE", "feath_KE", "LaEph_KE", 
                                "LaLoSi_KE", "palm_KE", "ribbn_KE", "stalk_KE"),"other_brown_kelp")

## revise red algae categories
dat <- revise_categories(dat, c("filam_RE", "bushy_RE", "branch_RE", "leaf_RE", "unk_RE"), "red_algae")

## revise shell categories
dat <- revise_categories(dat, c("Shell_SU", "lgshell_SU"), "shell_SU")

## revise hard substrate categories
dat <- revise_categories(dat, c("reef_SU", "bould_SU", "concr_SU"), "reef_SU")

## revise hard substrate categories
dat <- revise_categories(dat, c("glass_SU", "metal_SU", "poly_SU", "wood_SU", "anth_SU"), "anthro_substrate")

## revise eelgrass, surfgrass categories
dat <- revise_categories(dat, c("eel_SG", "surf_SG"), "seagrass")

## revise sessile invert categories
dat <- revise_categories(dat, c("HydBry_SI", "cup_SI", "CucEmb_SI", "mussel_SI", 
                                "scallop_SI", "SponSol_SI", "TunSol_SI", "anem_SI", "encru_SI"), "sessile_invert")

## revise mobile invert categories
dat <- revise_categories(dat, c("CaCuc_MS", "crab_MS", "gastro_MS", 
                                "fish_MS", "SStar_MS", "urchin_MS"), "mobile_invert")

## revise eelgrass, surfgrass categories
dat <- revise_categories(dat, c("art_CA", "crust_CA"), "coralline_algae")

## delete extraneous columns
dat <- remove_columns(dat, c("StriAci_BR", "undaria_BR", "unk_BR", "giant_KE", 
                             "bullBL_KE", "bullST_KE", "senes_RE", "senes_AL"))

## rename columns
dat <- rename_columns(dat, c("SS_SU", "cob_SU", "peb_SU"), c("soft_sediment","cobble","pebble")) 

## rename additional columns 
dat <- rename_columns(dat, c("KelpBry_SI", "UNIdent", "filam_BR", "FlatAci_BR", "fucus_BR", "holdfas_BR", "sargass_BR", "sugar_KE", "shell_SU"),
                      c("kelp_bryozoan", "unknown", "filamentous_brown", "acid_weed", "rock_weed", "kelp_holdfast", "sargassum", "sugar_kelp", "shell_debris"))

dat <- rename_columns(dat, "ulva_GR", "green_algae")
## END CoralNet category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd(data_output)
write.csv(dat, "revised_CoralNet_categories")
## END export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
