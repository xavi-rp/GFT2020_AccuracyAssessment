
#______________________________________________________________________________________________________________________#
#______________________________________________________________________________________________________________________#
#________                   Global Forest Type (GFT) Map 2020 v1 Accuracy Assessment                               ____#
#________                                                                                                          ____#
#________  Script based on the script used for the GFC 1st assessment, and shared in:                              ____#
#________  https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FOREST/EUFO/GFC2020_validation/                        ____#
#________  And its revised version in:                                                                             ____#
#________  https://github.com/xavi-rp/GFC2020_2ndAccuracyAssessment/blob/main/accuracy_assessment_GFCV2_xavi.R     ____#
#________                                                                                                          ____#
#______________________________________________________________________________________________________________________#
#______________________________________________________________________________________________________________________#

## The main objective of this script is to run the 1st assessment on the GFT2020 (v1) 
## using the new version of the validation data set (Collection2 -c2-).


## Setup ####

## libraries
library(mapaccuracy)
library(tidyverse)
library(openxlsx)
library(flextable)
library(officer)
library(sf)
library(rnaturalearth)



# set directories
#setwd("/validation")

dir_assessment2024 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first/GFC2020_validation/"

dir_Valid_c2 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"

dir_GFTv1_assessment_c2 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_GFT/GFTv1_assessment_c2/"




## Loading datasets ####

Valid_c2 <- read.csv(paste0(dir_Valid_c2, "Final_2026_GFC_Validation_Dataset.csv"))
names(Valid_c2)
head(Valid_c2)
nrow(Valid_c2) # 21752

# I need the centroid coordinates
valid_all <- read.csv(paste0(dir_Valid_c2, "AllValidations_2024_2026.csv"))
names(valid_all)


Valid_c2 <- valid_all %>% 
  select(location_id, "X2024_1st_pixel_center_x", "X2024_1st_pixel_center_y") %>% 
  #apply(2, function(x) sum(is.na))
  right_join(Valid_c2, by = "location_id") %>% 
  #nrow() # 21752
  filter(UsedIn_1stAssessment == "yes") #%>% nrow()  # 21612


writeFile <- "yes"
writeFile <- "no"
if(writeFile == "yes"){
  write.csv(Valid_c2, 
            paste0(dir_Valid_c2, "Valid_c2_coords.csv"), 
            row.names = FALSE)
}

  
Valid_c2 <- read.csv(paste0(dir_Valid_c2, "Valid_c2_coords.csv")) 

head(Valid_c2)
names(Valid_c2)
nrow(Valid_c2)  # 21612
table(Valid_c2$forest_class)
#     Forest   Non-forest 
#       6658        14954 


## _______________________________________
## This dataset is for GEE. There, I'll extract GFT values for each sample using the coordinates of the centroid
## _______________________________________


## Exploring "Valid_c2_coords_GFT2020_V1.csv" (exported from GEE)  ####

dwnld_it <- "no"

if(dwnld_it == "yes"){
  library(googledrive)
  drive_auth()
  2
  drive_ls() %>% print(n = 30)
  
  drive_download("Valid_c2_coords_GFT2020_V1.csv", 
                 path = paste0(dir_GFTv1_assessment_c2, "Valid_c2_coords_GFT2020_V1.csv"),
                 overwrite = TRUE,
                 verbose = FALSE) %>% with_drive_quiet()
}


## Exploring the dataset

Valid_c2_GFT2020_V1 <- read.csv(paste0(dir_GFTv1_assessment_c2, "Valid_c2_coords_GFT2020_V1.csv"))

Valid_c2_GFT2020_V1 <- Valid_c2_GFT2020_V1 %>% 
  rename(GFT2020_V1 = Map) #%>% head()

head(Valid_c2_GFT2020_V1)
table(Valid_c2_GFT2020_V1$GFT2020_V1)  # 1: Naturally regenerating forest; 10: Primary forest; 20: Planted/plantation forest
#     1     10     20 
#  3571   2527    627 

nrow(Valid_c2_GFT2020_V1)  # 6725
table(Valid_c2_GFT2020_V1$forest_class)
#     Forest  Non-forest 
#       5941         784 (non-forest in the validation dataset, but forest in GFT)


## Check samples that are forest in the validation dataset, but are not mapped as forest in GFTv1 (717 points like that) 
Valid_c2 %>% 
  filter_out(location_id %in% Valid_c2_GFT2020_V1$location_id) %>% 
  #nrow() # 14887   (14887 + 6725 = 21612)
  #pull(forest_class) %>% table()
#     Forest  Non-forest 
#        717       14170 
  select(location_id, X2024_1st_pixel_center_x, X2024_1st_pixel_center_y, forest_class, type_class) %>% 
  filter(forest_class == "Forest") %>% 
  tail(50)  ## With these coordinates, check in GEE
  


## Merging with those rows from the Validation dataset that are not exported from GEE, and therefore Non-forest in GFT2020v1 

Valid_c2_GFT2020_V1_full <- Valid_c2 %>%
  left_join(
    Valid_c2_GFT2020_V1 %>%
      select(location_id, GFT2020_V1),
    by = "location_id"
  ) 

head(Valid_c2_GFT2020_V1_full)
table(Valid_c2_GFT2020_V1_full$GFT2020_V1)    # 1: Naturally regenerating forest; 10: Primary forest; 20: Planted/plantation forest
#      1    10   20 
#   3571  2527  627 
sum(table(Valid_c2_GFT2020_V1_full$GFT2020_V1))   # 6725  Forest in GFT
sum(is.na(Valid_c2_GFT2020_V1_full$GFT2020_V1))   # 14887 Non-forest in GFT


## Recoding GFT2020_V1 into a binary variable Forest/Non-forest

Valid_c2_GFT2020_V1_full <- Valid_c2_GFT2020_V1_full %>%
  mutate(
    GFT_forest = if_else(
      is.na(GFT2020_V1),
      "Non-Forest",
      "Forest"
    )
  ) #%>% 

head(Valid_c2_GFT2020_V1_full)
table(Valid_c2_GFT2020_V1_full$GFT_forest)
sum(is.na(Valid_c2_GFT2020_V1_full$GFT_forest)) # 0

  
## 2 × 2 confusion matrix comparing the GFC2020 validation reference (forest_class) against the GFT-derived Forest/Non-Forest classification

cm <- Valid_c2_GFT2020_V1_full %>% 
  count(forest_class, GFT_forest) %>%
  rename(ValidC2_forest_class = forest_class) #%>% 
  #pivot_wider(
  #  names_from = GFT_forest,
  #  values_from = n,
  #  values_fill = 0
  #)
cm

p <- ggplot(cm, aes(x = GFT_forest, y = ValidC2_forest_class, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), size = 6) +
  scale_fill_gradient(low = "#DCEAF7", high = "#08519C") +
  labs(
    x = "GFT2020 V1",
    y = "ValidC2 reference",
    fill = "Samples"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 14)
  )

p

sve_it <- "no"
if(sve_it == "yes"){
  ggsave(paste0(dir_GFTv1_assessment_c2, "Valid_C2_GFT2020_V1_Forest_NonForest_confusion_matrix.png"), 
         p, width = 7, height = 6, dpi = 300)
}





## Validation ####

# load the the file with the strata names and count
strata_area <- read.csv(paste0(dir_assessment2024, "strata_size.csv"))

head(strata_area)
nrow(strata_area)   # 149
names(strata_area)   # 'Strata',  'strata_ha'
sort(unique(strata_area$Strata))   # e.g. 1001, etc
length(unique(strata_area$Strata))  # 149 strata, from 1001 to 8226


# load the files with the sample units interpretation and the map values

### GFT_v1_Valid_c2 ####
#scenario_kk <- read.csv(paste0(dir_assessment2024, "combined_scenario_all_GFCV2.csv"), sep = ",", na.strings = c#("NA", "N/A"), stringsAsFactors = FALSE)
#head(scenario_kk)
#
#table(scenario_kk$forest_class_num)
#table(scenario_kk$forest_class)

head(Valid_c2_GFT2020_V1_full)


scenario <- Valid_c2_GFT2020_V1_full %>% 
  rename(pixel_center_x = X2024_1st_pixel_center_x, 
         pixel_center_y = X2024_1st_pixel_center_y, 
         sample_id = X2024_1st_sample_id) %>% 
  mutate(
    forest_class_num = case_when(
      forest_class == "Forest" ~ 1,
      forest_class == "Non-forest" ~ 0,
      is.na(forest_class) ~ 100
    )
  ) %>% 
  select(pixel_center_x, 
         pixel_center_y, 
         sample_id, 
         forest_class_num, 
         type_class,
         strata, GFT2020_V1, gaul)

head(scenario)
table(scenario$forest_class)
table(scenario$forest_class_num)
#     0     1 
# 14954  6658 


names(scenario)   # "pixel_center_x"   "pixel_center_y"   "sample_id"  "forest_class_num"   "strata"   "GFT2020_V1"    "gaul"
nrow(scenario)   # 21612

apply(scenario, 2, function(x) sum(is.na(x)))   # no NAs, except GFT2020_V1 (14887) <- Non-Forest in GFT
sort(unique(scenario$forest_class_num))
head(scenario)


## Strata info ####

# Nh_strata numeric vector. Number of pixels forming each stratum (it's actually the area in ha). It must be named (see details)
head(strata_area)

Nh_strata <- strata_area$strata_ha
names(Nh_strata) <- strata_area$Strata

Nh_strata

head(Nh_strata)
sum(Nh_strata)



## Sample units interpretation and map values ####
dim(scenario)  # 21612, 7
sum(is.na(scenario$strata))  # 0


# drop the sample units with no strata value associated (0 value in the strata column)- 62 sample units
df_dropped <- scenario %>% 
  filter(strata != "0")

dim(df_dropped)


# drop the samples with no assignment (value 100) in the forest_class_num column - 24 samples

df_dropped_2 <- df_dropped %>% 
  filter(forest_class_num != "100")

dim(df_dropped_2)


# drop the samples located outside the FAO GAUL boundaries - 54 additional samples
table(df_dropped_2$gaul)

df_dropped_3 <- df_dropped_2 %>% 
  filter(gaul != "0")

dim(df_dropped_3)  # 21612, 8
head(df_dropped_3)

unique(df_dropped_3$type_class)
unique(df_dropped_3$forest_class_num)
unique(df_dropped_3$GFT2020_V1)
table(df_dropped_3$type_class)
table(df_dropped_3$forest_class_num)
table(df_dropped_3$GFT2020_V1)
sum(is.na(df_dropped_3$GFT2020_V1))


# Creating a new reference (validation data set c2) forest type variable
df_dropped_3 <- df_dropped_3 %>%
  mutate(
    forest_type = case_when(
      type_class %in% c(
        "no trees or shrubs present",
        "trees outside forest",
        "trees inside forest",
        "other wooded land",
        "trees for agricultural use",
        "trees in urban areas"
      ) ~ 0,
      type_class == "Naturally regenerating forest" ~ 1,
      type_class == "Planted or plantation forest" ~ 2
    )
  )
head(df_dropped_3)
sum(is.na(df_dropped_3$forest_type)) # 0
table(df_dropped_3$forest_type)
#     0     1 (natural)     2 (planted)
# 14954  6100             558 


# Creating the correspondent GFT forest type variable

df_dropped_3 <- df_dropped_3 %>%
  mutate(
    GFT_type = case_when(
      GFT2020_V1 %in% c(1, 10) ~ 1,
      GFT2020_V1 == 20 ~ 2,
      is.na(GFT2020_V1) ~ 0
    )
  )

head(df_dropped_3)
sum(is.na(df_dropped_3$GFT_type)) # 0
table(df_dropped_3$GFT_type)
#     0     1     2 
# 14887  6098   627      (6098 + 627 = 6725)

addmargins(
  table(
  Reference = df_dropped_3$forest_type,
  GFT = df_dropped_3$GFT_type
  )
)



# clean dataset
map_ref_values <- df_dropped_3
head(map_ref_values)
nrow(map_ref_values)  # 21612






## Analysis ####

#??stehman2014


# s character vector. Strata class labels. The object will be coerced to factor.
s <- map_ref_values$strata
sum(is.na(s)) # 0
table(s)


# r character vector. Reference class labels (validation dataset). The object will be coerced to factor.
r <- map_ref_values$forest_type
head(r)
table(r)


# m character vector. Map class labels (results of the model: GFC2020_vX -- v2 in this case). The object will be coerced to factor.
m <- map_ref_values$GFT_type
table(m)


# need to indicate the order for the labels of the matrix (0: Non-forest; 1: Natural/Primary; 2: Planted/Plantation.
order <- c(0, 1, 2)  

scenario_AA <- stehman2014(s,           # Strata class labels
                           r,           # Reference class labels (validation dataset)
                           m,           # Map class labels (results of the model: GFT2020_vX)
                           Nh_strata,   # Number of pixels forming each stratum (it's actually the area in ha) 
                           margins = TRUE,
                           order)

scenario_AA


### Results ####

## Overall accuracy
scenario_AA$OA   # 0.9154376
round(scenario_AA$OA, 3)   # 0.915


## User's accuracy of the 3 classes. Related to the Commission Error (False positives)
scenario_AA$UA

#  0 (Non-For)     1 (Natural)     2 (planted) 
#    0.9548987       0.8529271       0.4267526


1 - scenario_AA$UA#[2]

# 0            1            2 
# 0.04510134   0.14707287   0.57324737 

# Commission Error (Non-forest) = 0.045
# Commission Error (Natural)    = 0.147
# Commission Error (Planted)    = 0.573
# The estimated commission error for the Naturally regenerating Forest class was 14.7%, indicating that  
# approximately 15% of the area mapped as Naturally regenerating Forest is estimated to correspond  
# to other reference classes (Non-forest or Planted/plantation forest).

# More than half of the area mapped by GFT as Planted/plantation (57.3%) is estimated to belong to another reference class.



## GFCv2 (just for comparison)
#           0            1 
#  0.03692525   0.17995405     # GFCv2 Commission Error (forest)      = 0.17995405
                               # GFCv2 Commission Error (Non-forest)  = 0.037 
                               # (about 3.7% of the area (or pixels) mapped as Non-forest is 
                               # estimated to actually be Forest.)





## Producer's accuracy. Related to Omission Error (False Negatives)
scenario_AA$PA

#         0           1            2
# 0.9433225   0.8672492    0.5314436  


print(paste0("Ommission Error (Non-forest) = ",
             round((1 - scenario_AA$PA[1]), 3)))  # An estimated 5.7% of the pixels that are truly Non-forest were mapped as one of the forest classes.


print(paste0("Ommission Error (Naturally Regenerating Forest) = ",
             round((1 - scenario_AA$PA[2]), 3)))  # An estimated 13.3% of the pixels that are truly Naturally Regenerating Forest were mapped as
                                                  # either  Planted Forest or Non-Forest.

print(paste0("Ommission Error (Planted / Plantation Forest) = ",
             round((1 - scenario_AA$PA[3]), 3)))  # An estimated 46.9% of the pixels that are truly Planted / Plantation Forest were mapped as
                                                  # either Naturally Regenerating Forest or Non-Forest.


## Conclusion:
## 



## Standard error of OA
scenario_AA$SEoa    # 0.002206576 
round(((1.96*scenario_AA$SEoa)*100), 1)   # 0.4 


## Standard error of UA
scenario_AA$SEua
#           0             1             2
# 0.002035794   0.005266056   0.023853870


## standard error of PA
scenario_AA$SEpa
#           0             1             2
# 0.002195413   0.005055741   0.027248449  


## Proportion of area 
scenario_AA$area
#         0             1              2
# 0.70013412   0.28475867     0.01510722 


## standard error of area proportion
scenario_AA$SEa
#            0               1               2
#  0.003123383     0.003097202     0.000791796 



## Confusion error (area proportion). Rows and columns represent map and reference class labels, respectively.
## It is the area-adjusted error matrix: each cell is the estimated proportion of the total assessed area, not the number of validation samples.
scenario_AA$matrix

#                                 Reference (validation)
#                        0           1            2         sum  
#    G |  0    0.660452257  0.02895764  0.002236540  0.69164644
#    F |  1    0.037741473  0.24695673  0.004842043  0.28954025
#    T |  2    0.001940385  0.00884430  0.008028633  0.01881332
#         sum  0.700134115  0.28475867  0.015107215  1.00000000

round((scenario_AA$matrix * 100), 1)

# The problem is predominantly that GFT maps areas as Planted/plantation that the reference classifies as Naturally regenerating forest.
# UA (planted) = 42.7 %;  Commission Error (Planted) = 57.3 %
# Total area mapped as Planted:  0.001940385 + 0.00884430 + 0.008028633 = 0.01881332 
                              # (GFT estimates 1.881% of the total area as Planted/plantation forest.)

# 0.194 percentage points = GFT Planted -> Reference Non-Forest
# 0.884 percentage points = GFT Planted -> Reference Natural
# 0.803 percentage points = GFT Planted -> Reference Planted

# 0.00884430 / 0.01881332 = 47.0 % of the area mapped as Planted is estimated to actually be Natural.
# 0.001940385 / 0.01881332 = 10.3 % of the area mapped as Planted is estimated to actually be Non-Forest.
# 47.0 + 10.3 = 57.3 % Commission Error (Planted)

# Omission Error
# 46.9% of the reference Planted/plantation forest area is omitted by GFT.
# Reference Planted → GFT Natural: 32.1 %
# Reference Planted → GFT Non-Forest: 14.8 %


# Conclussion: 
# The major issue with GFT's Planted class is over-mapping Planted forest in areas that the reference classifies as Naturally 
# regenerating forest (commission error - planted: 57.3%).
# Similarly, GFT fails to map a substantial proportion of reference Planted forest as Planted (omission error - planted: 46.9%)



## Confidence intervals:   CI95 = 1.96 × SE
z <- 1.96

CI95_UA <- z * scenario_AA$SEua * 100; CI95_UA
#          0           1           2
#  0.3990156   1.0321469   4.6753586 


CI95_PA <- z * scenario_AA$SEpa * 100; CI95_PA
#          0           1           2
#  0.4303009   0.9909252   5.3406960 


## Commission error (%)
commission <- (1 - scenario_AA$UA) * 100
commission

## Omission error (%)
omission <- (1 - scenario_AA$PA) * 100
omission



### Reporting table ####
v <- "GFT_v1_Valid_c2"
xlsx_fileName <- paste0("Table_ErrorMatrix_", v, ".xlsx")

report_error_matrix <- function(data,
                                map_col,
                                ref_col,
                                assessment,
                                output_dir,
                                sheet_name = NULL,
                                excel_file = xlsx_fileName,
                                export_file = "no") {
  
  ## Names

  dataset_name <- as.character(substitute(data))
  
  if (is.null(sheet_name)) {
    sheet_name <- paste0("Err_matr_", map_col, "_", dataset_name)
  }
  
  ## Raw confusion matrix

  raw <- addmargins(table(
    Map = factor(data[[map_col]],
                 levels = c(0, 1, 2),
                 labels = c("Non-forest", "Natural Forest", "Planted Forest")),
    Reference = factor(data[[ref_col]],
                       levels = c(0, 1, 2),
                       labels = c("Non-forest", "Natural Forest", "Planted Forest"))
  ))
  
  
  ## Cell proportions (%)
  
  #prop <- round(100 * raw / sum(raw[1:2, 1:2]), 1)
  prop <- raw
  prop_1 <- round(assessment$matrix, 3) * 100
  
  prop['Non-forest', 'Non-forest'] <- prop_1["0", "0"]
  prop['Non-forest', 'Natural Forest'] <- prop_1["0", "1"]
  prop['Non-forest', 'Planted Forest'] <- prop_1["0", "2"]
  prop['Non-forest', 'Sum'] <- prop_1["0", "sum"]
  
  prop['Natural Forest', 'Non-forest'] <- prop_1["1", "0"]
  prop['Natural Forest', 'Natural Forest'] <- prop_1["1", "1"]
  prop['Natural Forest', 'Planted Forest'] <- prop_1["1", "2"]
  prop['Natural Forest', 'Sum'] <- prop_1["1", "sum"]
  
  prop['Planted Forest', 'Non-forest'] <- prop_1["2", "0"]
  prop['Planted Forest', 'Natural Forest'] <- prop_1["2", "1"]
  prop['Planted Forest', 'Planted Forest'] <- prop_1["2", "2"]
  prop['Planted Forest', 'Sum'] <- prop_1["2", "sum"]
  
  prop['Sum', 'Non-forest'] <- prop_1["sum", "0"]
  prop['Sum', 'Natural Forest'] <- prop_1["sum", "1"]
  prop['Sum', 'Planted Forest'] <- prop_1["sum", "2"]
  prop['Sum', 'Sum'] <- prop_1["sum", "sum"]
  
  

  ## Commission errors

  commission <- c(
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$UA["0"]) * 100,
            1.96 * assessment$SEua["0"] * 100),
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$UA["1"]) * 100,
            1.96 * assessment$SEua["1"] * 100),
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$UA["2"]) * 100,
            1.96 * assessment$SEua["2"] * 100),
    
    "",
    ""
    
  )
  
  ## Omission errors

  
  omission <- c(
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$PA["0"]) * 100,
            1.96 * assessment$SEpa["0"] * 100),
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$PA["1"]) * 100,
            1.96 * assessment$SEpa["1"] * 100),
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$PA["2"]) * 100,
            1.96 * assessment$SEpa["2"] * 100),
    
    "",
    paste0("OA Acc. (CI95) [%] = ",
    sprintf("%.1f (%.1f)",
            assessment$OA * 100,
            1.96 * assessment$SEoa * 100))
    
  )
  
  ## Reporting table

  report_table <- data.frame(
    
    Class = c(
      "Non-forest",
      "Natural Forest",
      "Planted Forest",
      "Total",
      "Omission (CI95) [%]"
    ),
    
    Raw_NonF    = c(raw[1,1], raw[2,1], raw[3,1], raw[4,1], ""),
    Raw_NatF    = c(raw[1,2], raw[2,2], raw[3,2], raw[4,2], ""),
    Raw_PlaF    = c(raw[1,3], raw[2,3], raw[3,3], raw[4,3], ""),
    Counts_Total   = c(raw[1,4], raw[2,4], raw[3,4], raw[4,4], ""),
    
    Prop_NonF   = c(prop[1,1], prop[2,1], prop[3,1], prop[4,1], omission[1]),
    Prop_NatF   = c(prop[1,2], prop[2,2], prop[3,2], prop[4,2], omission[2]),
    Prop_PlaF   = c(prop[1,3], prop[2,3], prop[3,3], prop[4,3], omission[3]),
    Prop_Total  = c(prop[1,4], prop[2,4], prop[3,4], prop[4,4], omission[4]),
    
    commission_ci95 = commission,
    check.names = FALSE
    
  )
  
  report_table[5, "commission_ci95"] <- omission[5]
  
  names(report_table)[10] <- "Commission (CI95) [%]"
  

  ## Excel

  excel_path <- file.path(output_dir, excel_file)
  
  if (file.exists(excel_path)) {
    
    wb <- openxlsx::loadWorkbook(excel_path)
    
    if (sheet_name %in% names(wb)) {
      removeWorksheet(wb, sheet_name)
    }
    
  } else {
    
    wb <- createWorkbook()
    
  }
  
  addWorksheet(wb, sheet_name)
  
  writeData(
    wb,
    sheet = sheet_name,
    x = report_table
  )
  
  if (export_file == "yes"){
    saveWorkbook(
      wb,
      excel_path,
      overwrite = TRUE
    )
  } else{
    print("excel table not exported to disk")
  }
  
  ## Flextable

  ft <- flextable(report_table)
  
  ft <- bg(ft, bg = "white", part = "all")
  
  grey <- "#808080"
  
  border <- fp_border(color = grey, width = 1)
  
  ft <- border_remove(ft)
  ft <- border_outer(ft, border)
  ft <- border_inner_h(ft, border)
  ft <- border_inner_v(ft, border)
  
  ft <- add_header_row(
    ft,
    values = c(
      "",
      "Raw counts (Reference)",
      "Proportions [%] (Reference)",
      ""
    ),
    colwidths = c(1, 4, 4, 1)
  )
  
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "header")
  ft <- bold(ft, j = 1, bold = TRUE, part = "body")
  ft <- align(ft, j = 2:10, align = "center", part = "body")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- autofit(ft)
  ft <- bold(ft, i = 5, j = 10, bold = TRUE, part = "body")
  

  ## PNG

  if (export_file == "yes"){

    save_as_image(
      ft,
      path = file.path(output_dir,
                       paste0(sheet_name, ".png"))
    )
  } else {
    print("png image not exported to disk")
  }
  

  

  ## Return objects

  invisible(
    list(
      raw_table = raw,
      table = report_table,
      flextable = ft
    )
  )
  
}



## Run the function
Valid_c2 <- map_ref_values   # change the name to write correct file/tab names

res <- report_error_matrix(
  data       = Valid_c2,             # change the name
  map_col    = "GFT_type",             
  ref_col    = "forest_type",
  assessment = scenario_AA,
  output_dir = dir_GFTv1_assessment_c2     # change the name
)

res$raw_table
res$table
res$flextable  ## Table 4 of the 2025 report





## MAPS ####

### Correctly and missclassified sample units (Figure 13) ####

head(scenario) ; nrow(scenario)
head(map_ref_values) ; nrow(map_ref_values)
unique(map_ref_values$GFT_type)  # 0, 1, 2
unique(map_ref_values$forest_type)  # 0, 1, 2   # c("Non-forest", "Natural Forest", "Planted Forest")

map_ref_values <- map_ref_values %>%
  mutate(
    class = case_when(
      GFT_type == 0 & forest_type == 0 ~ "Non forest",
      GFT_type == 1 & forest_type == 1 ~ "Natural Forest",
      GFT_type == 2 & forest_type == 2 ~ "Planted Forest",
      
      GFT_type == 1 & (forest_type == 0 | forest_type == 2 ) ~ "Commission error (Natural Forest)",
      GFT_type == 2 & (forest_type == 0 | forest_type == 1 ) ~ "Commission error (Planted Forest)",
      
      (GFT_type == 0 | GFT_type == 2) & forest_type == 1 ~ "Omission error (Natural Forest)",
      (GFT_type == 0 | GFT_type == 1) & forest_type == 2 ~ "Omission error (Planted Forest)"
    )
  )

head(map_ref_values)
unique(map_ref_values$class)

sum(is.na(map_ref_values$pixel_center_x))  # 0
sum(is.na(map_ref_values$pixel_center_y))  # 0


pts <- st_as_sf(
  map_ref_values,
  coords = c("pixel_center_x","pixel_center_y"),
  crs = 4326)

pts
table(pts$class)


##___________

### Write errors in KMLs ####
GFT_ref_errors <- pts %>% 
  rename(GFT_C2_Assessment = class)

table(GFT_ref_errors$GFT_C2_Assessment)


## all samples (can be filtered using 'GFT_C2_Assessment')
st_write(GFT_ref_errors, 
         paste0(dir_GFTv1_assessment_c2, "GFT_C2_Assessment.kml"),
         delete_dsn = TRUE)



## Commission error (Planted Forest)
GFT_C2_CommissErrorPlanted <- GFT_ref_errors %>% 
  filter(GFT_C2_Assessment == "Commission error (Planted Forest)") #%>% print()

GFT_C2_CommissErrorPlanted

st_write(GFT_C2_CommissErrorPlanted, 
         paste0(dir_GFTv1_assessment_c2, "GFT_C2_CommissErrorPlanted.kml"),
         delete_dsn = TRUE)



## All errors
table(GFT_ref_errors$GFT_C2_Assessment)

GFT_C2_AllErrors <- GFT_ref_errors %>% 
  filter(str_starts(GFT_C2_Assessment, "^(Comm|Om)")) #%>% pull("GFT_C2_Assessment") %>% table()

table(GFT_C2_AllErrors$GFT_C2_Assessment)

st_write(GFT_C2_AllErrors, 
         paste0(dir_GFTv1_assessment_c2, "GFT_C2_AllErrors.kml"),
         delete_dsn = TRUE)

##___________




#world <- ne_countries(scale = "medium", returnclass = "sf")
world <- ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf")

# map
figure_title <- "GFT2020_v1 / Valid_c2"



plot_error_map <- function(pts, world, figure_title = NULL) {
  
  # Classes actually present in pts
  classes <- unique(pts$class)
  classes <- classes[!is.na(classes)]
  
  # Colours
  class_colours <- c(
    "Natural Forest" = "#95B958",
    "Planted Forest" = "#6A3D9A",
    "Commission error (Natural Forest)" = "#FF7F00",
    "Commission error (Planted Forest)" = "#FDBF00",
    "Omission error (Natural Forest)" = "#1F78B4",
    "Omission error (Planted Forest)" = "#6E9F6D"
  )
  
  # Point sizes
  class_sizes <- c(
    "Natural Forest" = 0.60,
    "Planted Forest" = 0.60,
    "Commission error (Natural Forest)" = 1.20,
    "Commission error (Planted Forest)" = 1.20,
    "Omission error (Natural Forest)" = 1.20,
    "Omission error (Planted Forest)" = 1.20
  )
  
  # Legend point sizes
  legend_sizes <- c(
    "Natural Forest" = 2,
    "Planted Forest" = 2,
    "Commission error (Natural Forest)" = 4,
    "Commission error (Planted Forest)" = 4,
    "Omission error (Natural Forest)" = 4,
    "Omission error (Planted Forest)" = 4
  )
  
  # Start plot
  p <- ggplot() +
    geom_sf(
      data = world,
      fill = "white",
      colour = "black",
      linewidth = 0.2
    )
  
  # Add one layer for each class present
  for (cl in classes) {
    
    p <- p +
      geom_sf(
        data = filter(pts, class == cl),
        aes(colour = class),
        size = class_sizes[cl],
        alpha = ifelse(cl %in% c("Natural Forest", "Planted Forest"),
                       0.25, 0.95),
        show.legend = TRUE
      )
  }
  
  # Keep only colours/classes that actually occur
  
  class_order <- c(
    "Natural Forest",
    "Planted Forest",
    "Commission error (Natural Forest)",
    "Commission error (Planted Forest)",
    "Omission error (Natural Forest)",
    "Omission error (Planted Forest)"
  )
  
  p <- p +
    scale_colour_manual(
      values = class_colours[classes],
      breaks = class_order[class_order %in% classes]
    ) +
    
    guides(
      colour = guide_legend(
        override.aes = list(
          size = legend_sizes[class_order[class_order %in% classes]],
          alpha = 1
        )
      )
    ) +
    
    coord_sf(
      xlim = c(-180, 180),
      ylim = c(-60, 85),
      expand = FALSE
    ) +
    
    ggtitle(figure_title) +
    
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.90,
        face = "bold",
        size = 12,
        margin = margin(b = 10)
      ),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 11)
    )
  
  return(p)
}


head(pts)
unique(pts$class)
#"Natural Forest"                    "Non forest"                        "Omission error (Natural Forest)" #"Commission error (Natural Forest)" "Commission error (Planted Forest)" "Planted Forest"                  #"Omission error (Planted Forest)" 

## Planted forests
pts_planted <- pts %>% 
  #filter(class %in% c("Planted Forest", "Commission error (Planted Forest)", "Omission error (Planted Forest)"))
  filter(class %in% c("Commission error (Planted Forest)", "Omission error (Planted Forest)"))

p_planted_1<- plot_error_map(pts = pts_planted, world = world, figure_title = figure_title)
p_planted_1

# save figure
Fig13_dir <- dir_GFTv1_assessment_c2
Fig13_Filename <- paste0("Fig_ErrorDistribution_", "PlantedForest",
                         "_GFT2020_v1_Valid_c2.png")

ggsave(filename = file.path(Fig13_dir, Fig13_Filename), 
       plot = p_planted_1, 
       width = 20, height = 12, units = "cm", dpi = 600, bg = "white")



## Natural Foorests
pts_natural <- pts %>% 
  filter(class %in% c("Natural Forest", "Commission error (Natural Forest)", "Omission error (Natural Forest)"))

p_natural_1<- plot_error_map(pts = pts_natural, world = world, figure_title = figure_title)
p_natural_1



# save figure
Fig13_Filename <- paste0("Fig_ErrorDistribution_", "NaturalForest",
                         "_GFT2020_v1_Valid_c2.png")

ggsave(filename = file.path(Fig13_dir, Fig13_Filename), 
       plot = p_natural_1, 
       width = 20, height = 12, units = "cm", dpi = 600, bg = "white")


