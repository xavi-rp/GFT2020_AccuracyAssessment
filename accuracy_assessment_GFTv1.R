
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

library(googledrive)
drive_auth()
2
drive_ls() %>% print(n = 30)

drive_download("Valid_c2_coords_GFT2020_V1.csv", 
               path = paste0(dir_GFTv1_assessment_c2, "Valid_c2_coords_GFT2020_V1.csv"),
               overwrite = TRUE,
               verbose = FALSE) %>% with_drive_quiet()


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

ggsave(paste0(dir_GFTv1_assessment_c2, "Valid_C2_GFT2020_V1_Forest_NonForest_confusion_matrix.png"), 
       p, width = 7, height = 6, dpi = 300)



## Validation ####

# load the the file with the strata names and count
strata_area <- read.csv(paste0(dir_assessment2024, "strata_size.csv"))

head(strata_area)
nrow(strata_area)   # 149
names(strata_area)   # 'Strata',  'strata_ha'
sort(unique(strata_area$Strata))   # e.g. 1001, etc
length(unique(strata_area$Strata))  # 149 strata, from 1001 to 8226


# load the files with the sample units interpretation and the map values

### GFC_v2_Valid_v2 ####
scenario <- read.csv(paste0(dir_assessment2024, "combined_scenario_all_GFCV2.csv"), sep = ",", na.strings = c("NA", "N/A"), stringsAsFactors = FALSE)

head(scenario)
names(scenario)   # "pixel_center_x"   "pixel_center_y"   "sample_id"  "forest_class_num"   "strata"   "GFC_v2"    "gaul"
nrow(scenario)   # 21752

apply(scenario, 2, function(x) sum(is.na(x)))   # no NAs

sort(unique(scenario$forest_class_num))
sort(table(scenario$forest_class_num))
#   100     1       0 
#    24  6598   15130       # 24 samples with forest_class_num = 100 are those not assigned in 2024; not included in the assessment

table(scenario$strata)      # 64 samples with strata = 0 (no strata); not included in the assessment
table(scenario$gaul)        # 59 smaples with gaul = 0 (no gaul); not included in the assessment. However, keep in mind that 5 of these are 
#    0       1              # already not included because they do not have strata; therefore 54 additional samples not included
#   59   21693 


table(scenario$GFC_v2)      
#     0       1 
# 14386    7366  




### GFC_v2_Valid_v3 ####
scenario_GFC_v2_Valid_v3 <- read.csv(paste0(dir_Valid_v3, "Final_2026_GFC_Validation_Dataset.csv"))
head(scenario_GFC_v2_Valid_v3)
nrow(scenario_GFC_v2_Valid_v3)   # 21752

names(scenario_GFC_v2_Valid_v3)  # "ID" "Region" "location_id" "X2024_1st_sample_id" "groupid" "forest_class" "confidence_level" "type_class" "class_issues" "comment" "strata" "gaul" "continent_gaul" "UsedIn_1stAssessment"  


# "pixel_center_x"   "pixel_center_y"   "sample_id"  "forest_class_num"   "strata"   "GFC_v2"    "gaul"

head(scenario_GFC_v2_Valid_v3)

range(scenario_GFC_v2_Valid_v3$X2024_1st_sample_id)
range(scenario$sample_id)

scenario_GFC_v2_Valid_v3 %>% 
  select("X2024_1st_sample_id", "forest_class", "UsedIn_1stAssessment") %>% 
  #pull("forest_class") %>% is.na() %>% sum()          # 13
  #pull("forest_class") %>% table()                     # Forest  6666    Non-forest 15073
  pull("UsedIn_1stAssessment") %>% table()              #  no  140;   yes  21612

       
scenario %>% 
  #pull(forest_class_num) %>% is.na() %>% sum()      # 0
  pull(forest_class_num) %>% table()                 #  0  15130;     1  6598;     100  24



scenario_GFC_v2_Valid_v3 <- scenario_GFC_v2_Valid_v3 %>% 
  #select("X2024_1st_sample_id", "forest_class", "UsedIn_1stAssessment") %>% 
  select("X2024_1st_sample_id", "forest_class") %>% 
  mutate(
    forest_class_num_v3 = case_when(
      forest_class == "Forest" ~ 1,
      forest_class == "Non-forest" ~ 0,
      is.na(forest_class) ~ 100
    )
  )   %>% #head()
  select(-forest_class) #%>% head()


head(scenario_GFC_v2_Valid_v3)
table(scenario_GFC_v2_Valid_v3$forest_class_num_v3)
# 0         1   100 
# 15073  6666    13 



scenario <- scenario %>% 
  left_join(scenario_GFC_v2_Valid_v3, by = c("sample_id" = "X2024_1st_sample_id")) %>% #head()
  mutate(
    forest_class_num_v3 = if_else(
      forest_class_num == 100,
      100,
      forest_class_num_v3
    )
  ) #%>% filter(forest_class_num == 100)


head(scenario)
sum(is.na(scenario$forest_class_num_v3))  # 0


write.csv(scenario, paste0(dir_2ndAssessment_GFC2020_v2_Valid_v3, "scenario_GFC_v2_Valid_v3.csv"), row.names = FALSE)


### loading 'scenario_GFC_v2_Valid_v3' ####
scenario <- read.csv(paste0(dir_2ndAssessment_GFC2020_v2_Valid_v3, "scenario_GFC_v2_Valid_v3.csv"))


scenario <- scenario %>% #head()
  select(pixel_center_x, pixel_center_y, sample_id,
         forest_class_num_v3,
         strata, GFC_v2, gaul) %>% #head()
  rename(forest_class_num = forest_class_num_v3) #%>% head()




## Strata info ####

# Nh_strata numeric vector. Number of pixels forming each stratum (it's actually the area in ha). It must be named (see details)
head(strata_area)

Nh_strata <- strata_area$strata_ha
names(Nh_strata) <- strata_area$Strata

Nh_strata

head(Nh_strata)
sum(Nh_strata)



## Sample units interpretation and map values ####
dim(scenario)  # 21752, 7
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

df_dropped_3 <- df_dropped_2 %>% 
  filter(gaul != "0")

dim(df_dropped_3)  # 21612, 7

# clean dataset
map_ref_values <- df_dropped_3
head(map_ref_values)
nrow(map_ref_values)  # 21612




## Analysis ####

#??stehman2014


# s character vector. Strata class labels. The object will be coerced to factor.
s <- map_ref_values$strata
sum(is.na(s)) # 0



# r character vector. Reference class labels (validation dataset). The object will be coerced to factor.
r <- map_ref_values$forest_class_num


# m character vector. Map class labels (results of the model: GFC2020_vX -- v2 in this case). The object will be coerced to factor.
m <- map_ref_values$GFC_v2


# need to indicate the order for the labels of the matrix (0 non forest, 1 forest)
order <- c(0,1)
scenario_AA <- stehman2014(s,           # Strata class labels
                           r,           # Reference class labels (validation dataset)
                           m,           # Map class labels (results of the model: GFC2020_vX)
                           Nh_strata,   # Number of pixels forming each stratum (it's actually the area in ha) 
                           margins = TRUE,
                           order)

scenario_AA


### Results ####

## Overall accuracy
scenario_AA$OA   # 0.9150627          # Valid_v3: 0.9159055
round(scenario_AA$OA, 3)   # 0.915    # Valid_v3: 0.916


## User's accuracy of the two classes. Related to the Commission Error (False positives)
scenario_AA$UA

#    0 (Non-For)         1  (For)
# 0.9630748            0.8200459 
# 0.9636623            0.8213937      (Valid_v3)



1 - scenario_AA$UA#[2]
#           0            1 
#  0.03692525   0.17995405     # Coommission Error = 0.17995405
                               # The estimated commission error for the Forest class was 18.0%, indicating that approximately 18%
                               # of the area mapped as Forest is estimated to correspond to Non-forest in the reference data.
#  0.03633772   0.17860631     # Coommission Error = 0.17860631    (Valid_v3)



print(paste0("Commission Error (Forest class) = ",
             round((1 - scenario_AA$UA[2]), 3)))  

print(paste0("Commission Error (Non-Forest class) = ",
             round((1 - scenario_AA$UA[1]), 3)))        # 0.037 (about 3.7% of the area (or pixels) mapped as Non-forest is 
                                                        # estimated to actually be Forest.)
                                                        # 0.036   (Valid_v3)



# | Class      | User's Accuracy | Commission Error | Meaning                                              |
# | ---------- | --------------: | ---------------: | ---------------------------------------------------- |
# | Forest     |           82.0% |            18.0% | 18% of mapped Forest pixels are false positives      |
# | Non-forest |           96.3% |             3.7% | 3.7% of mapped Non-forest pixels are false positives |
  




## Producer's accuracy. Related to Omission Error (False Negatives)
scenario_AA$PA

#         0         1 
# 0.9137283   0.9181793 
# 0.9143668   0.9194979     (Valid_v3)


print(paste0("Ommission Error (Forest) = ",
             round((1 - scenario_AA$PA[2]), 3)))  # An estimated 8.2% of the pixels that are truly Forest were mapped as Non-forest.
                                                  #              8.1%   (Valid_v3)

print(paste0("Ommission Error (Non-Forest) = ",
             round((1 - scenario_AA$PA[1]), 3)))  # An estimated 8.6% of the pixels that are truly Non-forest were mapped as Forest.
#                                                                8.6%   (Valid_v3)


## Conclusion:
## The estimated omission error for the Forest class was 8.2% (Valid_v3: 8.1%), indicating that approximately 8.2% of the pixels that are truly 
## Forest were mapped as Non-forest. Conversely, the estimated commission error was 18.0% (Valid_v3: 17.9%), indicating that approximately 18.0% 
## of the pixels mapped as Forest are estimated to actually be Non-forest.

## Standard error of OA
scenario_AA$SEoa    # 0.002220113   (Valid_v3: 0.002267183)
round(((1.96*scenario_AA$SEoa)*100), 1)   # 0.4  (Valid_v3: 0.4)

## Standard error of UA
scenario_AA$SEua
#           0             1 
# 0.001892472   0.005269710 
# 0.001890410   0.005387148    (Valid_v3)


## standard error of PA
scenario_AA$SEpa
#           0           1 
# 0.002605602   0.004073868 
# 0.002690058   0.004066892    (Valid_v3)

## Proportion of area 
scenario_AA$area
#         0         1 
# 0.7001962 0.2998038 
# 0.7001341 0.2998659    (Valid_v3)

## standard error of area proportion
scenario_AA$SEa
#           0           1 
# 0.003158123   0.003158123 
# 0.003123383   0.003123383    (Valid_v3)


## Confusion error (area proportion). Rows and columns represent map and reference class labels, respectively
scenario_AA$matrix

#               0           1        sum
# 0    0.63978911  0.02453015  0.6643193
# 1    0.06040711  0.27527363  0.3356807
# sum  0.70019622  0.29980378  1.0000000
#
# 0    0.6401794   0.02413985  0.6643193      (Valid_v3)
# 1    0.0599547   0.27572604  0.3356807
# sum  0.7001341   0.29986588  1.0000000



## Confidence intervals:   CI95 = 1.96 × SE
z <- 1.96

CI95_UA <- z * scenario_AA$SEua * 100; CI95_UA
#          0           1 
#  0.3709245   1.0328633 
#  0.3705203   1.0558810      (Valid_v3)

CI95_PA <- z * scenario_AA$SEpa * 100; CI95_PA
#          0           1 
#  0.5106979   0.7984782 
#  0.5272513   0.7971108     (Valid_v3)

## Commission error (%)
commission <- (1 - scenario_AA$UA) * 100

## Omission error (%)
omission <- (1 - scenario_AA$PA) * 100



### Reporting table ####
v <- "GFC_v2_Valid_v3"
xlsx_fileName <- paste0("Table_ErrorMatrix_", v, ".xlsx")

report_error_matrix <- function(data,
                                map_col,
                                ref_col,
                                assessment,
                                output_dir,
                                sheet_name = NULL,
                                excel_file = xlsx_fileName) {
  
  ## Names

  dataset_name <- as.character(substitute(data))
  
  if (is.null(sheet_name)) {
    sheet_name <- paste0("Err_matr_", map_col, "_", dataset_name)
  }
  
  ## Raw confusion matrix

  raw <- addmargins(table(
    Map = factor(data[[map_col]],
                 levels = c(0, 1),
                 labels = c("Non-forest", "Forest")),
    Reference = factor(data[[ref_col]],
                       levels = c(0, 1),
                       labels = c("Non-forest", "Forest"))
  ))
  
  
  ## Cell proportions (%)
  
  #prop <- round(100 * raw / sum(raw[1:2, 1:2]), 1)
  prop <- raw
  prop_1 <- round(assessment$matrix, 3) * 100
  
  prop['Non-forest', 'Non-forest'] <- prop_1["0", "0"]
  prop['Non-forest', 'Forest'] <- prop_1["0", "1"]
  prop['Non-forest', 'Sum'] <- prop_1["0", "sum"]
  prop['Forest', 'Non-forest'] <- prop_1["1", "0"]
  prop['Forest', 'Forest'] <- prop_1["1", "1"]
  prop['Forest', 'Sum'] <- prop_1["1", "sum"]
  prop['Sum', 'Non-forest'] <- prop_1["sum", "0"]
  prop['Sum', 'Forest'] <- prop_1["sum", "1"]
  prop['Sum', 'Sum'] <- prop_1["sum", "sum"]
  
  

  ## Commission errors

  commission <- c(
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$UA["0"]) * 100,
            1.96 * assessment$SEua["0"] * 100),
    
    sprintf("%.1f (%.1f)",
            (1 - assessment$UA["1"]) * 100,
            1.96 * assessment$SEua["1"] * 100),
    
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
      "Forest",
      "Total",
      "Omission (CI95) [%]"
    ),
    
    Raw_NF    = c(raw[1,1], raw[2,1], raw[3,1], ""),
    Raw_F     = c(raw[1,2], raw[2,2], raw[3,2], ""),
    Raw_Total = c(raw[1,3], raw[2,3], raw[3,3], ""),
    
    Prop_NF    = c(prop[1,1], prop[2,1], prop[3,1], omission[1]),
    Prop_F     = c(prop[1,2], prop[2,2], prop[3,2], omission[2]),
    Prop_Total = c(prop[1,3], prop[2,3], prop[3,3], ""),
    
    commission_ci95 = commission,
    check.names = FALSE
    
  )
  
  report_table[4, "commission_ci95"] <- omission[4]
  
  names(report_table)[8] <- "Commission (CI95) [%]"
  

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
  
  saveWorkbook(
    wb,
    excel_path,
    overwrite = TRUE
  )
  
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
    colwidths = c(1, 3, 3, 1)
  )
  
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "header")
  ft <- bold(ft, j = 1, bold = TRUE, part = "body")
  ft <- align(ft, j = 2:8, align = "center", part = "body")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- autofit(ft)
  ft <- bold(ft, i = 4, j = 8, bold = TRUE, part = "body")
  

  ## PNG

  save_as_image(
    ft,
    path = file.path(output_dir,
                     paste0(sheet_name, ".png"))
  )
  

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
Valid_v2 <- map_ref_values   # change the name to write correct file/tab names
Valid_v3 <- map_ref_values   # change the name to write correct file/tab names

res <- report_error_matrix(
  data       = Valid_v3,             # change the name
  map_col    = "GFC_v2",             
  ref_col    = "forest_class_num",
  assessment = scenario_AA,
  output_dir = dir_2ndAssessment_GFC2020_v2_Valid_v3     # change the name
)

res$raw_table
res$table
res$flextable  ## Table 4 of the 2025 repoort





## MAPS ####

### Correctly and missclassified sample units (Figure 13) ####

head(scenario) ; nrow(scenario)
head(map_ref_values) ; nrow(map_ref_values)


map_ref_values <- map_ref_values %>%
  mutate(
    class = case_when(
      GFC_v2 == 0 & forest_class_num == 0 ~ "No forest",
      GFC_v2 == 1 & forest_class_num == 1 ~ "Forest",
      GFC_v2 == 1 & forest_class_num == 0 ~ "Commission error",
      GFC_v2 == 0 & forest_class_num == 1 ~ "Omission error"
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


world <- ne_countries(scale = "medium", returnclass = "sf")


# map
figure_title <- "GFC2020_v2 / Valid_v2"
figure_title <- "GFC2020_v2 / Valid_v3"

p <- ggplot() +
  geom_sf(data = world, fill = "white", colour = "black", linewidth = 0.2) +
  geom_sf(data = filter(pts, class == "No forest"), aes(colour = "No forest"),
    size = 0.10,
    alpha = 0.25,
    show.legend = TRUE) +
  geom_sf(data = filter(pts, class == "Forest"), aes(colour = "Forest"),
    size = 0.10,
    alpha = 0.25,
    show.legend = TRUE) +
  geom_sf(data = filter(pts, class == "Commission error"), aes(colour = "Commission error"),
    size = 1.20,
    alpha = 0.95,
    show.legend = TRUE) +
  geom_sf(data = filter(pts, class == "Omission error"), aes(colour = "Omission error"),
    size = 1.20,
    alpha = 0.95,
    show.legend = TRUE) +
  scale_colour_manual(values = c("No forest" = "grey70", "Forest" = "#33A02C", "Commission error" = "#FDBF00", "Omission error" = "#1F78B4"),
                      breaks = c("No forest", "Forest", "Commission error", "Omission error")) +
  guides(colour = guide_legend(override.aes = list(size = c(2, 2, 4, 4), alpha = 1))) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  ggtitle(figure_title) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.90, face = "bold", size = 12, margin = margin(b = 10)),
        plot.title.position = "plot", legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 11))

p

# save figure
Fig13_Filename <- "Fig_ErrorDistribution_GFC2020_v2_Valid_v2.png"
Fig13_Filename <- "Fig_ErrorDistribution_GFC2020_v2_Valid_v3.png"

Fig13_dir <- dir_2ndAssessment
Fig13_dir <- dir_2ndAssessment_GFC2020_v2_Valid_v3


ggsave(filename = file.path(Fig13_dir, Fig13_Filename), 
       plot = p, 
       width = 20, height = 12, units = "cm", dpi = 600, bg = "white")





## Sample units classified differently in GFC2020 V1 versus V2  --> a map (to be done!!)




## Forest area in GFC2020 (Table 6) ####

names(scenario_AA)
head(strata_area)


scenario_AA$area
# 0           1 
# 0.7001962   0.2998038 
# 0.7001341   0.2998659     (Valid_v3)

sum(scenario_AA$area)  # 1


total_area <- sum(strata_area$strata_ha)

forest_area <- scenario_AA$area["1"] * total_area
forest_area <- forest_area / 10^6    # Mha
forest_area   # 4021.075 Mha
              # 4021.908 Mha   (Valid_v3)

scenario_AA$SEa
forest_se   <- scenario_AA$SEa["1"] * total_area
forest_ci95 <- 1.96 * forest_se
forest_ci95 <- round((forest_ci95 / 10^6), 1)
forest_ci95  # 83
             # 82.1  (Valid_v3)

## In the repor



### Calculate total forest area (not adjusted) ####
#library(rgee)
#ee_install_upgrade()
#ee_Initialize()

## rgee doesn't work. We'll use 'reticulate'
library(reticulate)

# Use the dedicated Python environment
use_condaenv("rgee311", required = TRUE)

# Import the Earth Engine Python package
ee <- import("ee")

# Authenticate
ee$Authenticate()

# Initialize Earth Engine
ee$Initialize(project = "gfc2020-503311")


## GEE is initialised, now we can run the calculations

#gaul <- ee$FeatureCollection("FAO/GAUL/2015/level0")
#ee$data$getAsset("JRC/GFC2020/V2")   # image collection
#gfc <- ee$ImageCollection("JRC/GFC2020/V2")
#gfc$size()$getInfo()  # 1420 images (very likely one image per tile)
#gfc_img$bandNames()$getInfo()  # Each image has a single band called "Map". THerefore, first() would give us only the first tile

# The collection is organized as:
# Geographic tiles (N0_E0, N0_E10, N0_E100, ...)
# Each geographic tile is further split into four internal chunks (0000000000-0000000000, etc.)
# The tiles overlap, so mosaic() removes overlapping


## Forest area
## Clement's GEE script: https://code.earthengine.google.com/7cfb76f43e06c22856311f81662552da

run_this <- "no"

if(run_this == "yes"){
  
  py_run_string("
import ee

# ---------------------------------------------------------------------
# GFC v2
# ---------------------------------------------------------------------

# Mosaic the collection into a single image (remove overlapping)
GFCv2 = ee.ImageCollection('JRC/GFC2020/V2').mosaic()

# Spatial zones to iterate over
Zones = ee.FeatureCollection(
    'projects/ee-astridverhegghen/assets/EUFO/continents/gaul2015_GFC_1deg-final'
)

# ---------------------------------------------------------------------
# Forest layer
# ---------------------------------------------------------------------

# Keep only value 1 (forest), rename the band.
# ee.Image.cat() is kept for fidelity with the original script.
AllClasses = ee.Image.cat(
    GFCv2.eq(1).rename('Forest_v2')
)

# ---------------------------------------------------------------------
# Function applied to every feature
# ---------------------------------------------------------------------

def LOOPsamples(feature):

    vals = (
        AllClasses
        .multiply(ee.Image.pixelArea())
        .reduceRegion(
            reducer = ee.Reducer.sum(),
            geometry = feature.geometry(),
            scale = 10,
            maxPixels = 1e13
        )
    )

    return (
        ee.Feature(None, vals)
        .copyProperties(feature, feature.propertyNames())
    )

# Apply the function to every feature
LOOPresult2 = Zones.map(LOOPsamples)

# ---------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------

task = ee.batch.Export.table.toDrive(
    collection = LOOPresult2,
    description = 'JRC_GFC_v2_gaul2015_GFC_1deg-final',
    folder = 'EarthEngine',
    fileNamePrefix = 'JRC_GFC_v2_gaul2015_GFC_1deg-final',
    fileFormat = 'CSV'
)

task.start()

print('Task submitted.')
")

  
}





## Results after running Clement's GEE script (from this R session)
library(googledrive)

drive_ls("EarthEngine")
2
file <- drive_find("JRC_GFC_v2_gaul2015_GFC_1deg-final.csv")
drive_download(
  file,
  path = paste0(dir_2ndAssessment, "JRC_GFC_v2_gaul2015_GFC_1deg-final.csv"),
  overwrite = TRUE
)

areas <- read.csv(paste0(dir_2ndAssessment, "JRC_GFC_v2_gaul2015_GFC_1deg-final.csv"))
#areas <- read.csv(paste0(dir_2ndAssessment, "JRC_GFC_v2_gaul2015_GFC_1deg-final_old.csv"))
head(areas)
names(areas)
unique(areas$Forest_v2)

sum(is.na(areas$Forest_v2)) # 0 NA's


mapped_area <- round(sum(areas$Forest_v2)/10^10, 0)    # m2 to Mha
sum(areas$Forest_v2)/10^10    # m2 to Mha    # 4561.726       ; old script: 4561.726
sum(areas$Forest_v2)          # m2           # 4.561726e+13   ; old script: 4.561726e+13
format(sum(areas$Forest_v2), scientific = FALSE)  # 45617263160131  m2   ;  old script: 45617263160131

mapped_area  # this is exactly the number reported in the first assessment. (4562 Mha)






### Table 6 reproduction ####

fra_area <- 4058

report_table6 <- data.frame(
  Metric = "Forest area (Mha)",
  `GFC2020 V2` = sprintf("%.1f", mapped_area),
  `Reference set (95% CI)` = sprintf("%.1f (±%.1f)",
                                      forest_area,
                                      forest_ci95),
  `FAO-FRA 2020` = sprintf("%.0f", fra_area),
  check.names = FALSE
)

report_table6


ft <- flextable(report_table6)

ft <- theme_booktabs(ft)

ft <- bold(ft, part = "header")
ft <- bold(ft, j = 1)

ft <- align(ft, align = "center", part = "header")
ft <- align(ft, j = 1, align = "left", part = "body")
ft <- align(ft, j = 2:4, align = "right", part = "body")

ft <- border_outer(
  ft,
  border = fp_border(color = "black", width = 1)
)

ft <- border_inner_h(
  ft,
  border = fp_border(color = "grey70", width = 0.5)
)

ft <- border_inner_v(
  ft,
  border = fp_border(color = "grey70", width = 0.5)
)

ft <- bg(ft, bg = "white", part = "all")

ft <- autofit(ft)

ft


# to excel

wb <- createWorkbook()

addWorksheet(wb, "Forest_area_GFC_v2_Valid_v2")
addWorksheet(wb, "Forest_area_GFC_v2_Valid_v3")

writeData(wb,
          #sheet = "Forest_area_GFC_v2_Valid_v2",
          sheet = "Forest_area_GFC_v2_Valid_v3",
          report_table6)


v <- "GFC_v2_Valid_v3"
xlsx_fileName <- "Forest_area_report.xlsx"
xlsx_fileName <- paste0("Forest_area_report_", v, ".xlsx")

xlsx_dir <- dir_2ndAssessment
xlsx_dir <- dir_2ndAssessment_GFC2020_v2_Valid_v3

saveWorkbook(wb, file = file.path(xlsx_dir, xlsx_fileName), overwrite = TRUE)


# to png
png_fileName <- "Forest_area_GFC_v2_Valid_v2.png"
png_fileName <- paste0("Forest_area_", v, ".png")

save_as_image(ft, path = file.path(xlsx_dir, png_fileName))







##  Assessment of forest and land use types (Fig 18) --> bar charts and maps (to be done) ########

## Figure 18 shows the number of correctly or incorrectly classified sample units in GFC2020 V2 for each forest or 
## land use land type (columns “No trees or shrubs present” and “Primary or naturally regenerating forest” are 
## scaled to the second y-axis).


GFC_v2_gaul <- readxl::read_excel("/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first/GFC_v2_accuracy-assessment_gaul.xlsx", sheet = "5_combined")

GFC_v2_gaul %>% data.frame() %>% head()

head(GFC_v2_gaul)
names(GFC_v2_gaul)
nrow(GFC_v2_gaul)  # 21612, only those for the assessment


Valid_v3 <- read.csv(paste0(dir_Valid_v3, "Final_2026_GFC_Validation_Dataset.csv"))
head(Valid_v3)

Valid_v3 <- Valid_v3 %>% 
  filter(UsedIn_1stAssessment == "yes")  #%>% nrow()

nrow(Valid_v3)   # 21612
names(Valid_v3)
head(Valid_v3)


GFC_v2_Valid_v3 <- GFC_v2_gaul %>% 
  select(sample_id, GFC_v2) %>% 
  left_join(Valid_v3, by = c("sample_id" = "X2024_1st_sample_id")) #%>% 

head(GFC_v2_Valid_v3)
nrow(GFC_v2_Valid_v3)

sum(is.na(GFC_v2_Valid_v3$forest_class))   # 0
sum(is.na(GFC_v2_Valid_v3$type_class))   # 0
sort(unique(GFC_v2_Valid_v3$type_class))
sort(unique(GFC_v2_Valid_v3$GFC_v2))   # 0, 1



fig18 <- GFC_v2_Valid_v3 %>%
  mutate(
    Result = case_when(
      forest_class == "Forest"     & GFC_v2 == 1 ~ "Forest",       # FOR in Valid_v3 and FOR in GFC_v2
      forest_class == "Forest"     & GFC_v2 == 0 ~ "Forest mapped non-forest",
      forest_class == "Non-forest" & GFC_v2 == 0 ~ "Non-forest",
      forest_class == "Non-forest" & GFC_v2 == 1 ~ "Non-forest mapped forest"
    ),
    # Labels exactly as in the report
    Type = recode(
      type_class,   # Type class in Valid_v3
      "other wooded land"                  = "Other wooded land",
      "trees for agricultural use"         = "Trees for agricultural use",
      "trees in urban areas"               = "Trees in urban areas",
      "trees inside forest"                = "Trees inside forest",
      "trees outside forest"               = "Trees outside forest",
      "no trees or shrubs present"         = "No trees or shrubs present",
      "Naturally regenerating forest"      = "Primary or naturally regenerating forest",
      "Planted or plantation forest"       = "Planted or plantation forest"
    )
  ) 


fig18_sum <- fig18 %>%
  count(Type, Result)

fig18_sum


#defining the order:
fig18_sum$Type <- factor(
  fig18_sum$Type,
  levels = c(
    "Other wooded land",
    "Trees for agricultural use",
    "Trees in urban areas",
    "Trees inside forest",
    "Trees outside forest",
    "No trees or shrubs present",
    "Primary or naturally regenerating forest",
    "Planted or plantation forest"
  )
)

fig18_sum$Result <- factor(
  fig18_sum$Result,
  levels = c(
    "Non-forest",
    "Non-forest mapped forest",
    "Forest",
    "Forest mapped non-forest"
  )
)

fig18_sum
str(fig18_sum)

scale_factor <- 5

fig18_plot <- fig18_sum %>%
  mutate(
    n_plot = if_else(
      Type %in% c(
        "No trees or shrubs present",
        "Primary or naturally regenerating forest"
      ),
      n / scale_factor,
      n
    )
  )


figure_title <- "GFC2020_v2 / Valid_v2"
figure_title <- "GFC2020_v2 / Valid_v3"

p18 <- ggplot(fig18_plot, aes(x = Type, y = n_plot, fill = Result)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE))  +
  scale_fill_manual(
    values = c(
      "Non-forest"               = "#F57C00",
      "Non-forest mapped forest" = "#4F9DD9",
      "Forest"                   = "#66BB33",
      "Forest mapped non-forest" = "#F4C20D"
    ),
    breaks = c(
      "Non-forest",
      "Non-forest mapped forest",
      "Forest",
      "Forest mapped non-forest"
    )
  ) +
  scale_y_continuous(name = "Sample units", 
                     sec.axis = sec_axis(~ . * scale_factor, name = "Sample units")) +
  labs(x = NULL, y = "Sample units", fill = NULL) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 11),
    axis.text.y.right = element_text(colour = "red"),
    axis.title.y.right = element_text(colour = "red"),
    axis.ticks.y.right = element_line(colour = "red"),
    #axis.line.y.right = element_line(colour = "red")
  ) +
  ggtitle(figure_title) +
  geom_col(
    data = subset(
      fig18_plot,
      Type %in% c(
        "No trees or shrubs present",
        "Primary or naturally regenerating forest"
      )
    ),
    aes(y = n_plot),
    fill = NA,
    colour = "red",
    linewidth = 0.8,
    width = 0.75,
    position = position_stack(reverse = TRUE)
  )

p18

p18_v <- "GFC2020_v2_Valid_v2"
p18_v <- "GFC2020_v2_Valid_v3"

p18_filename <- paste0("Fig18_TypesBarPlot_", p18_v, ".png")

p18_dir <- paste0(dir_2ndAssessment, "GFC2020_v2_Valid_v3")

ggsave(
  filename = file.path(p18_dir, p18_filename),
  plot = p18,
  device = ragg::agg_png,
  width = 18,
  height = 15,
  units = "cm",
  dpi = 600,
  bg = "white"
)


## Plotting the results in a table

ft_18 <- fig18_sum %>%
  arrange(Type) %>%
  pivot_wider(
    names_from = Result,
    values_from = n,
    values_fill = 0
  )  %>% 
  flextable()

ft_18 <- theme_booktabs(ft_18)
ft_18 <- bold(ft_18, part = "header")
ft_18 <- bold(ft_18, j = 1)
ft_18 <- align(ft_18, align = "center", part = "header")
ft_18 <- align(ft_18, j = 2:5, align = "center", part = "body")
ft_18 <- border_outer(ft_18, border = fp_border(color = "black", width = 1))
ft_18 <- border_inner_h(ft_18, border = fp_border(color = "grey70", width = 0.5))
ft_18 <- border_inner_v(ft_18, border = fp_border(color = "grey70", width = 0.5))
ft_18 <- bg(ft_18, bg = "white", part = "all")
ft_18 <- autofit(ft_18)

ft_18

p18_dir

save_as_image(ft_18, path = file.path(paste0(p18_dir, "/Table_Figure18_", p18_v, ".png")))


#

fig18_sum_kk <- fig18_sum %>%
  arrange(Type) %>%
  pivot_wider(
    names_from = Result,
    values_from = n,
    values_fill = 0
  ) %>% as.data.frame()

apply(fig18_sum_kk[, 2:5], 2, sum)

#  Non-forest       Non-forest mapped forest        Forest        Forest mapped non-forest 
#  13710                     1244                     6097                      561 

#



## Regional Assessment ####
dir_assessment2024
scenario


dir_2ndAssessment

list.files(dir_assessment2024)
list.files("/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first")

GFC_v2_gaul <- readxl::read_excel("/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first/GFC_v2_accuracy-assessment_gaul.xlsx", sheet = "5_combined")

names(GFC_v2_gaul)
nrow(GFC_v2_gaul)  # 21612, only those for the assessment

sort(unique(GFC_v2_gaul$continent))
sort(unique(GFC_v2_gaul$continent_gaul))

#





## Agreement Validation dataset v2 and v3 ####

## Table 7. Overall agreement, underestimation and overestimation of forest in the first interpretation for assessment regions of the second interpretation and globally.



