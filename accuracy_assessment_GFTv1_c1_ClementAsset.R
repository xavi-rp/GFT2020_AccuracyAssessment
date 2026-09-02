
#______________________________________________________________________________________________________________________#
#______________________________________________________________________________________________________________________#
#________                   Global Forest Type (GFT) Map 2020 v1 Accuracy Assessment using                         ____#
#________                         the 1st validation dataset (Collection1 -c1-)                                    ____#
#________                         Checking inaccuracies with the GEE extraction                                    ____#
#________                                                                                                          ____#
#________  Script based on the script used for the GFC 1st assessment, and shared in:                              ____#
#________  https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FOREST/EUFO/GFC2020_validation/                        ____#
#________  And its revised version in:                                                                             ____#
#________  https://github.com/xavi-rp/GFC2020_2ndAccuracyAssessment/blob/main/accuracy_assessment_GFCV2_xavi.R     ____#
#________                                                                                                          ____#
#______________________________________________________________________________________________________________________#
#______________________________________________________________________________________________________________________#

## The main objective of this script is to check the inaccuracies when using the Google GFT2020v1 asset compared with 
## Clement's final dataset, which is 'mosaiced' before the extraction



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

dir_assessment2024 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_first/GFC2020_validation/"

dir_Valid_c2 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_Second/geowiki_2026/"

dir_GFTv1_assessment_c2 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_GFT/GFTv1_assessment_c2/"
dir_GFTv1_assessment_c1 <- "/Users/xavi_rp/Documents/JRC_D1/AccuracyAssessment_GFT/GFTv1_assessment_c1/"




## Loading datasets ####

Valid_c2 <- read.csv(paste0(dir_Valid_c2, "Final_2026_GFC_Validation_Dataset.csv"))
names(Valid_c2)
head(Valid_c2)
nrow(Valid_c2) # 21752


valid_all <- read.csv(paste0(dir_Valid_c2, "AllValidations_2024_2026.csv"))
names(valid_all)

valid_c1 <- valid_all %>% 
  select(location_id, X2024_1st_sample_id, X2024_1st_pixel_center_x, X2024_1st_pixel_center_y,
         X2024_final_forest_class, X2024_final_type_class, UsedIn_1stAssessment) %>% #head()
  filter(UsedIn_1stAssessment == "Yes") %>% #nrow()  # 21612
  left_join(Valid_c2 %>% 
    select(location_id, "strata", "gaul", "continent_gaul")
  ) %>% 
  rename(forest_class = X2024_final_forest_class,
         type_class = X2024_final_type_class)
  
head(valid_c1)

table(valid_c1$forest_class)
#     Forest   Non-forest 
#       6593        15019




## Merging GFT v1 data  ####

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

## Using Google stored GFT2020v1
Valid_c2_GFT2020_V1 <- read.csv(paste0(dir_GFTv1_assessment_c2, "Valid_c2_coords_GFT2020_V1.csv"))

## Using Clement's final version (before sending to Google). 
## This is 'mosaiced', and the argument 'tileScale16' included when extracting using the sampleRegions() function
Valid_c2_GFT2020_V1_Clement_tileScale <- read.csv(paste0(dir_GFTv1_assessment_c1, "ClementAssset/Valid_c2_coords_GFT2020_V1_ClementAsset_tileScale16.csv"))
# This gives differences in the GFT values extracted

## This one uses Clement's final version (before sending to Google), 'mosaiced', but not the argument 'tileScale16' 
Valid_c2_GFT2020_V1_ClementAsset <- read.csv(paste0(dir_GFTv1_assessment_c1, "ClementAssset/Valid_c2_coords_GFT2020_V1_ClementAsset.csv"))


## This one uses Google stored GFT2020v1, and the argument 'tileScale16'  
Valid_c2_GFT2020_V1_tileScale16 <- read.csv(paste0(dir_GFTv1_assessment_c1, "ClementAssset/Valid_c2_coords_GFT2020_V1_tileScale16.csv"))




Valid_c2_GFT2020_V1 <- Valid_c2_GFT2020_V1 %>% 
  rename(GFT2020_V1 = Map) #%>% head()

head(Valid_c2_GFT2020_V1)
head(valid_c1)

Valid_c1_GFT2020_V1 <- Valid_c2_GFT2020_V1 %>% 
  select(GFT2020_V1, location_id) %>% 
  right_join(valid_c1, by = "location_id") #%>% #nrow() #21612

head(Valid_c1_GFT2020_V1)
table(Valid_c1_GFT2020_V1$GFT2020_V1)  # 1: Naturally regenerating forest; 10: Primary forest; 20: Planted/plantation forest
#     1     10     20 
#  3571   2527    627  (Google stored GFT2020v1)
#  3565   2519    629  (ClementAsset + mosaic() + tileScale = 16)
#  3565   2519    629  (ClementAsset + mosaic())
#  3571   2527    627  (Google stored GFT2020v1 + tileScale = 16)


Valid_c2_GFT2020_V1_ClementAsset <- Valid_c2_GFT2020_V1_ClementAsset %>% 
  rename(GFT2020_V1_Clem = Map) #%>% head()

head(Valid_c2_GFT2020_V1_ClementAsset)
head(valid_c1)

Valid_c1_GFT2020_V1_ClementAsset <- Valid_c2_GFT2020_V1_ClementAsset %>% 
  select(GFT2020_V1_Clem, location_id) %>% 
  right_join(valid_c1, by = "location_id") #%>% #nrow() #21612

head(Valid_c1_GFT2020_V1_ClementAsset)
table(Valid_c1_GFT2020_V1_ClementAsset$GFT2020_V1_Clem)
#    1    10   20 
# 3565  2519  629 


names(Valid_c1_GFT2020_V1_ClementAsset)
names(Valid_c1_GFT2020_V1)


Valid_c1_GFT2020_V1_diff <- Valid_c1_GFT2020_V1 %>% 
  select(location_id, X2024_1st_pixel_center_x, X2024_1st_pixel_center_y, GFT2020_V1) %>% #head()
  left_join(Valid_c1_GFT2020_V1_ClementAsset %>% select(location_id, GFT2020_V1_Clem), by = "location_id") #%>% #

head(Valid_c1_GFT2020_V1_diff)
nrow(Valid_c1_GFT2020_V1_diff)
  
rows_diff <- Valid_c1_GFT2020_V1_diff %>% 
  filter(GFT2020_V1 != GFT2020_V1_Clem |
           xor(is.na(GFT2020_V1), is.na(GFT2020_V1_Clem))) #%>%    # This is to keep rows when one is NA
  
nrow(rows_diff) # 73
head(rows_diff)


##__  We decide to go on with the official GEE data asset  __##





