#############################################
# Name:       constants.r
# Written By: Ewan Wakeman <ewan.wakeman@nhs.net>
# Date:       2021-02-18
#############################################
library(fingertipsR)
library(tidyverse)

data_dir <- './data'

fingertips_lad_codes <- 
    fingertipsR::area_types() %>% 
    filter(str_detect(AreaTypeName, 'Lower tier local')) %>% 
    group_by(AreaTypeID, AreaTypeName) %>% 
    summarise()