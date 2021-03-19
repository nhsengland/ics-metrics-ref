#############################################
# Name:       access.r
# Written By: Ewan Wakeman <ewan.wakeman@nhs.net>
# Date:       2021-02-18
# Description: Functions for accessing data from source
#############################################

library(tidyverse)
library(rvest)
library(fingertipsR)

source(file.path('R/constants.r'),  local=F)

# get_links
# retrieves data from links

#' @param u     this is the base url to be searched
#' @param pat   the pattern to be searched (e.g. '.csv' or '.xl')
#' @param node  the css/xml node to search (defaults to 'a' for links)
#' @param attr  the css/xml attribute to be returned (default to 'href' for links)

get_links <-
  function(u, url_pat = '.*', name_pat = '.*', node = 'a', attr = 'all', ignore_case = T){
    
    dmn <- str_extract(u, 'http[s]\\:[\\/]{2}(.*?)(?=\\/)')
    
    h <- read_html(u)
    
    n <- html_nodes(h, node)
    
    t <- html_text(n) %>% str_squish %>% tibble(text = .)
    
    attrs <- 
      html_attrs(n) %>% 
      map_dfr(~t(.x) %>% as_tibble) %>% 
      mutate(full_url = case_when(str_detect(href, 'http') ~ href,
                                  !str_starts(href, '\\/') ~ paste(dmn, href, sep = '/'),
                                  T ~ paste0(dmn, href)))
    
    d <- 
      bind_cols(t, attrs) %>%
      filter(
        str_detect(text, regex(name_pat, ignore_case = ignore_case)),
        str_detect(href, regex(url_pat, ignore_case = ignore_case))
      )
    
    return(d)
  }

get_fingertips_data <- function(ids, area_types = fingertips_lad_codes$AreaTypeID){
    # define a list of ids with indicator codes as names
    # match our input ids to their respective fingertips ids
    
    # try and get indicator data for ids and area types provided

    # for any indicator ids we can't get at the required area_type level get indicator data at whatever area type it is availible at

    # use bind_rows() to append the two tables together into one

    # write the raw data to a csv file somewher in the ics-metrics-ref folder structure

    # make a record of when we got it, where from and where it is on our local machine   

    return() # where it is now
}

get_viewpoint_data <- function(ids){
    
}

get_gp_survey_data <- function(ids){

}

get_nhsd_data <- function(ids){

}

#' get_ons_boundaries
#' fetch boundaries for a given level of geography
#' 
#' @param geography specify a level of geography to return
#' 
get_ons_boundaries <- function(geography, overwrite = FALSE, detail = 0){

  implemented <- list(
    'lsoa' = list('ug'  = NULL,
                  'sg'  = 'https://opendata.arcgis.com/datasets/e9d10c36ebed4ff3865c4389c2c98827_0.geojson',
                  'g'   = 'https://opendata.arcgis.com/datasets/8bbadffa6ddc493a94078c195a1e293b_0.geojson',
                  'f'   = 'https://opendata.arcgis.com/datasets/1f23484eafea45f98485ef816e4fee2d_0.geojson'
    )
  )
  
  return_first <- function(x,i=1){
    if(is.null(x[[i]])){
      return(return_first(x, i+1))
    } else{
        return(x[[i]])
    }
  }

  if(is.character(detail)){
    detail <-
      case_when(detail == 'ug' ~ 1,
                detail == 'sg' ~ 2,
                detail == 'g'  ~ 3,
                detail == 'f'  ~ 4,
                TRUE           ~ 0
    )
  }

  if(detail <= len(implemented[[geography]])){
    url <- return_first(implemented[[geography]], detail)
  } else {
    url <- implemented[[geography]][[len(implemented[[geography]])]]
  }

  raw_data_file <- file.path(raw_data_dir, paste0(geography, '_boundaries.geojson'))
  
  if ( !file.exists(raw_data_file) | overwrite ){
    download.file(url, raw_data_file, method = 'libcurl')
  }
  
  sf <- st_read(raw_data_file)
  
  return(sf)
}


# [TODO] : Change this to get_ons_mapping and take "lsoa" as a parameter
#' get_lsoa_mapping 
#' fetch lsoa to stp mapping
#' 
#' @param year      One of 'latest' or a given year (>=2018) to retreve mapping for
#' @param overwrite Will use cached data in './raw_data' unless set to TRUE (defualt is FALSE)
#' 

get_lsoa_mapping <- function(year = 'latest', overwrite = FALSE){
  urls <- list(
    '2020' = 'https://opendata.arcgis.com/datasets/1631beea57ff4e9fb90d75f9c764ce26_0.csv'
    )
  if ( year == 'latest' ){
    year_ <- max(names(urls) %>% as.numeric) %>% as.character
  } else {
    year_<- as.character(year)
  }
  
  url <- urls[[year_]]
  raw_data_file <- file.path(raw_data_dir, 'lsoa_lookup.csv')
  if ( !file.exists(raw_data_file) | overwrite ){
    tmpfile <- tempfile(fileext = '.csv')
    download.file(url, tmpfile, method = 'libcurl')
    
    df <- read_csv(tmpfile)
    
    df_ <- df %>%
        rename_all(~str_to_lower(.x) %>% str_replace_all('[:digit:]{2}', '_'))
    
    write_csv(df_, raw_data_file)
  }
  
  df <- read_csv(raw_data_file)
  
  return(df)
}

#' get_imd_data
#' fetch latest imd data and combine with relevant geography for use at STP level
#' 
#' @param domains     One of 'all' to fetch all IMD domains as per 
#' `./markdown/demography.md` or one/a list of the domains to be returned
#' @param overwrite   Will use cached data in './raw_data' unless set to 
#'   `TRUE` (defualt is FALSE)
#' @param enrich      If set to `TRUE` enriches imd data with additional 
#' geocodes for CCG, STP and LAD (defualt is set to TRUE)
#' @param geo_bounds  If set to `TRUE` adds geographical boundary data 
#' for all lsaos and returns a tibble suitable for mapping 
#' (defualt is set to FALSE) (requires package sf)
#' 

get_imd_data <- function(domains = 'all', 
                         overwrite = FALSE, 
                         enrich = TRUE,
                         geo_bounds = FALSE){
  prefix <- 'https://opendatacommunities.org/downloads/cube-table'
  uri <- 'http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices'
  url <- paste0(prefix,'?','uri=',uri)
  raw_data_file <- file.path(raw_data_dir, 'imd.csv')
  
  if((!file.exists(raw_data_file)) | overwrite ){
    tmpfile <- tempfile(fileext = '.csv')
    download.file(url, tmpfile, method = 'libcurl')
    
    df <- read_csv(tmpfile)
    
    df_ <- df %>%
      rename_all(~str_to_lower(.x) %>% str_replace_all('\\s', '_')) %>%
      transmute(
        lsoa_cd  = featurecode,
        domain = str_remove(indices_of_deprivation, '[:alpha:]\\.\\s'),
        year  = datecode,
        mtype = measurement,
        value = value
      )
    
    write_csv(df_, raw_data_file)
  }

  df <- read_csv(raw_data_file)
  if ( domains != 'all' ){
    message('filtering not yet implemented, returning all files')
  }
  
  if ( enrich ){
    lsoa_mapping <- get_lsoa_mapping(overwrite)
    df <- df %>%
      left_join(lsoa_mapping %>% select(-c(fid, cal_cd,cal_nm)),
                by = 'lsoa_cd')
  }

  if ( geo_bounds ){
    lsoa_geo <- get_ons_boundaries('lsoa', overwrite) %>% 
      rename_all(~str_replace_all(.x, '[:digit:]{2}', '_') %>% str_to_lower) %>%
      select(c(lsoa_cd,long,lat))
    df <- df %>%
      left_join(lsoa_geo,
                by = 'lsoa_cd')
  }
  
  return(df)
}