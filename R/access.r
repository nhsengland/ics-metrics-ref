#############################################
# Name:       access.r
# Written By: Ewan Wakeman <ewan.wakeman@nhs.net>
# Date:       2021-02-18
# Description: Functions for accessing data from source
#############################################

library(tidyverse)
library(rvest)
library(fingertipsR)

source('./R/constants.r')

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

# create_registry
# creates a small csv file at a specified location to hold information about data downloaded

#' @param location     directory folder where registry file will be created, by default set to './raw_data'
#' @param overwrite   if set to TRUE will overwrite registry file at the specified location, by default set to FALSE

create_registry <- function(location=registry_dir, overwrite=F){
  
  if(!file.exists(location)){
    dir.create(location, recursive = T)
  }
  
  filename <- file.path(location, 'registry.csv')
  
  if(!file.exists(filename) | overwrite){
    tibble(source_name = character(),
           source_loc = character(),
           path = character(),
           rel_path = character(),
           retrieved_on = character()
    ) %>% write_csv(filename)
  }
  return(filename)
}

#add some notes
reg_dl <- function(u, source_nm, filename, method = 'libcurl'){
  dest_loc <- paste0('raw_data/', filename)
  if(file.exists(dest_loc)){
    if(!dir.exists('raw_data/archive')){
      dir.create('raw_data/archive')
    }
    lm <- file.mtime(dest_loc) %>% format('%y%m%d-%H%M%S')
    file.copy(dest_loc, paste0('raw_data/archive/', lm, '_', filename))
  }
  tryCatch(expr = {
    if(str_detect(u, 'http')){
      download.file(u, dest_loc, mode = 'wb', method = method)
    } else {
      file.copy(u, dest_loc, overwrite = TRUE)
    }
  },
  error = function(e){message('download failed'); message(e); break}
  )
  if(!file.exists(file.path(registry_dir,"registry.csv"))){
    create_registry()
  }
  dl_reg <-
    read_csv(file.path(registry_dir,"registry.csv"), col_types = cols(.default = 'c', retrieved_on = 'D'))
  dl_reg <-
    dl_reg %>%
    add_case(
      source_name = source_nm,
      source_loc = u,
      path = paste(getwd(), dest_loc, sep = '/'),
      rel_path = dest_loc,
      retrieved_on = as.Date(Sys.Date())
    )
  assign('dl_reg', dl_reg, envir = globalenv())
  write_csv(dl_reg, file.path(registry_dir,"registry.csv"), na = '')
  return(dest_loc)
}


# get fingertips data for relevant system metrics; by LAD where avaliable or lower geography level if not avaliable 
get_fingertips_data <- function(ids, area_types = fingertips_lad_codes$AreaTypeID){
    # define a list of ids with indicator codes as names
    # match system metrics input ids to their respective fingertips ids
    ft_inds <- c(
      hle_65 = 93505,
      hle_0 = 90362,
      cvmcp = 40402,
      phys_act = 93014,
      obs_y6 = 90323,
      smk_ad = 92443,
      of_rbl = 90585,
      ef_rbl = 90584,
      cpa_emp = 90420
    )
        
    # try and get indicator data for ids and area types provided; for most recent and complete data (= 101)
    ft_data_lad <-
    fingertips_data(ft_inds, AreaTypeID = 101)

    # for any indicator ids we can't get at the required area_type level get indicator data at whatever area type it is availible at
    ft_data_oth <-
    fingertips_data(ft_inds[!(ft_inds %in% unique(ft_data_lad$IndicatorID))], AreaTypeID = "All")
    
    # use bind_rows() to append the two tables together into one
    ft_data <- bind_rows(ft_data_lad, ft_data_oth)

    # write the raw data to a csv file somewhere in the ics-metrics-ref folder structure
    write_csv(ft_data, path = file <- tempfile())

    # make a record of when we got it, where from and where it is on our local machine  
    #reg_dl not working, do i need another package?
    
    reg_dl(file, source_nm = 'fingertips', 'fingertips_data.csv')
    
    # where it is now
    return(file) 
}


get_viewpoint_data <- function(ids){
    
}

get_gp_survey_data <- function(ids){

}

get_links <- function(url, pattern='*'){

}

get_nhsd_data <- function(ids){

}
