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
