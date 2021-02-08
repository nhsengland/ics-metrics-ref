#############################################
# Name:       metadata.r
# Written By: Ewan Wakeman <ewan.wakeman@nhs.net>
# Date:       2021-01-18
#############################################

require('readr')

get_indicators = function(return_type = "list") {
    
    return_types <- c("list", "tibble")
    if(!(return_type %in% return_types)){
        stop("return_type must be one of \"list\" or \"tibble\"")
    }

    inds <- read_csv("./data/system_metrics_indicator_list.csv", col_types = cols())

    if(return_type == "list"){
        vec <- inds[["ind_code"]]
        return(vec)
    } else {
        return(inds)
    }
}
