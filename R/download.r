# --- Downloading Data ---

# Need to do this for anything which isn't availiable on fingertips through the
# R package. We might also be able to get some data through the NCDR/Cube/HES (a
# boy can dream) in time. But for now it's NHS Digital and thier hideously poor
# website navigation system until I can figure out whether their data access API
# is a myth or not

# Avoidable Admissions (CCG level)-----------------------------------------------------------------------------------

'https://digital.nhs.uk/data-and-information/publications/statistical/ccg-outcomes-indicator-set' %>%
  get_links(pat = 'outcomes-indicator-set\\/') %>%
  .[[1]] %>%
  get_links(pat = '3-1-') %>%
  get_links(pat = '\\.csv') %>%
  reg_dl(source_nm = 'av_adm', filename = 'av_adm.csv')


# Death in usual place of residence-----------------------------------------------------------------------------------
'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10517rollingannualdeathregistrationsbyplaceofoccurrenceenglandperiodendingquarter1apriltojuneoffinancialyear2019to2020/rollingannualq1201920.xls' %>%
  reg_dl(source_nm = 'death_upr', filename = 'death_upr.xls')

# Adults in contact with secondary MH services in employment

# 'https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-outcomes-framework-ascof/upcoming/measures-from-the-adult-social-care-outcomes-framework-england-2018-19' %>%
#   get_links(pat = '\\.csv') %>%
#   reg_dl('mh_emp', 'mh_emp.csv')

# Get fingertips data-----------------------------------------------------------------------------------


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


# Most measures availiable at a Local Authority District level but not all of
# them.

ft_data_lad <-
  fingertips_data(ft_inds, AreaTypeID = 101)

# The others come in at County and UA level or CCG which is a pain, need to match down
# to LAD before moving any further

ft_data_oth <-
  fingertips_data(ft_inds[!(ft_inds %in% unique(ft_data_lad$IndicatorID))], AreaTypeID = "All")

ft_data <- bind_rows(ft_data_lad, ft_data_oth)

write_csv(ft_data, path = file <- tempfile())

reg_dl(file, source_nm = 'fingertips', 'fingertips_data.csv')

# Get GP survey data-----------------------------------------------------------------------------------

reg_dl('gp_survey/gp_raw_full.csv', source_nm = 'gp_survey', 'gp_survey.csv')

## Getting turnover/ratio data-----------------------------------------------------------------------------------

# Primary Care-----------------------------------------------------------------------------------

# start at url
su <- 'https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services'

l <- 
  su %>%
  get_links(pat = 'data-and-information.*medical-services\\/.*20[0-9]{2}')

# store names of website sections in case we need to use later
l_names <- str_extract(l, '([0-9]{2,4}.*20[0-9]{2})|([a-z]{3,12}\\-20[0-9]{2})')

# get zip file links
zfs <- map(l, ~get_links(.x, pat = '\\.zip'))

# table of details (maybe we can remove?)
outs <-
  tibble(source = l, period = l_names, files = zfs) %>%
  # drop anything without file
  filter(!is.na(zfs)) %>%
  # some sources have multiple zipfiles so need to unnest
  unnest(files) %>%
  # pull out anything with '[Ii]ndividul' in title
  filter(!str_detect(files, '[Ii]ndividual'))

# download to temp location and return paths
zf_d <-
  outs %>%
  unname() %>%
  pmap_chr(function(s,n,z){
    # create a temporary file
    t <- tempfile(pattern = n, fileext = '.zip')
    # donwload using libcurl
    download.file(url = z, destfile = t, method = 'libcurl', mode = 'wb')
    return(t)
  })

# unzip zipped folders and drop files into folders (return contents of folders)
uz_d <-
  zf_d %>%
  map(function(x){
    f <- basename(x)
    dir <- paste(dirname(x), 'unzipped', sep = '/')
    if(!dir.exists(dir)){dir.create(dir)}
    unzip(x, exdir = dir)
  })

# search resulting folders for 'practice level.csv' files and flatten to simple chr vector
rel_csv <- 
  map(uz_d, function(x){x[str_detect(str_to_lower(basename(x)), 'practice level\\.csv')]}) %>%
  flatten_chr()

# define columns needed from file and regex for extracting period in format 'mmmm yyyy' (roughly)
rel_cols <- c('PRAC_CODE', 'PRAC_NAME', 'CCG_CODE', 'TOTAL_PATIENTS', 'TOTAL_GP_FTE', 'TOTAL_NURSES_FTE', 'TOTAL_DPC_FTE', 'TOTAL_ADMIN_FTE')
rx <- '[a-z]{3,15}?\\s20[0-9]{2}'


# Single testing example comment out
# dtest<-
#   rel_csv[[1]] %>%
#   read_csv() %>%
#   select(rel_cols) %>%
#   rename_all(str_to_lower) %>%
#   mutate(period = str_extract(str_to_lower(basename(rel_csv[[1]])), rx))

# read all files add period and append results to single file and reduce to those after 2015
f <-
  rel_csv %>%
  map(function(x){
    period <- str_extract(str_to_lower(basename(x)), rx)
    d <- read_csv(x, col_types = cols(.default = 'c'))
    cols <- intersect(colnames(d), rel_cols)
    d <-
      d %>%
      select(all_of(cols)) %>%
      rename_all(str_to_lower) %>%
      mutate(period = period)
    return(d)
  }) %>%
  reduce(bind_rows) %>%
  filter(str_extract(period, '20[0-9]{2}') %>% as.numeric > 2015)

#  At this point use write to tempfile then use reg_dl to 'download' the file
    # This bit of the process probably needs a review at some point as in
    # reality think we should reg_dl the zipfiles rather than treating them in
    # download

write_csv(f, t <- tempfile(fileext = '.csv'))
reg_dl(t, 'wfs_pc', 'wfs_pc.csv')

# Secondary Care-----------------------------------------------------------------------------------

# Turnover Data
'https://files.digital.nhs.uk/2F/202422/Annual%20turnover%20from%20organisation%20benchmarking%20tool%2C%20September%202019.xlsx' %>%
  reg_dl(source_nm = 'sc_tvr', filename = 'sc_tvr.xlsx')

# Staff Numbers
u <- 'https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics'

tmp_files <- 
  u %>%
  get_links_dev(name_pat = 'Stat') %>%
  .[[1, 'full_url']] %>%
  get_links_dev(name_pat = 'csv') %>%
  pmap(function(...){x <- tibble(...); download.file(x$full_url, destfile = tmp <- tempfile(fileext='.zip'), method='libcurl'); return(tmp)}) %>%
  map(function(x){unzip(x, exdir = tmp <- tempdir()); return(tmp)}) %>%
  map_chr(list.files, pattern = 'Staff Group and Org.*\\.csv', full.names = T) %>%
  unique()
  
reg_dl(u = tmp_files, source_nm = 'sc_ratio', filename = 'wf_sc_num.csv')

# Adult Social Care Workforce -----------------------------------------------------------------------------------

u <- 'https://digital.nhs.uk/data-and-information/publications/statistical/personal-social-services-staff-of-social-services-departments'
  
d <- 
  u %>%
  get_links_dev(name_pat = 'Staff of Social Service.*20[0-9]{2}') %>%
  purrr::pluck('full_url') %>%
  map_dfr(~get_links_dev(.x, url_pat = 'csv')) %>%
  filter(str_detect(full_url, '\\.xls|\\.csv|\\.zip'))

tdir <- tempdir()

walk2(d$text, d$full_url, function(x, u){
  ext <-str_match(u, '.*(\\..*$)') %>% .[,2]
  fn <- paste0('adult_ss_data_', str_extract(x, '20[0-9]{2}'), ext)
  path <- paste(tdir, fn, sep = '/')
  mode <- if(ext == '.csv'){'w'}else{'wb'}
  download.file(u, destfile = path, method = 'libcurl', mode = mode)
})

final <- paste(tdir, 'final', sep = '/')
if(!dir.exists(final)){dir.create(final)}



# unzip files
list.files(tdir, full.names = T, pattern = '\\.zip') %>%
  walk(function(x){
    period <- str_extract(x, '20[0-9]{2}')
    cf <- list.files(final, full.names = T) # get files currently in final
    files <- 
      unzip(x, list = T) %>%
      filter(str_detect(tolower(Name), 'council level|jobrole'))
    unzip(x, files = files$Name, exdir = final)
    nf <- setdiff(list.files(final, full.names = T), cf)
    for(f in nf){file.rename(f, paste0(final,'/adult_ss_data_', period, str_extract(f, '\\.?[a-z]*$')))}
  })

# deal with regular .csv and xlsx files in tdir
tmpfiles <- list.files(tdir, full.names = T, recursive = T, pattern = '20[0-9]{2}\\.[csv|xls]')

# test <- if(str_ends(tmpfiles[14], '.csv')){ read_csv(tmpfiles[1])}

tl <-
  tmpfiles %>%
  map(function(x){
    d <- if(str_ends(x, 'csv')){
      read_csv(x)
    } else if(str_ends(x, 'xlsx')){
      readxl::read_excel(x)  
    }
    return(d)
  })

names(tl) <- str_extract(tmpfiles, '20[0-9]{2}')

tc <- 
  tl %>%
  map2_dfr(names(.), function(data, name){
    fcols <- c('geo_code', 'job_role_group', 'value', 'period')
    if(name %in% c(2013,2014)){
      tcols <- c('Council', paste0('JR', 28:32, 'TOTAL'))
      rcols <- c('Council', 'All job roles', 'Direct Care', 'Manager/Supervisor', 'Professional', 'Other')
      d <- select_at(data, tcols)
      colnames(d) <- rcols
      d <-
        d %>%
        pivot_longer(rcols[2:length(rcols)], names_to = 'Job_Role_Group', values_to = 'Value') %>%
        mutate(period = name)
    } else if(name == 2015){
      d <- 
        data %>%
        group_by(Council, Job_Role_Group) %>%
        summarise('Value' = sum(Value, na.rm = T)) %>%
        mutate(period = name)
    } else if (name == 2016){
      d <-
        data %>%
        filter(`Description - level 1` == 'Job role Group' & `Geographical level` == 'CASSR') %>%
        group_by(`Geography`, `Description - level 2`) %>%
        summarise('Value' = sum(as.numeric(str_remove_all(Value, '[:punct:]')))) %>%
        mutate(period = name)
      
    } else if (name %in% c(2017,2018)){
      d <-
        data %>%
        filter(!is.na(GEOGRAPHY_CODE), METRIC == 'Employees by Job Role Group') %>%
        group_by(GEOGRAPHY_CODE, FIELD_1_DESCRIPTION) %>%
        summarise('Value' = sum(as.numeric(COUNT_SUPPRESSED), na.rm = T)) %>%
        mutate('period' = name)
    } else if(name == 2019){
      d <-
        data %>%
        filter(!is.na(GEOGRAPHY_CODE), METRIC == 'Employees by Job Role Group') %>%
        group_by(GEOGRAPHY_CODE, VALUE_DESCRIPTION) %>%
        summarise('Value' = sum(as.numeric(COUNT), na.rm = T)) %>%
        mutate('period' = name)
    } else {d <- NA}
    
    colnames(d) <- fcols
    d <- d %>% ungroup () %>% mutate(geo_code = as.character(geo_code))
    return(d)
  })

all <- 
  tc %>%
  mutate(type = case_when(str_detect(geo_code, '-') ~ 'CASSR - name',
                          str_detect(geo_code, '^E[0-9]') ~ 'UTLA',
                          str_detect(geo_code, '^[0-9]{3}') ~ 'CASSR',
                          T ~ 'Other'),
         code = case_when(str_detect(type, 'CASSR') ~ str_extract(geo_code, '[0-9]{3}'),
                          type == 'UTLA' ~ geo_code,
                          T ~ NA_character_),
         temp_name = if_else(str_detect(type, 'name'), str_extract(geo_code, '(?<=\\s\\-\\s).*'), NA_character_)
  )

# try to get new codes from old ones
# can't find a lookup table so having to do this by name
# There are some iddues here but probably fixable ones (e.g. hull, iow, etc...)
# Will need to strip out key words such as 'city of' or 'borough' or 'council' and punct from both sides and drop case
cassr_name <- all %>% filter(type == 'CASSR - name')
cassr_only <- 
  all %>% 
  filter(type == 'CASSR') %>%
  select(-temp_name) %>%
  left_join(cassr_name %>% group_by(code, temp_name) %>% summarise,
            by = 'code')

names_fix <- read_csv('config/cassr_name_issues.csv')

cassr_fix <-
  union_all(cassr_name, cassr_only) %>%
  mutate(temp_name = if_else(temp_name %in% names_fix$temp_name,
                             names_fix[[str_which(names_fix$temp_name, 'Durham'),'new_name']],
                             temp_name)) %>%
  left_join(lookup %>% group_by (utla_cd, utla_nm) %>% summarise,
            by = c('temp_name' = 'utla_nm')) %>%
  transmute(
    utla_cd = utla_cd,
    job_role_group = tolower(job_role_group),
    job_role_group = case_when(str_detect(job_role_group, 'direct') ~ 'direct_care',
                               str_detect(job_role_group, 'profe')  ~ 'regulated_professional',
                               str_detect(job_role_group, 'manager') ~ 'manager_supervisor',
                               str_detect(job_role_group, 'all') ~ 'all_employees',
                               str_detect(job_role_group, 'social') ~ 'social_worker',
                               str_detect(job_role_group, 'oth') ~ 'other',
                               T ~ NA_character_),
    period = as.integer(period),
    value = as.integer(value)
  )

utla_data <-
  all %>%
  filter(type == 'UTLA') %>%
  transmute(
    utla_cd = geo_code,
    job_role_group = tolower(job_role_group),
    job_role_group = case_when(str_detect(job_role_group, 'direct') ~ 'direct_care',
                               str_detect(job_role_group, 'profe')  ~ 'regulated_professional',
                               str_detect(job_role_group, 'manager') ~ 'manager_supervisor',
                               str_detect(job_role_group, 'oth') ~ 'other',
                               str_detect(job_role_group, 'all') ~ 'all_employees',
                               str_detect(job_role_group, 'social') ~ 'social_worker',
                               str_detect(job_role_group, 'occ') ~ 'occupational_therapist',
                               T ~ NA_character_),
    period = as.integer(period),
    value = as.integer(value)
  )

asc_data <- 
  union_all(utla_data, cassr_fix) %>%
  left_join(lookup %>% group_by(utla_cd, utla_nm) %>% summarise, by = 'utla_cd') %>%
  arrange(utla_cd, job_role_group, period)

tf <- tempfile(fileext = '.csv')
write_csv(asc_data, tf)

reg_dl(tf, source_nm = 'wfs_asc', filename = 'wfs_asc.csv')

# Children's Social Care -----------------------------------------------------------------------------------

u <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/781857/Children_s_social_work_workforce_2018_underlying_data.zip'
download.file(u, destfile = t <- tempfile(fileext = '.zip'), method = 'libcurl')
unzip(t, exdir = td <- paste(dirname(t), str_extract(basename(t), '.*(?=\\.)'), sep = '/'))
file <- list.files(td, full.names = T, pattern = 'turnover.*\\.csv')
reg_dl(file, source_nm = 'wfs_csc', filename = 'wfs_csc.csv')
