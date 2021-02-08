# Name:         compile.r
# Author:       Ewan Wakeman (NHS England and NHS Improvement)
# Description:  Compiles datasets for Integrated Care Systems Metrics


# *** Setup *** -----------------------------------------------------------

# load packages

pk <-
  c('rvest',
    'tidyverse',
    'fingertipsR',
    'lubridate',
    'odbc')

unpack <- function(x) {
  if (!(x %in% installed.packages())) {
    install.packages(x, character.only = TRUE)
  }
  require(x, character.only = TRUE)
}

sapply(pk, unpack)

# source configuration

list.files('config', full.names = TRUE, pattern = '\\.[rR]') %>%
  walk(source)

# clean the workspace
rm(pk)

# file output format

#   stp_code
#   stp_name
#   area_code
#   area_name
#   area_type
#   ind_id
#   ind_code
#   ind_name
#   period_start
#   period_end
#   period_text
#   value
#   value_type

# create a raw data folder to save sourcing everything every single time

if (!dir.exists('raw_data')) {
  dir.create('raw_data')
}

# create a file to keep a log of what we download and when
if (file.exists('raw_data/dl_registry.csv')) {
  dl_reg <- read_csv('raw_data/dl_registry.csv')
} else {
  dl_reg <-
    tibble(
      source_name = character(),
      source_loc = character(),
      path = character(),
      rel_path = character(),
      retrieved_on = date()
    ) %>%
    write_csv('raw_data/dl_registry.csv')
}

# rm(dl_reg)

# Downloading Data ----------------------------------------------------------

force_dl <- F


# moved to download.r for tidyness (need to fix the download.r file so it can
# run independently - not just from this script). Breaking this out to reduce
# length of this script and also prevent always downloading new data every time
# the compile script is run (useful for when troubleshooting/developing or when
# only one or two files need updating. As a defualt we won't fetch new data if
# the the most recently fetched raw data is less than a week old.

if (max(dl_reg$retrieved_on) < Sys.Date() - 7 | force_dl) {
  source('download.r')
}

# Getting meta-data -----------------------------------------------------------

# We need some meta-data so we can try to get all the different sources of data
# back to a common unit. Most data is availiable at a Local Authority District
# level and so we'll need to convert CCG and County & UA level data into LAD.
# This will mean some "best-fit" type matching as well as some duplication.

# These don't take too long to download but again, no point fetching again and
# again as they don't change so fetch and save them if needed, else just load
# them from local source.

# Changing LADs in 2018/19 caused some issues when mapping data to different
# geographies, we'll work around this using 2018 and 2019 definitions together
# for now.

update_lookup <- FALSE

if (!file.exists('config/lookup.csv') | update_lookup) {
  ## 2019 LSOA >> LAD/CCG >> STP/ICS
  l_19 <-
    'https://opendata.arcgis.com/datasets/520e9cd294c84dfaaf97cc91494237ac_0.csv' %>%
    read_csv()
  
  ## 2018 LSOA >> LAD/CCG >> STP/ICS
  l_18 <-
    'https://opendata.arcgis.com/datasets/f0095af162f749ad8231e6226e1b7e30_0.csv' %>%
    read_csv()
  
  ## 2019 LAD >> UTLA
  
  lad_utla <-
    'https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv' %>%
    read_csv(
      col_names = c('lad_cd', 'lad_nm', 'utla_cd', 'utla_nm', 'fid'),
      skip = 1
    ) %>%
    select(lad_cd, utla_cd, utla_nm)
  
  l_18$LSOA11CD %in% l_19$LSOA11CD %>% reduce(sum) / nrow(l_19)
  
  # Dealing with discrepancies in Geography 
  
  # Some data is at CCG level and other data is at LAD level. Whilst CCG level
  # data has the advantage of fitting neatly into ICSs they're also pretty large
  # with 191 of them across England as opposed to 317 LADs. Most of our data comes
  # from the ONS and as such is produced at a LAD level which is more beneficial
  # for understaning an area than CCG.
  
  lg <-
    l_19 %>%
    select(-FID) %>%
    rename_all( ~
                  str_replace_all(.x, '[0-9]{2}', '_') %>%
                  str_to_lower()) %>%
    left_join(
      y =
        l_18 %>%
        select_if(str_ends(names(.), 'CD')) %>%
        rename_all( ~
                      str_replace_all(.x, '11', '_') %>%
                      str_to_lower()),
      by = 'lsoa_cd'
    ) %>%
    left_join(y = lad_utla,
              by = 'lad_cd') %>%
    group_by_if(!str_starts(names(.), 'lsoa')) %>%
    summarise(n_lsoa = n()) %>%
    ungroup() %>%
    # add a unique id for each combination of geographies
    mutate(x_id = sprintf('x_%03d', row_number()))
  
  # Write the table
  
  write_csv(lg, 'config/lookup.csv', na = '')
  
  # Remove files
  rm(list = c('l_18', 'l_19', 'lad_utla', 'lg'))
}

lookup <- read_csv('config/lookup.csv')
rm(update_lookup)

# Getting all of the data clean

src <-
  dl_reg %>%
  group_by(source_name) %>%
  filter(retrieved_on == max(retrieved_on)) %>%
  ungroup()

# crete a directory to write the treated data to (to save storing everything in
# memory)

dir <- 'cleaned_data'
if (!dir.exists(dir)) {
  dir.create(dir)
}

# clean the workspace
rm(list = setdiff(ls(), c(
  'dl_reg', 'src', 'lookup', 'dir', 'date_range'
)))

# create the cleaned data column spec
cs <-
  c(
    'area_code',
    'ind_id',
    'ind_code',
    'ind_name',
    'period_start',
    'period_end',
    'period_text',
    'val'
  )


# Avoidable Admissions ----------------------------------------------------

# get the file
f <- src[src[, 'source_name'] == 'av_adm',]

d <-
  f[1, 'rel_path'][[1]] %>%
  # read it
  read_csv() %>%
  # for now we'll just take "Person" and leave the two sexes sets of data alone
  # (come back for this later though?)
  filter(Gender == 'Person') %>%
  rename(ccg_cd = `ONS code`,
         val = `Indicator Value`,
         period = `Period of Coverage`) %>%
  rowwise() %>%
  transmute(
    area_code = ccg_cd,
    ind_id = 'sys_001',
    ind_code = 'av_adm',
    ind_name = 'Emergency admissions for acute conditions that should not usually require hospital admission (rate per 100,000)',
    period_start =
      str_extract_all(period, '([0-9]{2}\\/){2}[0-9]{4}', simplify = TRUE) %>%
      .[[1, 1]] %>%
      as.Date(format = '%d/%m/%Y'),
    period_end =
      str_extract_all(period, '([0-9]{2}\\/){2}[0-9]{4}', simplify = TRUE) %>%
      .[[1, 2]] %>%
      as.Date(format = '%d/%m/%Y'),
    period_text = paste(
      format(period_start, '%d-%b-%y'),
      format(period_end, '%d-%b-%y'),
      sep = ' - '
    ),
    val = val
  ) %>%
  ungroup()

write_csv(d,
          path = paste0(dir, '/', f$source_name[[1]], '.csv'),
          na = '')


# Death in usual place of residence ---------------------------------------

# This is a horrible xl file which needs some real tidying

f <- src[src[, 'source_name'] == 'death_upr',]

d <-
  f[1, 'rel_path'][[1]] %>%
  # read it
  readxl::read_excel(sheet = 'Usual residence')

# get the index and text in row 2 from the excel file, where it exists, this
# relates to the reporting period.
index <- which(str_detect(as.character(d[2, ]), '[0-9]{4}'))
period <- as.character(d[2, ])[index]

d <- d %>% filter(str_starts(...2, 'E0[6-9]{1}'))

dc <- tibble(
  'area_code' = character(),
  'area_name' = character(),
  'period' = character(),
  'all_deaths' = character(),
  'death_ur' = character(),
  'ind' = character(),
  'low_ci' = character(),
  'upp_ci' = character()
)

# fetch the data
for (i in seq_along(index)) {
  rd <- d[, c(2:3, index[i]:(index[i] + 4))] %>% as_tibble()
  colnames(rd) <-
    c('area_code',
      'area_name',
      'all_deaths',
      'death_ur',
      'ind',
      'low_ci',
      'upp_ci')
  rd$period <- period[[i]]
  dc <- bind_rows(dc, rd)
}

d <-
  dc %>%
  transmute(
    'area_code' = area_code,
    'ind_id' = 'sys_005',
    'ind_code' = 'death_upr',
    'ind_name' = 'Percentage of deaths in usual place of residence',
    'period' = str_extract(period, '[0-9]{4}\\/[0-9]{2}\\sQ[1-4]{1}.*'),
    'period_start' =
      str_extract_all(period, '[0-9]{4}(.*?)Q[1-4]', simplify = TRUE) %>% .[[1, 1]] %>%
      glue::glue(
        .envir = .,
        case_when(
          str_sub(., start = -2) == 'Q4' ~ as.numeric(str_sub(., end = 4)) + 1,
          TRUE ~ as.numeric(str_sub(., end = 4))
        ),
        case_when(
          str_sub(., start = -2) == 'Q1' ~ 4,
          str_sub(., start = -2) == 'Q2' ~ 7,
          str_sub(., start = -2) == 'Q3' ~ 10,
          str_sub(., start = -2) == 'Q4' ~ 1,
          TRUE ~ NA_real_
        ),
        1,
        .sep = '-'
      ) %>%
      as.Date(format = '%Y-%m-%d'),
    'period_end' = period_start + months(3) - days(1),
    'period_text' = paste(
      format(period_start, '%d-%b-%y'),
      format(period_end, '%d-%b-%y'),
      sep = ' - '
    ),
    'all_deaths' = as.numeric(all_deaths),
    'death_ur' = as.numeric(death_ur),
    'val' = as.numeric(ind),
    'low_ci' = as.numeric(low_ci),
    'upp_ci' = as.numeric(upp_ci)
  ) %>%
  select(cs)

write_csv(d,
          path = paste0(dir, '/', f$source_name[[1]], '.csv'),
          na = '')


# Mental Health and Employment --------------------------------------------



f <- src[src[, 'source_name'] == 'mh_emp',]

d <-
  f[1, 'rel_path'][[1]] %>%
  # read it
  read_csv(col_types = cols(.default = 'c')) %>%
  rename_all( ~ str_replace_all(.x, '\\s', '_') %>% str_to_lower()) %>%
  filter(
    disaggregation_level == 'Total',
    geographical_level == 'Council',
    measure_group == '1F',
    measure_type == 'Outcome'
  ) %>%
  transmute(
    'area_code' = ons_area_code,
    'ind_id' = 'sys_005',
    'ind_code' = 'death_upr',
    'ind_name' = 'Percentage of deaths in usual place of residence',
    'period_start' = as.Date('2018-04-01'),
    'period_end' = as.Date('2019-03-31'),
    'period_text' = paste(
      format(period_start, '%d-%b-%y'),
      format(period_end, '%d-%b-%y'),
      sep = ' - '
    ),
    'val' = as.numeric(measure_value) / 100,
  )

write_csv(d,
          path = paste0(dir, '/', f$source_name[[1]], '.csv'),
          na = '')


# Fingertips Data --------------------------------------------


f <- src[src[, 'source_name'] == 'fingertips',]

d <-
  f[1, 'rel_path'][[1]] %>%
  # read it
  read_csv(guess_max = 40000) %>%
  filter(str_detect(AreaType, 'UA|CCG')) %>%
  filter((
    IndicatorID %in% c(40402, 90323, 92443, 93014, 90584, 90585, 90420) &
      Sex == 'Persons' & Age != '18-64 yrs'
  )
  |
    ((IndicatorID) %in% c(90362, 93505))) %>%
  transmute(
     'area_code' = AreaCode,
    'ind_id' = str_replace_all(
      IndicatorID,
      c(
        '93505' = 'care_001',
        '40402' = 'care_002',
        '93014' = 'care_004',
        '90323' = 'care_005',
        '92443' = 'care_006',
        '90585' = 'sys_003',
        '90584' = 'sys_004',
        '90362' = 'care_001b'
      )
    ),
    'ind_code' = str_replace_all(
      IndicatorID,
      c(
        '93505' = 'hle_65',
        '40402' = 'cvmcp',
        '93014' = 'phys_act',
        '90323' = 'obs_y6',
        '92443' = 'smk_ad',
        '90585' = 'rc_rbm',
        '90584' = 'ef_rbm',
        '90362' = 'hle_0',
        '90420' = 'cpa_emp'
      )
    ),
    'ind_name' = IndicatorName,
    'period_start' = case_when(
      str_detect(Timeperiod, '\\/') ~ paste0(str_extract(Timeperiod, '20[0-9]{2}'),
                                             '0401') %>% as.Date(format = '%Y%m%d'),
      TRUE ~ paste0(str_extract(Timeperiod, '20[0-9]{2}'),
                    '0101') %>% as.Date(format = '%Y%m%d')
    ),
    'period_end' = case_when(
      str_detect(Timeperiod, '\\/') ~ paste0(
        str_sub(Timeperiod, end = 2),
        str_sub(Timeperiod, start = -2),
        '0331'
      ) %>% as.Date(format = '%Y%m%d'),
      TRUE ~ paste0(
        str_sub(Timeperiod, end = 2),
        str_sub(Timeperiod, start = -2),
        '1231'
      ) %>% as.Date(format = '%Y%m%d')
    ),
    'period_text' = paste(
      format(period_start, '%d-%b-%y'),
      format(period_end, '%d-%b-%y'),
      sep = ' - '
    ),
    'val' = Value
  )

write_csv(d, path = paste0(dir, '/fingertips.csv'), na = '')

# GP Survey data --------------------------------------------

f <- src[src[, 'source_name'] == 'gp_survey',]

d <-
  f[1, 'rel_path'][[1]] %>%
  # read it
  read_csv(guess_max = 20000) %>%
  transmute(
    area_code = lad_code,
    ind_id = case_when(q == 14 ~ 'meta_001',
                       q == 18 ~ 'meta_002',
                       q == 67 ~ 'meta_003'),
    ind_code =  case_when(q == 14 ~ 'gp_wk_appt',
                          q == 18 ~ 'gp_exp_gd',
                          q == 67 ~ 'gp_cls_ae'),
    ind_name = case_when(is.na(str_extract(dnm_text, '.*(?<=\\s-)')) ~ paste(dnm_text, num_text, sep = ' - '),
                         T ~ paste(str_extract(dnm_text, '.*(?<=\\s-)'), num_text)
    ),
    period_start = as.Date(paste(period, 01, 01, sep ='-')),
    period_end = as.Date(paste(period, 03, 31, sep = '-')),
    period_text = paste('Jan', period, '- March', period),
    val = num /dnm
  )

write_csv(d, path = paste0(dir, '/gp_survey.csv'), na = '')




# Primary Care Workforce Data ---------------------------------------------

f <- src[src[, 'source_name'] == 'wfs_pc',]
d <- read_csv(f['rel_path'][[1]])

# Get practice level geographical information from NCDR and clean up a bit

con <-dbConnect(odbc(), dsn = 'NCDR', database = 'NHSE_Reference')
sql_q <- read_lines('gp_survey/all_gp_by_lad.sql') %>% paste(collapse = ' ') %>% str_remove_all('\\t')
res <- dbGetQuery(conn = con, statement = sql_q)
res <-
  res %>% 
  as_tibble() %>% 
  # taking "first availiable lad" as some belong to multiple in data (why?)
  group_by(practice_code) %>% 
  summarise(lad_code = first(lad_code))

# Get populations from ONS, something weird with reg patients
lsoa_lookup <- read_csv('config/lsoa_lookup.csv')

pop <-
  read_csv('config/lsoa_pop_est.csv') %>%
  left_join(x = lsoa_lookup,
            y = transmute(., LSOA11CD = area_codes, pop = all_ages),
            by = 'LSOA11CD'
  ) %>%
  group_by(LAD19CD) %>%
  summarise(pop = sum(pop, na.rm = T))

# Join sql result to table and group to lad level



d <- 
  d %>%
  mutate_if(str_detect(colnames(.), 'total_'), as.numeric) %>%
  pivot_longer(cols = -c(prac_code, prac_name, ccg_code, period)) %>%
  group_by(prac_code, period, name) %>%
  summarise(value = mean(value, na.rm = T), .groups = 'drop') %>%
  filter(!is.nan(value)) %>%
  pivot_wider(id_cols = c('prac_code', 'period'), names_from = name, values_from = value) %>%
  # join to practice /lad table
  inner_join(res, by = c('prac_code' = 'practice_code')) %>%
  #group to practice at each period
  group_by(lad_code, period) %>%
  # summarise and create grouped columns
  summarise(
    total_pat = sum(as.numeric(total_patients), na.rm = T),
    total_gp_fte = sum(as.numeric(total_gp_fte), na.rm = T),
    total_nurses_fte = sum(as.numeric(total_nurses_fte), na.rm = T),
    total_dpc_fte = sum(as.numeric(total_dpc_fte), na.rm = T),
    total_admin_fte = sum(as.numeric(total_admin_fte), na.rm = T)
  ) %>%
  ungroup() %>%
  # fix period to be actual date
  left_join(pop, by = c('lad_code' = 'LAD19CD')) %>%
  mutate(gp_ratio = total_gp_fte / (pop/1e5),
         nurse_ratio = total_nurses_fte / (pop/1e5),
         dpc_ratio = total_dpc_fte / (pop/1e5),
         admin_ratio = total_admin_fte / (pop/1e5),
         period = parse_date(str_to_sentence(period), format = "%B %Y")
  ) %>%
  # select only required columns
  select(lad_code, period, gp_ratio, nurse_ratio, dpc_ratio, admin_ratio, total_pat) %>%
  # gather measures /values in key-value pairs (long format)
  gather(key = 'measure', value = 'value', -c(lad_code, period)) %>%
  mutate(measure = paste0('pc_', measure))

final <-
  d %>%
  inner_join(y = select(lookup, stp_cd, stp_nm, lad_cd, lad_nm),
                        by = c('lad_code' = 'lad_cd')) %>%
  transmute(
    area_code = lad_code,
    ind_id = case_when(measure == 'pc_gp_ratio' ~ 'pcr_001',
                       measure == 'pc_nurse_ratio' ~ 'pcr_002',
                       measure == 'pc_dpc_ratio' ~ 'pcr_003',
                       measure == 'pc_admin_ratio' ~ 'pcr_004',
                       measure == 'pc_total_pat' ~ 'pcr_005',
                       T ~ NA_character_),
    ind_code = measure,
    ind_name = case_when(measure == 'pc_gp_ratio' ~ 'Number of GPs per 100,000 patients (registered)',
                      measure == 'pc_nurse_ratio' ~ 'Number of Primary Care Nurses per 100,000 patients (registered)',
                      measure == 'pc_dpc_ratio' ~ 'Number of Primary Care Direct Patient Contact roles per 100,000 patients (registered)',
                      measure == 'pc_admin_ratio' ~ 'Number of Primary Care Admin staff per 100,000 patients (registered)',
                      measure == 'pc_total_pat' ~ 'Number of Primary Care Registered Patients',
                      T ~ NA_character_),
    period_start = period,
    period_end = period,
    period_text = format(period, '%b %y'),
    val = value
  )

write_csv(final, 'cleaned_data/wfs_pc.csv')


# Secondary Care Workforce Data --------------------------------------------
f <- src[src[, 'source_name'] == 'sc_tvr',]

d <- 
  readxl::read_excel(f$rel_path, sheet = 'Source data, FTE') %>%
  mutate(
    period_start = case_when(
      # if find 'to' then covers a range
      str_detect(Period, 'to') ~
        # take start from 'ymd to ymd'
        str_extract(Period, '.*(?=\\sto)') %>%
        # format as date (1st month)
        paste('01') %>% as.Date(format = '%Y%m%d') %>%
        # add a month and subtract day (for end of month)
        ceiling_date('month') - days(1),
      # if it had length 5 char then is Excel style numeric date (starting at 1899-12-30 for some reason)
      nchar(Period) == 5 ~ as.Date(as.numeric(Period), origin = '1899-12-30'),
      # throw errors on anything else (there shouldn't be any)
      T ~ as.Date(NA)
    ),
    # take end from 'ymd to ymd' (rinse and repeat)
    period_end   = case_when(
      str_detect(Period, 'to') ~
        str_extract(Period, '(?<=to\\s).*') %>%
        paste('01') %>% as.Date(format = '%Y%m%d') %>%
        ceiling_date('month') - days(1),
      nchar(Period) == 5 ~ as.Date(as.numeric(Period), origin = '1899-12-30'),
      T ~ as.Date(NA)
    )
  ) %>%
  rename_all(.funs = ~str_replace_all(.x, '\\s', '_') %>% str_to_lower())

num <- d %>% filter(type %in% c('Joiner', 'Leaver'))
dnm <- 
  d %>% filter(type == 'Denom') %>%
  transmute(
    org_code = org_code,
    dnm_period = period_start,
    dnm = fte
  )

f <-
  num %>%
  # spread laever/joiner
  spread(type,fte, fill = 0) %>%
  # drop to lowercase column names because_snake_case
  rename_all(str_to_lower) %>%
  # join dnms back on and ignore the fact that this will generate matches for every period combo for now
  left_join(
    y = dnm,
    by = 'org_code'
  ) %>%
  # add a column to measure diff between period_start and dnm_period
  mutate(
    p_diff = sqrt(as.numeric(period_start - dnm_period)^2) # normalise
  ) %>%
  # group on org_code and period and filter to most appropriate timeframe
  group_by(org_code, period_start) %>%
  filter(p_diff == min(p_diff)) %>%
  mutate('nrow' = n()) %>%
  # ungroup and calculate metrics
  ungroup() %>%
  mutate(
    turnover = leaver / dnm,
    growth = joiner - leaver,
    growth_factor = growth / dnm
  )

con <- dbConnect(drv = odbc(), dsn = 'NCDR', database = 'NHSE_Reference')

s <- 
  read_lines('config/all_sec_by_lsoa_lad.sql', skip_empty_rows = T) %>%
  paste(collapse = '')

lkp <- dbGetQuery(con, s)

if(!file.exists('config/lsoa_lookup.csv')){download.file(url = 'https://opendata.arcgis.com/datasets/520e9cd294c84dfaaf97cc91494237ac_0.csv', destfile = 'config/lsoa_lookup.csv')}

# get population estimates for each LAD
pop <-
  read_csv('config/lsoa_pop_est.csv') %>%
  left_join(x = lsoa_lookup,
            y = transmute(., LSOA11CD = area_codes, pop = all_ages),
            by = 'LSOA11CD'
  ) %>%
  group_by(STP19CD) %>%
  summarise(pop = sum(pop, na.rm = T))


final <- 
  f %>% 
  filter((!cluster_group %in% c('Ambulance', 'Clinical Commissioning Group'))) %>%
  left_join(lkp, by = c('org_code' = 'prov_code')) %>%
  left_join(lookup, by = 'lad_cd') %>%
  group_by(stp_cd, stp_nm, period_start, period_end, cluster_group) %>%
  summarise(
    n_staff = sum(dnm, na.rm = T),
    leavers = sum(leaver, na.rm = T),
    joiners = sum(joiner, na.rm = T),
    org_count = n()
  ) %>%
  ungroup() %>%
  left_join(pop, by = c('stp_cd' = 'STP19CD')) %>%
  mutate(
    scs_turnover = leavers /n_staff,
    scs_growth = (joiners - leavers) / n_staff,
    scs_ratio = n_staff / (pop/1e5)
  ) %>%
  select(stp_cd, period_start, cluster_group, scs_turnover, scs_growth, scs_ratio) %>%
  gather('measure', 'value', -c('stp_cd', 'period_start', 'cluster_group')) %>%
  transmute(
    area_code = stp_cd,
    ind_id = paste0('scw_0',
                   case_when(measure == 'scs_turnover' ~ '1',
                       measure == 'scs_growth' ~ '1',
                       measure == 'scs_ratio' ~ '3',
                       T ~ NA_character_),
                   case_when(cluster_group == "Clinical Commissioning Group" ~ '1', 
                             cluster_group == "Community Provider Trust" ~ '2',
                             cluster_group == "Acute" ~ '3',                      
                             cluster_group == "Mental Health" ~ '4',
                             cluster_group == "Ambulance" ~ '5')
    ),
    ind_code = paste(measure,case_when(cluster_group == "Clinical Commissioning Group" ~ 'ccg', 
                                       cluster_group == "Community Provider Trust" ~ 'comm',
                                       cluster_group == "Acute" ~ 'acute',                     
                                       cluster_group == "Mental Health" ~ 'mh',
                                       cluster_group == "Ambulance" ~ 'amb'),
                     sep = '_'),
    ind_name = paste(cluster_group,
                     case_when(measure == 'scs_turnover' ~ 'Staff Turnover (Total Leavers / Total Workforce at start of period)',
                               measure == 'scs_growth' ~ 'Staff Growth ((Total Starters - Total Leavers) / Total Workforce at start of period)',
                               measure == 'scs_ratio' ~ 'Staff per 100,000 people (System Population (2018))',
                              T ~ NA_character_)
    ),
    period_start = period_start,
    period_end = period_start,
    period_text = format(period_start, '%b %y'),
    val = value
  )
  
write_csv(final, 'cleaned_data/wfs_sc.csv', na = '')

# Secondary Care Staff Numbers ------------
f <- src[src[, 'source_name'] == 'sc_ratio',]
 
# read the raw data
d <-
  f$rel_path[[1]] %>%
  read_csv() %>%
  # clear spaces and drop to lowercase
  rename_all(~str_replace_all(.x, '\\s', '_') %>% str_to_lower())

# read grouping metadata
scs_groups <- read_csv('config/scs_groups.csv')


d <- 
  d %>%
  # Only FTE
  filter(data_type == 'FTE') %>%
  # get lad codes for each provider
  left_join(lkp, by = c('org_code' = 'prov_code')) %>% # these tables borrowed from SCS turnover section
  # get stp codes for each lad
  left_join(lookup, by = 'lad_cd') %>% # this too
  # add grouping configurations
  left_join(scs_groups, by = c('staff_group' = 'grade')) %>%
  # group to stp and create stp summaries
  group_by(stp_cd, stp_nm, date, group) %>%
  summarise(fte = sum(total, na.rm = T),
            includes = list(org_name)) %>%
  ungroup() %>%
  # joing pop estimates for each_stp
  left_join(pop, by = c('stp_cd' = 'STP19CD')) %>% # and this
  # take out NA groups (likely totals etc...)
  filter(!is.na(group)) %>%
  # create final table definition
  transmute(
    area_code = stp_cd,
    ind_id = case_when(group == 'Doctors' ~ 'scr_001',
                       group == 'Admin' ~ 'scr_005',
                       group == 'Ambulance Staff' ~ 'scr_003',
                       group == 'Clinical Other' ~ 'scr_006',
                       group == 'Midwives' ~ 'scr_004',
                       group == 'Nurses' ~ 'scr_002',
                       T ~ 'scr_007'),
    ind_code = case_when(group == 'Doctors' ~ 'sc_doc_ratio',
                         group == 'Admin' ~ 'sc_adm_ratio',
                         group == 'Ambulance Staff' ~ 'sc_amb_ratio',
                         group == 'Clinical Other' ~ 'sc_clin_ratio',
                         group == 'Midwives' ~ 'sc_mwv_ratio',
                         group == 'Nurses' ~ 'sc_nurse_ratio',
                         T ~ 'sc_other_ratio'),
    ind_name = case_when(ind_code != 'N/A' ~ paste('Number of secondary care', group, 'per 100,000 people'),
                         T ~ 'Number of secondary care other staff per 100,000 people'),
    period_start = date,
    period_end = date,
    period_text = format(date, '%b %y'),
    # doing per 100,000 people but consider 1,000,000 for sc
    #fte = fte,
    #pop = pop,
    val = fte / (pop / 1e5)
  )

write_csv(d, 'cleaned_data/sc_ratios.csv', na = '')

# Adult Social Care Workforce Data --------------------------------------------

f <- src[src[, 'source_name'] == 'wfs_asc',]

d <- read_csv(f$rel_path)

pop <-
  read_csv('config/lsoa_pop_est.csv') %>%
  left_join(lsoa_lookup, by = c('area_codes' = 'LSOA11CD')) %>%
  left_join(lookup %>% group_by(lad_cd, utla_cd) %>% summarise, by = c('LAD19CD' = 'lad_cd')) %>%
  group_by(utla_cd) %>%
  summarise(pop = sum(all_ages))

d <- 
  d %>%
  left_join(pop, by = 'utla_cd') %>%
  transmute(
    area_code = utla_cd,
    ind_id = case_when(job_role_group == 'all_employees' ~ 'asc_001',
                       job_role_group == 'direct_care' ~ 'asc_002',
                       job_role_group == 'manager_supervisor' ~ 'asc_003',
                       job_role_group == 'social_worker' ~ 'asc_004',
                       job_role_group == 'regulated_professional' ~ 'asc_005',
                       job_role_group == 'occupational_therapist' ~ 'asc_006',
                       T ~ 'asc_other'),
    ind_code = case_when(job_role_group == 'all_employeer' ~ 'asc_all_ratio',
                         job_role_group == 'direct_care' ~ 'asc_directcare_ratio',
                         job_role_group == 'manager_supervisor' ~ 'asc_manager_ratio',
                         job_role_group == 'social_worker' ~ 'asc_socialworker_ratio',
                         job_role_group == 'regulated_professional' ~ 'asc_regprof_ratio',
                         job_role_group == 'occupational_therapist' ~ 'asc_occther_ratio',
                         T ~ 'asc_other_ratio'),
    ind_name = case_when(ind_code != 'N/A' ~ paste('Number of adult social care employees in', str_replace_all(job_role_group, '_', ' '), 'roles per 100,000 people'),
                         T ~ 'Number of secondary care other staff per 100,000 people'),
    period_start = as.Date(paste0(period, '-01-01')),
    period_end = as.Date(paste0(period, '-12-31')),
    period_text = as.character(format(period_start, '%b-%Y')),
    # doing per 100,000 people but consider 1,000,000 for sc
    emp = value,
    pop = pop,
    val = value / (pop / 1e5)
  )

write_csv(d, 'cleaned_data/asc_ratios.csv', na = '')  

# Fixing Geographies ----------------------------------------------------------

# clean the workspace 
rm(list = setdiff(ls(), c('lookup', 'dl_reg', 'date_range')))

# Create a long (gathered/melted/unpivoted) version of the lookup table listing
# the combination id ('x_id') the ons_code ('code') and the types that
# corresponds to ('types')

l_melt <-
  lookup %>%
  select_if(str_ends(names(.), 'cd|id')) %>%
  gather(key = 'type',
         value = 'code',-x_id) %>%
  group_by(x_id, code) %>%
  summarise(types = list(type))

# Read in the cleaned data and join to get x_id
d <-
  map_dfr(list.files('cleaned_data', full.names = TRUE), read_csv) %>%
  left_join(y = l_melt,
            by = c('area_code' = 'code'))

# col spec

cs <-
  c(
    'stp_code',
    'stp_name',
    'area_code',
    'area_name',
    'area_type',
    'ind_id',
    'ind_code',
    'ind_name',
    'period_start',
    'period_end',
    'period_text',
    'val',
    'val_type',
    'is_ics'
  )

d %>%
  filter(is.na(x_id)) %>%
  mutate(types = paste(types)) %>%
  write_csv('output/codes_with_issues.csv', na = '')

f <-
  d %>%
  filter(!is.na(x_id) & !is.na(val)) %>%
  inner_join(y = rename(lookup, 'stp_code' = stp_cd, 'stp_name' = stp_nm), 'x_id') %>%
  mutate(area_code = lad_cd,
         area_name = lad_nm,
         area_type = 'lad') %>%
  group_by(.dots = intersect(colnames(.), cs[1:11])) %>%
  summarise(
    actuals = list(val),
    weights = list(n_lsoa),
    val = weighted.mean(val, n_lsoa),
    uw_mean = mean(val),
    val_type = if_else(n() > 1, 'd', 'a')
  ) %>%
  ungroup()


ics_codes <- c(
  "E54000005",
  "E54000007",
  "E54000009",
  "E54000014",
  "E54000023",
  "E54000024",
  "E54000030",
  "E54000034",
  "E54000035",
  "E54000041",
  "E54000043",
  "E54000044",
  "E54000048",
  "E54000049"
)

f <-
  f %>%
  mutate(is_ics = stp_code %in% ics_codes) %>%
  transmute(
    'stp_code' = stp_code,
    'stp_name' = stp_name,
    'area_code' = area_code,
    'area_name' = area_name,
    'area_type' = area_type,
    'ind_id' = ind_id,
    'ind_code' = ind_code,
    'ind_name' = ind_name,
    'period_start' = period_start,
    'period_end' = period_end,
    'period_text' = period_text,
    'val' = val,
    'val_type' = val_type,
    'is_ics' = is_ics,
    'actuals' = paste(actuals) %>% str_remove('c'),
    'weights' = paste(weights) %>% str_remove('c')
  )

write_csv(f, path = 'output/final_data_lad.csv')

f_latest <-
  f %>%
  group_by(ind_code) %>%
  filter(period_start == max(period_start, na.rm = TRUE)) %>%
  ungroup()

write_csv(f_latest, path = 'output/final_data_latest_lad.csv', na = '')

rm(list = ls())
