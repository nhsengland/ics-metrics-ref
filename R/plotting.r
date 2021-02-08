#############################################
# Name:       plotting.r
# Written By: Ewan Wakeman <ewan.wakeman@nhs.net>
# Date:       2020-12-10
#############################################
labs <- "{format(period_start, '%b-%y')} to {format(period_end, '%b-%y')}"

time_series_plot <- function(code,name,d){
  p_start <-  d %>% pluck('period_start') %>% min
  p_end <- if(sum(!is.na(d['period_end'])) == 0){d %>% pluck('period_start') %>% max}else{d %>% pluck('period_end') %>% max}
  p <- 
    ggplot(d, aes(x = if_else(is.na(period_end), glue("{format(period_start, '%b-%y')}"), glue(labs)), 
                  y = mean, ymin = mean - se, ymax = mean + se,
                  group = 'all')) +
    geom_ribbon(colour = '#ffffff50', fill = thm$nhse_colours$light_blue, alpha = .3) +
    geom_line(colour= thm$nhse_colours$dark_blue, size = 1) +
    geom_point(shape = 'x', size = 4, colour = thm$nhse_colours$dark_blue) +
    labs(
      title = str_wrap(name, 80),
      x = 'timeframe',
      y = 'mean (std. err)'
    ) +
    guides('x' = guide_axis(n.dodge = 2))
  return(p)
}

# Primary / Secondary Care Plot
pc_sc_plot <- function(data){
  p <- 
    data %>%
    ungroup() %>%
    filter(str_detect(ind_code, 'ratio') & str_detect(ind_code, 'gp_|doc_|nurse_|direct') & year(period_start) > 2010) %>%
    mutate(year = year(period_start)) %>%
    group_by(ind_code, ind_name,
             year) %>%
    summarise(
      'count' = n(),
      'mean' = mean(val, na.rm =T),
      'sd' = sd(val, na.rm = T),
      'se' = sd/sqrt(count),
      .groups = 'drop'
    ) %>%
    mutate(dn = case_when(str_detect(ind_code, 'nurse') ~ 'Nurse', 
                          str_detect(ind_code, 'gp|doc') ~ 'Doctor',
                          T ~ 'Direct Care'),
           ps = case_when(str_detect(ind_code, 'pc') ~ 'Primary Care', 
                          str_detect(ind_code, 'asc') ~ 'Social Care',
                          T ~ 'Secondary Care')) %>%
    ggplot(aes(x = year, y = mean,
               colour =dn, fill = dn,
               shape = ps, linetype = ps)) +
    geom_ribbon(aes(ymin = mean-(sd*2),ymax=mean+(sd*2)), alpha = .1, colour = '#ffffff30') +
    geom_ribbon(aes(ymin = mean-sd,ymax=mean+sd), alpha = .1, colour = '#ffffff30') +
    geom_line() +
    geom_point(colour = thm$nhse_colours$dark_grey, size = 3) +
    facet_grid(~dn) +
    scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020)) +
    scale_colour_manual(values = c(thm$nhse_colours$dark_blue, thm$nhse_colours$light_blue, thm$nhse_colours$dark_pink), 
                        aesthetics = c('colour','fill')) + 
    labs(
      title = 'Doctors/Nurses/Direct Care Staff per 100,000 residents',
      subtitle = 'Primary and Secondary Care over time',
      colour = 'Staff Type',
      fill = 'Staff Type',
      shape = 'Setting',
      linetype = 'Setting',
      x = 'Period',
      y = 'Mean (+ Std. Devs)'
    )
    theme(
      legend.position = 'bottom'
    )
  return(p)
}
