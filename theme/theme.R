## Themes for plotting in R

# ---- dependencies ----
library(tidyverse)
library(showtext)
library(patchwork)

# ---- setting graphics device ----

# These steps may be unnecessary in future versions of RStudio
# but for now gets around the issue of plots being
# horribly aliased and fonts not working in ggplot2

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png" # set RStudio inbuilt plot handler to use cairo
    antialias <- "subpixel" # Antialias plots to avoid pixelation
    dpi <-96 # set dpi to 96 (72 by default)
  }
}), print = FALSE)

trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
  showtext::showtext_opts(dpi = 96)
}), print = FALSE)




# ---- custom fonts ----

font_paths('~/R/base/theme/fonts') # add fonts folder to font paths
proj_fonts <- normalizePath('theme/fonts')
if(file.exists(proj_fonts)){
  font_paths(proj_fonts)
}

# find all fonts stored in theme folder calculate font weights and infer a "bold"
custom_fonts <-
  font_files() %>%
  filter(str_detect(path, 'theme\\/fonts')) %>%
  mutate(family = if_else(str_detect(face, 'Bold'), paste(family, 'Bold'), family),
         face = if_else(str_detect(face, 'Bold'), 
                        str_replace_all(face, c('Bold Italic' = 'Italic', 'Bold' = 'Regular')),
                        face),
         weight = case_when(str_detect(str_to_lower(family), 'thin') ~ 100,
                            str_detect(str_to_lower(family), 'extra?.light') ~ 200,
                            str_detect(str_to_lower(family), 'semi?.light') ~ 350,
                            str_detect(str_to_lower(family), 'light') ~ 300,
                            str_detect(str_to_lower(family), 'black') ~ 900,
                            str_detect(str_to_lower(family), 'extra?.bold') ~ 800,
                            str_detect(str_to_lower(family), 'semi?.bold') ~ 600,
                            str_detect(str_to_lower(family), 'bold') ~ 700,
                            str_detect(str_to_lower(family), 'medium') ~ 500,
                            T ~ 400,
                            ),
         main_family = str_remove(ps_name, '\\-.*') %>% str_remove('Black'),
         bold_weight = round(weight + 300, digits = -2)
         ) %>%
  select(main_family, family, face, file, weight, bold_weight) %>%
  unique() %>%
  pivot_wider(id_cols = c(main_family, family, weight, bold_weight), names_from = face, values_from = file) %>%
  left_join(y = transmute(., main_family, weight, Bold = Regular, BoldItalic = Italic), by = c('bold_weight' = 'weight', 'main_family' = 'main_family'))

# register fonts with fontdb
custom_fonts %>%
  select(family, Regular, Italic, Bold, BoldItalic) %>%
  unname() %>%
  pmap(function(f,r,i,b,bi){
    if(is.na(b)){
      font_add(family = f, regular = r, italic = i)
    } else {
      font_add(family = f, regular = r, bold = b, italic = i, bolditalic = bi)
    }
   
  })

## use showtext
showtext_auto()

# --- extrafont spec (deprecated) ----
#fonts <- c('Segoe UI Bold', 'Segoe UI Semibold', 'Segoe UI', 'Segoe UI Semilight', 'Segoe UI Light')
#fonts %in% windowsFonts()

# windowsFonts('Segoe UI Bold' = windowsFont('Segoe UI Bold'))
# windowsFonts('Segoe UI Semibold' = windowsFont('Segoe UI Semibold'))
# windowsFonts('Segoe UI' = windowsFont('Segoe UI'))
# windowsFonts('Segoe UI Semilight' = windowsFont('Segoe UI Semilight'))
# windowsFonts('Segoe UI Light' = windowsFont('Segoe UI Light'))

# ---- theme colours ----
# set of nice useful colours
colours <- list(dark = '#222629',
             light = '#D7D2C8',
             storm = '#494E6B',
             cloud = '#98878F',
             sunset = '#985E6D',
             evening = '#192231',
             coral = '#E14658',
             fresh = '#F7Ef6A',
             grape = '#575DA9',
             fuschia = '#E42D9F',
             blue = '#0b53c1')

# ---- nhse_colours ----
nhse_colours <- 
  list(
    # blues
    blue        = '#005EB8',
    dark_blue   = '#003087',
    bright_blue = '#0072CE',
    light_blue  = '#41B6E6',
    aqua_blue   = '#00A9CE',

    # neutrals
    black       = '#231F20',
    dark_grey   = '#425563',
    mid_grey    = '#768692',
    pale_grey   = '#E8EDEE',

    #support greens
    dark_green  = '#006747',
    green       = '#009639',
    light_green = '#78BE20',
    aqua_green  = '#00A499',

    # highlights
    purple      = '#330072',
    dark_pink   = '#7C2855',
    pink        = '#AE2573',
    dark_red    = '#8A1538',
    red         = '#DA291C',
    orange      = '#ED8B00',
    warm_yellow = '#FFB81C',
    yellow      = '#FAE100'
  )
  

# --- ordinal colours (neg/neut/pos) ----
ord_colours <-
  list(
    neg = '#EF4566',
    neut = '#F9CDAE',
    pos = '#83AE9B'
  )


# --- ggplot2 themes ---
theme_light <-
  function(base_family = 'Inter Light', base_size = 12, panel.grid = F){
    thm <- 
      theme_minimal(base_family = base_family, base_size = base_size) %+%
      theme(
        plot.background = element_rect(fill = nhse_colours$pale_grey, colour = 'transparent'),
        plot.title = element_text(size = rel(1.2), margin = margin(20,2,20,2)),
        plot.subtitle = element_text(size = rel(1)),
        text = element_text(colour = colours$dark, size = base_size),
        strip.text = element_text(size = rel(1)),
        axis.text = element_text(size = rel(.9)),
        axis.title = element_text(size = rel(.9))
      )
    if(panel.grid){
      thm <- thm %+% theme(panel.grid = element_line(colour = paste0(colours$dark, 30)))
    } else {
      thm <- thm %+% theme(panel.grid = element_blank())
    }
    return(thm)
  }

theme_nhs_light <-
  function(base_family = 'Inter Light', base_size = 12, panel.grid = F){
    thm <- 
      theme_minimal(base_family = base_family, base_size = base_size) %+%
      theme(
        plot.background = element_rect(fill = nhse_colours$pale_grey, colour = 'transparent'),
        plot.title = element_text(colour=nhse_colours$dark_blue, size = rel(1.2), margin=margin(5,2,5,2)),
        plot.title.position = 'plot',
        plot.subtitle = element_text(colour=nhse_colours$blue, size = rel(1), margin=),
        text = element_text(colour = colours$dark, size = base_size),
        strip.text = element_text(size = rel(1)),
        axis.text = element_text(size = rel(.9)),
        axis.title = element_text(size = rel(.9))
      )
    if(panel.grid){
      thm <- thm %+% theme(panel.grid = element_line(colour = paste0(colours$dark, 30)))
    } else {
      thm <- thm %+% theme(panel.grid = element_blank())
    }
    return(thm)
  }

theme_tpt <-
  function(base_family = 'Inter Light', base_size = 12, panel.grid = F){
    thm <-
      theme_minimal(base_family = base_family, base_size = base_size) %+%
      theme(
        plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
        panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
        plot.title = element_text(size = rel(1.2), margin = margin(20,2,20,2)),
        plot.subtitle = element_text(size = rel(1)),
        text = element_text(colour = colours$dark, size = base_size),
        strip.text = element_text(size = rel(1)),
        axis.text = element_text(size = rel(.9)),
        axis.title = element_text(size = rel(.9))
      )
    if(panel.grid){
      thm <- thm %+% theme(panel.grid = element_line(colour = paste0(colours$dark, 30)))
    } else {
      thm <- thm %+% theme(panel.grid = element_blank())
    }
    return(thm)
  }

theme_dark <-
  function(base_family = 'Inter Light', base_size = 12, panel.grid = F){
    thm <-
      theme_minimal(base_family = base_family, base_size = base_size) %+%
      theme(
        plot.background = element_rect(fill =colours$dark, colour = 'transparent'),
        panel.grid.minor = element_blank(),
        text = element_text(family = base_family, colour = colours$light, size = base_size),
        plot.title = element_text(family = base_family, size = rel(1.3), margin = margin(20,2,20,2)),
        plot.subtitle = element_text(size = rel(1)),
        axis.title = element_text(size = rel(.9)),
        axis.text = element_text(colour = colours$light, size = rel(.9)),
        strip.text = element_text(colour = colours$light, size = rel(1)),
        line = element_line(colour = colours$light)
      )
    if(panel.grid){
      thm <- thm %+% theme(panel.grid = element_line(colour = paste0(colours$light, 15)))
    } else {
      thm <- thm %+% theme(panel.grid = element_blank())
    }
    return(thm)
  }

set_theme_defaults <- function(colourful = F){
  hl <- theme_get()$text$colour
  bg <- theme_get()$plot.background$fill
  chl <- theme_get()$plot.title$colour
  
  params <- ls(pattern = '^geom_', env = as.environment('package:ggplot2'))
  geoms <- gsub("geom_", "", params)
  
  attempt_update <- function(x, ...){tryCatch(update_geom_defaults(x, ...), 
                                         error = function(err){print(paste('unable to update', x))})}
  if(colourful){
    lapply(geoms, attempt_update, list(colour = chl))
  } else {
    lapply(geoms, attempt_update, list(colour = hl))
  }  
  return(list('hl' = hl, 'bg' = bg))
}

fix_margins <- function(x){
  if(!is.null(x$labels$subtitle)){
    x <- x + theme(plot.title = element_text(margin=margin(5,5,5,5,'pt')))
  }
  return(x)
}



default_colours <- set_theme_defaults()
# thm <- list('colours' = colours,
#                  'nhse_colours' = nhse_colours,
#                  'default_colours' = default_colours,
#                  'ord_colours' = ord_colours,
#                  'set_theme_defaults' = set_theme_defaults,
#                  'theme_dark' = theme_dark,
#                  'theme_light' = theme_light,
#                  'theme_tpt' = theme_tpt)

theme_set(theme_dark())

rm('custom_fonts')

   