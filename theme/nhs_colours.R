#' R Color Scheme for NHS England
#' Developed by: Ewan Wakeman
#' Date: 11/12/2021
#' Based on: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

require(assert_that)
require(ggplot2)

# NHS England Colours

nhse_colours <- 
  c(
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
  

#' extract colours as hex codes
#' 
#' @param ... Character names of nhse_colours
#' 
nhse_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (nhse_colours)
  
  nhse_colours[cols]
}

# Palettes

nhse_palettes <- list(
    blues = nhse_cols("light_blue", "aqua_blue", "bright_blue", "blue","dark_blue"),
    neutrals = nhse_cols("light_grey", "mid_grey", "dark_grey"),
    pinks = nhse_cols("pink", "dark_pink"),
    greens = nhse_cols("light_green", "green"),
    highlights = nhse_cols("pink", "dark_pink", "light_green", "green"),
    grey_pink = nhse_cols("mid_grey", "pink"),
    grey_green = nhse_cols("mid_grey", "green"),
    grey_blue = nhse_cols("mid_grey", "blue")
)

#' Return function to interpolate a nhse colour pallette
#' 
#' @param palette Character name of palette in nhse colour palette
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' 
nhse_pal <- function(palette = "blues", reverse = FALSE, ...) {
  pal <- nhse_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for nhse colours
#' 
#' @param palette Character name of palette in nhse_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or scale_colour_gradientn(), used respectively when discrete is TRUE or FALSE
#' 
scale_colour_nhse <- function(palette = "blues", discrete = TRUE, reverse = FALSE, ...) {
  pal <- nhse_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("nhse_", palette), palette = pal, ...)
  } else {
    scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for nhse colours
#' 
#' @param palette Character name of palette in nhse_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' 
scale_fill_nhse <- function(palette = "blues", discrete = TRUE, reverse = FALSE, ...) {
  pal <- nhse_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("nhse_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



shade <- function(hex, mod=0){
  assert_that(str_detect(hex, '#[\\d|a-f|A-F]{6}[\\d]{0,2}$'), 
              msg = "Please enter a valid 6 or 8 digit hex code (e.g. \\'#03A9BC'")
  assert_that((-1<=mod) & (mod<=1))
  match_pat <- '#([\\d|a-f|A-F]{2})([\\d|a-f|A-F]{2})([\\d|a-f|A-F]{2})([\\d]{0,2})'
  groups <- str_match(hex, match_pat)[2:5]
  rgb <- map(groups[1:3], ~strtoi(.x, base=16L))
  if(mod>0){
    mix <- map(rgb, ~round(weighted.mean(c(.x, 255), c(1-mod, mod))))
  } else{
    mix <- map(rgb, ~round(weighted.mean(c(.x, 0), c(1-(mod*-1), mod*-1))))
  }
  hex <- reduce(map(mix, ~format(as.hexmode(.x), width=2)), paste0)
  out_colour <- paste0('#', hex, groups[4])
  return(out_colour)
}
