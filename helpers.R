# custom ggplot2 theme for charts
theme_ashby <- function (...) {
  theme_minimal(...) %+replace%
    theme(
      axis.ticks = element_line(colour = "grey92"),
      axis.title = element_text(size = 9, hjust = 1),
      legend.key.height = unit(4, "mm"),
      legend.key.width = unit(6, "mm"),
      legend.position = "bottom",
      legend.spacing.x = unit(2, "mm"),
      legend.title = element_text(size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.caption = element_text(size = 9, colour = "grey33", hjust = 1, 
                                  margin = margin(t = 3)),
      plot.tag = element_text(size = 12, face = "bold", colour = "grey33", 
                              hjust = 0),
      plot.tag.position = c(0.01, 0.01),
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      strip.text.y = element_text(angle = 0, hjust = 0)
    )
}



# common values for chart elements, which cannot be specified in a theme
chart_elements <- list(
  linetype = c("solid", "11", "52"),
  label_arrow = grid::arrow(length = grid::unit(4, "points"), ends = "first"),
  label_line_colour = "grey40",
  label_line_curvature = 0.3,
  label_text_colour = "grey33",
  label_text_fill = "white",
  label_text_lineheight = 0.9,
  label_text_size = 9 / (14 / 5),
  average_line_colour = "grey50",
  average_line_linetype = "11",
  reference_line_colour = "grey50"
)



# function to turn a vector into a printed list
# This has now been superceded by the `combine_words()` function in `knitr`,
# around which this is now a wrapper
vector_to_text <- function (x, sep = ", ", final_sep = " and ") {
	
	knitr::combine_words(x, sep = sep, and = final_sep, oxford_comma = FALSE)
  
  # if (length(x) == 1) {
  #   x
  # } else if (length(x) == 0) {
  #   ""
  # } else {
  #   paste0(paste(x[1:length(x) - 1], collapse = sep), final_sep, x[[length(x)]])
  # }
  
}



# Convert small integers to text
number_to_text <- function (x, ...) {
  
  if (length(x) != 1 | !is.numeric(x)) {
    stop("number_to_text() can only convert a single number")
  }
  
  if (x == round(x) & x >= 0 & x <= 10) {
    dplyr::recode(x, `1` = "one", `2` = "two", `3` = "three", `4` = "four",
                  `5` = "five", `6` = "six", `7` = "seven", `8` = "eight",
                  `9` = "nine", `10` = "ten", `0` = "zero")
  } else {
    scales::comma(x, ...)
  }
  
}



# define colour scheme, based on the UCL colour scheme defined at
# https://www.ucl.ac.uk/cam/brand/guidelines/colour
ucl_colours <- tibble::tribble(
  ~name, ~hex_code,
  "Dark Green", "#555025",
  "Dark Red", "#651D32",
  "Dark Purple", "#4B384C",
  "Dark Blue", "#003D4C",
  "Dark Brown", "#4E3629",
  "Mid Green", "#8F993E",
  "Mid Red", "#93272C",
  "Mid Purple", "#500778",
  "Mid Blue", "#002855",
  "Stone", "#D6D2C4",
  "Bright Green", "#B5BD00",
  "Bright Red", "#D50032",
  "Bright Blue", "#0097A9",
  "Bright Pink", "#AC145A",
  "Light Green", "#BBC592",
  "Light Red", "#E03C31",
  "Light Purple", "#C6B0BC",
  "Light Blue", "#8DB9CA",
  "Yellow", "#F6BE00",
  "Orange", "#EA7600",
  "Grey", "#8C8279",
  "Blue Celeste", "#A4DBE8"
)

ucl_colours_list <- tibble::deframe(ucl_colours)
