#' @importFrom purrr map discard
#' @importFrom stringr str_c
#' @importFrom dplyr %>%
#' @importFrom azwmisc.fonts fontpath fontdir fontfile
fontfamily_fontspec_options <- function(x){
  allSlots <- slotNames(x)
  fonts <- allSlots %>% map(~ slot(x, .x))
  fonts
  opts <- list()
  ## opts$Path <- fontdir(x) %>% str_c("/")
  ## opts$Extension <- ".otf"
  opts$UprightFont <- fontpath(x@UprightFont)
  opts$ItalicFont <- fontpath(x@ItalicFont)
  opts$BoldFont <- fontpath(x@BoldFont)
  opts$BoldItalicFont <- fontpath(x@BoldItalicFont)
  opts <- opts %>% discard(is.null) %>%
    discard(~ .x == "")
  opts %>%
    names %>%
    map_chr(~ str_c(.x, " = ", opts[[.x]])) %>%
    c("Path", .)
}
