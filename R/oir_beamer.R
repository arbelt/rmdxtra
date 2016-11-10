#' OIR beamer format
#'
#' @param ... Parameters passed to \code{beamer_presentation}
#' @param includes Additional includes passed to \code{beamer_presentation}
#' @param latex_engine Passed to \code{beamer_presentation}
#' @param textfont Font to use for body text. Must be font provided in package
#'   \code{azwmisc.fonts}
#' @export
#' @importFrom azwmisc.fonts font_family
#' @importFrom rmarkdown beamer_presentation
oir_beamer <- function(..., includes = list(), latex_engine = "xelatex",
                       textfont = "FiraSans"){
  includes$in_header <- c(oir_beamer_header(), includes$in_header)
  format <- beamer_plus(..., latex_engine = latex_engine, includes = includes)
  format$pandoc$args <- c(format$pandoc$args, oir_beamer_yaml(textfont = textfont))
  format
}

#' Beamer header includes
oir_beamer_header <- function(){
  system.file("resources", "templates", "oir_beamer_header.sty", package = packageName())
}

#' Generate additional YAML metadata
#' @importFrom azwmisc.fonts get_font fontpath fontfile fontdir createFontFamily
#' @importFrom yaml as.yaml
oir_beamer_yaml <- function(..., textfont = "FiraSans"){
  meta <- list()
  meta$fonts <- list()
  fsc <- createFontFamily("FiraSansCondensed", reg_wt = "Book", bold_wt = "Medium")
  mainfont <- font_family(textfont, reg_wt = "Book", bold_wt = "Semibold")
  ## mainfont <- createFontFamily("CooperHewitt", reg_wt = "Book", bold_wt = "Semibold")
  meta$mainfont <- textfont
  meta$mainfontoptions <- fontfamily_fontspec_options(mainfont)
  meta$fonts <- list(
    list(name = "Fira Sans Condensed",
         latexcmd = "\\firasanscond",
         options = fontfamily_fontspec_options(fsc))
  )
  meta$theme <- "metropolis"
  meta$colortheme <- "owl"
  meta$colorthemeoptions <- "snowy"
  meta$classoption <- "aspectratio=1610,11pt"
  meta$themeoptions <- list(
    "progressbar=foot",
    "titleformat title=regular",
    "titleformat section=regular",
    "numbering=fraction"
  )
  meta$monofont <- "FiraMono"
  meta$monofontoptions <- fontfamily_fontspec_options(createFontFamily("FiraMono", reg_wt = "Regular"))
  tempfile <- tempfile(fileext = ".yaml")
  metablock <- paste(c("---", as.yaml(meta), "---"), sep = "\n")
  writeLines(enc2utf8(metablock), tempfile, useBytes = TRUE)
  tempfile
}
