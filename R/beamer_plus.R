#' @importFrom methods slotNames
#' @importFrom utils head packageName tail
NULL

import:::from(rmarkdown, pandoc, quoted)

#' Beamer presentation with additional arguments
#'
#' @export
#' @importFrom purrr detect_index map map_chr reduce
#' @importFrom rmarkdown pandoc_variable_arg pandoc_path_arg
beamer_plus <- function(...,
                        themeoptions = "default",
                        colorthemeoptions = "default",
                        fontthemeoptions = "default",
                        fontsize = "11pt",
                        classoption = list("aspectratio=1610")
                        ){
  format <- rmarkdown::beamer_presentation(...)
  format$inherits <- "beamer_presentation"

  tmpl_idx <- format$pandoc$args %>%
    detect_index(~ . == "--template")


  if (tmpl_idx == 0){
    command <- quoted(pandoc()) %>% c("-D", "beamer") %>%
      paste(collapse = " ")
    oldtmpl <- suppressWarnings({
      system(command, intern = TRUE)
    })
  } else {
    oldtmpl <- readLines(format$pandoc$args[tmpl_idx + 1])
  }

  args <- format$pandoc$args

  newtmpl <- patch_beamer_template(oldtmpl)

  if (tmpl_idx != 0){
    args[tmpl_idx + 1] <- newtmpl
  } else {
    args <- c(args, "--template", pandoc_path_arg(newtmpl))
  }


  if(!identical(themeoptions, "default")){
    args <- c(args, pandoc_variable_arg("themeoptions", themeoptions))
  }
  if(!identical(colorthemeoptions, "default")){
    args <- c(args, pandoc_variable_arg("colorthemeoptions", colorthemeoptions))
  }
  if(!identical(colorthemeoptions, "default")){
    args <- c(args, pandoc_variable_arg("fontthemeoptions", fontthemeoptions))
  }

  args <- c(args, beamer_plus_yaml(fontsize = fontsize, classoption = classoption))

  format$pandoc$args <- args

  format
}

#' @importFrom yaml as.yaml
beamer_plus_yaml <- function(...){
  tempfile <- tempfile(fileext = ".yaml")
  meta <- list(...)
  meta_yaml <- c(
    "---",
    as.yaml(meta),
    "---"
  )
  writeLines(enc2utf8(paste(meta_yaml, collapse = "\n")),
             tempfile,
             useBytes = TRUE)
  tempfile
}

#' @importFrom stringr str_replace str_detect
#' @importFrom dplyr %>%
#' @importFrom purrr detect_index
patch_beamer_template <- function(template){
  template <- template %>%
    patch_command_with_options("usecolortheme", "colorthemeoptions") %>%
    patch_beamer_add_themeoptions() %>%
    add_beamer_colors() %>%
    patch_beamer_addfont()
  newtmpfile <- tempfile(fileext = ".tex")
  template <- paste(template, collapse = "\n")
  writeLines(enc2utf8(template), newtmpfile, useBytes = TRUE)
  newtmpfile
}

#' @importFrom stringr str_c
options_template <- function(varname){
  str_c("$if(", varname, ")$[$", varname, "$]$endif$", sep = "")
}

import:::from(rmarkdown, with_pandoc_safe_environment)

#' @importFrom rmarkdown beamer_presentation
#' @importFrom purrr detect_index
get_beamer_template <- function(format = beamer_presentation()){
  args <- format$pandoc$args
  idx <- args %>% detect_index(~ . == "--template")
  if (idx == 0){
    command <- paste(quoted(pandoc()), "-D beamer", sep = " ")
    template <- with_pandoc_safe_environment({
      tryCatch(
        system(command, intern = TRUE),
        error = function(e) NULL)
    })
    template
  } else {
    if (length(args) < idx + 1) return(NULL)
    templateFile <- args[idx + 1]
    if (!file.exists(templateFile)) return(NULL)
    template <- tryCatch(readLines(templateFile), error = function(e) NULL)
    template
  }
}

patch_beamer_add_themeoptions <- function(template){
  patch <- c("\\usetheme[$for(themeoptions)$$themeoptions$$sep$,$endfor$]{$theme$}")
  lines <- local({
    index <- grep(patch, template, fixed = TRUE)
    if (length(index) == 1) index else -1
  })
  if (lines > 0) {
    template
  } else {
    targetLine <- template %>%
      detect_index(~ str_detect(.x, "^\\s*\\\\usetheme(\\[[^\\[\\]]*\\])?\\{\\$theme\\$\\}\\s*$"))
    template[targetLine] <- patch
    template
  }
}

patch_command_with_options <- function(template, cmd, optionvar){
  template %>% str_replace(
    paste0("\\\\", cmd),
    paste0("\\\\", cmd, options_template(optionvar))
  )
}

add_beamer_colors <- function(template){
  template %>% str_replace(
    "\\\\begin\\{document\\}",
    paste(
      c("$for(beamercolor)$",
        "\\\\setbeamercolor{$beamercolor.name$}{$beamercolor.color$}",
        "$endfor$",
        "\\\\begin{document}"),
      collapse = "\n"
    )
  )
}

patch_beamer_addfont <- function(template){
  # Move font section to just before header-includes. Useful because the theme
  # often overrides font specifications.
  fontsection <- c(
    "\\ifnum 0\\ifxetex 1\\fi\\ifluatex 1\\fi=0",
    "\\else",
    "$if(mainfont)$",
    "    \\setmainfont[$for(mainfontoptions)$$mainfontoptions$$sep$,$endfor$]{$mainfont$}",
    "$endif$",
    "$if(sansfont)$",
    "    \\setsansfont[$for(sansfontoptions)$$sansfontoptions$$sep$,$endfor$]{$sansfont$}",
    "$endif$",
    "$if(monofont)$",
    "    \\setmonofont[Mapping=tex-ansi$if(monofontoptions)$,$for(monofontoptions)$$monofontoptions$$sep$,$endfor$$endif$]{$monofont$}",
    "$endif$",
    "$if(mathfont)$",
    "    \\setmathfont(Digits,Latin,Greek)[$for(mathfontoptions)$$mathfontoptions$$sep$,$endfor$]{$mathfont$}",
    "$endif$",
    "$if(CJKmainfont)$",
    "    \\usepackage{xeCJK}",
    "    \\setCJKmainfont[$for(CJKoptions)$$CJKoptions$$sep$,$endfor$]{$CJKmainfont$}",
    "$endif$",
    "\\fi")

  patch <- c(
    fontsection,
    "$for(fonts)$",
    "\\newfontfamily{$fonts.latexcmd$}{$fonts.name$}[",
    "$for(fonts.options)$",
    "$fonts.options$$sep$,",
    "$endfor$",
    ## "Path = $fonts.path$,",
    ## "Extension = .otf,",
    ## "UprightFont = $fonts.upright$,",
    ## "ItalicFont = $fonts.italic$,",
    ## "BoldFont = $fonts.bold$,",
    ## "BoldItalicFont = $fonts.bolditalic$",
    "]",
    "$if(fonts.main)$\\usefonttheme{serif}$endif$",
    "$endfor$"
  )
  targetLine <- template %>%
    detect_index(~ str_detect(.x, "\\$for\\(header-includes\\)\\$"))

  if (targetLine == 0) return(template)

  template <- c(
    head(template, n = targetLine - 1),
    patch,
    tail(template, n = -(targetLine - 1))
  )
}
