
#' Beamer presentation with additional arguments
#'
#' @export
#' @importFrom purrr detect_index map map_chr reduce
#' @importFrom rmarkdown pandoc_variable_arg
beamer_plus <- function(...,
                        themeoptions = "default",
                        colorthemeoptions = "default",
                        fontthemeoptions = "default"
                        ){
  format <- rmarkdown::beamer_presentation(...)
  format$inherits <- "beamer_presentation"

  tmpl_idx <- format$pandoc$args %>%
    detect_index(~ . == "--template") %>% `+`(1)

  newtmpl <- patch_beamer_template(format$pandoc$args[tmpl_idx])
  format$pandoc$args[tmpl_idx] <- newtmpl

  args <- format$pandoc$args

  if(!identical(themeoptions, "default")){
    args <- c(args, pandoc_variable_arg("themeoptions", themeoptions))
  }
  if(!identical(colorthemeoptions, "default")){
    args <- c(args, pandoc_variable_arg("colorthemeoptions", colorthemeoptions))
  }
  if(!identical(colorthemeoptions, "default")){
    args <- c(args, pandoc_variable_arg("fontthemeoptions", fontthemeoptions))
  }

  format$pandoc$args <- args

  format
}

#' @importFrom stringr str_replace
#' @importFrom dplyr %>%
patch_beamer_template <- function(filepath){
  template <- readLines(filepath)
  template <- template %>%
    patch_command_with_options("usecolortheme", "colorthemeoptions") %>%
    patch_command_with_options("usetheme", "themeoptions") %>%
    add_beamer_colors()
  newtmpfile <- tempfile(fileext = ".tex")
  template <- paste(template, collapse = "\n")
  writeLines(enc2utf8(template), newtmpfile, useBytes = TRUE)
  newtmpfile
}

#' @importFrom stringr str_c
options_template <- function(varname){
  str_c("$if(", varname, ")$[$", varname, "$]$endif$", sep = "")
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