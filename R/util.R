is_osx <- function(){
  Sys.info()["sysname"] == "Darwin"
}

find_program <- function(program){
  if (is_osx()){
    res <- suppressWarnings({
      sanitized_path <- Sys.getenv("PATH")
    })

  } else {
    Sys.which(program)
  }
}

#' @importFrom purrr map map_chr
add_to_path <- function(p){
  p %>% map_chr(~ sanitize_envvar)
}

#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
sanitize_envvar <- function(value){
  value %>%
    gsub("\\", "\\\\", ., fixed = TRUE) %>%
    gsub("\"", "\\\"", ., fixed = TRUE)
}
