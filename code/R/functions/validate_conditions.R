validate_conditions <- function(conditions) {
  # Check if excluded_code_groups is numeric or NA
  if (!is.null(conditions$excluded_code_groups) && !is.na(conditions$excluded_code_groups) && !is.numeric(conditions$excluded_code_groups)) {
    stop("excluded_code_groups must be a numeric vector or NA")
  }
  
  specific_excl <- conditions$specific_exclusions
  if (!is.null(specific_excl) && !is.na(specific_excl)) {
    if (!is.list(specific_excl)) {
      stop("specific_exclusions must be a list or NA")
    }
    if (length(specific_excl) > 0 && is.list(specific_excl[[1]])) {
      for (condition in specific_excl) {
        if (!all(c("code_group", "code_cultu") %in% names(condition))) {
          stop("Each specific_exclusions item must be a list with code_group and code_cultu")
        }
        if (!is.numeric(condition$code_group)) {
          stop("code_group must be numeric")
        }
        if (!is.character(condition$code_cultu) && !(is.vector(condition$code_cultu) && all(sapply(condition$code_cultu, is.character)))) {
          stop("code_cultu must be a string or a vector of strings")
        }
      }
    } else {
      if (!all(c("code_group", "code_cultu") %in% names(specific_excl))) {
        stop("specific_exclusions must be a list with code_group and code_cultu")
      }
      if (!is.numeric(specific_excl$code_group)) {
        stop("code_group must be numeric")
      }
      if (!is.character(specific_excl$code_cultu) && !(is.vector(specific_excl$code_cultu) && all(sapply(specific_excl$code_cultu, is.character)))) {
        stop("code_cultu must be a string or a vector of strings")
      }
    }
  }
}
