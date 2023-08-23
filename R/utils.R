tdirp <- fs::path("P:/02.personal/wb384996/temporal/R/")

get_elements <- function(x, element) {
  if (is.list(x)) {
    if (any(names(x) == element)) x[[element]]
    else lapply(x, \(y) get_elements(y, element = element))
  }
}

remove_null <- function(x){
  if(is.list(x)) {
    x |>
      purrr::discard(is.null) |>
      purrr::discard(rlang::is_na) |>
      purrr::discard(\(x) length(x) == 0) |>
      purrr::map(remove_null) |>
      purrr::map(remove_null) # to remove empty lists
  } else {
    x
  }
}

flatten_recurse <- function(x) {
  if (is.list(x)) {
    if (any(map_lgl(x, is.numeric))) {
      # I think it should be all(map_lgl...)
      x
    } else {
      list_flatten(x) |>
        flatten_recurse()
    }
  } else {
    x
  }
}
