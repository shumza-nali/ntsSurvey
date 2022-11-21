#' Build the National Travel Survey data set from a list
#'
#' @param df_names
#' @param df_cols
#' @param year_min
#' @param year_max
#' @param nts_extract
#'
#' @return
#' @export
#'
#' @examples
build_nts_list <- function(nts_dir, nts_extract = 'Default'){

  if(!inherits(nts_extract , 'list')){

    if(nts_extract == 'Default'){

      nts_extract <- basic_extract

    } else {

      stop("Argument 'nts_extract' must be a list otherwise use 'Default'")

    }

  } else {

    # Check list names order and reorder if necessary
    nts_extract <- sanitize_names_levels(nts_extract, nts_names)

  }

  nts_df <- purrr::map2(names(nts_extract), nts_extract, nts_read_tab, nts_dir)

  nts_df <- purrr::reduce(nts_df, dplyr::left_join)

}

build_nts_csv <- function(nts_dir, year_min = 2002, year_max = 2019, csv_dir = NULL){

  if(is.null(csv_dir)){

    stop("Provide path to a CSV input or use 'build_nts_list' with a list input")

  }
  if(inherits(csv_dir, 'list')){

    stop("Use 'build_nts_list' for a list input")

  }

  nts_extract <- vroom::vroom(csv_dir)

  # Convert to list and remove NA
  nts_extract <- nts_extract %>%
    as.list() %>%
    purrr::map(., na.omit)

  # Check list names order and reorder if necessary
  nts_extract <- sanitize_names_levels(nts_extract, nts_names)

  nts_df <- purrr::map2(names(nts_extract), nts_extract, nts_read_tab, nts_dir)

  nts_df <- purrr::reduce(nts_df, dplyr::left_join)

}
