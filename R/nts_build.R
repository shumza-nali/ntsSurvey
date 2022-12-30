#' Build the National Travel Survey data set from a list
#'
#' @param nts_dir Directory to folder where NTS .tab data files are stored
#' @param nts_extract Default is a basic extract of NTS. Use a custom named list, e.g. basic_extract
#'
#' @return A tibble of NTS tables joined together
#' @export
build_nts_list <- function(nts_dir, nts_extract = 'Default', nts_names = nts_names){

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

#' Build the National Travel Survey data set from a CSV input
#'
#' @param nts_dir Directory to folder where NTS .tab data files are stored
#' @param csv_dir Directory to input csv file
#'
#' @return A tibble of NTS tables joined together
#' @export
build_nts_csv <- function(nts_dir, csv_dir = NULL, nts_names = nts_names){

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
