nts_names <- c(
  'psu',
  'household',
  'day',
  'individual',
  'trip',
  'stage',
  'vehicle',
  'ticket',
  'ldj',
  'attitudes'
)

detect_nts_years <- function(nts_dir){

  nts_files <- list.files(nts_dir)
  nts_years <- stringr::str_extract(nts_files, '[0-9]{4}-[0-9]{4}')

  if(length(unique(nts_years)) == 1){

    nts_years <- stringr::str_split(nts_years[1], '-')[[1]]

  } else {

    stop("The min and max years of .tab NTS files must be consistent for all files.
       An example of a correct file name: 'attitudes_special_2002-2019_protect.tab'")

  }

}

#' Read in individual NTS data tables
#'
#' @param df_name A data table name to read
#' @param nts_dir Path to folder .tab files are in
#' @param year_min Minimum year of data
#' @param year_max Maximum year of data
#'
#' @return A tibble of an NTS data table
#' @export
nts_read_tab <- function(df_name, df_cols = NULL, nts_dir){

  if(is.null(df_cols)){

    df_cols <- fetch_nts_cols(df_name, nts_dir)

  }

  # Check an existing name is supplied
  nts_check_names(df_name)

  # Add neccessary vars to df_cols if not already in
  df_cols <- sanitize_extract_cols(df_name, df_cols)

  # Path and read
  input_path <- list.files(nts_dir)
  input_path <- stringr::str_subset(input_path, df_name)
  input_path <- paste0(nts_dir, '/', input_path)

  #input_path <- paste0(nts_dir, df_name, '_special_', min_year, '-', max_year, '_protect.tab')

  vroom::vroom(input_path, col_select = tidyselect::all_of(df_cols))

}

nts_check_names <- function(df_name, nts_name = nts_names){

  if(!all(df_name %in% nts_name)){

    incorrect_names <- df_name[!df_name %in% nts_name]
    incorrect_names <- paste0(incorrect_names, collapse = ', ')

    stop(paste0('The following name(s) are not NTS data tables: ', incorrect_names))

  }

  return(invisible(NULL))

}

fetch_nts_cols <- function(df_name, nts_dir = '', pattern = NULL, year_min = 2002, year_max = 2019){

  nts_check_names(df_name)

  # Path and read
  input_path <- paste0(nts_dir, df_name, '_special_', year_min, '-', year_max, '_protect.tab')

  nts_cols <- names(readr::read_table(input_path, n_max = 1))

  if(!is.null(pattern)){

    nts_cols <- stringr::str_subset(nts_cols, pattern = pattern)

  }

  return(nts_cols)

}

sanitize_extract_cols <- function(df_name, df_cols){

  if('psu' == df_name){

    cols_output <- union(df_cols, 'PSUID')

  }
  if('household' == df_name){

    cols_output <- union(df_cols, c('PSUID', 'HouseholdID'))

  }
  if('individual' == df_name){

    cols_output <- union(df_cols, c('PSUID', 'HouseholdID', 'IndividualID'))

  }
  if('day' == df_name){

    cols_output <- union(df_cols, c('PSUID', 'HouseholdID', 'IndividualID', 'DayID'))

  }
  if('trip' == df_name){

    cols_output <- union(df_cols, c('PSUID', 'HouseholdID', 'IndividualID', 'DayID', 'TripID'))

  }

  return(cols_output)

}

sanitize_names_levels <- function(nts_extract, nts_names){

  # Factor nts_names (ordered to match nts hierarchy)
  nts_names_factor <- factor(nts_names, ordered = TRUE, levels = nts_names)

  # Reorder nts names list with ordered factor
  names(nts_extract) <- nts_names_factor[nts_names_factor %in% names(nts_extract)]

  # Ordered list
  nts_extract[names(nts_extract)]

}

create_nts_var_lu <- function(nts_names, nts_dir){

  # Obtain list of variables by nts dataframe
  names_list <- purrr::map(nts_names, fetch_nts_cols, nts_dir = nts_dir)
  names_list <- purrr::set_names(names_list, nts_names)

  # Bind list to dataframe
  names_list %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = c(value)) %>%
    dplyr::rename(nts_df = name,
           nts_var = value)

}

detect_nts_vars <- function(nts_vars, nts_names, nts_dir){

  nts_vars <- paste0(test_vars, collapse = '|')

  nts_lu <- create_nts_var_lu(nts_names, nts_dir)

  nts_lu %>%
    filter(str_detect(nts_var, nts_vars))

}

check_var_avail <- function(nts_var, nts_dir, nts_names){

  # Find which nts df each nts_var belongs to
  lu <- detect_nts_vars(test_vars, nts_names, nts_dir)

  # If multiple df's then just choose one
  lu <- lu %>%
    dplyr::group_by(nts_var) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Add SurveYear as a variable to each df table
  lu <- lu %>%
    tidyr::complete(nesting(nts_df), nts_var = 'SurveyYear') %>%
    dplyr::arrange(nts_df, nts_var)

  # Nest by nts var
  lu <- chop(lu, nts_var)

  output_list <- purrr::map2(lu$nts_df, lu$nts_var, munge_year_avail, nts_dir)

  dplyr::bind_rows(output_list)

}

munge_year_avail <- function(df_name, nts_vars, nts_dir){

  # Read data with selected columns
  nts_df <- nts_read_tab(df_name, nts_dir, nts_vars)

  # Check var availability by each year
  nts_df <- nts_df %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(-SurveyYear, ~ ifelse(is.na(.), 'N', 'Y'))) %>%
    dplyr::distinct() %>%
    dplyr::arrange(SurveyYear)

  # Add in any missing years with 'N'
  nts_years <- detect_nts_years(nts_dir)
  nts_years <- as.double(nts_years)
  nts_years <- seq(nts_years[1], nts_years[2])

  nts <- nts_df %>%
    tidyr::complete(SurveyYear = nts_years) %>%
    mutate(dplyr::across(dplyr::all_of(nts_vars), ~ ifelse(is.na(.), 'N', .)))

  # Pivot for final output (wide)
  nts_df <- nts_df %>%
    tidyr::pivot_longer(-SurveyYear, names_to = 'nts_var', values_to = 'is_available') %>%
    tidyr::pivot_wider(names_from = SurveyYear, values_from = is_available)

}

basic_extract <- list(
  psu = c('PSUID',
          'PSUPSect',
          'PSUPopDensity',
          'PSUAreaType1_B01ID',
          'PSUAreaType2_B01ID',
          'PSUGOR_B02ID',
          'PSUCounty_B01ID'),
  household = c('PSUID',
                'HouseholdID',
                'SurveyYear',
                'HHoldOSWard_B01ID',
                'HHoldOSLAUA_B01ID',
                'HHoldAreaType1_B01ID',
                'HHoldNumAdults',
                'NumCarVan_B02ID',
                'NumCarVan',
                'W1',
                'W2',
                'W3'),
  individual = c('PSUID',
                 'HouseholdID',
                 'IndividualID',
                 'Age_B01ID',
                 'Sex_B01ID',
                 'XSOC2000_B02ID',
                 'NSSec_B03ID',
                 'SIC2007_B02ID',
                 'CarAccess_B01ID',
                 'DrivLic_B02ID',
                 'EcoStat_B01ID',
                 'EcoStat_B02ID'),
  day = c('PSUID',
          'IndividualID',
          'HouseholdID',
          'DayID',
          'TravDay',
          'TravelWeekDay_B01ID',
          'TravelWeekDay_B02ID'),
  trip = c('PSUID',
           'HouseholdID',
           'IndividualID',
           'DayID',
           'TripID',
           'JJXSC',
           'JOTXSC',
           'JTTXSC',
           'JD',
           'MainMode_B03ID',
           'MainMode_B04ID',
           'MainMode_B11ID',
           'TripPurpFrom_B01ID',
           'TripPurpTo_B01ID',
           'TripStart_B01ID',
           'TripEnd_B01ID',
           'TripDisIncSW',
           'TripDisIncSW_B01ID',
           'TripTravTime',
           'TripOrigCounty_B01ID',
           'TripDestCounty_B01ID',
           'TripDestUA2009_B01ID',
           'TripOrigUA2009_B01ID',
           'W5',
           'W5xHH',
           'TripPurpose_B01ID',
           'TripOrigGOR_B02ID',
           'TripDestGOR_B02ID',
           'TripOrigCounty_B01ID',
           'TripDestCounty_B01ID',
           'TripOrigNTMZonTy_B01ID',
           'TripOrigUA1998_B01ID',
           'TripDestUA1998_B01ID')
)
