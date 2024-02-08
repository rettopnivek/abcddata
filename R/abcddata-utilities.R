# Utilities
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2024-01-15

# Table of contents
# 1) Operators
#   1.1) `%with%`
# 2) General-purpose data processing functions
#   2.1) abcddata_replace
#   2.2) abcddata_merge_data_sets
#   2.3) abcddata_propagate
#   2.4) abcddata_rows_with_missing

#### 1) Operators ####

#### 1.1) `%with%` ####
#' Operator to Match Patterns in Strings
#'
#' Operator to find all elements in a
#' character vector that contain a specified
#' pattern. Makes a call to [base::grepl],
#'
#' @param chr_vector A character vector.
#' @param chr_pattern A character string,
#'   the pattern to match against.
#'
#' @returns A logical vector equal in length
#' to `chr_vector`, set to `TRUE` when
#' the given string contains `chr_pattern`.
#'
#' @examples
#' c( 'cat', 'bat', 'dog' ) %with% 'at'
#'
#' @export

`%with%` <- function(
    chr_vector,
    chr_pattern ) {

  lgc_matches <- grepl(
    chr_pattern, chr_vector, fixed = TRUE
  )

  return( lgc_matches )
}

#### 2) General-purpose data processing functions ####

#### 2.1) abcddata_replace ####
#' Replace Values in a Vector
#'
#' Function to replace values in a vector. Robust against
#' `NA` values.
#'
#' @param vec_values A vector of values.
#' @param vec_to_replace A vector, the unique elements in
#'   `vec_values` to replace.
#' @param vec_replace_with A vector (equal in length to
#'   `vec_to_replace`), the new elements to replace those
#'   specified in `vec_to_replace`.
#' @param obj_default The default value to use for elements
#'   in `vec_values` that aren't updated.
#' @param lgc_exact Logical; if `TRUE` elements in
#'   `vec_to_replace` must exactly match those in `vec_values`,
#'   otherwise uses [base::grepl] for partial pattern matching.
#'
#' @returns A vector equal in length to `vec_values` with
#' updated values.
#'
#' @examples
#' chr_example <- rep( LETTERS[1:3], each = 2 )
#' abcddata_replace( chr_example, c( 'A', 'B' ), c( 1, 2 ) )
#'
#' @export

abcddata_replace <- function(
    vec_values,
    vec_to_replace,
    vec_replace_with,
    obj_default = NA,
    lgc_exact = TRUE ) {

  # Initialize output
  vec_output <- rep( obj_default, length( vec_values ) )

  # Check that elements to replace/replace with match in length
  if ( length(vec_to_replace) != length(vec_replace_with) ) {

    # Error message
    stop(
      paste0(
        'Number of elements to replace does not number to replace with'
      )
    )

    # Close 'Check that elements to replace/replace with match in length'
  }

  # Loop over elements to replace
  for ( i in seq_along( vec_to_replace ) ) {

    # If element is NA
    if ( is.na(vec_to_replace[i]) ) {

      lgc_cases <- is.na( chr_vector )

      # Close 'If element is NA'
    } else {

      # Exact matches
      if ( lgc_exact ) {

        lgc_cases <- vec_values %in% vec_to_replace[i]

        # Close 'Exact matches'
      } else {

        lgc_cases <- vec_values %with% vec_to_replace[i]

        # Close else for 'Exact matches'
      }

      # Close else for 'If element is NA'
    }

    vec_output[ lgc_cases ] <- vec_replace_with[i]

    # Close 'Loop over elements to replace'
  }

  return( vec_output )
}

#### 2.2) abcddata_merge_data_sets ####
#' Merge Columns From Data Sets
#'
#' Function to merge columns from a data set to
#' a base data set.
#'
#' @param dtf_base A data frame, the data set to
#'   add columns to.
#' @param dtf_to_merge A data frame, the data set
#'   to add columns from.
#' @param lst_to_match_over A list of character
#'   vectors with the columns to match over for,
#'   each vector giving [1] the column in `dtf_base`
#'   and [2] the column in `dtf_to_merge`. Allows
#'   matching over, for example, participant IDs and
#'   time points. Columns should be chosen to ensure
#'   only a single row is matched.
#' @param chr_columns_to_add A character vector, the
#'   columns in `dtf_to_merge` to add to `dtf_base`.
#'   If a named vector is provided, the vector names
#'   are used as the new column labels.
#' @param lgc_progress Logical; if `TRUE` displays the
#'   progress of the function for debugging purposes.
#'
#' @returns A data frame.
#'
#' @examples
#' dtf_base <- data.frame(
#'   V1 = rep( LETTERS[1:3], each = 3 ),
#'   V2 = rep( 1:3, 3 )
#' )
#'
#' dtf_to_merge <- data.frame(
#'   va = rep( LETTERS[1:4], each = 3 ),
#'   vb = rep( c( 1, 3, 4 ), 4 ),
#'   y1 = 1:12,
#'   y2 = 13:24
#' )
#'
#' dtf_base <- abcddata_merge_data_sets(
#'   dtf_base, dtf_to_merge,
#'   list( c( 'V1', 'va' ), c( 'V2', 'vb' ) ),
#'   c(
#'     Y1 = y1, Y2 = y2
#'   )
#' )
#'
#' @export

abcddata_merge_data_sets <- function(
    dtf_base,
    dtf_to_merge,
    lst_to_match_over,
    chr_columns_to_add,
    lgc_progress = FALSE ) {

  if ( lgc_progress ) {
    message( 'Start: abcddata_replace' )
  }

  dat_start <- Sys.time()

  chr_match_base_columns <- sapply(
    seq_along( lst_to_match_over ), function(m) {
      lst_to_match_over[[m]][1]
    }
  )
  chr_match_to_merge_columns <- sapply(
    seq_along( lst_to_match_over ), function(m) {
      lst_to_match_over[[m]][2]
    }
  )
  mat_matches <- matrix(
    FALSE, nrow(dtf_base), length( chr_match_base_columns )
  )

  # Create progress bar
  if ( lgc_progress ) {

    message( '     Match rows' )

    # Create a progress bar using a base R function
    obj_progress <- txtProgressBar(
      min = 1, max = nrow(dtf_to_merge), style = 3
    )

    # Close 'Create progress bar'
  }

  int_rows_to_merge <- 1:nrow( dtf_to_merge )
  int_rows_base <- sapply(
    int_rows_to_merge, function(r) {

      int_row <- NA

      # Loop over columns
      for ( k in 1:ncol(mat_matches) ) {

        mat_matches[, k] <-
          dtf_base[[ chr_match_base_columns[k] ]] %in%
          dtf_to_merge[[ chr_match_to_merge_columns[k] ]][r]

        # Close 'Loop over columns'
      }
      int_matches <- rowSums( mat_matches )

      # Select row with all matches
      if ( any( int_matches == ncol(mat_matches ) ) ) {

        int_row <- which( int_matches == ncol(mat_matches ) )[1]

        # Close 'Select row with all matches'
      }

      # Update progress bar
      if ( lgc_progress ) {

        setTxtProgressBar(obj_progress, r)

        # Close 'Update progress bar'
      }

      return( int_row )
    }
  )
  close( obj_progress )
  lgc_match <- !is.na( int_rows_base )

  # If no new column names provided
  if ( is.null( names( chr_columns_to_add ) ) ) {

    names( chr_columns_to_add ) <- chr_columns_to_add

    # Close 'If no new column names provided'
  }

  if (lgc_progress) {
    message( '  Adding ' )
  }

  # Loop over columns to add
  for ( k in seq_along( chr_columns_to_add ) ) {

    chr_old <- chr_columns_to_add[k]
    chr_new <- names(chr_columns_to_add)[k]

    if (lgc_progress) {
      message( paste0( '    ', chr_new ) )
    }

    # If column does not already exist
    if ( !chr_new %in% colnames(dtf_base) ) {

      dtf_base$NEW <- rep( NA, nrow(dtf_base) )
      chr_base_columns <- colnames(dtf_base)
      chr_base_columns[ chr_base_columns == 'NEW' ] <-
        chr_new
      colnames(dtf_base) <- chr_base_columns

      # Close 'If column does not already exist'
    }

    dtf_base[[ chr_new ]][int_rows_base[lgc_match]] <-
      dtf_to_merge[[ chr_old ]][lgc_match]

    # Close 'Loop over columns to add'
  }

  dat_end <- Sys.time()

  # End of progress
  if ( lgc_progress ) {

    message(
      paste0(
        'End: abcddata_merge_data_sets (',
        format( difftime( dat_end, dat_start ) ),
        ')'
      )
    )

    # Close 'End of progress'
  }

  return( dtf_base )
}

#### 2.3) abcddata_propagate ####
#' Propagate Values Over Multiple Columns
#'
#' Function to propagate a value across
#' multiple rows based on an index variable.
#'
#' @param dtf_data A data frame.
#' @param chr_over A character string, the
#'   column to use as the index variable.
#' @param chr_columns A character vector, the
#'   columns for whom values should be propagated.
#' @param lgc_subset An optional logical vector
#'   equal in length to the number of rows of
#'   `dtf_data`, the subset of rows to consider.
#' @param lgc_progress Logical; if `TRUE` displays the
#'   progress of the function for debugging purposes.
#'
#' @returns A data frame, equivalent to `dtf_data` but
#' with propagated values for the columns listed in
#' `chr_columns`.
#'
#' @examples
#' dtf_example <- data.frame(
#'   ID = rep( 1:3, each = 3 ),
#'   X = c( 1, NA, NA, 2, NA, NA, 3, NA, NA ),
#'   Y = c( NA, NA, 'A', NA, NA, 'B', NA, NA, 'C' )
#' )
#'
#' dtf_example <- abcddata_propagate(
#'   dtf_example,
#'   chr_over = 'ID',
#'   chr_columns = c( 'X', 'Y' )
#' )
#'
#' @export

abcddata_propagate <- function(
    dtf_data,
    chr_over,
    chr_columns,
    lgc_subset = NULL,
    lgc_progress = FALSE ) {

  if ( lgc_progress ) {
    message( 'Start: abcddata_propagate' )
  }

  dat_start <- Sys.time()

  # By default use all rows
  if ( is.null(lgc_subset) ) {

    lgc_subset <- rep( TRUE, nrow(dtf_data) )

    # Close 'By default use all rows'
  }

  if ( lgc_progress ) {
    message( '  Identify sets of rows' )
  }

  obj_unique <- unique( dtf_data[[ chr_over ]][lgc_subset] )
  obj_unique <- obj_unique[ !is.na( obj_unique ) ]
  lst_matches <- lapply(
    obj_unique, function(u) {
      which( dtf_data[[ chr_over]][lgc_subset] %in% u )
    }
  )

  # Function to return one unique value
  fun_unique_value <- function( x ) {

    if ( any( !is.na(x) ) ) {

      return( x[ !is.na(x) ][1] )

    } else {

      return( NA )

    }

  }

  # Create a progress bar using a base R function
  if (lgc_progress) {

    message( '  Propagate values' )

    obj_progress <- txtProgressBar(
      min = 1, max = length(lst_matches), style = 3
    )

    # Close 'Create a progress bar using a base R function'
  }

  # Loop over matches
  for ( u in seq_along(lst_matches) ) {

    # Loop over columns
    for ( k in seq_along(chr_columns) ) {

      # If any matches
      if ( length( lst_matches[[u]] ) > 0 ) {

        dtf_data[[ chr_columns[k] ]][ lst_matches[[u]] ] <-
          fun_unique_value(
            dtf_data[[ chr_columns[k] ]][ lst_matches[[u]] ]
          )

        # Close 'If any matches'
      }

      # Close 'Loop over columns'
    }

    # Update progress bar
    if ( lgc_progress ) {

      setTxtProgressBar( obj_progress, u )

      # Close 'Update progress bar'
    }

    # Close 'Loop over matches'
  }
  close( obj_progress )

  dat_end <- Sys.time()

  # End of progress
  if ( lgc_progress ) {

    message(
      paste0(
        'End: abcddata_propagate (',
        format( difftime( dat_end, dat_start ) ),
        ')'
      )
    )

    # Close 'End of progress'
  }

  return( dtf_data )
}

#### 2.4) abcddata_rows_with_missing ####
#' Identify Rows with Missing Data
#'
#' Function to identify rows with any/all
#' missing data.
#'
#' @param dtf_data A data frame.
#' @param vec_missing A vector with codes
#'   for missing values (can include \code{NA}).
#' @param lgc_any A logical value; if \code{TRUE}
#'   flags rows with any missing values, otherwise
#'   flags rows with all missing values.
#'
#' @returns A logical vector.
#'
#' @examples
#' dtf_data <- data.frame(
#'   C1 = c( NA, '', 'A', '' ),
#'   C2 = c( 'B', NA, 'C', '' )
#' )
#'
#' abcddata_rows_with_missing( dtf_data )
#' abcddata_rows_with_missing( dtf_data, vec_missing = NA )
#' abcddata_rows_with_missing( dtf_data, lgc_any = FALSE )
#'
#' @export

abcddata_rows_with_missing <- function(
    dtf_data,
    vec_missing = c( NA, '' ),
    lgc_any = TRUE ) {

  # NA values only
  if ( all( is.na( vec_missing ) ) ) {

    # Any missing values
    if ( lgc_any ) {

      lgc_missing <- apply(
        dtf_data, 1, function(x) any( is.na(x) )
      )

      # Close 'Any missing values'
    } else {

      lgc_missing <- apply(
        dtf_data, 1, function(x) all( is.na(x) )
      )

      # Close else for 'Any missing values'
    }

    # Close 'NA values only'
  } else {

    # Non-NA codes for missing values
    vec_missing <- vec_missing[ !is.na( vec_missing ) ]

    # Any missing values
    if ( lgc_any ) {

      lgc_missing <- apply(
        dtf_data, 1, function(x) {
          any( is.na(x) | x %in% vec_missing )
        }
      )

      # Close 'Any missing values'
    } else {

      lgc_missing <- apply(
        dtf_data, 1, function(x) {
          all( is.na(x) | x %in% vec_missing )
        }
      )

      # Close else for 'Any missing values'
    }

    # Close else for 'NA values only'
  }

  return( lgc_missing )
}

