# Codebook functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2024-01-29

# Table of contents
# 1) Helper functions
#   1.1) abcddata_codebook_column_types
#   1.2) abcddata_codebook_content
#   1.3) abcddata_codebook_collected_over
# 2) ABCD codebook functions
#   2.1) abcddata_codebook_new_entry
#   2.2) abcddata_codebook_add_entry
#   2.3) abcddata_codebook_extract
#   2.4) abcddata_codebook_display

#### 1) Helper functions ####

#### 1.1) abcddata_codebook_column_types ####
#' Column Types for ABCD Codebook Entries
#'
#' Function to convert abbreviations used in a
#' column name into the associated label to
#' include in a codebook entry for ABCD data.
#'
#' @param chr_column_name A character string,
#'   the column name to process. If the
#'   empty character set \code{''}, lists
#'   the abbreviations and associated labels
#'   for each abbreviation type.
#' @param chr_abbr A character string, the
#'   abbreviation type to consider, either
#'   \code{'Category'}, \code{'Data type'},
#'   or \code{'Responder'}.
#'
#' @returns Either a character string, the label
#' associated with the abbreviation of interest,
#' or displays a message to the console listing
#' abbreviations and labels.
#'
#' @examples
#'
#' abcddata_codebook_column_types()
#'
#' chr_column <- 'IDS.CHR.GD.Subject'
#' abcddata_codebook_column_types( chr_column, 'Category' )
#' abcddata_codebook_column_types( chr_column, 'Data type' )
#' abcddata_codebook_column_types( chr_column, 'Reporter' )
#'
#' @export

abcddata_codebook_column_types <- function(
    chr_column_name = '',
    chr_abbr = NULL ) {

  # WWW.XXX.YY.Description
  # GD = General description

  # PC = Parents reporting on child
  # PS = Parents reporting on selves
  # PO = Parent reporting on others
  # CS = Child reporting on self
  # GD = General details

  # Pre-defined abbreviations and
  # labels for variable categories
  mat_abbr.category <- rbind(

    c( 'IDS', 'Identifiers' ),
    c( 'DTQ', 'Data quality' ),
    c( 'INC', 'Indices for inclusion' ),

    c( 'SSS', 'Session details' ),
    c( 'SMP', 'Sample characteristics' ),
    c( 'PHS', 'Physical health' ),
    c( 'MNT', 'Mental health' ),

    c( 'IMG', 'Imaging data' ),
    c( 'CGN', 'Cognitive functioning' ),
    c( 'BMR', 'Biomarkers' ),
    c( 'ENV', 'Environment' ),

    c( 'SBS', 'Substance use' ),
    c( 'QST', 'Questionnaire' )

  )
  colnames( mat_abbr.category ) <- c(
    'Abbr',
    'Label'
  )

  # Pre-defined abbreviations and labels
  # for data types
  mat_abbr.data_type <- rbind(
    c( 'INT', 'Integer' ),
    c( 'DBL', 'Double precision floating point number' ),
    c( 'NMR', 'Numeric' ),
    c( 'LGC', 'Logical' ),
    c( 'CHR', 'Character string' ),
    c( 'DTT', 'R posixt datetime variable' ),
    c( 'FCT', 'Enumerated type - factor' )
  )
  colnames( mat_abbr.data_type ) <- c(
    'Abbr',
    'Label'
  )

  # Pre-defined abbreviations and labels
  # for reporter
  mat_abbr.reporter <- rbind(
    c( 'CS', 'Child reporting on self' ),
    c( 'PC', 'Parents reporting on child' ),
    c( 'PS', 'Parents reporting on self' ),
    c( 'PO', 'Parents reporting on other' ),
    c( 'GD', 'General details' )
  )
  colnames( mat_abbr.reporter ) <- c(
    'Abbr',
    'Label'
  )

  # The primary types of abbreviation/label pairs
  lst_abbr <- list(
    category = c(
      'Variable categories', 'variable categories',
      'Variable category', 'variable category',
      'Variable', 'variable',
      'Category', 'category',
      '1'
    ),
    type = c(
      'Data types', 'data types',
      'Data type', 'data type',
      'Data', 'data',
      'Type', 'type',
      '2'
    ),
    reporter = c(
      'Reporter',
      'reporter',
      'Person reporting',
      'person reporting',
      'Responder',
      'responder',
      'Respondent',
      'respondent',
      'Person responding',
      'person responding',
      '3'
    )
  )

  # Display abbreviations
  if ( chr_column_name == '' & is.null( chr_abbr ) ) {

    message( 'Variable categories' )
    obj_nuisance <- sapply(
      1:nrow( mat_abbr.category ), function(r) {
        paste0( '  ', mat_abbr.category[r, 1],
                ' = ', mat_abbr.category[r, 2] ) |> message()
      }
    )
    message('')
    message( 'Data types' )
    obj_nuisance <- sapply(
      1:nrow( mat_abbr.data_type ), function(r) {
        paste0( '  ', mat_abbr.data_type[r, 1],
                ' = ', mat_abbr.data_type[r, 2] ) |> message()
      }
    )
    message('')
    message( 'Reporter types' )
    obj_nuisance <- sapply(
      1:nrow( mat_abbr.reporter ), function(r) {
        paste0( '  ', mat_abbr.reporter[r, 1],
                ' = ', mat_abbr.reporter[r, 2] ) |> message()
      }
    )

    # Close 'Display abbreviations'
  }

  # Display pre-defined abbreviations and labels
  if ( chr_column_name == '' & !is.null( chr_abbr ) ) {

    # Options for variable categories
    if ( chr_abbr %in% lst_abbr$category ) {

      message(
        'Abbreviations and labels for variable categories:\n'
      )

      message( paste(
        paste0( mat_abbr.category[, 1], ' = ',
                mat_abbr.category[, 2], '\n' ),
        collapse = ''
      ) )

      # Close 'Options for variable categories'
    }

    # Options for data types
    if ( chr_abbr %in% lst_abbr$type ) {

      message(
        'Abbreviations and labels for data types:\n'
      )

      message( paste(
        paste0( mat_abbr.data_type[, 1], ' = ',
                mat_abbr.data_type[, 2], '\n' ),
        collapse = ''
      ) )

      # Close 'Options for data types'
    }

    # Options for reporter types
    if ( chr_abbr %in% lst_abbr$reporter ) {

      message(
        'Abbreviations and labels for reporter types:\n'
      )

      message( paste(
        paste0( mat_abbr.reporter[, 1], ' = ',
                mat_abbr.reporter[, 2], '\n' ),
        collapse = ''
      ) )

      # Close 'Options for reporter types'
    }

    # Close 'Display pre-defined abbreviations and labels'
  }

  # Match labels and abbreviations
  if ( chr_column_name != '' & !is.null( chr_abbr ) ) {

    # Split variable name into different parts
    chr_name_parts <- strsplit(
      chr_column_name,
      split = '.',
      fixed = TRUE
    )[[1]]

    # Loop over types
    for ( i in 1:3 ) {

      # Check abbreviation type
      if ( chr_abbr %in% lst_abbr[[i]] ) {

        # Extract abbreviation
        chr_current <- chr_name_parts[i]
        lgc_match <- get(
          c( 'mat_abbr.category',
             'mat_abbr.data_type',
             'mat_abbr.reporter' )[i]
        )[, 1] %in% chr_current

        # If a match is found
        if ( sum(lgc_match) == 1 ) {

          # Return label
          return(
            get(
              c( 'mat_abbr.category',
                 'mat_abbr.data_type',
                 'mat_abbr.reporter' )[i]
            )[lgc_match, 2]
          )

          # Close 'If a match is found'
        } else {

          stop( 'Abbreviation not found or has duplicates' )

          # Close else for 'If a match is found'
        }

        # Close 'Check abbreviation type'
      }

      # Close 'Loop over types'
    }

    stop(
      "Check specification of 'chr_abbr' argument"
    )

    # Close 'Match labels and abbreviations'
  }

  # If column but not abbreviation specified
  if ( chr_column_name != '' & is.null(chr_abbr) ) {

    stop(
      paste0(
        "Specification of the 'chr_column_name' argument ",
        "requires specification of the 'chr_abbr' argument as well"
      )
    )

    # Close 'If column but not abbreviation specified'
  }

}

#### 1.2) abcddata_codebook_content ####
#' List of Content for Codebook Entries
#'
#' Function to create an appropriately labeled
#' list with the elements 'content' and
#' 'additional_content' for use with codebook
#' entries.
#'
#' @param vec_content A vector, must match in length
#'   to \code{'vec_additional_content'}.
#' @param vec_additional_content A vector, must match
#'   in length to \code{'vec_content'}.
#'
#' @returns A list of two vectors of matching length
#' labeled 'content' and 'additional_content'
#' respectively.
#'
#' @examples
#' abcddata_codebook_content( 1:3, c( 'A', 'B', 'C' ) )
#'
#' @export

abcddata_codebook_content <- function(
    vec_content,
    vec_additional_content ) {

  # If lengths do not match
  if ( length(vec_content) != length(vec_additional_content) ) {

    stop(
      paste0(
        "Inputs must be two vectors matching in length - inputs ",
        "will be converted to character vectors"
      )
    )

    # Close 'If lengths do not match'
  }

  return(
    list(
      content = as.character( vec_content ),
      additional_content = as.character( vec_additional_content )
    )
  )

}

#### 1.3) abcddata_codebook_collected_over ####
#' Determine What Entries a Variable was Collected Over
#'
#' Function to determine the unique entries over which
#' a variable was collected over. Useful, for example,
#' to determine the time points over which a variable
#' was collected.
#'
#' @param dtf_base A data frame.
#' @param chr_variable A character string, the
#'   column name for the variable of interest.
#' @param chr_factors A character vector, the
#'   column names for the variables whose
#'   entries should be considered.
#' @param obj_missing An optional vector with
#'   codes for missing data.
#'
#' @returns A list of two vectors of matching length
#' labeled 'content' and 'additional_content'
#' respectively.
#'
#' @examples
#' # Example data set
#' dtf_base <- data.frame(
#'   IDS.CHR.G.G.Site = c( '1', '2', '1', '2' ),
#'   SSS.CHR.G.G.Time_point = c( 'A', 'A', 'B', 'B' ),
#'   QST.INT.G.Y.Example = c( 1, 2, NA, NA )
#' )
#' abcddata_codebook_collected_over(
#'   dtf_base, 'QST.INT.G.Y.Example',
#'   c( 'IDS.CHR.G.G.Site', 'SSS.CHR.G.G.Time_point' )
#' )
#'
#' @export

abcddata_codebook_collected_over <- function(
    dtf_base,
    chr_variable,
    chr_factors,
    obj_missing = NA ) {

  # Check if column exists
  if ( !chr_variable %in% colnames(dtf_base) ) {

    stop( 'Variable not found' )

    # Close 'Check if column exists'
  }

  lgc_missing <-
    dtf_base[[ chr_variable ]] %in% obj_missing

  vec_content <- c()
  vec_additional_content <- c()

  # If any non-missing data
  if ( any(!lgc_missing) ) {

    # Loop over factors
    for ( f in chr_factors ) {

      # Check if column exists
      if ( !f %in% colnames(dtf_base) ) {

        stop( 'Factor not found' )

        # Close 'Check if column exists'
      }

      vec_unq <- unique( dtf_base[[ f ]][!lgc_missing] )

      # If any non-missing terms
      if ( any( !is.na(vec_unq) ) ) {

        vec_unq <- vec_unq[ !is.na(vec_unq) ]

        vec_additional_content <- c(
          vec_additional_content,
          vec_unq
        )
        vec_content <- c(
          vec_content,
          rep( f, length(vec_unq) )
        )

        # Close 'If any non-missing terms'
      }

      # Close 'Loop over factors'
    }

    # Close 'If any non-missing data'
  }

  # If no data
  if ( length(vec_content) == 0 ) {

    vec_content <- ''
    vec_additional_content <- ''

    # Close 'If no data'
  }

  lst_output <- list(
    content = vec_content,
    additional_content = vec_additional_content
  )

  return( lst_output )
}

#### 2) ABCD codebook functions ####

#### 2.1) abcddata_codebook_new_entry ####
#' Create a New Codebook Entry
#'
#' Function to create a new codebook entry
#' for a ABCD data set variable.
#'
#' @param chr_variable_name A character string, the column
#'   name for the variable.
#' @param chr_category A character string, the category type
#'   for the variable (see [abcddata::abcddata_codebook_column_types]).
#' @param chr_data_type A character string, the data type
#'   for the variable (see [abcddata::abcddata_codebook_column_types]).
#' @param chr_reporter A character string, the reporter
#'   for the variable (see [abcddata::abcddata_codebook_column_types]).
#' @param chr_description A character string, a human-readable
#'   description of the variable and what it is for.
#' @param lst_values_and_labels A list of two vectors,
#'   the values and their associated labels if applicable.
#' @param lst_collected_over A list of two vectors giving the
#'   levels and associated variables over which the given
#'   variable was collected.
#' @param chr_source_files A character vector, the source
#'   files from which the variable was taken.
#' @param chr_source_variables A character vector, the
#'   original names of the source variables from which
#'   the variable was derived.
#' @param chr_notes A character vector, additional
#'   notes on the variable.
#'
#' @returns A data frame.
#'
#' @examples
#' abcddata_codebook_new_entry()
#'
#' @export

abcddata_codebook_new_entry <- function(
    chr_variable_name = '',
    chr_category = '',
    chr_data_type = '',
    chr_reporter = '',
    chr_description = '',
    lst_values_and_labels = NULL,
    lst_collected_over = NULL,
    chr_source_files = '',
    chr_source_variables = '',
    chr_notes = '' ) {

  # Check inputs
  abcddata_internal.input_is.character( chr_variable_name )
  abcddata_internal.input_is.character( chr_category )
  abcddata_internal.input_is.character( chr_data_type )
  abcddata_internal.input_is.character( chr_reporter )
  abcddata_internal.input_is.character( chr_description )
  abcddata_internal.input_is.character( chr_source_files )
  abcddata_internal.input_is.character( chr_source_variables )
  abcddata_internal.input_is.character( chr_notes )

  # Values and associated labels
  if ( !is.null(lst_values_and_labels) ) {

    lst_values_and_labels <- abcddata_internal.input_is.content_list(
      lst_values_and_labels
    )
    vec_values_and_labels.content <- lst_values_and_labels$content
    vec_values_and_labels.additional_content <-
      lst_values_and_labels$additional_content

    # Close 'Values and associated labels'
  } else {

    vec_values_and_labels.content <- ''
    vec_values_and_labels.additional_content <- ''

    # Close else for 'Values and associated labels'
  }

  # Levels collected over
  if ( !is.null(lst_collected_over) ) {

    lst_collected_over <- abcddata_internal.input_is.content_list(
      lst_collected_over
    )
    vec_collected_over.content <- lst_collected_over$content
    vec_collected_over.additional_content <-
      lst_collected_over$additional_content

    # Close 'Values and associated labels'
  } else {

    vec_collected_over.content <- ''
    vec_collected_over.additional_content <- ''

    # Close else for 'Values and associated labels'
  }

  n_values_and_labels <- length( vec_values_and_labels.content )
  n_collected_over <- length( vec_collected_over.content )
  n_source_files <- length( chr_source_files )
  n_source_variables <- length( chr_source_variables )
  n_notes <- length( chr_notes )

  chr_content_type <- c(
    "Category",
    "Data type",
    "Reporter",
    "Description",
    rep( "Values and labels", n_values_and_labels ),
    rep( "Collected over", n_collected_over ),
    rep( "Source files", n_source_files ),
    rep( "Source variables", n_source_variables ),
    rep( "Notes", n_notes )
  )

  chr_content <- c(
    chr_category,
    chr_data_type,
    chr_reporter,
    chr_description,
    vec_values_and_labels.content,
    vec_collected_over.content,
    chr_source_files,
    chr_source_variables,
    chr_notes
  )

  chr_additional_content <- c(
    "", # Category
    "", # Data type
    "", # Reporter
    "", # Description
    vec_values_and_labels.additional_content,
    vec_collected_over.additional_content,
    rep( "", n_source_files ),
    rep( "", n_source_variables ),
    rep( "", n_notes )
  )

  dtf_codebook <- data.frame(
    Variable = chr_variable_name,
    Content_type = chr_content_type,
    Content = chr_content,
    Additional_content = chr_additional_content
  )
  lgc_no_entries <-
    apply( dtf_codebook[, -(1:2)], 1, function(x) all( x == '' ) )
  dtf_codebook <- dtf_codebook[!lgc_no_entries, ]

  return( dtf_codebook )
}

#### 2.2) abcddata_codebook_add_entry ####
#' Add a Codebook Entry to a Data Frame Column
#'
#' Function to update a column of a data frame to
#' have a codebook entry.
#'
#' @param dtf_base A data frame.
#' @param chr_column A character string, the column name
#'   of the variable for which a codebook entry will be added.
#' @param chr_description A character string, a human-readable
#'   description of the variable and what it is for.
#' @param lst_values_and_labels A list of two vectors,
#'   the values and their associated labels if applicable.
#' @param lst_collected_over A list of two vectors giving the
#'   levels and associated variables over which the given
#'   variable was collected.
#' @param chr_source_files A character vector, the source
#'   files from which the variable was taken.
#' @param chr_source_variables A character vector, the
#'   original names of the source variables from which
#'   the variable was derived.
#' @param chr_notes A character vector, additional
#'   notes on the variable.
#' @param chr_category A character string, the category type
#'   for the variable (see [abcddata::abcddata_codebook_column_types]).
#' @param chr_data_type A character string, the data type
#'   for the variable (see [abcddata::abcddata_codebook_column_types]).
#' @param chr_reporter A character string, the responder type
#'   for the variable (see [abcddata::abcddata_codebook_column_types]).
#'
#' @returns A data frame with the associated column updated with
#' a new attribute.
#'
#' @examples
#' dtf_base <- data.frame(
#'   IDS.CHR.GD.Participant = LETTERS[1:3],
#'   QST.INT.CS.Example = 4:6
#' )
#'
#' dtf_base <- abcddata_codebook_add_entry(
#'   dtf_base, 'IDS.CHR.GD.Participant',
#'   chr_description = 'Participant identifier'
#' )
#' abcddata_codebook_display( dtf_base$IDS.CHR.GD.Participant )
#'
#' dtf_base <- abcddata_codebook_add_entry(
#'   dtf_base, 'QST.INT.CS.Example',
#'   chr_description = 'Example questionnaire responses'
#' )
#' abcddata_codebook_display( dtf_base$QST.INT.CS.Example )
#'
#' @export

abcddata_codebook_add_entry <- function(
    dtf_base,
    chr_column,
    chr_description = '',
    lst_values_and_labels = NULL,
    lst_collected_over = NULL,
    chr_source_files = '',
    chr_source_variables = '',
    chr_notes = '',
    chr_category = '',
    chr_data_type = '',
    chr_reporter = '' ) {

  lst_attr <-
    attributes( dtf_base[[ chr_column ]] )

  # If column has no attributes
  if ( is.null( lst_attr ) ) {

    lst_attr <- list(
      abcddata_codebook = c()
    )

    # Close 'If column has no attributes'
  } else {

    lst_attr$abcddata_codebook <- c()

    # Close else for 'If column has no attributes'
  }

  # Add category label
  if ( chr_category == '' ) {

    chr_category <- abcddata_codebook_column_types(
      chr_column,
      chr_abbr = 'Category'
    )

    # Close 'Add category label'
  }

  # Add data type label
  if ( chr_data_type == '' ) {

    chr_data_type <- abcddata_codebook_column_types(
      chr_column,
      chr_abbr = 'Data type'
    )

    # Close 'Add data type label'
  }

  # Add reporter label
  if ( chr_reporter == '' ) {

    chr_reporter <- abcddata_codebook_column_types(
      chr_column,
      chr_abbr = 'Reporter'
    )

    # Close 'Add reporter label'
  }

  dtf_codebook_entry <- abcddata_codebook_new_entry(
    chr_variable_name = chr_column,
    chr_category = chr_category,
    chr_data_type = chr_data_type,
    chr_reporter = chr_reporter,
    chr_description = chr_description,
    chr_source_files = chr_source_files,
    chr_source_variables = chr_source_variables,
    chr_notes = chr_notes
  )

  lst_attr$abcd_codebook <- dtf_codebook_entry

  attributes( dtf_base[[ chr_column ]] ) <- lst_attr

  return( dtf_base )
}

#### 2.3) abcddata_codebook_extract ####
#' Extract ABCD Codebook Entry
#'
#' Function to extract the data frame with the
#' ABCD codebook entry contained in a vector's
#' attributes.
#'
#' @param vec_values A vector with an \code{'abcd_codebook'}
#' element in the attributes.
#'
#' @returns A data frame.
#'
#' @export

abcddata_codebook_extract <- function(
    vec_values ) {

  lst_attr <- attributes(vec_values)
  dtf_codebook <- lst_attr$abcd_codebook

  return( dtf_codebook )
}

#### 2.4) abcddata_codebook_display ####
#' Display ABCD Codebook Entry
#'
#' Function to display an ABCD codebook entry
#' contained in a vector's attributes.
#'
#' @param vec_values A vector with an \code{'abcd_codebook'}
#' element in the attributes.
#'
#' @returns As a side effect, displays the codebook entry
#' in the console window.
#'
#' @export

abcddata_codebook_display <- function(
    vec_values,
    int_text_wrap = 60 ) {

  # Extract codebook entry
  lst_attr <- attributes(vec_values)
  dtf_codebook <- lst_attr$abcd_codebook

  # Function to display text in console window
  fun_display_text <- function(
    chr_text,
    int_text_wrap,
    chr_header = '  ' ) {

    # If text needs to wrap
    if ( nchar(chr_text) > int_text_wrap ) {

      chr_split_text <-
        strsplit( chr_text, split = ' ', fixed = TRUE )[[1]]

      int_cumul_char <-
        cumsum( nchar( chr_split_text ) ) %% int_text_wrap
      int_wrap <-
        which( c( FALSE, diff( int_cumul_char ) < 0 ) ) - 1
      int_wrap <- c( 0, int_wrap, length( int_cumul_char ) )

      # Loop over cut-offs
      for ( j in seq_along(int_wrap)[-1] ) {

        int_index <-
          c( int_wrap[j - 1] + 1, int_wrap[j] )
        chr_line <- paste(
          chr_split_text[int_index[1]:int_index[2]], collapse = " "
        )
        message( paste0( chr_header, chr_line ) )

        # Close 'Loop over cut-offs'
      }

      # Close 'If text needs to wrap'
    } else {

      message( paste0( chr_header, chr_text ) )

      # Close else for 'If text needs to wrap'
    }

  }

  message( paste0( 'Variable: \n  ', unique( dtf_codebook$Variable ) ) )

  # Content types available for variable
  chr_content <- unique( dtf_codebook$Content_type )

  # Loop over content types
  for ( i in seq_along(chr_content) ) {

    message( paste0( chr_content[i], ': ' ) )

    lgc_rows <- dtf_codebook$Content_type %in% chr_content[i]

    # If no additional content
    if ( all( dtf_codebook$Additional_content == '' ) ) {

      # Loop over rows
      for ( r in which(lgc_rows) ) {

        fun_display_text(
          dtf_codebook$Content[r],
          int_text_wrap
        )

        # Close 'Loop over rows'
      }

      # Close 'If no additional content'
    }

    # Close 'Loop over content types'
  }

  invisible( NULL )
}

