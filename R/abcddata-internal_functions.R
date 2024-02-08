# Internal functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-11-19

# Table of contents
# 1) Functions to check inputs
#   1.1) abcddata_internal.input_is.character
#   1.2) abcddata_internal.input_is.same_in_length
#   1.3) abcddata_internal.input_is.content_list

#### 1) Functions to check inputs ####

#### 1.1) abcddata_internal.input_is.character ####

abcddata_internal.input_is.character <- function(
    obj_to_check ) {

  if ( is.null( obj_to_check ) ) {
    stop( "NULL passed for input" )
  }

  if ( any( is.na( obj_to_check ) ) ) {
    stop( "NA passed for input" )
  }

  if ( !is.character( obj_to_check ) ) {
    stop( "Input not a character vector" )
  }

}

#### 1.2) abcddata_internal.input_is.same_in_length ####

abcddata_internal.input_is.same_in_length <- function(
    obj_to_check_first,
    obj_to_check_second ) {

  if ( is.null( obj_to_check_first ) |
       is.null( obj_to_check_second ) ) {
    stop( "NULL passed for input" )
  }

  if ( any( is.na( obj_to_check_first ) ) |
       any( is.na( obj_to_check_second ) ) ) {
    stop( "NA passed for input" )
  }

  if ( is.vector( obj_to_check_first ) &
       is.vector( obj_to_check_second ) ) {

    if ( is.list( obj_to_check_first ) |
         is.list( obj_to_check_second ) ) {

      stop( "Input is not a vector" )

    }

    if ( length( obj_to_check_first ) !=
         length( obj_to_check_second ) ) {

      stop( "Inputs have differing length" )

    }

  } else {

    stop( "Input is not a vector" )

  }

}

#### 1.3) abcddata_internal.input_is.content_list ####

abcddata_internal.input_is.content_list <- function(
    obj_to_check ) {

  if ( is.null( obj_to_check ) ) {
    obj_to_check <- list(
      content = "",
      additional_content = ""
    )
  }

  if ( !is.list( obj_to_check ) ) {

    stop( "Input is not a list" )

  }

  chr_instructions <- paste0(
    "Input must be a list with ",
    "two character vectors matching ",
    "in length named 'content' and ",
    "'additional_content'"
  )
  chr_names <- c( 'content', 'additional_content' )

  if ( length( obj_to_check ) != 2 ) {

    stop( chr_instructions )

  }

  if ( !all( names( obj_to_check ) %in% chr_names ) ) {

    stop( chr_instructions )

  }

  abcddata_internal.input_is.same_in_length(
    obj_to_check[[1]],
    obj_to_check[[2]]
  )

  return( obj_to_check )
}

