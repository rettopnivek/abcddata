# Data creation
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2024-02-09

# Table of contents



#### 1) Creation of base data frames ####

#### 1.1) abcddata_initialize_long_form ####
#' Initialize Long-Form Data Frame
#'
#' Function to initialize a long-form data frame
#' for the ABCD® study with identifiers for
#' participants, families, and sites, as
#' well as session details and group splits
#' for reproducible matched samples (Feczko et al., 2021).
#' See \url{https://abcdstudy.org/} for details on the
#' ABCD® study, and refer to Saragosa-Harris et al. (2022)
#' for helpful notes and instructions for researchers.
#'
#' @param chr_files A character vector with
#'   the file names for 1) the data on
#'   longitudinal tracking of the youths, and
#'   2) genetics for the youths.
#' @param chr_paths A character vector with
#'   the associated folder paths for the
#'   longitudinal tracking and genetics files.
#' @param lgc_progress A logical value; if
#'   \code{TRUE} displays the progress of the
#'   data processing.
#'
#' @references
#' Feczko, E., Conan, G., Marek, S., Tervo-Clemmens, B.,
#'   Cordova, M., Doyle, O., ... & Fair, D. A. (2021). Adolescent
#'   Brain Cognitive Development (ABCD) community MRI collection
#'   and utilities. BioRxiv.
#'   https://www.biorxiv.org/content/10.1101/2021.07.09.451638v1.abstract
#'
#' Saragosa-Harris, N. M., Chaku, N., MacSweeney, N., Williamson,
#'   V. G., Scheuplein, M., Feola, B., ..., & Mills, K. L. (2022).
#'   A practical guide for researchers and reviewers using the ABCD
#'   Study and other large longitudinal datasets.
#'   Developmental Cognitive Neuroscience, 55, 1-11.
#'   https://doi.org/10.1016/j.dcn.2022.101115
#'
#' @returns A data frame.
#'
#' @export

abcddata_initialize_long_form <- function(
    chr_files = c( 'abcd_y_lt.csv', 'gen_y_pihat.csv' ),
    chr_paths = c( 'core/abcd-general', 'core/genetics' ),
    lgc_progress = TRUE ) {

  #### 1.1.1) Load in data ####

  # Read in file for base data
  chr_full_path <- chr_files[1]
  if ( chr_paths[1] != '' ) {
    chr_full_path <- paste0( chr_paths[1], '/', chr_files[1] )
  }
  dtf_base <- read.csv(
    file = chr_full_path,
    header = TRUE
  )

  # Read in file for genetics
  chr_full_path <- chr_files[2]
  if ( chr_paths[2] != '' ) {
    chr_full_path <- paste0( chr_paths[2], '/', chr_files[2] )
  }
  dtf_genetics <- read.csv(
    file = chr_full_path,
    header = TRUE
  )

  #### 1.1.2) Initialize output ####

  chr_participants <- sort( unique( dtf_base$src_subject_id ) )
  int_participants <- length( chr_participants )

  chr_time_points <- unique( dtf_base$eventname  )
  int_time_points <- length( chr_time_points )

  num_years <- abcddata_replace(
    chr_time_points,
    c(
      "baseline",
      "6_month_follow_up",
      "1_year_follow_up",
      "18_month_follow",
      "2_year_follow_up",
      "30_month_follow_up",
      "3_year_follow_up",
      "42_month_follow",
      "4_year_follow_up"
    ),
    c(
      0,
      6/12,
      12/12,
      18/12,
      24/12,
      30/12,
      36/12,
      42/12,
      48/12
    ),
    lgc_exact = FALSE
  )

  dtf_ABCD_long_form <- data.frame(
    IDS.CHR.GD.Participant = rep(
      chr_participants,
      each = int_time_points
    ),
    IDS.INT.GD.Participant = rep(
      seq_along( chr_participants ),
      each = int_time_points
    ),
    SSS.CHR.GD.Time_point = rep(
      chr_time_points,
      int_participants
    ),
    SSS.DBL.GD.Year = rep(
      num_years,
      int_participants
    )
  )

  #### 1.1.3) Add variables [Base] ####

  dtf_base$recoded.visit_type <- abcddata_replace(
    dtf_base$visit_type,
    1:3,
    c(
      'On-site', # 1
      'Remote', # 2
      'Hybrid' # 3
    )
  )

  dtf_ABCD_long_form <- abcddata_merge_data_sets(
    dtf_ABCD_long_form,
    dtf_base,
    list(
      c( 'IDS.CHR.GD.Participant', 'src_subject_id' ),
      c( 'SSS.CHR.GD.Time_point', 'eventname' )
    ),
    c(
      IDS.INT.GD.Family = 'rel_family_id',
      IDS.INT.GD.Birth = 'rel_birth_id',
      IDS.CHR.GD.Site = 'site_id_l',
      IDS.INT.GD.School = 'school_id',
      IDS.INT.GD.District = 'district_id',
      SSS.CHR.GD.Interview_date = 'interview_date',
      SSS.CHR.GD.Visit_type = 'recoded.visit_type',
      SMP.INT.GD.Interview_age_in_months = 'interview_age'
    ),
    lgc_progress = lgc_progress
  )

  # Convert age in months to years
  dtf_ABCD_long_form$SMP.DBL.GD.Interview_age_in_years <-
    dtf_ABCD_long_form$SMP.INT.GD.Interview_age_in_months / 12

  # Convert dates to R's date-time object
  dtf_ABCD_long_form$SSS.DTT.GD.Interview_date <-
    as.Date(
      dtf_ABCD_long_form$SSS.CHR.GD.Interview_date,
      format = '%m/%d/%Y', tz = 'UTC'
    )

  #### 1.1.4) Add variables [Genetics] ####

  dtf_genetics$recoded.sibling_status <- abcddata_replace(
    dtf_genetics$rel_relationship,
    c( 0:3 ),
    c(
      'Single', # 0
      'Sibling', # 1
      'Twin', # 2
      'Triplet' # 3
    )
  )

  dtf_ABCD_long_form <- abcddata_merge_data_sets(
    dtf_ABCD_long_form,
    dtf_genetics,
    list(
      c( 'IDS.CHR.GD.Participant', 'src_subject_id' ),
      c( 'SSS.CHR.GD.Time_point', 'eventname' )
    ),
    c(
      SMP.CHR.GD.Sibling_status = 'recoded.sibling_status'
    ),
    lgc_progress = lgc_progress
  )

  #### 1.1.5) Add variables [Discovery/Validation] ####

  dtf_ABCD_long_form <- abcddata_merge_data_sets(
    dtf_ABCD_long_form,
    abcddata::abcddata_ARMS,
    list(
      c( 'IDS.CHR.GD.Participant', 'IDS.CHR.GD.Participant' )
    ),
    c(
      "SSS.CHR.GD.ARMS_groups",
      "SSS.CHR.GD.Reproducible_matched_samples",
      "SSS.INT.GD.ARMS_groups"
    ),
    lgc_progress = lgc_progress
  )

  #### 1.1.6) Additional processing ####

  dtf_ABCD_long_form <- abcddata_propagate(
    dtf_ABCD_long_form,
    'IDS.CHR.GD.Participant',
    c( 'IDS.INT.GD.Family',
       'IDS.INT.GD.Birth',
       'IDS.CHR.GD.Site',
       'SSS.CHR.GD.ARMS_groups',
       'SSS.CHR.GD.Reproducible_matched_samples',
       'SSS.INT.GD.ARMS_groups',
       'SMP.CHR.GD.Sibling_status' ),
    lgc_progress = lgc_progress
  )

  # Identify cases with no interview date
  dtf_ABCD_long_form$INC.LGC.GD.No_interview <-
    is.na(
      dtf_ABCD_long_form$SSS.CHR.GD.Interview_date
    ) |
    dtf_ABCD_long_form$SSS.CHR.GD.Interview_date %in% ''

  #### 1.1.4) Codebook entries ####

  lst_codebook_entries <- list(

    IDS.CHR.GD.Site = list(
      chr_description = paste0(
        "Site ID at each event"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "site_id_l"
    ),

    IDS.INT.GD.District = list(
      chr_description = paste0(
        "Anonymized school district identifier based on ",
        "National Center for Education Statistics (NCES) ",
        "school identifiers"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "district_id"
    ),

    IDS.INT.GD.School = list(
      chr_description = paste0(
        "Anonymized school identifier based on ",
        "National Center for Education Statistics (NCES) ",
        "school identifiers"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "school_id"
    ),

    IDS.INT.GD.Family = list(
      chr_description = paste0(
        "Family identifier - participants belonging to the ",
        "same family share the same ID"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "rel_family_id",
      chr_notes = c(
        paste0(
          "The family ID is autocalculated and will change ",
          "after the addition or removal of subjects from ",
          "the ABCD study"
        ),
        "Family IDs will therefore differ between data releases"
      )
    ),

    IDS.INT.GD.Birth = list(
      chr_description = paste0(
        "Birth identifier within family identifier - ",
        "The X in <Family identifier>X indicates birth order"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "rel_birth_id"
    ),

    IDS.CHR.GD.Participant = list(
      chr_description = paste0(
        "Subject ID as it is defined in lab or project"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "src_subject_id"
    ),

    IDS.INT.GD.Participant = list(
      chr_description = paste0(
        "Integer index for each subject"
      )
    ),

    SSS.CHR.GD.ARMS_groups = list(
      chr_description = paste0(
        "The reproducible matched sample to which a participant ",
        "was assigned per Feczko et al. (2021)"
      ),
      lst_values_and_labels = list(
        content = c( paste0( 'ARMS-', 1:3 ), 'Excluded' ),
        additional_content = c(
          'Discovery set',
          'Replication set',
          'Development set',
          'Excluded'
        )
      )
    ),

    SSS.CHR.GD.Reproducible_matched_samples = list(
      chr_description = paste0(
        "The reproducible matched sample to which a participant ",
        "was assigned per Feczko et al. (2021)"
      )
    ),

    SSS.INT.GD.ARMS_groups = list(
      chr_description = paste0(
        "The reproducible matched sample to which a participant ",
        "was assigned per Feczko et al. (2021)"
      ),
      lst_values_and_labels = list(
        content = 1:4,
        additional_content = c(
          'Discovery set',
          'Replication set',
          'Development set',
          'Excluded'
        )
      )
    ),

    SSS.CHR.GD.Time_point = list(
      chr_description = paste0(
        "The event name for which the data was collected"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "eventname"
    ),

    SSS.DBL.GD.Year = list(
      chr_description = paste0(
        "The number of years since baseline (categorical)"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "eventname"
    ),

    SSS.CHR.GD.Interview_date = list(
      chr_description = paste0(
        "The date of the interview as M/D/Y"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "interview_date"
    ),

    SSS.DTT.GD.Interview_date = list(
      chr_description = paste0(
        "The date of the interview"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "interview_date"
    ),

    SSS.CHR.GD.Visit_type = list(
      chr_description = paste0(
        "Visit format"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "visit_type"
    ),

    INC.LGC.GD.No_interview = list(
      chr_description = paste0(
        "Indicator for whether no interview was conducted - based on ",
        "missing interview date"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "interview_date"
    ),

    SMP.INT.GD.Interview_age_in_months = list(
      chr_description = paste0(
        "Age in months at the time of the interview/test/sampling/imaging"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "interview_age",
      chr_notes = c(
        "Age is rounded to chronological month",
        paste0(
          "If the research participant is 15-days-old at time of ",
          "interview the appropriate value would be 0 months"
        ),
        paste0(
          "If the participant is 16-days-old the value would be 1 month"
        )
      )
    ),

    SMP.DBL.GD.Interview_age_in_years = list(
      chr_description = paste0(
        "Age in years at the time of the interview/test/sampling/imaging"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "interview_age",
      chr_notes = 'Interview age in months divided by 12'
    ),

    SMP.CHR.GD.Sibling_status = list(
      chr_description = paste0(
        "Whether participant has siblings or has a twin or triplet"
      ),
      chr_source_files = chr_files[2],
      chr_source_variables = 'rel_relationship'
    )

  )

  # Loop over codebook entries
  for ( l in seq_along(lst_codebook_entries) ) {

    lst_arg <- lst_codebook_entries[[l]]
    lst_arg$dtf_base <- dtf_ABCD_long_form
    lst_arg$chr_column <- names( lst_codebook_entries )[l]

    dtf_ABCD_long_form <- do.call(
      abcddata_codebook_add_entry,
      lst_arg
    )

    # Close 'Loop over codebook entries'
  }

  #### 1.1.5) Output ####

  # Reorganize columns
  dtf_ABCD_long_form <-
    dtf_ABCD_long_form[, names(lst_codebook_entries)]

  return( dtf_ABCD_long_form )
}

#### 2) Additional variables ####

#### 2.1) abcddata_add.sample_characteristics ####
#' Add Data for Sample Characteristics
#'
#' Function to add data on sample characteristics
#' for the ABCD® study. The function extracts
#' variables regarding a child's sex at birth,
#' current gender identity, ethnicity, and
#' race (per the 2015 NIH standards). The function
#' also extracts variables regarding the
#' combined income, employment status, and marital
#' status for the child's parents/guardians.
#'
#' @param dtf_ABCD_long_form A data frame, output from
#'   the [abcddata::abcddata_initialize_long_form]
#'   function.
#' @param chr_files A character vector with
#'   the file name for the data on demographics.
#' @param chr_paths A character vector with
#'   the associated folder path for the
#'   demographics file.
#' @param lgc_progress A logical value; if
#'   \code{TRUE} displays the progress of the
#'   data processing.
#'
#' @returns A data frame.
#'
#' @export

abcddata_add.sample_characteristics <- function(
    dtf_ABCD_long_form,
    chr_files = 'abcd_p_demo.csv',
    chr_paths = 'core/abcd-general',
    lgc_progress = TRUE ) {

  #### 2.1.1) Load in data ####

  # Read in demographics file
  chr_full_path <- chr_files[1]
  if ( chr_paths[1] != '' ) {
    chr_full_path <- paste0( chr_paths[1], '/', chr_files[1] )
  }
  dtf_demo <- read.csv(
    file = chr_full_path,
    header = TRUE
  )

  #### 2.1.2) Recode variables ####

  #### 2.1.2.1) Sex at birth ####

  dtf_demo$recoded.sex_at_birth <- abcddata_replace(
    dtf_demo$demo_sex_v2,
    c( 1:4, 777, 999 ),
    c(
      'Male', # 1
      'Female', # 2
      'Intersex - male', # 3
      'Intersex - female', # 4
      'Refuse to answer', # 777
      'Do not know' # 999
    )
  )

  #### 2.1.2.2) Gender ####

  # Collapse categories to be consistent with
  # subsequent categories post-baseline
  dtf_demo$recoded.baseline_gender <- abcddata_replace(
    dtf_demo$demo_gender_id_v2,
    c( 1:9, 777, 999 ),
    c(
      'Male', # 1
      'Female', # 2
      'Transgender male', # 3
      'Transgender female', # 4
      'Gender Queer', # 5
      'Different other identity', # 6
      'Different other identity', # Agender = 7
      'Different other identity', # Nonbinary = 8
      'Different other identity', # Other/prefer to self identify = 9
      'Refuse to answer', # 777
      'Do not know' # 999
    )
  )

  dtf_demo$recoded.current_gender <- abcddata_replace(
    dtf_demo$demo_gender_id_v2_l,
    c( 1:6, 777, 999 ),
    c(
      'Male', # 1
      'Female', # 2
      'Transgender male', # 3
      'Transgender female', # 4
      'Gender Queer', # 5
      'Different other identity', # 6
      'Refuse to answer', # 777
      'Do not know' # 999
    )
  )

  # Combine baseline and post-basline
  lgc_baseline <-
    is.na( dtf_demo$recoded.current_gender ) &
    !is.na( dtf_demo$recoded.baseline_gender )
  dtf_demo$recoded.current_gender[
    lgc_baseline
  ] <- dtf_demo$recoded.baseline_gender[
    lgc_baseline
  ]

  #### 2.1.2.3) Race ####

  dtf_demo$recoded.race <- '_'

  # White
  lgc_checked_box <- dtf_demo$demo_race_a_p___10 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + White'
  )

  # Black/African American
  lgc_checked_box <- dtf_demo$demo_race_a_p___11 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Black/African American'
  )

  # American Indian/Native American
  lgc_checked_box <- dtf_demo$demo_race_a_p___12 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + American Indian/Native American'
  )

  # Alaska Native
  lgc_checked_box <- dtf_demo$demo_race_a_p___13 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Alaska Native'
  )

  # Native Hawaiian
  lgc_checked_box <- dtf_demo$demo_race_a_p___14 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Native Hawaiian'
  )

  # Guamanian
  lgc_checked_box <- dtf_demo$demo_race_a_p___15 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Guamanian'
  )

  # Samoan
  lgc_checked_box <- dtf_demo$demo_race_a_p___16 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Samoan'
  )

  # Other Pacific Islander
  lgc_checked_box <- dtf_demo$demo_race_a_p___17 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Other Pacific Islander'
  )

  # Asian Indian
  lgc_checked_box <- dtf_demo$demo_race_a_p___18 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Asian Indian'
  )

  # Chinese
  lgc_checked_box <- dtf_demo$demo_race_a_p___19 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Chinese'
  )

  # Filipino
  lgc_checked_box <- dtf_demo$demo_race_a_p___20 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Filipino'
  )

  # Japanese
  lgc_checked_box <- dtf_demo$demo_race_a_p___21 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Japanese'
  )

  # Korean
  lgc_checked_box <- dtf_demo$demo_race_a_p___22 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Korean'
  )

  # Vietnamese
  lgc_checked_box <- dtf_demo$demo_race_a_p___23 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Vietnamese'
  )

  # Other Asian
  lgc_checked_box <- dtf_demo$demo_race_a_p___24 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Other Asian'
  )

  # Other race
  lgc_checked_box <- dtf_demo$demo_race_a_p___25 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Other race'
  )

  # Refused to answer
  lgc_checked_box <- dtf_demo$demo_race_a_p___26 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Refused to answer'
  )

  # Do not know
  lgc_checked_box <- dtf_demo$demo_race_a_p___26 %in% 1
  dtf_demo$recoded.race[lgc_checked_box] <- paste0(
    dtf_demo$recoded.race[lgc_checked_box],
    ' + Do not know'
  )

  dtf_demo$recoded.race <- gsub(
    '_ + ', '', dtf_demo$recoded.race, fixed = TRUE
  )
  dtf_demo$recoded.race[
    dtf_demo$recoded.race == '_'
  ] <- NA

  lst_collapsed_categories <- list(
    # American Indian or Alaska Native
    c(
      "American Indian/Native American"
    ),
    # Asian
    c(
      "Asian Indian",
      "Asian Indian + Chinese",
      "Asian Indian + Korean",
      "Chinese",
      "Chinese + Filipino",
      "Chinese + Japanese",
      "Chinese + Korean",
      "Chinese + Other Asian",
      "Chinese + Vietnamese",
      "Filipino",
      "Filipino + Japanese",
      "Filipino + Korean",
      "Filipino + Other Asian",
      "Japanese",
      "Japanese + Korean",
      "Korean",
      "Korean + Other Asian",
      "Other Asian",
      "Vietnamese",
      "Vietnamese + Other Asian"
    ),
    # Black or African American
    c(
      "Black/African American"
    ),
    # Native Hawaiian or other Pacific Islander
    c(
      "Native Hawaiian",
      "Other Pacific Islander"
    ),
    # White
    c(
      "White"
    ),
    # Multiple races
    c(
      "American Indian/Native American + Korean",
      "American Indian/Native American + Other race",
      "Asian Indian + Other race",
      "Black/African American + American Indian/Native American",
      paste0(
        "Black/African American + American Indian/Native American + ",
        "Other Pacific Islander",
        "Samoan",
        "Samoan + Other Pacific Islander"
      ),
      paste0(
        "Black/African American + American Indian/Native American + ",
        "Other race"
      ),
      "Black/African American + Asian Indian",
      "Black/African American + Chinese",
      "Black/African American + Filipino",
      "Black/African American + Japanese",
      "Black/African American + Korean",
      "Black/African American + Korean + Other Asian",
      "Black/African American + Korean + Other race",
      "Black/African American + Native Hawaiian + Filipino",
      "Black/African American + Other Asian",
      "Black/African American + Other Asian + Other race",
      "Black/African American + Other Pacific Islander",
      "Black/African American + Other race",
      "Black/African American + Vietnamese",
      "Black/African American + Vietnamese + Other Asian",
      "Chinese + Japanese + Other race",
      "White + Alaska Native",
      "White + American Indian/Native American",
      "White + American Indian/Native American + Alaska Native",
      "White + American Indian/Native American + Chinese",
      "White + American Indian/Native American + Chinese + Filipino",
      "White + American Indian/Native American + Filipino",
      "White + American Indian/Native American + Filipino + Japanese",
      "White + American Indian/Native American + Korean",
      paste0(
        "White + American Indian/Native American + Native Hawaiian + ",
        "Other Pacific Islander"
      ),
      paste0(
        "White + American Indian/Native American + Native Hawaiian + ",
        "Other race"
      ),
      "White + American Indian/Native American + Other Pacific Islander",
      "White + American Indian/Native American + Other race",
      "White + American Indian/Native American + Vietnamese",
      "White + Asian Indian",
      "White + Asian Indian + Other race",
      "White + Black/African American",
      "White + Black/African American + American Indian/Native American",
      paste0(
        "White + Black/African American + American Indian/Native ",
        "American + Chinese"
      ),
      paste0(
        "White + Black/African American + American Indian/Native ",
        "American + Filipino + Other race"
      ),
      paste0(
        "White + Black/African American + American Indian/Native ",
        "American + Japanese"
      ),
      paste0(
        "White + Black/African American + American Indian/Native ",
        "American + Korean"
      ),
      paste0(
        "White + Black/African American + American Indian/Native ",
        "American + Other Asian + Other race"
      ),
      paste0(
        "White + Black/African American + American Indian/Native ",
        "American + Other Pacific Islander"
      ),
      paste0(
        "White + Black/African American + American Indian/Native ",
        "American + Other race"
      ),
      "White + Black/African American + Asian Indian",
      "White + Black/African American + Chinese",
      "White + Black/African American + Chinese + Other race",
      "White + Black/African American + Filipino",
      "White + Black/African American + Japanese",
      "White + Black/African American + Korean",
      paste0(
        "White + Black/African American + Native Hawaiian + ",
        "Chinese + Filipino"
      ),
      "White + Black/African American + Other Asian",
      "White + Black/African American + Other Asian + Other race",
      paste0(
        "White + Black/African American + Other Pacific Islander + ",
        "Other race"
      ),
      "White + Black/African American + Other race",
      "White + Black/African American + Samoan",
      "White + Black/African American + Vietnamese",
      "White + Chinese",
      "White + Chinese + Filipino",
      "White + Chinese + Japanese",
      "White + Chinese + Japanese + Korean",
      "White + Chinese + Japanese + Vietnamese",
      "White + Chinese + Other Asian",
      "White + Chinese + Other race",
      "White + Chinese + Vietnamese",
      "White + Chinese + Vietnamese + Other Asian",
      "White + Filipino",
      "White + Filipino + Japanese",
      "White + Filipino + Other Asian",
      "White + Filipino + Other race",
      "White + Guamanian",
      "White + Japanese",
      "White + Japanese + Other Asian",
      "White + Japanese + Other race",
      "White + Korean",
      "White + Korean + Other Asian",
      "White + Korean + Other race",
      "White + Native Hawaiian",
      "White + Native Hawaiian + Chinese",
      "White + Native Hawaiian + Chinese + Filipino",
      "White + Native Hawaiian + Chinese + Korean",
      "White + Native Hawaiian + Filipino",
      "White + Native Hawaiian + Filipino + Other race",
      "White + Other Asian",
      "White + Other Asian + Other race",
      "White + Other Pacific Islander",
      "White + Other Pacific Islander + Chinese",
      "White + Other Pacific Islander + Filipino",
      "White + Other race",
      "White + Samoan",
      "White + Samoan + Japanese",
      "White + Samoan + Other race",
      "White + Vietnamese",
      "Chinese + Other race",
      "Filipino + Other race",
      "Japanese + Other race",
      "Native Hawaiian + Asian Indian",
      "Native Hawaiian + Other race",
      "Other Asian + Other race",
      "Other Pacific Islander + Asian Indian",
      "Other Pacific Islander + Filipino",
      "Other Pacific Islander + Other race",
      "Guamanian + Filipino"
    ),
    # Other
    c(
      "Other race"
    )
  )
  chr_race_NIH_2015 <- c(
    'American Indian or Alaska Native',
    'Asian',
    'Black or African American',
    'Native Hawaiian or other Pacific Islander',
    'White',
    'Multiple races',
    'Other'
  )

  dtf_demo$recoded.race_NIH_2015 <- abcddata_replace(
    dtf_demo$recoded.race,
    unlist( lst_collapsed_categories ),
    lapply(
      1:7, function(l) {
        rep( chr_race_NIH_2015[l], length( lst_collapsed_categories[[l]] ) )
      }
    ) |> unlist()
  )

  dtf_demo$recoded.race_ethnicity <- abcddata_replace(
    dtf_demo$race_ethnicity,
    c( 1:5 ),
    c(
      'White', # 1
      'Black', # 2
      'Hispanic', # 3
      'Asian', # 4
      'Other' # 5
    )
  )

  #### 2.1.2.4) Ethnicity ####

  dtf_demo$recoded.ethnicity <- abcddata_replace(
    dtf_demo$demo_ethn_v2,
    c( 1, 2, 777, 999 ),
    c(
      'Hispanic or Latino/a',
      'Not Hispanic or Latino/a',
      'Refuse to answer',
      'Do not know'
    )
  )

  #### 2.1.2.5) Income ####

  dtf_demo$recoded.combined_income_baseline <- abcddata_replace(
    dtf_demo$demo_comb_income_v2,
    c( 1:10, 999, 777 ),
    c(
      'Less than $5000', # 1
      '$5000 through $11999', # 2
      '$12000 through $15999', # 3
      '$16000 through $24999', # 4
      '$25000 through $34999', # 5
      '$35000 through $49999', # 6
      '$50000 through $74999', # 7
      '$75000 through $99999', # 8
      '$100000 through $199999', # 9
      '$200000 and greater', # 10
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  dtf_demo$recoded.combined_income <- abcddata_replace(
    dtf_demo$demo_comb_income_v2_l,
    c( 1:10, 999, 777 ),
    c(
      'Less than $5000', # 1
      '$5000 through $11999', # 2
      '$12000 through $15999', # 3
      '$16000 through $24999', # 4
      '$25000 through $34999', # 5
      '$35000 through $49999', # 6
      '$50000 through $74999', # 7
      '$75000 through $99999', # 8
      '$100000 through $199999', # 9
      '$200000 and greater', # 10
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  # Combine baseline and post-baseline
  lgc_baseline <-
    is.na( dtf_demo$recoded.combined_income ) &
    !is.na( dtf_demo$recoded.combined_income_baseline )
  dtf_demo$recoded.combined_income[
    lgc_baseline
  ] <- dtf_demo$recoded.combined_income_baseline[
    lgc_baseline
  ]

  # Collapse categories due to small cell counts
  dtf_demo$recoded.combined_income_collapsed_8 <- abcddata_replace(
    dtf_demo$recoded.combined_income,
    c(
      'Less than $5000', # 1
      '$5000 through $11999', # 2
      '$12000 through $15999', # 3
      '$16000 through $24999', # 4

      '$25000 through $34999', # 5
      '$35000 through $49999', # 6

      '$50000 through $74999', # 7

      '$75000 through $99999', # 8

      '$100000 through $199999', # 9

      '$200000 and greater', # 10

      'Do not know', # 999
      'Refuse to answer' # 777
    ),
    c(
      '<$25k', # 1
      '<$25k', # 2
      '<$25k', # 3
      '<$25k', # 4

      '$25k - $49k', # 5
      '$25k - $49k', # 6

      '$50k - $74k', # 7

      '$75k - $99k', # 8

      '$100k - $199k', # 9

      '$200k+', # 10

      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  # Collapse categories due to small cell counts
  dtf_demo$recoded.combined_income_collapsed_4 <- abcddata_replace(
    dtf_demo$recoded.combined_income,
    c(
      'Less than $5000', # 1
      '$5000 through $11999', # 2
      '$12000 through $15999', # 3
      '$16000 through $24999', # 4
      '$25000 through $34999', # 5
      '$35000 through $49999', # 6

      '$50000 through $74999', # 7
      '$75000 through $99999', # 8

      '$100000 through $199999', # 9
      '$200000 and greater', # 10

      'Do not know', # 999
      'Refuse to answer' # 777
    ),
    c(
      '<$50k', # 1
      '<$50k', # 2
      '<$50k', # 3
      '<$50k', # 4
      '<$50k', # 5
      '<$50k', # 6

      '$50k - $99k', # 7
      '$50k - $99k', # 8

      '$100k+', # 9
      '$100k+', # 10

      'Not provided', # 999
      'Not provided' # 777
    )
  )

  #### 2.1.2.6) Marital status ####

  dtf_demo$recoded.marital_status_baseline <- abcddata_replace(
    dtf_demo$demo_prnt_marital_v2,
    c( 1:6, 999, 777 ),
    c(
      'Married', # 1
      'Widowed', # 2
      'Divorced', # 3
      'Separated', # 4
      'Never married', # 5
      'Living with partner', # 6
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  dtf_demo$recoded.marital_status <- abcddata_replace(
    dtf_demo$demo_prnt_marital_v2_l,
    c( 1:6, 999, 777 ),
    c(
      'Married', # 1
      'Widowed', # 2
      'Divorced', # 3
      'Separated', # 4
      'Never married', # 5
      'Living with partner', # 6
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  # Combine baseline and post-baseline
  lgc_baseline <-
    is.na( dtf_demo$recoded.marital_status ) &
    !is.na( dtf_demo$recoded.marital_status_baseline )
  dtf_demo$recoded.marital_status[
    lgc_baseline
  ] <- dtf_demo$recoded.marital_status_baseline[
    lgc_baseline
  ]

  # Collapse categories due to small cell counts
  dtf_demo$recoded.marital_status_collapsed_4 <- abcddata_replace(
    dtf_demo$recoded.marital_status,
    c(
      'Married', # 1
      'Widowed', # 2
      'Divorced', # 3
      'Separated', # 4
      'Never married', # 5
      'Living with partner', # 6
      'Do not know', # 999
      'Refuse to answer' # 777
    ),
    c(
      'Married', # 1
      'Other family type', # 2
      'Other family type', # 3
      'Other family type', # 4
      'Other family type', # 5
      'Other family type', # 6
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  #### 2.1.2.7) Parental employment ####

  dtf_demo$recoded.employment_status_baseline <- abcddata_replace(
    dtf_demo$demo_prnt_empl_v2,
    c( 1, 2, 9, 10, 3, 11, 4, 5, 6, 7, 8, 777 ),
    c(
      'Working now - Full/part time', # 1
      'Temporarily laid off', # 2
      'Sick leave', # 9
      'Maternity leave', # 10
      'Looking for work', # 3
      'Unemployed - not looking for work', # 11
      'Retired', # 4
      'Disabled - permanently/temporarily', # 5
      'Stay at home parent', # 6
      'Student', # 7
      'Other', # 8
      'Refuse to answer' # 777
    )
  )

  dtf_demo$recoded.employment_status <- abcddata_replace(
    dtf_demo$demo_prnt_empl_v2_l,
    c( 1, 2, 9, 10, 3, 11, 4, 5, 6, 7, 8, 777 ),
    c(
      'Working now - Full/part time', # 1
      'Temporarily laid off', # 2
      'Sick leave', # 9
      'Maternity leave', # 10
      'Looking for work', # 3
      'Unemployed - not looking for work', # 11
      'Retired', # 4
      'Disabled - permanently/temporarily', # 5
      'Stay at home parent', # 6
      'Student', # 7
      'Other', # 8
      'Refuse to answer' # 777
    )
  )

  # Combine baseline and post-baseline
  lgc_baseline <-
    is.na( dtf_demo$recoded.employment_status ) &
    !is.na( dtf_demo$recoded.employment_status_baseline )
  dtf_demo$recoded.employment_status[
    lgc_baseline
  ] <- dtf_demo$recoded.employment_status_baseline[
    lgc_baseline
  ]

  dtf_demo$recoded.employment_status_partner_baseline <- abcddata_replace(
    dtf_demo$demo_prtnr_empl_v2,
    c( 1, 2, 9, 10, 3, 11, 4, 5, 6, 7, 8, 999, 777 ),
    c(
      'Working now - Full/part time', # 1
      'Temporarily laid off', # 2
      'Sick leave', # 9
      'Maternity leave', # 10
      'Looking for work', # 3
      'Unemployed - not looking for work', # 11
      'Retired', # 4
      'Disabled - permanently/temporarily', # 5
      'Stay at home parent', # 6
      'Student', # 7
      'Other', # 8
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  dtf_demo$recoded.employment_status_partner <- abcddata_replace(
    dtf_demo$demo_comb_income_v2_l,
    c( 1, 2, 9, 10, 3, 11, 4, 5, 6, 7, 8, 999, 777 ),
    c(
      'Working now - Full/part time', # 1
      'Temporarily laid off', # 2
      'Sick leave', # 9
      'Maternity leave', # 10
      'Looking for work', # 3
      'Unemployed - not looking for work', # 11
      'Retired', # 4
      'Disabled - permanently/temporarily', # 5
      'Stay at home parent', # 6
      'Student', # 7
      'Other', # 8
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  # Combine baseline and post-baseline
  lgc_baseline <-
    is.na( dtf_demo$recoded.employment_status_partner ) &
    !is.na( dtf_demo$recoded.employment_status_partner_baseline )
  dtf_demo$recoded.employment_status_partner[
    lgc_baseline
  ] <- dtf_demo$recoded.employment_status_partner_baseline[
    lgc_baseline
  ]

  # Create collapsed measure
  dtf_demo$recoded.employment_status_collapsed <- 'None in labor force'

  # Both in labor force
  lgc_cases <-
    dtf_demo$recoded.employment_status %in% c(
      'Working now - Full/part time'
    ) &
    dtf_demo$recoded.employment_status_partner %in% c(
      'Working now - Full/part time'
    )
  dtf_demo$recoded.employment_status_collapsed[lgc_cases] <-
    'Both in labor force'

  # One in labor force
  lgc_cases <-
    ( dtf_demo$recoded.employment_status %in% c(
        'Working now - Full/part time'
      ) &
      !dtf_demo$recoded.employment_status_partner %in% c(
        'Working now - Full/part time'
      )
    ) | (
      !dtf_demo$recoded.employment_status %in% c(
        'Working now - Full/part time'
      ) &
      dtf_demo$recoded.employment_status_partner %in% c(
        'Working now - Full/part time'
      )
    )
  dtf_demo$recoded.employment_status_collapsed[lgc_cases] <-
    'One in labor force'

  # Missing values
  lgc_cases <-
    dtf_demo$recoded.employment_status_collapsed %in% c(
      'None in labor force'
    ) & (
      is.na( dtf_demo$recoded.employment_status ) |
      dtf_demo$recoded.employment_status %in% 'Refused to answer'
    ) & (
      is.na( dtf_demo$recoded.employment_status_partner ) |
        dtf_demo$recoded.employment_status_partner %in% 'Refused to answer'
    )
  dtf_demo$recoded.employment_status_collapsed[lgc_cases] <-
    NA

  #### 2.1.2.8) Parental education ####

  dtf_demo$recoded.education_baseline <- abcddata_replace(
    dtf_demo$demo_prnt_ed_v2,
    c( 0:21, 999, 777 ),
    c(
      'Never attended or Kindergarten only', # 0
      '1st grade', # 1
      '2nd grade', # 2
      '3rd grade', # 3
      '4th grade', # 4
      '5th grade', # 5
      '6th grade', # 6
      '7th grade', # 7
      '8th grade', # 8
      '9th grade', # 9
      '10th grade', # 10
      '11th grade', # 11
      '12th grade', # 12
      'High school graduate', # 13
      'General education degree or equivalent diploma', # 14
      'Some college', # 15
      'Associate degree - occupational', # 16
      'Associate degree - academic program', # 17
      'Bachelor degree (e.g., BA)', # 18
      'Master degree (e.g., MA)', # 19
      'Professional school degree (e.g., MD)', # 20
      'Doctoral degree (e.g., PhD)', # 21
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  dtf_demo$recoded.education <- abcddata_replace(
    dtf_demo$demo_prnt_ed_v2_l,
    c( 0:21, 999, 777 ),
    c(
      'Never attended or Kindergarten only', # 0
      '1st grade', # 1
      '2nd grade', # 2
      '3rd grade', # 3
      '4th grade', # 4
      '5th grade', # 5
      '6th grade', # 6
      '7th grade', # 7
      '8th grade', # 8
      '9th grade', # 9
      '10th grade', # 10
      '11th grade', # 11
      '12th grade', # 12
      'High school graduate', # 13
      'General education degree or equivalent diploma', # 14
      'Some college', # 15
      'Associate degree - occupational', # 16
      'Associate degree - academic program', # 17
      'Bachelor degree (e.g., BA)', # 18
      'Master degree (e.g., MA)', # 19
      'Professional school degree (e.g., MD)', # 20
      'Doctoral degree (e.g., PhD)', # 21
      'Do not know', # 999
      'Refuse to answer' # 777
    )
  )

  # Combine baseline and post-baseline
  lgc_baseline <-
    is.na( dtf_demo$recoded.education ) &
    !is.na( dtf_demo$recoded.education_baseline )
  dtf_demo$recoded.education[
    lgc_baseline
  ] <- dtf_demo$recoded.education_baseline[
    lgc_baseline
  ]

  #### 2.1.3) Add variables ####

  dtf_ABCD_long_form <- abcddata_merge_data_sets(
    dtf_ABCD_long_form,
    dtf_demo,
    list(
      c( 'IDS.CHR.GD.Participant', 'src_subject_id' ),
      c( 'SSS.CHR.GD.Time_point', 'eventname' )
    ),
    c(
      SMP.CHR.PC.Sex.At_birth =
        'recoded.sex_at_birth',
      SMP.CHR.PC.Gender =
        'recoded.current_gender',
      SMP.CHR.PC.Race_NIH_2015 =
        'recoded.race_NIH_2015',
      # SMP.CHR.PC.Race_and_ethnicity =
      #   'recoded.race_ethnicity',
      SMP.CHR.PC.Ethnicity =
        'recoded.ethnicity',
      SMP.CHR.PS.Income.Combined =
        'recoded.combined_income',
      SMP.CHR.PS.Income.Combined.Collapsed_8 =
        'recoded.combined_income_collapsed_8',
      SMP.CHR.PS.Income.Combined.Collapsed_4 =
        'recoded.combined_income_collapsed_4',
      SMP.CHR.PS.Marital_status =
        'recoded.marital_status',
      SMP.CHR.PS.Marital_status.Collapsed_4 =
        'recoded.marital_status_collapsed_4',
      SMP.CHR.PS.Employment_status =
        'recoded.employment_status',
      SMP.CHR.PS.Employment_status_partner =
        'recoded.employment_status_partner',
      SMP.CHR.PS.Employment_status.Collapsed_3 =
        'recoded.employment_status_collapsed',
      SMP.CHR.PS.Highest_education_level =
        'recoded.education'
    ),
    lgc_progress = lgc_progress
  )

  #### 2.1.4) Additional processing ####


  dtf_ABCD_long_form <- abcddata_propagate(
    dtf_ABCD_long_form,
    'IDS.CHR.GD.Participant',
    c( 'SMP.CHR.PC.Sex.At_birth',
       'SMP.CHR.PC.Race_NIH_2015',
       'SMP.CHR.PC.Ethnicity' ),
    lgc_progress = lgc_progress
  )


  #### 2.1.5) Codebook entries ####

  lst_codebook_entries <- list(

    SMP.CHR.PC.Sex.At_birth = list(
      chr_description = paste0(
        "Sex of child assigned at birth on original birth certificate"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "demo_sex_v2"
    ),

    SMP.CHR.PC.Gender = list(
      chr_description = paste0(
        "The current gender identity of the child"
      ),
      lst_collected_over = abcddata_codebook_collected_over(
        dtf_ABCD_long_form,
        'SMP.CHR.PC.Gender',
        'SSS.DBL.GD.Year'
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = c(
        "demo_gender_id_v2",
        "demo_gender_id_v2_l"
      )
    ),

    SMP.CHR.PC.Race_NIH_2015 = list(
      chr_description = paste0(
        "Race of child based on National Institute of Health 2015 standards"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = c(
        "demo_race_a_p___10",
        "demo_race_a_p___11",
        "demo_race_a_p___12",
        "demo_race_a_p___13",
        "demo_race_a_p___14",
        "demo_race_a_p___15",
        "demo_race_a_p___16",
        "demo_race_a_p___17",
        "demo_race_a_p___18",
        "demo_race_a_p___19",
        "demo_race_a_p___20",
        "demo_race_a_p___21",
        "demo_race_a_p___22",
        "demo_race_a_p___23",
        "demo_race_a_p___24",
        "demo_race_a_p___25",
        "demo_race_a_p___26"
      )
    ),

    SMP.CHR.PC.Ethnicity = list(
      chr_description = paste0(
        "Ethnicity of child"
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = "demo_ethn_v2"
    ),

    SMP.CHR.PS.Income.Combined = list(
      chr_description = paste0(
        "The combined income of the parents"
      ),
      lst_collected_over = abcddata_codebook_collected_over(
        dtf_ABCD_long_form,
        'SMP.CHR.PS.Income.Combined',
        'SSS.DBL.GD.Year'
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = c(
        "demo_comb_income_v2",
        "demo_comb_income_v2_l"
      )
    ),

    SMP.CHR.PS.Income.Combined.Collapsed_8 = list(
      chr_description = paste0(
        "The combined income of the parents - collapsed to 8 categories"
      ),
      lst_collected_over = abcddata_codebook_collected_over(
        dtf_ABCD_long_form,
        'SMP.CHR.PS.Income.Combined.Collapsed_8',
        'SSS.DBL.GD.Year'
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = c(
        "demo_comb_income_v2",
        "demo_comb_income_v2_l"
      )
    ),

    SMP.CHR.PS.Income.Combined.Collapsed_4 = list(
      chr_description = paste0(
        "The combined income of the parents - collapsed to 4 categories"
      ),
      lst_collected_over = abcddata_codebook_collected_over(
        dtf_ABCD_long_form,
        'SMP.CHR.PS.Income.Combined.Collapsed_4',
        'SSS.DBL.GD.Year'
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = c(
        "demo_comb_income_v2",
        "demo_comb_income_v2_l"
      )
    ),

    SMP.CHR.PS.Marital_status = list(
      chr_description = paste0(
        "The marital status of the parents"
      ),
      lst_collected_over = abcddata_codebook_collected_over(
        dtf_ABCD_long_form,
        'SMP.CHR.PS.Marital_status',
        'SSS.DBL.GD.Year'
      ),
      chr_source_files = chr_files[1],
      chr_source_variables = c(
        "demo_prnt_marital_v2",
        "demo_prnt_marital_v2_l"
      )
    )

  )

  # Loop over codebook entries
  for ( l in seq_along(lst_codebook_entries) ) {

    lst_arg <- lst_codebook_entries[[l]]
    lst_arg$dtf_base <- dtf_ABCD_long_form
    lst_arg$chr_column <- names( lst_codebook_entries )[l]

    dtf_ABCD_long_form <- do.call(
      abcddata_codebook_add_entry,
      lst_arg
    )

    # Close 'Loop over codebook entries'
  }

  #### 2.1.6) Output ####

  return( dtf_ABCD_long_form )
}



#### 2.2) abcddata_add.UPPS ####
#' Add Data for the UPPS-P Scale
#'
#' Function to add individual items and summed scores
#' for subscales of the UPPS-P Impulsive Behavior
#' Scale for Children. The ABCD® study used a modified
#' 20 item version of the scale developed by
#' Zapolski et al. (2010). There were 5 subscales
#' (Negative urgency, perseverance, premeditation,
#' sensation seeking, and positive urgency)
#' consisting of 4 items each on with a response format
#' of 1 (Not at all like me) to 4 (Very much like me)
#' coded so higher scores always indicate greater
#' impulsivity. See Barch et al. (2018) for further
#' details on the use of the UPPS-P scale in the
#' ABCD® study.
#'
#' @param dtf_ABCD_long_form A data frame, output from
#'   the [abcddata::abcddata_initialize_long_form]
#'   function.
#' @param chr_files A character vector with
#'   the file name for the data with responses
#'   to the UPPS-P items.
#' @param chr_paths A character vector with
#'   the associated folder path for the
#'   UPPS-P responses file.
#' @param lgc_progress A logical value; if
#'   \code{TRUE} displays the progress of the
#'   data processing.
#'
#' @references
#' Barch, D. M., Albaugh, M. D., Avenevoli, S., Chang, L., Clark,
#'   D. B., Glantz, M. D., Hudziak, J. J., Jernigan, T. L.,
#'   Tapert, S. F., Yurgelun-Todd, D., Alia-Klein, N., Potter,
#'   A. S., Paulus, M. P., Prouty, D., Zucker, R. A., & Sher,
#'   K. J. (2018). Demographic, physical and mental health
#'   assessments in the Adolescent Brain and Cognitive Development
#'   study: Rationale and description. Developmental Cognitive
#'   Neuroscience, 32, 55–66. https://doi.org/10.1016/j.dcn.2017.10.010
#'
#' Zapolski, T. C. B., Stairs, A. M., Settles, R. F., Combs,
#'   J. L., & Smith, G. T. (2010). The measurement of dispositions
#'   to rash action in children. Assessment, 17 (1), 116-125.
#'   https://doi.org/10.1177/1073191109351372
#'
#' @returns A data frame.
#'
#' @export

abcddata_add.UPPS <- function(
    dtf_ABCD_long_form,
    chr_files = 'mh_y_upps.csv',
    chr_paths = 'core/mental-health',
    lgc_progress = TRUE ) {

  #### 2.2.1) Load in data ####

  # Read in file
  chr_full_path <- chr_files[1]
  if ( chr_paths[1] != '' ) {
    chr_full_path <- paste0( chr_paths[1], '/', chr_files[1] )
  }
  dtf_upps <- read.csv(
    file = chr_full_path,
    header = TRUE
  )

  #### 2.2.2) Individual items ####

  int_items <- c(
    6,  7, 11, 12, 15,
    16, 17, 18, 19, 20,
    21, 22, 23, 24, 27,
    28, 35, 36, 37, 39
  )

  lst_response_format <- list(
    content = 1:4,
    additional_content = c(
      'Not at all like me',
      'Not like me',
      'Somewhat like me',
      'Very much like me'
    )
  )
  lst_response_format_reverse <- list(
    content = 1:4,
    additional_content = rev( lst_response_format$additional_content )
  )

  chr_items <- paste0(
    'upps',
    int_items,
    '_y'
  )
  names( chr_items ) <- paste0(
    'QST.INT.CS.UPPS.Item_', int_items
  )

  dtf_ABCD_long_form <- abcddata_merge_data_sets(
    dtf_ABCD_long_form,
    dtf_upps,
    list(
      c( 'IDS.CHR.GD.Participant', 'src_subject_id' ),
      c( 'SSS.CHR.GD.Time_point', 'eventname' )
    ),
    chr_items,
    lgc_progress = lgc_progress
  )

  #### 2.2.3) Summed scores for subscales ####

  chr_columns <- paste0(
    'QST.INT.CS.UPPS.Item_', c( 7, 11, 17, 20 )
  )
  dtf_ABCD_long_form$QST.INT.CS.UPPS.Negative_urgency.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )


  chr_columns <- paste0(
    'QST.INT.CS.UPPS.Item_', c( 15, 19, 22, 24 )
  )
  dtf_ABCD_long_form$QST.INT.CS.UPPS.Perseverance.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )


  chr_columns <- paste0(
    'QST.INT.CS.UPPS.Item_', c( 6, 16, 23, 28 )
  )
  dtf_ABCD_long_form$QST.INT.CS.UPPS.Premeditation.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )


  chr_columns <- paste0(
    'QST.INT.CS.UPPS.Item_', c( 12, 18, 21, 27 )
  )
  dtf_ABCD_long_form$QST.INT.CS.UPPS.Sensation_seeking.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )


  chr_columns <- paste0(
    'QST.INT.CS.UPPS.Item_', c( 35, 36, 37, 39 )
  )
  dtf_ABCD_long_form$QST.INT.CS.UPPS.Positive_urgency.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )

  #### 2.2.4) Codebook entries ####

  # Initialize list for codebook entries
  # 20 items, 5 subscales
  lst_codebook_entries <- lapply(
    1:25, function(x) return(NULL)
  )

  # Column names
  names( lst_codebook_entries ) <- c(
    names( chr_items ),
    paste0(
      'QST.INT.CS.UPPS.',
      c(
        'Negative_urgency',
        'Perseverance',
        'Premeditation',
        'Sensation_seeking',
        'Positive_urgency'
      ),
      '.Summed_score'
    )
  )

  # Timepoints at which data were collected
  lst_collected_over <- abcddata_codebook_collected_over(
    dtf_ABCD_long_form,
    'QST.INT.CS.UPPS.Negative_urgency.Summed_score',
    'SSS.DBL.GD.Year'
  )

  # Individual items
  chr_item_content <- c(

    # Negative urgency
    Item_7 = paste0(
      "When I feel bad I often do things I later regret ",
      "in order to make myself feel better now"
    ),
    Item_11 = paste0(
      "Sometimes when I feel bad I keep doing something ",
      "even though it is making me feel worse"
    ),
    Item_17 = paste0(
      "When I am upset I often act without thinking"
    ),
    Item_20 = paste0(
      "When I feel rejected I often say things that I later regret"
    ),

    # Perseverance (lack of)
    Item_15 = paste0( # Reverse coded
      "I finish what I start"
    ),
    Item_19 = paste0( # Reverse coded
      "I tend to get things done on time"
    ),
    Item_22 = paste0( # Reverse coded
      "I am a person who always gets the job done"
    ),
    Item_24 = paste0( # Reverse coded
      "I almost always finish projects that I start"
    ),

    # Premeditation (lack of)
    Item_6 = paste0( # Reverse coded
      "I like to stop and think about things before I do it"
    ),
    Item_16 = paste0( # Reverse coded
      "I try to take a careful approach to things"
    ),
    Item_23 = paste0( # Reverse coded
      "I am very careful"
    ),
    Item_28 = paste0( # Reverse coded
      "I tend to stop and think before doing things"
    ),

    # Sensation seeking
    Item_12 = paste0(
      "I enjoy taking risks"
    ),
    Item_18 = paste0(
      "I like new thrilling things even if they ",
      "are a little scary"
    ),
    Item_21 = paste0(
      "I would like to learn to fly an airplane"
    ),
    Item_27 = paste0(
      "I would like to ski very fast down a high mountain slope"
    ),

    # Positive urgency
    Item_35 = paste0(
      "When I am in a great mood I tend to do things that ",
      "can cause me problems"
    ),
    Item_36 = paste0(
      "I tend to act without thinking when I am very very happy"
    ),
    Item_37 = paste0(
      "When I get really happy about something I tend to do ",
      "things that can lead to trouble"
    ),
    Item_39 = paste0(
      "I tend to lose control when I am in a great mood"
    )

  )

  # Starting text for each item entry
  chr_item_description <- paste0(
    "Individual item for the modified UPPS-P Impulsive Behavior ",
    "Scale for Children: "
  )

  # Loop over individual items
  for ( i in 1:20 ) {

    lst_codebook_entries[[i]] <- list(
      chr_description =
        paste0( chr_item_description, chr_item_content[i] ),
      lst_values_and_labels = switch(
        # Identify reverse coded items
        as.numeric(
          names(chr_item_content)[i] %in% names(chr_item_content)[5:12]
        ) + 1,
        lst_response_format,
        lst_response_format_reverse
      ),
      lst_collected_over = lst_collected_over,
      chr_source_files = chr_files[1],
      chr_source_variables = chr_items[i]
    )

    # Close 'Loop over individual items'
  }

  # Description for each subscale
  chr_subscale_description <- paste0(
    "Summed scores for the 4-item ",
    c(
      "negative urgency ",
      "perseverance (lack of)",
      "premeditation (lack of)",
      "sensation seeking",
      "positive urgency"
    ),
    " subscale from the modified UPPS-P Impulsive Behavior ",
    "Scale for Children. Higher scores indicate ",
    c(
      "a greater tendency towards rashness during negative moods",
      "a greater inability to remain focused on a task",
      "a greater tendency to act without thinking beforehand",
      "a greater willingness to seek out novel and thrilling experiences",
      "a greater tendency towards rashness during positive moods"
    ),
    "."
  )

  # Loop over subscales
  for ( i in 1:5 ) {

    lst_codebook_entries[[20 + i]] <- list(
      chr_description =
        chr_subscale_description[i],
      lst_collected_over = lst_collected_over,
      chr_source_files = chr_files[1],
      chr_source_variables = chr_items[
        list(
          c( 7, 11, 17, 20 ),
          c( 15, 19, 22, 24 ),
          c( 6, 16, 23, 28 ),
          c( 12, 18, 21, 27 ),
          c( 35, 36, 37, 39 )
        )[[i]]
      ]
    )

    # Close 'Loop over individual items'
  }

  # Loop over codebook entries
  for ( l in seq_along(lst_codebook_entries) ) {

    lst_arg <- lst_codebook_entries[[l]]
    lst_arg$dtf_base <- dtf_ABCD_long_form
    lst_arg$chr_column <- names( lst_codebook_entries )[l]

    dtf_ABCD_long_form <- do.call(
      abcddata_codebook_add_entry,
      lst_arg
    )

    # Close 'Loop over codebook entries'
  }

  return( dtf_ABCD_long_form )
}

#### 2.3) abcddata_add.substance_use ####

abcddata_add.substance_use <- function(
    dtf_ABCD_long_form,
    chr_files = 'su_y_sui.csv',
    chr_paths = 'core/substance-use',
    lgc_progress = TRUE ) {

  #### 2.3.1) Load in data ####

  # Read in demographics file
  chr_full_path <- chr_files[1]
  if ( chr_paths[1] != '' ) {
    chr_full_path <- paste0( chr_paths[1], '/', chr_files[1] )
  }
  dtf_sui <- read.csv(
    file = chr_full_path,
    header = TRUE
  )

  # Initialize columns
  # ALC = Alcohol
  dtf_ABCD_long_form$SUB.CHR.CS.ALC.Type_of_use <- ''
  # NCT = Nicotine or tobacco
  dtf_ABCD_long_form$SUB.CHR.CS.NCT.Type_of_use <- ''
  # CNN = Cannabis
  dtf_ABCD_long_form$SUB.CHR.CS.CNN.Type_of_use <- ''
  # OTH = Other substances
  dtf_ABCD_long_form$SUB.CHR.CS.OTH.Type_of_use <- ''

  #### 2.3.2) Variables for heard of items ####

  lst_column.heard_of <- list(
    SUB.CHR.CS.ALC.Type_of_use = c(
      'tlfb_alc',
      'tlfb_alc_l'
    ),
    SUB.CHR.CS.NCT.Type_of_use = c(
      'tlfb_tob',
      'tlfb_tob_l'
    ),
    SUB.CHR.CS.CNN.Type_of_use = c(
      'tlfb_mj',
      'tlfb_mj_l'
    ),
    SUB.CHR.CS.OTH.Type_of_use = c(

      'tlfb_mj_synth', # Synthetic MJ [K2 or spice]
      'tlfb_mj_synth_l',

      'tlfb_bitta', # Bittamugen or byphoditin
      'tlfb_bitta_l',

      'tlfb_inhalant', # Sniffing products to get high
      'tlfb_inhalant_l',

      'tlfb_rx_misuse', # Prescription med abuse
      'tlfb_rx_misuse_l',

      'tlfb_list_yes_no',
      'tlfb_list_yes_no_l',

      'tlfb_list___1', # Cocaine
      'tlfb_list___2', # Bath salts
      'tlfb_list___3', # Meth
      'tlfb_list___4', # Ecstasy
      'tlfb_list___5', # Ketamine
      'tlfb_list___6', # GBH
      'tlfb_list___7', # Heroin
      'tlfb_list___8', # Hallucinogens
      'tlfb_list___9', # Mushrooms
      'tlfb_list___10', # Salvia
      'tlfb_list___11'  # Steroids

    )
  )

  #### 2.3.3) Variables for experimentation ####

  lst_column.experimentation <- list(
    SUB.CHR.CS.ALC.Type_of_use = c(
      'tlfb_alc_sip',
      'tlfb_alc_sip_l'
    ),
    SUB.CHR.CS.NCT.Type_of_use = c(
      'tlfb_tob_puff',
      'tlfb_tob_puff_l'
    ),
    SUB.CHR.CS.CNN.Type_of_use = c(
      'tlfb_mj_puff',
      'tlfb_mj_puff_l'
    ),
    SUB.CHR.CS.OTH.Type_of_use = c(

      "tlfb_mj_synth_use", # Synthetic MJ [K2 or spice]
      "tlfb_mj_synth_use_l",

      "tlfb_bitta_use", # Bittamugen or byphoditin
      "tlfb_bitta_use_l",

      "tlfb_inhalant_use", # Sniffing products to get high
      "tlfb_inhalant_use_l",

      "tlfb_sniff_use",
      # "tlfb_sniff_use_l",

      "tlfb_cough_use", # Prescription med abuse
      "tlfb_cough_use_l",

      "tlfb_tranq_use",
      "tlfb_tranq_use_l",

      "tlfb_vicodin_use",
      "tlfb_vicodin_use_l",

      "tlfb_opi_use",
      "tlfb_opi_use_l",

      "tlfb_coc_use", # Cocaine
      "tlfb_coc_use_l",

      "tlfb_bsalts_use", # Bath salts
      "tlfb_bsalts_use_l",

      "tlfb_meth_use", # Meth
      "tlfb_meth_use_l",

      "tlfb_amp_use", # Ecstasy
      "tlfb_amp_use_l",

      "tlfb_mdma_use",
      "tlfb_mdma_use_l",

      "tlfb_ket_use", # Ketamine
      "tlfb_ket_use_l",

      "tlfb_ghb_use", # GBH
      "tlfb_ghb_use_l",

      "tlfb_hall_use", # Hallucinogens
      # "tlfb_hall_use_l",

      "tlfb_shrooms_use", # Mushrooms
      "tlfb_shrooms_use_l",

      "tlfb_salvia_use", # Salvia
      "tlfb_salvia_use_l",

      "tlfb_steroids_use", # Steroids
      "tlfb_steroids_use_l"

    )
  )

  #### 2.3.4) Variables for initiation ####

  lst_column.initiation <- list(
    SUB.CHR.CS.ALC.Type_of_use = c(
      'tlfb_alc_use',
      'tlfb_alc_use_l',
      'isip_2_2',
      'isip_2_l'
    ),
    SUB.CHR.CS.NCT.Type_of_use = c(
      'tlfb_cig_use',
      'tlfb_cig_use_l',
      'tlfb_ecig_use',
      'tlfb_ecig_use_l',
      'tlfb_chew_use',
      'tlfb_chew_use_l',
      'tlfb_cigar_use',
      'tlfb_cigar_use_l',
      'tlfb_hookah_use',
      'tlfb_hookah_use_l',
      'tlfb_pipes_use',
      'tlfb_pipes_use_l'
    ),
    SUB.CHR.CS.CNN.Type_of_use = c(
      'tlfb_mj_use',
      'tlfb_mj_use_l',
      'tlfb_blunt_use',
      'tlfb_blunt_use_l',
      'tlfb_edible_use',
      'tlfb_edible_use_l',
      'tlfb_mj_conc_use',
      'tlfb_mj_conc_use_l',
      'tlfb_mj_drink_use',
      'tlfb_mj_drink_use_l',
      'tlfb_tincture_use',
      'tlfb_tincture_use_l'
    ),
    SUB.CHR.CS.OTH.Type_of_use = c(

      "tlfb_mj_synth_use", # Synthetic MJ [K2 or spice]
      "tlfb_mj_synth_use_l",

      "tlfb_bitta_use", # Bittamugen or byphoditin
      "tlfb_bitta_use_l",

      "tlfb_inhalant_use", # Sniffing products to get high
      "tlfb_inhalant_use_l",

      "tlfb_sniff_use",
      # "tlfb_sniff_use_l",

      "tlfb_cough_use", # Prescription med abuse
      "tlfb_cough_use_l",

      "tlfb_tranq_use",
      "tlfb_tranq_use_l",

      "tlfb_vicodin_use",
      "tlfb_vicodin_use_l",

      "tlfb_opi_use",
      "tlfb_opi_use_l",

      "tlfb_coc_use", # Cocaine
      "tlfb_coc_use_l",

      "tlfb_bsalts_use", # Bath salts
      "tlfb_bsalts_use_l",

      "tlfb_meth_use", # Meth
      "tlfb_meth_use_l",

      "tlfb_amp_use", # Ecstasy
      "tlfb_amp_use_l",

      "tlfb_mdma_use",
      "tlfb_mdma_use_l",

      "tlfb_ket_use", # Ketamine
      "tlfb_ket_use_l",

      "tlfb_ghb_use", # GBH
      "tlfb_ghb_use_l",

      "tlfb_hall_use", # Hallucinogens
      # "tlfb_hall_use_l",

      "tlfb_shrooms_use", # Mushrooms
      "tlfb_shrooms_use_l",

      "tlfb_salvia_use", # Salvia
      "tlfb_salvia_use_l",

      "tlfb_steroids_use", # Steroids
      "tlfb_steroids_use_l"

    )
  )

  #### 2.3.5) Update columns ####

  chr_columns <- names( lst_column.experimentation )

  # Loop over substance categories
  for ( l in seq_along( chr_columns ) ) {

    # Track progress
    if ( lgc_progress ) message( paste0( '  ', chr_columns[l] ) )

    chr_match <- c(
      'IDS.CHR.GD.Participant',
      'SSS.CHR.GD.Time_point'
    )

    dtf_ABCD_long_form[[ chr_columns[l] ]] <- sapply(
      1:nrow( dtf_ABCD_long_form ), function(j) {

        # Initialize output
        chr_out <- NA

        lgc_row <-
          dtf_sui$src_subject_id == dtf_ABCD_long_form[[ chr_match[1] ]][j] &
          dtf_sui$eventname == dtf_ABCD_long_form[[ chr_match[2] ]][j]

        # If data entries exist
        if ( any(lgc_row) ) {

          # Isolate row
          k <- which( lgc_row )

          # Experimentation

          x <- unlist(
            dtf_sui[k, lst_column.experimentation[[l]] ]
          )

          # If non-missing data
          if ( any( !is.na(x) ) ) {

            # If endorsed
            if ( any( x %in% 1 ) ) {

              chr_out <- 'Experimentation'

              # Close 'If endorsed'
            } else {

              chr_out <- 'No use'

              # Close else for 'If endorsed'
            }

            # Close 'If non-missing data'
          } else {

            # Check if heard of substance
            x <- unlist(
              dtf_sui[k, lst_column.heard_of[[l]] ]
            )

            # Any non-missing data
            if ( any( !is.na(x) ) ) {

              # If haven't heard of
              if ( all( x[ !is.na(x) ] %in% 0 ) ) {
                chr_out <- 'No use'
              }

              # Close 'Any non-missing data'
            }

            # Close else for 'If non-missing data'
          }

          # Initiation

          x <- unlist(
            dtf_sui[k, lst_column.initiation[[l]] ]
          )

          # If non-missing data
          if ( any( !is.na(x) ) ) {

            # If endorsed
            if ( any( x %in% 1 ) ) {

              chr_out <- 'Initiation'

              # Close 'If endorsed'
            }

            # Close 'If non-missing data'
          }

          # Close 'If data entries exist'
        }

        return( chr_out )
      }
    )

    # Close 'Loop over substance categories'
  }

  #### 2.3.6) Carry forward use from prior years ####

  chr_ids <- unique( dtf_ABCD_long_form$IDS.CHR.GD.Participant )

  # Loop over participants
  for ( s in chr_ids ) {

    lgc_rows <- dtf_ABCD_long_form$IDS.CHR.GD.Participant == s

    # Loop over substance categories
    for ( p in seq_along( chr_columns ) ) {

      lgc_exp <-
        dtf_ABCD_long_form[[
          chr_columns[p]
        ]][lgc_rows] %in% 'Experimentation'
      lgc_carry_forward <- cumsum(lgc_exp) > 0

      # If use should be carried forward
      if ( any( lgc_carry_forward ) ) {

        dtf_ABCD_long_form[[
          chr_columns[p]
        ]][lgc_rows][
          lgc_carry_forward
        ] <- 'Experimentation'

        # Close 'If use should be carried forward'
      }

      lgc_exp <-
        dtf_ABCD_long_form[[ chr_columns[p] ]][lgc_rows] %in% 'Initiation'
      lgc_carry_forward <- cumsum(lgc_exp) > 0

      # If use should be carried forward
      if ( any( lgc_carry_forward ) ) {

        dtf_ABCD_long_form[[ chr_columns[p] ]][lgc_rows][
          lgc_carry_forward
        ] <- 'Initiation'

        # Close 'If use should be carried forward'
      }

      # Close 'Loop over substance categories'
    }

    # Close 'Loop over participants'
  }

  #### 2.3.7) Overall substance use ####

  dtf_ABCD_long_form$SUB.CHR.CS.ANS.Type_of_use <- sapply(
    1:nrow( dtf_ABCD_long_form ), function(r) {

      x <- unlist( dtf_ABCD_long_form[r, chr_columns] )

      chr_out <- NA

      if ( any( x %in% 'No use' ) ) chr_out <- 'No use'
      if ( any( x %in% 'Experimentation' ) ) chr_out <- 'Experimentation'
      if ( any( x %in% 'Initiation' ) ) chr_out <- 'Initiation'

      return( chr_out )
    }
  )

  #### 2.3.8) Codebook entries ####

  return( dtf_ABCD_long_form )
}

#### 2.4) abcddata_add.BIS_BAS ####
#' Add Data for the BIS/BAS Scale
#'
#' Function to add individual items and summed scores
#' for subscales of the BIS/BAS scale. The ABCD® study
#' used a modified 20-item version of the scale developed by
#' Pagliaccio et al. (2016). There were 4 subscales
#' (BIS with 7 items, BAS: Reward Responsiveness with 5 items,
#' BAS: Drive with 4 items, and BAS: Fun Seeking with 4 items)
#' with a response format of 0 (Not true) to 3 (Very true).
#' The modified scale incorporated the BAS: Fun Seeking
#' subscale from the original measure by Carver and White (1994).
#' See Barch et al. (2018) for further details on the use of
#' the BIS/BAs scale in the ABCD® study.
#'
#' @param dtf_ABCD_long_form A data frame, output from
#'   the [abcddata::abcddata_initialize_long_form]
#'   function.
#' @param chr_files A character vector with
#'   the file name for the data with responses
#'   to the BIS/BAS items.
#' @param chr_paths A character vector with
#'   the associated folder path for the
#'   BIS/BAS responses file.
#' @param lgc_progress A logical value; if
#'   \code{TRUE} displays the progress of the
#'   data processing.
#'
#' @references
#' Barch, D. M., Albaugh, M. D., Avenevoli, S., Chang, L., Clark,
#'   D. B., Glantz, M. D., Hudziak, J. J., Jernigan, T. L.,
#'   Tapert, S. F., Yurgelun-Todd, D., Alia-Klein, N., Potter,
#'   A. S., Paulus, M. P., Prouty, D., Zucker, R. A., & Sher,
#'   K. J. (2018). Demographic, physical and mental health
#'   assessments in the Adolescent Brain and Cognitive Development
#'   study: Rationale and description. Developmental Cognitive
#'   Neuroscience, 32, 55–66. https://doi.org/10.1016/j.dcn.2017.10.010
#'
#' Carver, C. S., and White, T. I. (1994). Behavioral inhibition,
#'   behavioral activation and affective responses to impending
#'   reward and punishment: The BIS/BAS scales. Journal of
#'   Personality and Social Psychology, 67 (2), 319-333.
#'
#' Pagliaccio, D., Luking, K. R., Anokhin, A. P., Gotlib, I. H.,
#'   Hayden, E. P., Olino, T. M., Peng, C.-Z., Hajcak, G., &
#'   Barch, D. M. (2016). Revising the BIS/BAS Scale to study
#'   development: Measurement invariance and normative effects
#'   of age and sex from childhood through adulthood.
#'   Psychological Assessment, 28 (4), 429–442.
#'   https://doi.org/10.1037/pas0000186
#'
#' @returns A data frame.
#'
#' @export

abcddata_add.BIS_BAS <- function(
    dtf_ABCD_long_form,
    chr_files = 'mh_y_bisbas.csv',
    chr_paths = 'core/mental-health',
    lgc_progress = TRUE ) {

  #### 2.4.1) Load in data ####

  # Read in file
  chr_full_path <- chr_files[1]
  if ( chr_paths[1] != '' ) {
    chr_full_path <- paste0( chr_paths[1], '/', chr_files[1] )
  }
  dtf_bis_bas <- read.csv(
    file = chr_full_path,
    header = TRUE
  )

  #### 2.4.2) Individual items ####

  int_items <- 1:20

  lst_response_format <- list(
    content = 0:3,
    additional_content = c(
      'Not true',
      'Somewhat true',
      'True',
      'Very true'
    )
  )

  chr_items <- paste0(
    'bisbas',
    int_items,
    '_y'
  )
  # For item 5 use reverse coded response
  chr_items[ chr_items == 'bisbas5_y' ] <- 'bisbas5r_y'

  names( chr_items ) <- paste0(
    'QST.INT.CS.BISBAS.Item_', int_items
  )

  dtf_ABCD_long_form <- abcddata_merge_data_sets(
    dtf_ABCD_long_form,
    dtf_upps,
    list(
      c( 'IDS.CHR.GD.Participant', 'src_subject_id' ),
      c( 'SSS.CHR.GD.Time_point', 'eventname' )
    ),
    chr_items,
    lgc_progress = lgc_progress
  )

  #### 2.4.3) Summed scores for subscales ####

  chr_columns <- paste0(
    'QST.INT.CS.BISBAS.Item_', c( 1:7 )
  )
  dtf_ABCD_long_form$QST.INT.CS.BISBAS.BIS.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )

  chr_columns <- paste0(
    'QST.INT.CS.BISBAS.Item_', c( 8:12 )
  )
  dtf_ABCD_long_form$QST.INT.CS.BISBAS.BAS_RR.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )


  chr_columns <- paste0(
    'QST.INT.CS.BISBAS.Item_', c( 13:16 )
  )
  dtf_ABCD_long_form$QST.INT.CS.UPPS.BAS_D.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )


  chr_columns <- paste0(
    'QST.INT.CS.BISBAS.Item_', c( 17:20 )
  )
  dtf_ABCD_long_form$QST.INT.CS.UPPS.BAS_FS.Summed_score <-
    rowSums( dtf_ABCD_long_form[, chr_columns] )


  #### 2.4.4) Codebook entries ####

  # Initialize list for codebook entries
  # 20 items, 4 subscales
  lst_codebook_entries <- lapply(
    1:24, function(x) return(NULL)
  )

  # Column names
  names( lst_codebook_entries ) <- c(
    names( chr_items ),
    paste0(
      'QST.INT.CS.BISBAS.',
      c(
        'BIS',
        'BAS_RR',
        'BAS_D',
        'BAS_FS'
      ),
      '.Summed_score'
    )
  )

  # Timepoints at which data were collected
  lst_collected_over <- abcddata_codebook_collected_over(
    dtf_ABCD_long_form,
    'QST.INT.CS.BISBAS.BIS.Summed_score',
    'SSS.DBL.GD.Year'
  )

  # Individual items
  chr_item_content <- c(

    # BIS
    Item_1 = paste0(
      "I usually get very tense when I think something ",
      "unpleasant is going to happen"
    ),
    Item_2 = paste0(
      "I worry about making mistakes"
    ),
    Item_3 = paste0(
      "I am hurt when people scold me or tell me that I do ",
      "something wrong"
    ),
    Item_4 = paste0(
      "I feel pretty upset when I think that someone is angry with me"
    ),
    Item_5 = paste0(
      "I do not become fearful or nervous even when something ",
      "bad happens to me [Reverse coded]"
    ),
    Item_6 = paste0(
      "I feel worried when I have done poorly at something"
    ),
    Item_7 = paste0(
      "I am very fearful compared to my friends"
    ),

    # BAS: Reward Responsiveness
    Item_8 = paste0(
      "I feel excited and full of energy when I get ",
      "something that I want"
    ),
    Item_9 = paste0(
      "When I am doing well at something I like to keep doing this"
    ),
    Item_10 = paste0(
      "I get thrilled when good things happen to me"
    ),
    Item_11 = paste0(
      "It would excite me to win a contest"
    ),
    Item_12 = paste0(
      "I get really excited when I see an opportunity ",
      "to get something I like"
    ),

    # BAS: Drive
    Item_13 = paste0(
      "When I want something I usually go all the way to get it"
    ),
    Item_14 = paste0(
      "I do everything to get the things that I want"
    ),
    Item_15 = paste0(
      "When I see an opportunity  to get something I want ",
      "I go for it right away"
    ),
    Item_16 = paste0(
      "Nobody can stop me when I want something"
    ),

    # BAS: Fun seeking
    Item_17 = paste0(
      "I often do things for no other reason than they might be fun"
    ),
    Item_18 = paste0(
      "I crave exitement and new sensations"
    ),
    Item_19 = paste0(
      "I am always willing to try something new when I think it will be fun"
    ),
    Item_20 = paste0(
      "I often do things on the spur of the moment"
    )

  )

  # Starting text for each item entry
  chr_item_description <- paste0(
    "Individual item for the modified BIS/BAS scale: "
  )

  # Loop over individual items
  for ( i in 1:20 ) {

    lst_codebook_entries[[i]] <- list(
      chr_description =
        paste0( chr_item_description, chr_item_content[i] ),
      lst_values_and_labels = lst_response_format,
      lst_collected_over = lst_collected_over,
      chr_source_files = chr_files[1],
      chr_source_variables =
    )

    # Close 'Loop over individual items'
  }

  # Description for each subscale
  chr_subscale_description <- paste0(
    "Summed scores for the ",
    c( 7, 5, 4, 4 ),
    "-item ",
    c(
      "BIS",
      "BAS: Reward Responsiveness",
      "BAS: Drive",
      "BAS: Fun Seeking"
    ),
    " subscale from the modified BIS/BAS ",
    "scale. Higher scores indicate ",
    c(
      "a greater degree of worry and fearfulness",
      "a greater excitement over reinforcing outcomes",
      "a greater intensity of goal-directed behavior",
      "a greater enjoyment of enjoyment for its own sake"
    ),
    "."
  )

  # Loop over subscales
  for ( i in 1:4 ) {

    lst_codebook_entries[[20 + i]] <- list(
      chr_description =
        chr_subscale_description[i],
      lst_collected_over = lst_collected_over,
      chr_source_files = chr_files[1],
      chr_source_variables =
        chr_items[
          list(
            1:7,
            8:12,
            13:16,
            17:20
          )[[i]]
        ]
    )

    # Close 'Loop over subscales'
  }

  # Loop over codebook entries
  for ( l in seq_along(lst_codebook_entries) ) {

    lst_arg <- lst_codebook_entries[[l]]
    lst_arg$dtf_base <- dtf_ABCD_long_form
    lst_arg$chr_column <- names( lst_codebook_entries )[l]

    dtf_ABCD_long_form <- do.call(
      abcddata_codebook_add_entry,
      lst_arg
    )

    # Close 'Loop over codebook entries'
  }

  return( dtf_ABCD_long_form )
}

#### 2.5) abcddata_add.CBCL ####

abcddata_add.CBCL <- function(
    dtf_ABCD_long_form,
    chr_files = 'mh_p_cbcl.csv',
    chr_paths = 'core/mental-health',
    lgc_progress = TRUE ) {

  #### 2.5.1) Load in data ####

  # Read in file
  chr_full_path <- chr_files[1]
  if ( chr_paths[1] != '' ) {
    chr_full_path <- paste0( chr_paths[1], '/', chr_files[1] )
  }
  dtf_cbcl <- read.csv(
    file = chr_full_path,
    header = TRUE
  )

  #### 2.5.2) Individual items ####

  int_items <- 1:112

  lst_response_format <- list(
    content = 0:2,
    additional_content = c(
      'Not true',
      'Somewhat/sometimes true',
      'Very true/Often true'
    )
  )

  chr_items <- paste0(
    'cbcl_q',
    int_items,
    '_p'
  )
  # For items 1-9 add leading zero
  chr_items[1:9] <- gsub(
    'cbcl_q', 'cbcl_q0', chr_items[1:9], fixed = TRUE
  )
  # For item 56 add options a - h
  chr_items <- chr_items[ chr_items != 'cbcl_q56_p']
  chr_items <- c(
    chr_items,
    paste0( 'cbcl_q56', c( 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' ),
            '_p' )
  )
  chr_items <- sort( chr_items )

  dtf_ABCD_long_form <- abcddata_merge_data_sets(
    dtf_ABCD_long_form,
    dtf_cbcl,
    list(
      c( 'IDS.CHR.GD.Participant', 'src_subject_id' ),
      c( 'SSS.CHR.GD.Time_point', 'eventname' )
    ),
    chr_items,
    lgc_progress = lgc_progress
  )

  #### 2.5.3) Summed scores for subscales ####

  #### 2.5.4) Codebook entries ####

  # Individual items
  chr_item_content <- c(

    Item_1 = paste0(
      "Acts too young for their age"
    ),
    Item_2 = paste0(
      "Drinks alcohol without parent approval"
    ),
    Item_3 = paste0(
      "Argues a lot"
    ),
    Item_4 = paste0(
      "Fails to finish things they start"
    ),
    Item_5 = paste0(
      "There is very little they enjoy"
    ),
    Item_6 = paste0(
      "Bowel movements outside toilet"
    ),
    Item_7 = paste0(
      "Bragging or boasting"
    ),
    Item_8 = paste0(
      "Cannot concentrate or cannot pay attention for long"
    ),
    Item_9 = paste0(
      "Cannot get their mind off certain thoughts or obsessions"
    ),
    Item_10 = paste0(
      "Cannot sit still - restless or hyperactive"
    ),
    Item_11 = paste0(
      "Clings to adults or too dependent"
    ),
    Item_12 = paste0(
      "Complains of loneliness"
    ),
    Item_13 = paste0(
      "Confused or seems to be in a fog"
    ),
    Item_14 = paste0(
      "Cries a lot"
    ),
    Item_15 = paste0(
      "Cruel to animals"
    ),
    Item_16 = paste0(
      "Cruelty or bullying or meanness to others"
    ),
    Item_17= paste0(
      "Daydreams or gets lost in their thoughts"
    ),
    Item_18 = paste0(
      "Deliberately harms self or attempts suicide"
    ),
    Item_19 = paste0(
      "Demands a lot of attention"
    ),
    Item_20 = paste0(
      "Destroys their own things"
    ),
    Item_21= paste0(
      "Destroys things belonging to their to their family or others"
    ),
    Item_22 = paste0(
      "Disobedient at home"
    ),
    Item_23 = paste0(
      "Disobedient at school"
    ),
    Item_24 = paste0(
      "Does not eat well"
    ),
    Item_25 = paste0(
      "Does not get along with other kids"
    ),
    Item_26 = paste0(
      "Does not seem to feel guilty after misbehaving"
    ),
    Item_27 = paste0(
      "Easily jealous"
    ),
    Item_28 = paste0(
      "Breaks rules at home or school or elsewhere"
    ),
    Item_29 = paste0(
      "Fears certain animals or situations or places other than school"
    ),
    Item_30 = paste0(
      "Fears going to school"
    ),
    Item_31 = paste0(
      "Fears they might think or do something bad"
    ),
    Item_32 = paste0(
      "Feels they have to be perfect"
    ),
    Item_33 = paste0(
      "Feels or complains that no one loves them"
    ),
    Item_34 = paste0(
      "Feels others are out to get them"
    ),
    Item_35 = paste0(
      "Feels worthless or inferior"
    ),
    Item_36 = paste0(
      "Gets hurt a lot - accident prone"
    ),
    Item_37 = paste0(
      "Gets in many fights"
    ),
    Item_38 = paste0(
      "Gets teased a lot"
    ),
    Item_39 = paste0(
      "Hangs around with others who get in trouble"
    ),
    Item_40 = paste0(
      "Hears sound or voices that are not there"
    ),
    Item_41 = paste0(
      "Impulsive or acts without thinking"
    ),
    Item_42 = paste0(
      "Would rather be alone than with others"
    ),
    Item_43 = paste0(
      "Lying or cheating"
    ),
    Item_44 = paste0(
      "Bits fingernails"
    ),
    Item_45 = paste0(
      "Nervous or highstrung or tense"
    ),
    Item_46 = paste0(
      "Nervous movements or twitching"
    ),
    Item_47 = paste0(
      "Nightmares"
    ),
    Item_48 = paste0(
      "Not liked by other kids"
    ),
    Item_49 = paste0(
      "Constipated - does not move bowels"
    ),
    Item_50 = paste0(
      "Too fearful or anxious"
    ),
    Item_51 = paste0(
      "Feels dizzy or lightheaded"
    ),
    Item_52 = paste0(
      "Feels too guilty"
    ),
    Item_53 = paste0(
      "Overeating"
    ),
    Item_54 = paste0(
      "Overtired without good reason"
    ),
    Item_55 = paste0(
      "Overweight"
    ),
    Item_56a = paste0(
      "Aches or pains - not stomach or headaches"
    ),
    Item_56b = paste0(
      "Headaches"
    ),
    Item_56c = paste0(
      "Nausea - feels sick"
    ),
    Item_56d = paste0(
      "Problems with eyes - not if corrected by glasses"
    ),
    Item_56e = paste0(
      "Rashes or other skin problems"
    ),
    Item_56f = paste0(
      "Stomachaches"
    ),
    Item_56g = paste0(
      "Vomiting - throwing up"
    ),
    Item_56h = paste0(
      "Other - physical problems without knowing physical cause"
    ),
    Item_57 = paste0(
      "Physically attacks people"
    ),
    Item_58 = paste0(
      "Picks nose or skin or other parts of body"
    ),
    Item_59 = paste0(
      "Plays with own sex parts in public"
    ),
    Item_60 = paste0(
      "Plays with own sex parts too much"
    ),
    Item_61 = paste0(
      "Poor school work"
    ),
    Item_62 = paste0(
      "Poorly coordinated or clumsy"
    ),
    Item_63 = paste0(
      "Prefers being with older kids"
    ),
    Item_64 = paste0(
      "Prefers being with younger kids"
    ),
    Item_65 = paste0(
      "Refuses to talk"
    ),
    Item_66 = paste0(
      "Repeats certain acts over and over - compulsions"
    ),
    Item_67 = paste0(
      "Runs away from home"
    ),
    Item_68 = paste0(
      "Screams a lot"
    ),
    Item_69 = paste0(
      "Secretive - keeps things to self"
    ),
    Item_70 = paste0(
      "Sees things that are not there"
    ),
    Item_71 = paste0(
      "Self-conscious or easily embarrassed"
    ),
    Item_72 = paste0(
      "Sets fires"
    ),
    Item_73 = paste0(
      "Sexual problems"
    ),
    Item_74 = paste0(
      "Showing off or clowning"
    ),
    Item_75 = paste0(
      "Too shy or timid"
    ),
    Item_76 = paste0(
      "Sleeps less than most kids"
    ),
    Item_77 = paste0(
      "Sleeps more than most kids during day and/or night"
    ),
    Item_78 = paste0(
      "Inattentive or easily distracted"
    ),
    Item_79 = paste0(
      "Speech problems"
    ),
    Item_80 = paste0(
      "Stares blankly"
    ),
    Item_81 = paste0(
      "Steals at home"
    ),
    Item_82 = paste0(
      "Steals outside the home"
    ),
    Item_83 = paste0(
      "Stores up too many things they do not need"
    ),
    Item_84 = paste0(
      "Strange behavior"
    ),
    Item_85 = paste0(
      "Strange ideas"
    ),
    Item_86 = paste0(
      "Stubborn or sullen or irritable"
    ),
    Item_87 = paste0(
      "Sudden changes in mood or feelings"
    ),
    Item_88 = paste0(
      "Sulks a lot"
    ),
    Item_89 = paste0(
      "Suspicious"
    ),
    Item_90 = paste0(
      "Swearing or obscene language"
    ),
    Item_91 = paste0(
      "Talks about killing self"
    ),
    Item_92 = paste0(
      "Talks or walks in sleep"
    ),
    Item_93 = paste0(
      "Talks too much"
    ),
    Item_94 = paste0(
      "Teases a lot"
    ),
    Item_95 = paste0(
      "Temper tantrums or hot temper"
    ),
    Item_96 = paste0(
      "Thinks about sex too much"
    ),
    Item_97 = paste0(
      "Threatens people"
    ),
    Item_98 = paste0(
      "Thumb-sucking"
    ),
    Item_99 = paste0(
      "Smokes or chews or sniffs tobacco"
    ),
    Item_100 = paste0(
      "Trouble sleeping"
    ),
    Item_101 = paste0(
      "Truancy - skips school"
    ),
    Item_102 = paste0(
      "Underactive or slow moving or lacks energy"
    ),
    Item_103 = paste0(
      "Unhappy or sad or depressed"
    ),
    Item_104 = paste0(
      "Unusually loud"
    ),
    Item_105 = paste0(
      "Uses drugs for non-medical purposes - do not include ",
      "alcohol or tobacco"
    ),
    Item_106 = paste0(
      "Vandalism"
    ),
    Item_107 = paste0(
      "Wets self during the day"
    ),
    Item_108 = paste0(
      "Wets the bed"
    ),
    Item_109 = paste0(
      "Whining"
    ),
    Item_110 = paste0(
      "Wishes to be of opposite sex"
    ),
    Item_111 = paste0(
      "Withdrawn - does not get involved with others"
    ),
    Item_112 = paste0(
      "Worries"
    )

  )

}

