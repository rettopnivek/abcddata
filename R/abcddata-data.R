# Provided data sets
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2024-01-31

# Table of contents
# 1) abcddata_ARMS

#### 1) abcddata_ARMS ####
#' ABCD Reproducible Matched Samples (ARMS)
#'
#' A data frame providing the discovery, replication, and
#' development splits created by Feczko et al. (2021) for
#' the ABCD study.
#'
#' @format ## `abcddata_ARMS`
#' A data frame with 11,868 rows and 4 columns:
#' \describe{
#'   \item{IDS.CHR.GD.Participant}{Identifier for participant}
#'   \item{SSS.INT.GD.ARMS_groups}{The different samples (1 = Discovery,
#'     2 = Replication, 3 = Development, 4 = Excluded)}
#'   \item{SSS.CHR.GD.ARMS_groups}{The different samples (using the
#'     notation from Feczko et al.)}
#'   \item{SSS.CHR.GD.Reproducible_matched_samples}{The different samples}
#' }
#' @references
#' Feczko, E., Conan, G., Marek, S., Tervo-Clemmens, B.,
#'   Cordova, M., Doyle, O., ... & Fair, D. A. (2021). Adolescent
#'   Brain Cognitive Development (ABCD) community MRI collection
#'   and utilities. BioRxiv.
#'   https://www.biorxiv.org/content/10.1101/2021.07.09.451638v1.abstract
"abcddata_ARMS"
