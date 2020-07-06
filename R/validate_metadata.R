validate_datetime <- function(datetime) {
  datetime_valid <- purrr::map_chr(datetime, function(x) {tryCatch({
    parse_datetime(x)
  }, error = function(e) {NA})})


  invalid <- (is.na(datetime_valid))

  if (sum(invalid) != 0)  {
    stop(
      abort_bad_argument(
        "date",
        sprintf(
          "follow format YYYY-MM-DD. Rows # %s dont follow it",
          paste(which(invalid), collapse = ", # ")
        )
      )
    )
  }
}

#' Verify the pased metadata is compliant
#'
#' @param metadata A data.table describing an experiment
#' It must contain the columns region_id, machine_name and date_time
#' It can contain the column reference_hour to provide a difference ZT for groups of flies
#' It can contain any other column that is not used in the analysis
#' It must NOT contain columns with NA values, as that will make fslscopr ignore the row
#' date_time must follow the format %YYYY-%MM-%DD_%HH-%MM-%SS
#' @export
validate_metadata <- function(metadata) {


  # make sure the required colums are available
  required_columns <- c("file", "start_datetime", "stop_datetime")
  invalid <- !(required_columns %in% colnames(metadata))

  if (any(invalid)) {
    stop(abort_bad_argument(
      arg = "metadata",
      must = sprintf("contain columns %s", paste(required_columns[invalid], collapse = " "))
    ))
  }

  # validate the date column
  validate_datetime(metadata$start_datetime)
  validate_datetime(metadata$stop_datetime)

  dups <- duplicated(metadata)
  if (any(dups)) {
    stop(abort_bad_argument(
      arg = "metadata",
      must = sprintf(
        "not contain repeated rows: Rows # %s are repeated",
         paste(which(dups), collapse = ' ')
        )
    ))
  }


  invisible(TRUE)
}

