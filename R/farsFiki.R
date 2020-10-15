#' fars_read function
#'
#' This function extracts a database from a .csv file and turns it
#' into a tibble, ignoring all simple diagnostic messages and not
#' showing any progress bar
#'
#'
#' @param filename file with a "csv" format that contains a database
#' that will be extracted to an object called "data"
#'
#' @return an object called "data" of tbl_df class.
#'
#' @details If the filename doesn't exist, it will stop and print a
#' message telling us that.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make_filename function
#'
#' This function creates a filename with this format:
#' "accident_$$$$.csv.bz2" indicating a year instead of $$$$
#'
#' @param year a year to be included in the filename
#'
#' @return a filename with the format "accident_$$$$.csv.bz2" indicating
#' a year instead of $$$$.
#'
#' @examples
#' make_filename(1980)
#' make_filename(2010)
#'
#' @export


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years function
#'
#' This function evaluates if the elements of a vector called "years"
#' correspond to the years that appear in the names of the files
#' with the format "accident_$$$$.csv.bz2" where $$$$ is the year and
#' returns a list of data frames with year and month.
#'
#' @param years vector of numeric values that correspond to years
#'
#' @return a list of data frames for the valid years from object "years"
#' containing years and months from each file.
#'
#' @details If a year doesn't appears in any filename, it will return
#' a warning.
#'
#' @examples
#'
#' @importFrom dplyr mutate select
#'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years function
#'
#' This function creates a table that summarize the number of
#' observation in every file with the format "accident_$$$$.csv.bz2"
#' where $$$$ is a year.
#'
#' @param years vector of numeric values that correspond to years
#'
#' @return a table where each row is a month and each column is a year,
#' and contains the number of observations for each month and year in
#' the files with the mentioned format
#'
#' @details If a year doesn't appears in any filename, it will return
#' a warning.
#'
#' @examples
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state function
#'
#' This function creates a map with the accidents happened in a certain
#' year in a selected state.
#'
#' @param year a numeric value corresponding to a year
#' @param state.num a numeric value corresponding to the state code
#'
#' @return a map of the selected state with point corresponding to the
#' places where the accidents occurred in the selected year.
#'
#' @details If state.num doesn't correspond to a state code, the
#' process stops. If in the selected year and state there were no
#' accidents, a warning is shown.
#'
#' @examples
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
