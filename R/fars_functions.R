#' fars_read()
#'
#' \code{fars_read()} Reads csv files and loads them into a dataframe table.
#'
#' @param filename a character string of the file(s) path or file(s) name only (if file(s) is within work directory)
#'  you would like to load into your data frame.
#'
#' @return A tibble and prints the tibble.
#'
#' @note An error will occur stating the given file doesn't exist if the path is invalid.
#'
#' @details
#' File to be loaded must be in a csv format.
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("C:/local/documents/accident_2014.csv")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'make_filename()
#'
#'\code{make_filename()} Creates a file name for accident file(s) for a given a calander year.
#'
#' @param year a numerical entry which will be converted to an integer.
#'
#' @return A string in the following format: "accident_%d.csv.bz2" with the user entered year as an integer.
#'
#' @note An error will occur if a non-numberic entry is entered as an input.
#'
#' @examples
#' \dontrun{
#' make_filename("2014")
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'fars_read_years
#'
#'\code{fars_read_years} #'Reads multiple accident files into a dataframe table using make_filename and fars_read functions from this
#'  package but requires the targeted files to be in the working directy.
#'
#' @param years a numberic vector (or string of numberic values) containing the desired years of accident files
#'  to read.
#'
#' @return A tibble with the month and year columns from each file read into the function.
#'
#' @note A warning will appear for each year entered that doesn't have a corresponding accident file.
#' @note An error will occur if a none numberical entry is made.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' fars_read_years("2013, 2014, 2015")
#' }
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

#'\code{fars_summarize_years} Creates a data frame with a number of year(s) file(s) entered in the years element using the fars_read_years
#'  function from this package. Desired files need to be in the working directory.
#'
#' @param years a numberic vector (or string of numberic values) of desired accident file years.
#'
#' @return A tibble summarizing the number of years of accident files loaded.
#'
#' @note A warning will appear if any year entered into years doesn't have a corresponding file.
#' @note An error will occur if a non-numeric value is entered into years.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years("2013, 2014, 2015")
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' \code{fars_map_state} Uses make_filename and fars_read functions from this package to gather the desired year of accident file and
#'  plots a point map for desired state.
#'
#' @param state.num a numeric value (or string numberic value) corresponding to the desired state.
#'
#' @param year a numeric value (or string numberic value) corresponding to the desired year of accident
#'   reports.
#'
#' @return A point map for indicated state and year.
#'
#' @note An error will occur if a value is entered to state.num which can not be converted to an integer.
#' @note An error will occur if a value is entered to state.num which doens't correspond to an existing state.
#' @note An error will occur if a value is entered to year which can not be converted to an integer.
#'
#' @note A message will be issued if no accidents exist for the entered state or year.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state("1", "2014")
#' }
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

