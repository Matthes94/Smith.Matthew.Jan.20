# Smith.Matthew.Jan.20
Week 4 Peer Reviewed Work

title: "fars_vignette"

##fars_read()

A function used to read csv data into a tibble for the package.

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


##make_filename()

A function used to a create file name for accident file(s)


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("inst/extdata/accident_%d.csv.bz2", year)


##fars_read_years()

A function to read multiple accident files into a tibble using "make_filename()" and "fars_read()" functions from this package but requires the targeted files to be in the working directy.



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


##fars_summarize_years()

A function which Creates a data frame with a number of year(s) file(s) entered in the years element using the "fars_read_years()"
function from this package. Desired files need to be in the working directory.


fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


##fars_map_state()

A function which uses "make_filename()" and "fars_read()" functions from this package to gather the desired year of accident file and
plots a point map for desired state.


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

https://travis-ci.org/Matthes94/Smith.Matthew.Jan.20.svg?branch=master
