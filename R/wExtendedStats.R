
#' Get the extended player stats game by game
#' Gets the extended player stats game by game
#'
#' @param comp = "AFLW", only services AFLW at the moment
#' @param season Season year, 2022 will return both 2022 seasons, will default to current year, can provide a vector e.g. c(2022:2026)
#' @param round Round numbers to return. NA is all rounds, can provide single round or multiple
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' fetch_stats_aflw()
#' fetch_stats_aflw(round=c(2:4))
#'
#'
fetch_stats_aflw <- function(comp="AFLW",
                        season = NA,#as.integer(format(Sys.Date(), "%Y")),
                             round = NA)
{
  if(all(is.na(season)))
  {
    season = as.integer(format(Sys.Date(),"%Y"))
  }

  print(season)

  compSeasons <- get_comp_seasons(comp)
  relevantSeasons <- compSeasons |>
    dplyr::mutate(year = strtoi(substr(.data$name,1,4))) |>
    dplyr::filter(.data$year %in% season)

  allRounds <- get_season_rounds(relevantSeasons$id[1])
  if(nrow(relevantSeasons) > 1)
  {
    for(i in 2:nrow(relevantSeasons))
    {
      allRounds <-
        rbind(allRounds,
              get_season_rounds(relevantSeasons$id[i]))
    }
  }

  if(!(all(is.na(round))))
  {
    print(round)
    allRounds <- allRounds |>
      dplyr::filter(.data$roundNumber %in% round)
  }

  files <- allRounds$providerId
  files <- paste0("AFLW_extendedStats_",
                  files)

  results <- lapply(files, function(f) {
    tryCatch(
      fetch_data(f, subdir = "wExtendedStats"),
      error = function(e) {
        warning("Skipping '", f, "': ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  })

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) stop("None of the requested files could be fetched.", call. = FALSE)

  data <- dplyr::bind_rows(results)
  return(data)

}
