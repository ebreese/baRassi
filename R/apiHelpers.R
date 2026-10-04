
get_comp_seasons <- function(comp=c("AFLW","AFL","U18B","U18G","VFL","VFLW"))
{
  comp = match.arg(comp)
  compId <- get_comp_metadata(comp)
  compId <- compId$id
  seasons <- access_api(paste0(
    "https://aflapi.afl.com.au/afl/v2/competitions/",
    compId,
    "/compseasons"))
  return(seasons$compSeasons)

}

get_comps <- function()
{
  comps <- access_api("https://aflapi.afl.com.au/afl/v2/competitions")
  comps <- comps$competitions


  return(comps)
}

get_comp_metadata <- function(compName = c("AFLW","AFL","U18B","U18G","VFL","VFLW"))
{
  compName = match.arg(compName)

  compList <- get_comps()

  return(compList |>
           dplyr::filter(.data$code == compName))

}

get_season_rounds <- function(seasonId)
{
  rounds <- access_api(paste0(
    "https://aflapi.afl.com.au/afl/v2/compseasons/",
    seasonId,
    "/rounds"))

  rounds <- rounds$rounds
  return(rounds)
}

get_stats_by_period <- function(matchId,period)
{
  url <- paste0("https://api.afl.com.au/cfs/afl/matchStats/",matchId,"/period/",period)
  rawData <- access_api(url)
  return(rawData)
}
