get_i50_by_period <- function(statFeed)
{


  matchIdsAndPeriod <- statFeed |>
    dplyr::ungroup() |>
    dplyr::select(.data$matchId,.data$period) |>
    dplyr::distinct()


  if(nrow(matchIdsAndPeriod) > 1)
  {
    print(paste(matchIdsAndPeriod$matchId[1],matchIdsAndPeriod$period[1]))
    result <-   get_i50_by_period(matchIdsAndPeriod[1,])
    for(i in 2:nrow(matchIdsAndPeriod))
    {
      print(paste(matchIdsAndPeriod$matchId[i],matchIdsAndPeriod$period[i]))

      result <- rbind(result,get_i50_by_period(matchIdsAndPeriod[1,]))
    }
    return(result)
  }



  rawData <- get_stats_by_period(matchIdsAndPeriod$matchId[[1]],matchIdsAndPeriod$period[[1]])

    return(data.frame(
    matchId = matchIdsAndPeriod$matchId[[1]],
    teamId = c(rawData$homeTeamTotals$teamName$teamName,
               rawData$awayTeamTotals$teamName$teamName),
    period = matchIdsAndPeriod$period[[1]],
    i50teamTotal = c(rawData$homeTeamTotals$stats$inside50s,
                     rawData$awayTeamTotals$stats$inside50s)
    ,
    r50teamTotal = c(rawData$homeTeamTotals$stats$rebound50s,
                     rawData$awayTeamTotals$stats$rebound50s),
    goals = c(rawData$homeTeamTotals$stats$goals)))
}

get_i50_r50 <- function(statFeed)
{
  qtrByQtrStats <- get_i50_by_period(statFeed)

  i50Flags <<- statFeed |>
    dplyr::select(matchId,team,opponent,period,periodSeconds,description,shotAtGoal) |>
    dplyr::filter(description %in% c("rebound50","kickIn") |
                    (!is.na(shotAtGoal) & shotAtGoal == "goal")) |>
    dplyr::mutate(tempTeam = team) |>
    dplyr::mutate(team = dplyr::if_else(description == "kickIn",
                              opponent,
                              team),
                  opponent = dplyr::if_else(description == "kickIn",
                         tempTeam,
                         opponent),
           description = dplyr::if_else(description == "kickIn",
                           "behind",
                           description),
           periodSeconds = dplyr::if_else(description == "kickIn",
                                   periodSeconds - 1,
                                   periodSeconds)) |>
    dplyr::select(-tempTeam)

  inferredI50s <<- i50Flags |>
    dplyr::mutate(tempTeam = team) |>
    dplyr::mutate(team = dplyr::if_else(description == "rebound50",
                          opponent,
                              team),
           opponent = dplyr::if_else(description == "rebound50",
                         tempTeam,
                         opponent)) |>
    dplyr::select(-tempTeam) |>
    dplyr::mutate(periodSeconds = periodSeconds - 1) |>
    dplyr::mutate(description = "inside50")


  i50R50List <<- dplyr::bind_rows(i50Flags,inferredI50s) |>
    dplyr::arrange(matchId,period,periodSeconds) |>
    dplyr::group_by(matchId,period) |>
    dplyr::filter(description != "shotAtGoal")
    # dplyr::mutate(flagToRemove =
    #          !is.na(lag(description)) &
    #          description == "inside50" &
    #          shotAtGoal %in% c("behind","goal") &
    #          team == lag(team)) |>
#    dplyr::filter(!flagToRemove) |>
#    dplyr::select(-flagToRemove)

  print(colnames(qtrByQtrStats))
  missingI50s <<- i50R50List |>
    dplyr::group_by(matchId,team,period,opponent) |>
    dplyr::summarise(i50Inferred = sum(description == "inside50")) |>
    dplyr::left_join(qtrByQtrStats |>
                dplyr::group_by(matchId,teamId,period) |>
                dplyr::slice(1) |>
                dplyr::ungroup(),
              by=c("matchId","team" = "teamId","period")) |>
    dplyr::mutate(diff = i50Inferred - i50teamTotal) |>
    dplyr::filter(diff < 0) |>
    dplyr::transmute(matchId,period,team,opponent,
              description = "inside50",
              periodSeconds = 9999)

  quarterEnd <- statFeed |>
    dplyr::group_by(matchId,period) |>
    dplyr::summarise() |>
    dplyr::mutate(periodSeconds = 10000,
           description = "quarterEnd",
           team = NA,
           opponent = NA)

  # shotAtGoal <- statFeed |>
  #   filter(stats == "shotAtGoal") |>
  #   select(matchId,period,periodSeconds,team,opp,stats)

  i50R50List <- i50R50List |>
    rbind(missingI50s,quarterEnd) |>
    dplyr::arrange(matchId,period,periodSeconds,desc(description)) |>
    dplyr::select(matchId,period,periodSeconds,team,description,opponent,shotAtGoal)

  return(i50R50List)
}


#' Produce a dataframe listing the i50 events and whether the next i50 was a repeat i50, score, or opponent i50
#' Produces a dataframe listing the i50 events and whether the next i50 was a repeat i50, score, or opponent i50
#'
#' @param statFeed A statfeed as returned by fetch_statFeed
#'
#' @return A dataframe
#' @export
#'
#'
#'
get_next_i50_data <- function(statFeed)
{
  i50R50List <- get_i50_r50(statFeed)

  i50Tracker <- i50R50List |>
    dplyr::group_by(matchId) |>
    dplyr::arrange(matchId,period,periodSeconds) |>
    dplyr::mutate(insideTracker = cumsum(description %in% c("inside50","quarterEnd"))) |>
    dplyr::group_by(matchId,period,insideTracker) |>
    dplyr::summarise(periodSeconds = dplyr::first(periodSeconds),
              team = dplyr::first(team),
              opponent = dplyr::first(opponent),
              rebounded = any(description == "rebound50"),
              goal = any(!is.na(shotAtGoal) & shotAtGoal == "goal"),
              score = any(!is.na(shotAtGoal) & shotAtGoal %in% c("goal","behind"))) |>
    dplyr::group_by(matchId,period) |>
    dplyr::mutate(nextI50 = dplyr::lead(team))


  i50NextStep <- i50Tracker |>
    dplyr::group_by(matchId,period) |>
    dplyr::mutate(nextStepResult = dplyr::if_else(goal,
                                    "Goal",
                                    dplyr::if_else(dplyr::lead(team) == team,
                                            "RepeatI50",
                                            dplyr::if_else(dplyr::lead(goal),
                                                    "oppGoal",
                                                    "oppI50"))))

  return(i50NextStep)
}
