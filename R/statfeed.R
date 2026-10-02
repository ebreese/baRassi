usethis::use_package("dplyr")
usethis::use_package("jsonlite")
usethis::use_package("lubridate")
usethis::use_package("usethis")


#' Get the stat feed for a single match
#' Gets the stat feed for a single match
#'
#' @param matchId Match ID in format CD_M20262640101
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' fetch_statfeed("CD_M20262640101")
#'
#'
fetch_statfeed <- function(matchId) {

  standAlone <- c(
    "kick",
    "handball",
    "groundBallGet",
    "centreClearance",
    "tackle",
    "freeFor",
    "freeAgainst",
    "spoil",
    "stoppageClearance",
    "rebound50",
    "shotAtGoal",
    "kickIn",
    "hitout",
    "hitoutToAdvantage",
    "runningBounce",
    "goalAssist"
  )

  matchIdTrimmed <- gsub("CD_M", "", matchId)

  json_file <- paste0(
    "https://www.afl.com.au/statspro/json/vision-trxs-",
    matchIdTrimmed,
    ".json"
  )

  json_data <- jsonlite::fromJSON(json_file)

  squadInfo <- data.frame(
    squadId = c(
      json_data$report$matchInfo$homeSquadId,
      json_data$report$matchInfo$awaySquadId
    ),
    team = c(
      json_data$report$matchInfo$homeSquadName,
      json_data$report$matchInfo$awaySquadName
    ),
    homeAway = c("HOME", "AWAY")
  )

  playerInfo <- json_data$report$players |>
    dplyr::select(
      dplyr::all_of(
        c(
          "playerId",
          "firstname",
          "surname",
          "displayName",
          "jumperNumber"
        )
      )
    )

  matchInfo <- data.frame(
    matchId = matchId,
    homeTeam = json_data$report$matchInfo$homeSquadName,
    awayTeam = json_data$report$matchInfo$awaySquadName,
    venue = json_data$report$matchInfo$venueName,
    utcStartTime = json_data$report$matchInfo$utcStartTime,
    localStartTime = json_data$report$matchInfo$localStartTime,
    seasonId = substr(
      json_data$report$matchInfo$matchId,
      1,
      4
    ),
    compId = substr(
      json_data$report$matchInfo$matchId,
      5,
      7
    ),
    round = strtoi(
      substr(
        json_data$report$matchInfo$matchId,
        8,
        9
      ),
      10L
    )
  )

  year <- lubridate::year(
    json_data$report$matchInfo$utcStartTime
  )

  statFeed <- json_data$report$matchTrxs |>
    dplyr::mutate(
      rankingOrder = dplyr::row_number()
    ) |>

    tidyr::unnest(.data$stats) |>

    dplyr::group_by(.data$ranking) |>

    dplyr::mutate(
      possession = dplyr::case_when(
        any(.data$stats == "contestedPossession") ~ "contested",
        any(.data$stats == "uncontestedPossession") ~ "uncontested",
        TRUE ~ NA_character_
      )
    ) |>

    dplyr::mutate(
      disposal = dplyr::case_when(
        any(.data$stats == "effectiveDisposal") ~ "effective",
        any(.data$stats == "clanger") ~ "clanger",
        any(.data$stats == "disposal") ~ "ineffective",
        TRUE ~ NA_character_
      )
    ) |>

    dplyr::mutate(
      shotAtGoal = dplyr::case_when(
        !any(.data$stats == "shotAtGoal") ~ NA_character_,
        any(.data$stats == "behind") ~ "behind",
        any(.data$stats == "goal") ~ "goal",
        TRUE ~ "noScore"
      )
    ) |>

    dplyr::mutate(
      turnover = dplyr::if_else(
        any(.data$stats == "turnover"),
        TRUE,
        NA
      ),
      scoreLaunch = dplyr::if_else(
        any(.data$stats == "scoreLaunch"),
        TRUE,
        NA
      ),
      possGain = dplyr::if_else(
        any(.data$stats == "possGain"),
        TRUE,
        NA
      ),
      f50MarkTackle = dplyr::case_when(
        any(.data$stats == "f50Tackle") ~ "f50Tackle",
        any(.data$stats == "f50Mark") ~ "f50Mark",
        TRUE ~ NA_character_
      )
    ) |>

    dplyr::group_by(
      .data$rankingOrder,
      .data$utcTimestamp,
      .data$period,
      .data$matchSeconds,
      .data$periodSeconds,
      .data$ranking,
      .data$squadId,
      .data$playerId,
      .data$possession,
      .data$disposal,
      .data$shotAtGoal,
      .data$turnover,
      .data$scoreLaunch,
      .data$possGain,
      .data$f50MarkTackle
    ) |>

    dplyr::summarise(
      description = dplyr::case_when(
        any(.data$stats %in% standAlone) ~
          dplyr::first(.data$stats[.data$stats %in% standAlone]),

        any(.data$stats == "markOnLead") ~
          "markOnLead",

        any(.data$stats == "contestedMark") ~
          "contestedMark",

        any(.data$stats == "mark") ~
          "uncontestedMark",

        any(.data$stats == "onePercenter") ~
          "onePercenter",

        any(.data$stats == "disposal") ~
          "disposal",

        any(.data$stats == "contestedPossession") ~
          "otherContested",

        any(.data$stats == "uncontestedPossession") ~
          "otherUncontested",

        TRUE ~ NA_character_
      ),
      .groups = "drop"
    ) |>

    dplyr::mutate(
      matchId = matchId
    ) |>

    dplyr::left_join(
      matchInfo,
      by = "matchId"
    ) |>

    dplyr::left_join(
      playerInfo,
      by = "playerId"
    ) |>

    dplyr::left_join(
      squadInfo,
      by = "squadId"
    ) |>

    dplyr::transmute(
      matchId = .data$matchId,
      rankingOrder = .data$rankingOrder,
      period = .data$period,
      periodSeconds = .data$periodSeconds,
      team = .data$team,
      displayName = .data$displayName,
      description = .data$description,
      possession = .data$possession,
      disposal = .data$disposal,
      shotAtGoal = .data$shotAtGoal,
      possGain = .data$possGain,
      turnover = .data$turnover,
      scoreLaunch = .data$scoreLaunch,
      f50MarkTackle = .data$f50MarkTackle,
      playerId = paste0("CD_I", .data$playerId),
      utcTimestamp = .data$utcTimestamp,
      ranking = .data$ranking,
      jumperNumber = .data$jumperNumber,
      firstname = .data$firstname,
      surname = .data$surname,
      homeAway = .data$homeAway,
      compId = .data$compId,
      seasonId = .data$seasonId,
      round = .data$round,
      venue = .data$venue,
      localStartTime = .data$localStartTime,
      utcStartTime = .data$utcStartTime,
      year = year
    ) |>

    dplyr::group_by(
      .data$matchId,
      .data$period
    ) |>

    dplyr::arrange(
      .data$matchId,
      .data$period,
      .data$rankingOrder
    ) |>

    dplyr::mutate(
      chainStart = dplyr::case_when(
        .data$description %in% c("stoppageClearance","centreClearance") ~ "clearance",

        .data$description == "kickIn" ~ "kickIn",

        .data$description %in% c("kick", "handball") &
          dplyr::lag(.data$team) != .data$team ~
          "OOBFree",

        is.na(dplyr::lag(.data$description)) ~
          "centreBounce",

        dplyr::lag(.data$shotAtGoal) == "goal" ~
          "centreBounce",

        .data$description == "hitout" ~
          "stoppage",

        .data$possGain ~
          "possGain",

        TRUE ~ NA_character_
      )) |>
      dplyr::mutate(chainEnd = dplyr::case_when(
        dplyr::lead(.data$chainStart) == "possGain" ~ "turnover",
        dplyr::lead(.data$chainStart) == "clearance" ~ "clearance",
        .data$shotAtGoal == "goal" ~ "goal",
        .data$shotAtGoal == "behind" ~ "behind",
        dplyr::lead(.data$description) == "kickIn" ~ "rushed",
        dplyr::lead(.data$chainStart) == "OOBFree" ~ "OOBFree",
        dplyr::lead(.data$chainStart) == "stoppage" ~ "stoppage",
        dplyr::lead(.data$possGain) ~ "turnover",
        is.na(dplyr::lead(.data$description)) ~ "quarterEnd",
        TRUE ~ NA_character_ )
      ) |>
    dplyr::ungroup() |>
    dplyr::mutate(chainNumber = cumsum(!is.na(.data$chainStart))) |>
    dplyr::group_by(.data$matchId,.data$chainNumber) |>
    dplyr::mutate(chainStart = dplyr::first(.data$chainStart),
                  chainEnd = dplyr::last(.data$chainEnd))


  return(statFeed)
}


