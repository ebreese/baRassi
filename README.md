Initial version of an AFL data package for R.

At the moment I'm focusing mostly on AFLW because there's significant gaps between what is available for the men's and women's competitions.

To install run the following in R

pak::pak("ebreese/baRassi")

Then load the package via:

library(baRassi)

At the moment you should also load the fitzRoy package, because I forgot to add a required import in for one function I'm calling from it

install.packages("fitzRoy")
library(fitzRoy)


Currently three functions available in baRassi


fetch_stats_aflw(comp="AFLW", season = NA, round = NA)
- Fetches the extended stats for AFLW which are currently only available as season totals, things like 1on1 marking contests etc
- Leave comp and season for now, as I've only got AFLW/2026 data in there.
- For round if you leave as NA it will grab all available rounds
- You can also pass it a single integer or an integer vector e.g. fetch_stats_aflw(1) or fetch_stats_aflw(2:5)
- Currently I have a script that grabs the season totals every monday, then uses each the record of previous season totals to derive match by match data.
- That then gets shipped off to a public repo and stored in a file per round

fetch_stat_feed_afl(matchId)
- Grabs the stat feed for a given AFLW match. This data is available for 2025 and 2026
- The matchId is in the Champion Data format, e.g. "CD_M20262640301"
- CD_M - prefix for match Ids
- 2026 - season year (generally just the year, but in the case of the second AFLW 2022 season they use 2101)
- 264 - The competition ID - 264 is AFLW, 014 is AFLM
- 03 - Round number with a leading zero to make two digits
- 01 - Game number within a round, leading zero to make two digits
- You can get lists of matchIds by calling functions from the fitzRoy package like fetch_results_afl(comp="AFLW",season=2026)
- fetch_stats_aflw() also returns a matchId column in the same format.
- Right now I think you need to load fitzRoy as well, because I forgot to properly do the require code for it in the current package
- The columns:
-   matchId - Champion Data format match ID
-   rankingOrder - retains original ranking order from source data to ensure you can rebuild the order even where timestamps are the same if needed
-   period - which quarter an event took place in
-   periodSeconds - seconds into that quarter
-   team - team of the player recording the stat
-   opponent - opposing team
-   displayName F.Lastname style display of name
-   description - the actual stat recorded e.g. a hitout, a handball, etc
-     Not all stats are recorded fully here, so there are some placeholder descriptions like "otherUncontested" where the data indicated an uncontested possession but no further detail. The majority of these I imagine are handball receives, so I'll look later at inferring that based on the immediate preceding event being a teammate handball
-   possession - records whether the stat was given as a possession or not: "contested", "uncontested", or NA for no possession
-   dipsosal - records whether the stat was given as a disposal or not: "effective", "ineffective", "clanger", or NA for no possession
-   shotAtGoal - records whether it was recorded as a shot at goal and the result: "goal", "behind", "noScore" or NA for no shot
-   possGain - records whether the stat was recorded as a possession gain (CD stat term for intercept) - TRUE or NA
-   turnover - records whether the stat was recorded as a turnover - TRUE or NA
-   scoreLaunch - records whether the stat was recorded as a score launch. From what I can see these are only included for events that are also possGains. Score launches should be credited to HTAs or clearances leading to a score in that chain, but that's not in the source data. I'm going to look at how accurately I can infer later but there's a challenge with the chain data covered below
-   f50MarkTackle - rather than recording separate events for forward 50 marks or tackles, I've recorded them as marks/tackles then set this flag where they were f50. "f50Tackle", "f50Mark", or NA
-   playerId - the champion data fromat player Id e.g. "CD_I1033166"
-   utcTimeStamp - timestamp of the event
-   ranking - Have retained this from the source data but don't currently use this. Currently each stat event is given a ranking unique to it within that given match. I believe it refers to it's rank among player rating point values within the match.
-   jumperNumber
-   firstname
-   surname
-   homeAway - records home/away status of the player's team "HOME" or "AWAY"
-   compId - separate IDs used by the afl website/api, not currently used
-   seasonId - separate IDs used by the afl website/api, not currently used
-   venue
-   localStartTime - match start time in local timezone
-   utcStartTime - match start time in UTC
-   year
-   CHAINS
-     The source data doesn't assign things to chains so I've attempted to infer chain order based on the data I have.
-     One big limitation is I don't have access to the ballup/throwin events, so there's no clear signal as to when one chain stops and a new stoppage chain begins
-     I've tried a bunch of methods and so far haven't come up with any way to accurately infer where a stoppage happens. There is a big variance in how long the restart takes, and the shorter restarts are quicker than the game otherwise goes without recording a stat
-     e.g. a long dump kick to a messy ground contest can easily go ten seconds without a recognised stat being recorded. Any reasonable stoppage inference based on "how long til something else happened" would pick this up as a potential stoppage
-     I can clearly state where some chains begin: intercept chains, kick ins, and centre bounces.
-     I can say with a relatively good degree of confidence where lasso / out on full free kick chains are, although there are some false positives based on a team's disposal being followed by an opposing team's ground kick (I'm looking for a change in teams and a kick without a possession being credited as the trigger for lasso)
-     For stoppage I've recorded a stoppage chain wherever a hitout is recorded, but there's still a good chunk with no hitout
-     Because of that I've created a new chain type oustide of what CD recognises - a clearance chain.
-     My logic is if nothing happens before a chain reset, I don't really care that much about it. Interesting stuff is post-clearance.
-     Because of this there will be instances where a previous chain is continuing (e.g. an intercept chain ends in a stoppage, no hitout is recorded in the ruck contest, my code still assumes it is still the previous intercept chain until a clearance occurs)
-   chainStart - the initiation of the current chain - "centreBounce", "clearance", "possGain", "stoppage", "kickIn", "OOBFree" - OOBFree represents lasso and out on the full resets.
-   chainEnd - the end state of the current chain - "clearance" (for when a stoppage chain becomes a clearance chain), "turnover", "stoppage", "behind", "rushed" (inferred based on a kick in not being preceded by a shot at goal), "goal", "OOBFree", or "quarterEnd"
-   chainNumber - to track progress through chains


get_next_i50_data(testFeed)
-  Feed this function the output of a fetch_statfeed() call
-  This summarises the flow of inside 50s through a game to help territory-control analysis
-  You may end up noticing that the statfeed data doesn't contain inside 50 entries in it, this was extremely annoying.
-  To deal with that I've taken the rebound50 events, which do exist, and used it to infer that at some point prior to that there had to be an inside50 to the opposing team.
-    rebounded: whether the inside50 led to the opposing team rebounding it. So, they might have either immediately recorded a rebound 50 (dump kick to an intercept mark in d50), or there may have been several chain starts (stoppages, intercepts), or there may have even been a behind scored.
-    goal: whether the inside50 led to a goal by that team before their opponent recorded a rebound50
-    score: whether the inside50 led to a score by that team before the opponent recorded a rebound50
-    nextI50: the team that recorded the next I50 event after this one
-    nextStepResult: records the result+1 of the current row's inside 50 event.
-      Goal - the attacking team scored a goal from this I50
-      RepeatI50 - the defending team recorded a rebound 50, but the attacking team was the next to record an i50
-      oppI50 - the defending team rebounded it, and the defending team was the next to record an i50
-      oppGoal - the defending team rebounded it, and the defending team was the next to record an i50 which also resulted in a goal
-  Inferring i50s from the presence of rebound 50s or goals has one problem, sometimes the quarter ends before either of those happens
-  To address this, it checks the official quarter by quarter i50 counts against the inferred i50 count. If a team is missing an i50, we add a dummy i50 entry in at periodSeconds=999 for that quarter
-  We also add in a quarter end row at periodSeconds = 1000
-  
