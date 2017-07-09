rm(list = ls())

# this function plots your player exposures
get_mfl_lineups <- function(user = NULL, pass = NULL, year = NULL, rk = 20) {
  # arguments:
  # user = myfantasyleague.com username
  # pass = myfantasyleague.com password
  # year = desired year for lineups
  # rk = maximum number of players to be returned
  
  # libraries
  require(dplyr)
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(httr)
  require(jsonlite)
  
  # get your login information cookie
  print("Getting credentials...")
  login.url <- paste0("https://api.myfantasyleague.com/", year,
                      "/login?USERNAME=", user,
                      "&PASSWORD=", pass)
  login <- httr::GET(login.url)
  val <- login$cookies$value[login$cookies$name == "MFL_USER_ID"]
  
  # find all leagues for user and year; export as a JSON
  print("Getting leagues...")
  leagues <- httr::GET(paste0("https://www72.myfantasyleague.com/", year, 
                              "/export?TYPE=myleagues&FRANCHISE_NAMES=1&JSON=1"),
                       set_cookies("MFL_USER_ID" = val))
  
  
  # parse the JSON; output list of leagues as a dataframe
  js.leagues <- jsonlite::fromJSON(rawToChar(leagues$content))
  leagues.df <- as.data.frame(js.leagues$leagues)
  leagues.df$leagueID <- trimws(substr(leagues.df$league.name,
                                       start = nchar(leagues.df$league.name)-5,
                                       stop = nchar(leagues.df$league.name)))
  
  # loop through leagues to get rosters from each
  drafts.list <- c()
  for (i in 1:nrow(leagues.df)) {
    L <- leagues.df$leagueID[[i]]
    franch <- leagues.df$league.franchise_id[[i]]
    print(paste0("Getting draft info for league ", L, ", franchise ", franch, "..."))
    draft.url <- paste0("https://www76.myfantasyleague.com/", year,
                        "/export?TYPE=rosters&L=", L, 
                        "&APIKEY=&FRANCHISE=", franch, 
                        "&JSON=1")
    draft <- httr::GET(draft.url,
                       set_cookies("MFL_USER_ID" = val))
    
    js.drafts <- jsonlite::fromJSON(rawToChar(draft$content))
    draft.temp <- as.data.frame(js.drafts$rosters$franchise$player)
    drafts.list[[length(drafts.list) + 1]] <- draft.temp
  }
  drafts.df <- do.call(rbind, drafts.list)
  # recode draft round and position
  drafted <- strsplit(drafts.df$drafted, ".", fixed = TRUE)
  drafts.df$rd <- as.numeric(lapply(drafted, '[[', 1))
  drafts.df$loc <- as.numeric(lapply(drafted, '[[', 2))
  drafts.df$dpos <- ((drafts.df$rd - 1) * 12) + drafts.df$loc
  
  # get player info associated with id's
  players <- httr::GET(paste0("https://www72.myfantasyleague.com/", year,
                              "/export?TYPE=players&DETAILS=&SINCE=&PLAYERS=&JSON=1"))
  js.players <- jsonlite::fromJSON(rawToChar(players$content))
  players.df <- as.data.frame(js.players$players$player)
  
  # join it all together, summarise by player, order and rank
  draft.summaries <- dplyr::left_join(drafts.df, players.df, by = "id") %>%
    group_by(name, position) %>%
    summarise(drafted_count = n(),
              dpos_min = min(dpos),
              dpos_median = median(dpos),
              dpos_mean = mean(dpos),
              dpos_max = max(dpos)) %>%
    # sort by position, then count of that position, then average dpos, then min dpos
    arrange(position, desc(drafted_count), dpos_mean, dpos_min) %>%
    ungroup() %>%
    group_by(position) %>%
    mutate(pos_count_rank = row_number()) %>%
    ungroup() %>%
    arrange(desc(drafted_count), dpos_mean, dpos_min) %>%
    mutate(overall_count_rank = row_number()) %>%
    mutate(player_factor = factor(name, rev(name)))
  
  # make some graphs
  overall.plot <-
    ggplot(data = filter(draft.summaries, overall_count_rank <= rk),
           mapping = aes(x = player_factor, y = drafted_count)) +
    geom_point() +
    scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, 1)) +
    xlab(NULL) +
    ylab("Draft Count") +
    coord_flip() +
    ggtitle("Overall") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  
  plot.list <- c()
  for (i in unique(draft.summaries$position)) {
    p <- 
      ggplot(data = filter(draft.summaries, position == i, pos_count_rank <= rk),
             mapping = aes(x = player_factor, y = drafted_count)) +
      geom_point() +
      scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, 1)) +
      xlab(NULL) +
      ylab("Draft Count") +
      coord_flip() +
      ggtitle(paste0(i)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank())
    plot.list[[i]] <- p
  }
  ordered.list <- list(plot.list$WR,
                       plot.list$RB,
                       plot.list$TE,
                       plot.list$QB,
                       plot.list$Def,
                       overall.plot)
  plots <- arrangeGrob(grobs = ordered.list,
                       ncol = 3, 
                       top = "MFL Player Exposure")
  # generate output
  outputs <- list(data = select(draft.summaries, -player_factor), 
                  plots = plots)
  return(outputs)
}
