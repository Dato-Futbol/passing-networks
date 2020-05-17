
soccerPassNetTracking <- function(gameID = "1", TeamName = "Home", 
                                  pos_source = c("event", "track"), node_pos = "origin",
                                  context = c("all", "attacking", "defending"), half = c("all", "own", "opp"), 
                                  pass_dir = T, minPass = 3, convex = T, 
                                  nodeFill = "blue", edgeAlpha = 0.8, edgeCol = "black",
                                  maxNodeSize = 18, maxEdgeSize = 6, labelSize = 4, 
                                  label = TRUE, shortNames = TRUE, title = NULL, field = 1) {
        
        ## background image
        img <- jpeg::readJPEG(paste0("fields/football-pitch_cut", field, ".jpg"))
        g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = F)
        
        ## read data
        read_track <- function(path, teamname){
                track <- read_csv(path, skip = 2)
                
                jerseys <- sub("Player", "", names(track))[seq(4,30,2)]
                for (i in 1:length(jerseys)){
                        names(track)[i*2 +2] <- paste0(teamname, "_", jerseys[i], "_x")
                        names(track)[i*2 +3] <- paste0(teamname, "_", jerseys[i], "_y")
                }
                
                names(track)[ncol(track)-1] <- "ball_x"
                names(track)[ncol(track)] <- "ball_y"
                return(track)
        }
        
        to_single_playing_direction_and_transform <- function(df){
                
                df <- df %>%
                        mutate_at(vars(ends_with("x")), funs(ifelse(Period == 2 , 1 - ., .))) %>%
                        mutate_at(vars(ends_with("x")), funs(.*105)) %>%
                        mutate_at(vars(ends_with("y")), funs((1 - .)*68))
                return(df)
        }
        
        events <- read_csv(paste0("data/Sample_Game_", gameID, "_RawEventsData.csv")) %>%
                mutate(Minute = `Start Time [s]` / 60.0)
        
        track <- read_track(paste0("data/Sample_Game_", gameID, "_RawTrackingData_", TeamName, "_Team.csv"), TeamName)
        df_track <- to_single_playing_direction_and_transform(track)
        df_events <- to_single_playing_direction_and_transform(events)
        
        ## rename variables
        df_events <- df_events %>% 
                rename("origin_pos_x" = "Start X",
                       "origin_pos_y" = "Start Y",
                       "end_pos_x" = "End X",
                       "end_pos_y" = "End Y")

        ## Passes until time of 1st substitution or red card
        max_minute <- max(df_events$Minute, na.rm = T)
        
        first_red_card_minute <- df_events %>%
                filter(Type == "CARD" & "Subtype" == "RED") %>%
                summarise_at("Minute", min, na.rm = T)
        
        first_substitution_player <- names(df_track)[26]
        
        first_substitution_minute <- df_track %>%
                filter_at(vars(starts_with(first_substitution_player)), ~!is.na(.)) %>%
                summarise_at("Time [s]", min, na.rm = T)
        
        num_minutes <- min(first_red_card_minute$Minute, first_substitution_minute$`Time [s]`/60.0, max_minute, na.rm = T)
        
        ## passes data to use
        passes <- df_events %>% 
                filter(Type == "PASS" & Team == TeamName & Minute <= num_minutes)
        
        ## get nodes and edges for plotting
        # node position and size (based on number of passes)
        c_passes <- passes
        
        if (pos_source == "event"){
                
                if(node_pos == "both"){
                        nodes <- c_passes %>%
                                group_by(From) %>%
                                dplyr::summarise(xini = median(origin_pos_x, na.rm=T), yini = median(origin_pos_y, na.rm=T),
                                                 xend = median(end_pos_x, na.rm=T), yend = median(end_pos_y, na.rm=T),
                                                 num_pass = n()) %>% 
                                mutate(x = (xini + xend)/2, y = (yini + yend)/2) %>%
                                na.omit() %>%
                                ungroup() %>%
                                mutate(size = scales::rescale(num_pass, c(2, maxNodeSize), c(min(num_pass), max(num_pass))))
                        
                        location_text <- "Location: origin & end of passes"
                }
                if(node_pos == "origin"){
                        nodes <- c_passes %>%
                                group_by(From) %>%
                                dplyr::summarise(x = median(origin_pos_x, na.rm=T), y = median(origin_pos_y, na.rm=T), num_pass = n()) %>%
                                na.omit() %>%
                                ungroup() %>%
                                mutate(size = scales::rescale(num_pass, c(2, maxNodeSize), c(min(num_pass), max(num_pass))))
                        
                        location_text <- "Location: origin of passes"
                }
        }

        if (pos_source == "track"){
                
                ## Possesion frames
                poss_start_events <- c('PASS', 'RECOVERY', 'SET PIECE', 'SHOT') 
                poss_change_events <- c("BALL LOST", "BALL OUT", "HEAD-ON TARGET-GOAL", "ON TARGET-GOAL", 
                                        "WOODWORK-GOAL", "OFF TARGET-OUT", "HEAD-OFF TARGET-OUT", "ON TARGET-SAVED")

                df_events_simple <- df_events %>%
                        filter(!Type %in% c("CHALLENGE", "CARD"))
                
                current_window_start <- df_events %>%
                        filter(Subtype == "KICK OFF") %>%
                        dplyr::select("Start Frame") %>%
                        head(1) %>%
                        unlist()
                
                on_ball_frames <- list()
                off_ball_frames <- list()

                for (event_index in 1:nrow(df_events_simple)){

                        event_type = df_events_simple[event_index, "Type"] %>%
                                as.character()

                        if(event_type %in% poss_change_events | df_events_simple[event_index, "Subtype"] %in% poss_change_events){
                                
                                if(event_type == "BALL OUT"){
                                        current_window_end <- df_events_simple[event_index, "Start Frame"] %>% unlist()
                                }else{
                                        current_window_end <- df_events_simple[event_index, "End Frame"] %>% unlist()
                                }

                                next_starts <- df_events_simple %>%
                                        filter(row_number() > event_index & row_number() <= (event_index +10) & Type %in% poss_start_events)

                                if(nrow(next_starts) >0){
                                        next_start <- next_starts %>%
                                                slice(1)
                                        
                                        temp <- seq(current_window_start, current_window_end)
                                        
                                        if(df_events_simple[event_index, "Team"] == TeamName){
                                                on_ball_frames <- c(on_ball_frames, temp)
                                        }else{
                                                off_ball_frames <- c(off_ball_frames, temp)
                                        }

                                        current_window_start <- next_start[, "Start Frame"] %>% unlist()
                                }
                        }
                }
                
                ## Nodes location
                df_track <- df_track %>%
                        filter(Frame < max(c_passes$`End Frame`))
                
                x_columns <- df_track %>% dplyr::select(ends_with("_x"), -"ball_x") %>% names()
                y_columns <- df_track %>% dplyr::select(ends_with("_y"), -"ball_y") %>% names()

                ## Filters depending on context selection
                # possesion context
                if(context != "All"){
                        if(context == "attacking"){
                                df_track <- df_track %>%
                                        filter(Frame %in% on_ball_frames)
                                
                                location_text <- "Location: on ball"
                        }
                        
                        if(context == "defending"){
                                df_track <- df_track %>%
                                        filter(Frame %in% off_ball_frames)
                                
                                location_text <- "Location: off ball"
                        }    
                        location_text <- "Location: mean tracking position"
                }
                
                # ball position context
                match_start <- df_events %>%
                        filter(Subtype == "KICK OFF") %>%
                        dplyr::select("Start Frame") %>%
                        head(1) %>%
                        unlist()        
                
                mean_x <- track %>%
                        filter(Frame == match_start) %>%
                        dplyr::select(x_columns) %>%
                        unlist() %>%
                        mean(na.rm=T) *105
                
                if (half != "all"){

                        if(half == "own") {
                                if(mean_x < (0.5*105)){
                                        df_track <- df_track %>%
                                                filter(ball_x < (0.5*105))
                                }else{
                                        df_track <- df_track %>%
                                                filter(ball_x >= (0.5*105))
                                }
                                
                                location_text <- paste0(location_text, ", own half")
                        }
                        
                        if(half == "opp") {
                                if(mean_x < (0.5*105)){
                                        df_track <- df_track %>%
                                                filter(ball_x >= (0.5*105))
                                }else{
                                        df_track <- df_track %>%
                                                filter(ball_x < (0.5*105))
                                }
                                
                                location_text <- paste0(location_text, ", opponent half")
                        }
                }else{location_text <- paste0(location_text, ", all field")}
                
                df_pos_x = df_track %>%
                        pivot_longer(cols = all_of(x_columns)) %>%
                        rename("player" = "name") %>%
                        group_by(player) %>%
                        summarise(x = median(value, na.rm = T))

                df_pos_y = df_track %>%
                        pivot_longer(cols = all_of(y_columns)) %>%
                        rename("player" = "name") %>%
                        group_by(player) %>%
                        summarise(y = median(value, na.rm = T))
                
                player_position = df_pos_x %>%
                        bind_cols(df_pos_y) %>%
                        mutate(Player = paste0("Player", str_match(player, "_(.*?)_")[, 2]))
                
                frames <- df_track$Frame
               
                nodes <- c_passes %>%
                        filter(`Start Frame` %in% frames | `End Frame` %in% frames) %>%
                        group_by(From) %>%
                        dplyr::summarise(num_pass = n()) %>%
                        na.omit() %>%
                        mutate(size = scales::rescale(num_pass, c(2, maxNodeSize), c(min(num_pass), max(num_pass))))
                
                nodes <- nodes %>%
                        left_join(player_position %>% dplyr::select(Player, x, y), by = c("From" = "Player"))
        }
        
        ## edges based only on completed passes [pass_dir = T means splitting by direction and ploting arrows]
        if(pass_dir){
                
                segmentsDf <- function(data, shorten.start, shorten.end, offset){
                        
                        data$dx = data$xend - data$x
                        data$dy = data$yend - data$y
                        data$dist = sqrt( data$dx^2 + data$dy^2 )
                        data$px = data$dx/data$dist
                        data$py = data$dy/data$dist
                        
                        data$x = data$x + data$px * shorten.start
                        data$y = data$y + data$py * shorten.start
                        data$xend = data$xend - data$px * shorten.end
                        data$yend = data$yend - data$py * shorten.end
                        data$x = data$x - data$py * offset
                        data$xend = data$xend - data$py * offset
                        data$y = data$y + data$px * offset
                        data$yend = data$yend + data$px * offset
                        
                        return(data)
                }
                
                edgelist <- c_passes %>%
                        filter(`Start Frame` %in% frames | `End Frame` %in% frames) %>%
                        select(from = From, to = To) %>%
                        group_by(from, to) %>% 
                        dplyr::summarise(pass_value = n()) %>%
                        na.omit()
                
                edges <- edgelist %>%
                        left_join(nodes %>% select(From, x, y), by = c("from" = "From")) %>%
                        left_join(nodes %>% select(From, xend = x, yend = y), by = c("to" = "From")) %>%
                        segmentsDf(2, 2, 0.6)     
                
                arrow <- arrow(type = "closed", angle = 30, length = unit(0.1, "inches"))
                
        }else{
                edgelist <- c_passes %>%
                        filter(`Start Frame` %in% frames | `End Frame` %in% frames) %>%
                        mutate(pairs = paste(pmin(From, To), pmax(From, To), sep = "-")) %>%
                        group_by(pairs) %>% 
                        summarise(pass_value = n()) %>%
                        mutate(name1 = sub("-.*", "", pairs),
                               name2 = sub(".*-", "", pairs))
                
                edges <- edgelist %>%
                        left_join(nodes %>% select(From, x, y), by = c("name1" = "From")) %>%
                        left_join(nodes %>% select(From, xend = x, yend = y), by = c("name2" = "From"))
                
                arrow <- NULL
        }
        
        edges <- edges %>% 
                filter(pass_value >= minPass) %>%
                ungroup() %>%
                mutate(size = scales::rescale(pass_value, c(0.1, maxEdgeSize), c(min(pass_value), max(pass_value))))
        
        ## Completing subtitle
        var_minute <- ifelse(first_red_card_minute$Minute == num_minutes, "until first red card",
                             ifelse(first_substitution_minute$`Time [s]`/60 == num_minutes, "until first substitution", "until game finished"))
        subtitle <- paste0("Data ", var_minute ,": 1' - ", round(num_minutes, 0), "'. ")
        subtitle <- paste0(subtitle, minPass, "+ passes shown. ", "Context: ", context, ", ", half, " field.")
        
        ## shorten player name
        if(shortNames) {
                nodes$From <- sub(":", " ", sub(".* ", "", sub(" (di|De|de|El|el|Da|da|Dos|dos|Van|van|Von|von|Le|le|La|la|N') ", " \\1:", nodes$From)))
        }
        
        # if no title given
        if(is.null(title)) {
                title <- paste0(TeamName, "'s passing network against ", setdiff(unique(df_events$Team), TeamName), " (Metrica Sport tracking data)")
        }
        
        if(convex){
                # convex hull
                if(mean_x > (0.5*105)){
                        xown = 105
                        xopp = 0
                        xGK = max(nodes$x)
                }else{
                        xown = 0
                        xopp = 105
                        xGK = min(nodes$x)
                }
                
                hull_data <- nodes %>%
                        filter(x != xGK) %>%  # removing GK
                        dplyr::select(x, y) %>%
                        slice(chull(x, y))
                
                hull_center <- hull_data %>%
                        summarise(xmean = mean(x), ymean = mean(y))
                
                dist_own_goal <- sqrt( (hull_center$xmean - xown)^2 + (hull_center$ymean - 34)^2 )
                dist_opp_goal <- sqrt( (hull_center$xmean - xopp)^2 + (hull_center$ymean - 34)^2 )
                amplitude <- max(hull_data$y) - min(hull_data$y)
                depth <- max(hull_data$x) - min(hull_data$x)
                occupied_area <- areapl(cbind(hull_data$x, hull_data$y))        
        }
        
        if(field == 3){
                colText = "black"
                colConvex = "darkred"
        } else{
                colText = "white"
                colConvex = "gold2"
        }
        
        ## plot network
        p <-   ggplot() +
                annotation_custom(g, xmin = -0.5, xmax = 105.5, ymin = -0.5, ymax = 68.5) +
                scale_x_continuous(limits = c(-0.5, 105.5), expand = c(0,0)) +
                scale_y_continuous(limits = c(-0.5, 68.5), expand = c(0,0)) +
                labs(x = "", y = "", title = title, subtitle = subtitle, caption = "@DatoFutbol_cl") +
                theme_bw() +
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      plot.title = element_text(vjust = -0.2, size = 14),
                      plot.subtitle = element_text(size = 10, face = "italic"),
                      plot.caption = element_text(size = 10, vjust = 7),
                      plot.margin = unit(c(0.6, 0.8, 0.2, 0.4), "cm"))
                
                # Convex hull                
         if (convex == T) {
                p <- p + geom_polygon(data = hull_data, aes(x=x, y=y), col = colConvex, fill = "transparent", alpha = 0.2, size = 0.7) +
                        geom_point(data = hull_center, aes(x=xmean, y=ymean), pch = 21, col = colConvex,
                                fill = "black", stroke = 2, size = 6) +
                        geom_text_repel(data = hull_center, aes(x=xmean, y=ymean), label = "CENTROID", point.padding = 0.5,
                                size = 2, col=colConvex, alpha=0.8, nudge_y = 1) +
                        annotate("text", 1, 1, label = paste0("Distance to own goal: ", round(dist_own_goal, 1), " [m]",
                                                              "\nDistance to opponent goal: ", round(dist_opp_goal, 1), " [m]",
                                                              "\nAmplitude: ", round(amplitude, 1), " [m]",
                                                              "\nDepth: ", round(depth, 1), " [m]",
                                                              "\nOccupied area: ", round(occupied_area, 1), " [m2]"),
                                 hjust = 0, vjust = 0, size = 4 * 7/8, col = colConvex)
        }        
                
        p <- p +
                geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend, size = size), 
                             arrow = arrow, alpha = edgeAlpha, col = edgeCol) +
                
                geom_point(data = nodes, aes(x = x, y = y, size = size), 
                           pch = 21, fill = nodeFill, col = colText, alpha = 0.8, stroke = 1.5) +
                
                scale_size_identity() +
                scale_alpha_identity() +
                guides(size = F) +
                annotate("text", 104, 1, label = paste0(location_text,
                                                        "\nSize: number of passes"),
                         hjust = 1, vjust = 0, size = 4 * 7/8, col = "grey") 

        # add player labels
        if(label) {
                p <- p +
                        geom_label_repel(data = nodes, aes(x, y, label = From), point.padding = 0.1,
                                         size = labelSize, col="black", fill="white", alpha=0.8, min.segment.length = 1) 
        }     
        
        return(p)
}