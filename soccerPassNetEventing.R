
soccerPassNetEventing <- function(gameID = 7545, TeamName = "Argentina", poss = F, pass_dir = T, convex = T,
                          minPass = 3, node_pos = "origin", nodeFill = "blue", edgeAlpha = 0.8, edgeCol = "black",
                          label = TRUE, shortNames = TRUE, maxNodeSize = 18, maxEdgeSize = 6, 
                          labelSize = 4, title = NULL, Flipx = F, field = 1) {
        
        ##read background image
        img <- jpeg::readJPEG(paste0("fields/football-pitch_cut", field, ".jpg"))
        g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = F)
        
        ## read data
        df <- readRDS("data/SB_WC_events_clean.RDS") %>%
                filter(match_id == gameID)
        
        ## set variable names, filter team and transform coordinates
        df_events <- df %>% 
                  soccermatics::soccerTransform(method = "statsbomb") %>%
                  rename("x" = "location.x",
                         "y" = "location.y",
                         "endx" = "pass.end_location.x",
                         "endy" = "pass.end_location.y",
                         "From" = "player.name",
                         "team" = "team.name") %>%
                  mutate(y = 68 - y,
                         endy = 68 - endy,
                         x = ifelse(Flipx & !is.na(x), 105 - x, x),
                         endx = ifelse(Flipx & !is.na(x), 105 - endx, endx))

        # # Proteger si Equipo no está en el match ID ingresado
        data <- df_events %>%
                filter(team == TeamName)
        
        #Passes until time of 1st substitution or red card
        first_red_card_minute <- data %>% 
                filter(foul_committed.card.name %in% c("Second Yellow", "Red Card")) %>%
                summarise_at("minute", min, na.rm = T) %>%
                as.numeric()
        
        first_substitution_minute <- data %>% 
                filter(type.name == "Substitution") %>%
                summarise_at("minute", min, na.rm = T) %>%
                as.numeric()
        
        max_minute <- max(data$minute, na.rm = T)
        
        num_minutes <- min(first_red_card_minute, first_substitution_minute, max_minute, na.rm = T)
        
        ## passes data to use
        passes <- data %>% 
                filter(type.name == "Pass" & team == TeamName & minute < num_minutes)
        
        c_passes <- passes %>%
                filter(is.na(pass.outcome.name))
        
        ### get nodes and edges for plotting
        # node position (tracking mean position) and size (based on number of passes)
        if(node_pos == "both"){
                nodes <- c_passes %>%
                        group_by(From) %>%
                        dplyr::summarise(xini = median(x, na.rm=T), yini = median(y, na.rm=T),
                                         xend = median(endx, na.rm=T), yend = median(endy, na.rm=T),
                                         num_pass = n()) %>%
                        mutate(x = (xini + xend)/2, y = (yini + yend)/2) %>%
                        na.omit() %>%
                        mutate(size = scales::rescale(num_pass, c(2, maxNodeSize), c(min(num_pass), max(num_pass))))
                
                location_text <- "Location: origin & end of passes"
        }
        
        if(node_pos == "origin"){
                nodes <- c_passes %>%
                        group_by(From) %>%
                        dplyr::summarise(x = median(x, na.rm=T), y = median(y, na.rm=T), num_pass = n()) %>%
                        na.omit() %>%
                        mutate(size = scales::rescale(num_pass, c(2, maxNodeSize), c(min(num_pass), max(num_pass))))
                
                location_text <- "Location: origin of passes"
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
                        select(from = From, to = pass.recipient.name) %>% 
                        group_by(from, to) %>% 
                        dplyr::summarise(pass_value = n()) %>% 
                        na.omit()
                
                edges <- edgelist %>%
                        left_join(nodes %>% select(From, x, y), by = c("from" = "From")) %>%
                        left_join(nodes %>% select(From, xend = x, yend = y), by = c("to" = "From")) %>%
                        segmentsDf(3, 3, 0.6)     
                
                arrow <- arrow(type = "closed", angle = 30, length = unit(0.1, "inches"))
                
        }else{
                edgelist <- c_passes %>%
                        select(from = From, to = pass.recipient.name) %>% 
                        mutate(pairs = paste(pmin(from, to), pmax(from, to), sep = "-")) %>%
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
        
        ### Stats
        ## Passing stats 
        pass_n <- nrow(passes)
        pass_pc <- nrow(c_passes)/nrow(passes) * 100
        pass_length_m <- median(c_passes$pass.length, na.rm = T)
        
        ## Posession stats
        poss_data <- df %>%
                filter(minute < num_minutes) %>%
                group_by(possession_team.name, possession) %>%
                summarise(num_pass = sum(type.name == "Pass"),
                          pT = max(TimeInPoss))

        teams_poss <- poss_data %>%
                group_by(possession_team.name) %>%
                summarise(num_poss = n(),
                          total_pass = sum(num_pass),
                          total_pT_min = sum(pT)/60)
        
        effective_poss_time <- sum(teams_poss$total_pT_min)
        
        ##Subtitle
        
        var_minute <- ifelse(first_red_card_minute == num_minutes, "until first red card",
                             ifelse(first_substitution_minute == num_minutes, "until first substitution", "until game finished"))
        subtitle <- paste0("Data ", var_minute ,": 1' - ", round(num_minutes, 0), "'. ")
        subtitle <- paste0(subtitle, "Effective played time: ", round(effective_poss_time, 0), " mins. ", minPass, "+ passes shown.")

        poss_data <- teams_poss %>%
                filter(possession_team.name == TeamName)

        possession <- poss_data$num_poss
        pass_per_poss <- poss_data$total_pass / possession
        poss_minutes <- poss_data$total_pT_min
        poss_percent <- poss_data$total_pT_min / effective_poss_time *100
        pass_per_1M_poss <- poss_data$total_pass / poss_minutes

        ## shorten player name
        if(shortNames) {
                nodes$From <- sub(":", " ", sub(".* ", "", sub(" (di|De|de|El|el|Da|da|Dos|dos|Van|van|Von|von|Le|le|La|la|N') ", " \\1:", nodes$From)))
        }
        
        # if no title given
        if(is.null(title)) {
                title <- paste0(TeamName, "'s passing network against ", setdiff(unique(df_events$team), TeamName), " (Statsbomb eventing data)")
        }
        
        if(convex){
                # convex hull
                if(Flipx == T){
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
                
                xlabel <- 104
                ylabel <- 64
                hjust <- 1
        }else{
                xlabel <- 1
                ylabel <- 1
                hjust <- 0
        }
        
        if(field == 3){
                colText = "black"
                colConvex = "darkblue"
        } else{
                colText = "white"
                colConvex = "gold2"
        }
        
        # plot network
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
                             arrow = arrow, alpha = edgeAlpha) +
                
                geom_point(data = nodes, aes(x = x, y = y, size = size), 
                           pch = 21, fill = nodeFill, col = "white", alpha = 0.8, stroke = 1.5) +
                
                scale_size_identity() +
                scale_alpha_identity() +
                guides(size = F) +
                (if (poss){
                        annotate("text", 104, 1, label = paste0("Total passes: ", pass_n, 
                                                              "\nCompleted: ", sprintf("%.1f", pass_pc), 
                                                              "%\nMedian pass length:", round(pass_length_m,1), " [m]",
                                                              "\nPossession time: ", round(poss_minutes, 1), " min. (", round(poss_percent, 0), "%)", 
                                                              "\nPasses per min. possession: ", round(pass_per_1M_poss, 1), 
                                                              "\nPasses per possesion: ", round(pass_per_poss, 1)), 
                                 hjust = 1, vjust = 0, size = labelSize * 7/8, col = colText)
                } else{
                        annotate("text", 104, 1, label = paste0("Total Passes: ", pass_n,
                                                              "\nCompleted: ", sprintf("%.1f", pass_pc), "%\n"),
                                 hjust = 1, vjust = 0.3, size = labelSize * 6.5/8, col = colText)
                }) +
                
                annotate("text", xlabel, ylabel, label = paste0(location_text,
                                                        "\nSize: number of passes"),
                         hjust = hjust, vjust = 0, size = labelSize * 7/8, col = "grey")

        # add labels
        if(label) {
                p <- p +
                        geom_label_repel(data = nodes, aes(x, y, label=From), point.padding = 0.1,
                                         size = labelSize, col="black", fill="white", alpha=0.8,
                                         min.segment.length = 1) 
        }     
        
        return(p)
}