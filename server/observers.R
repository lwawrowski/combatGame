# reactive values ---------------------------------------------------------

game <- reactiveValues(map = NULL)

log <- reactiveValues(data = list(), start = NULL)

# new game ----------------------------------------------------------------

observeEvent(input$newgame, {
  
  if(nrow(units_players$player1) > 0 & nrow(units_players$player2) > 0){
    
    game$map <- NULL
    
    obst_game <- data.frame(type=c(rep(1, 7), # high obstacles
                                   rep(2, 12)), # low obstacles
                            id=c("ID138", "ID137", "ID136", "ID224", "ID223", "ID222", "ID221",
                                 "ID50", "ID66", "ID82", "ID98", "ID99", "ID100", "ID101", "ID273", "ID274", "ID275", "ID276", "ID277"))
    
    units_battle_hex <- draw_units_hex()
    
    map_sp <- maps$grid
    map_df <- maps$grid_df
    map_ct <- maps$grid_ct
    
    units_type <- units_battle_hex %>%
      select(id, name) %>%
      mutate(id=as.character(id))
    
    map_ct_units <- left_join(map_ct, units_type, by="id") %>%
      mutate(name=ifelse(is.na(name), "", as.character(name)))
    
    obstacles <- obstacles_update(units_battle_hex, obst_game)
    
    obstacles <- obstacles %>%
      mutate(id=as.character(id))
    
    map_df_units <- left_join(map_df, obstacles, by="id") %>%
      mutate(type=ifelse(is.na(type), "", as.character(type)))
    
    player <- sample(c(-1,1),1)
    
    game$map <- list(map_sp=map_sp, 
                     map_df=map_df_units, 
                     map_ct=map_ct_units, 
                     units=units_battle_hex, 
                     obst=obstacles, 
                     unit_clicked="",
                     player=player,
                     end_game=0,
                     shots=0,
                     turn_log=c(1,0,0),
                     curr_mobility=c(0,0),
                     last_move="")
    
    # log
    log$data <- list()
    log$start <- stringi::stri_rand_strings(1, 4) # game id
  }
  
})

# observe map_click -------------------------------------------------------

observeEvent(input$map_click, {
  
    if(is_unit_clicked()){
      if(check_clicked_id() != game$map$unit_clicked){
        clear_mobility()
        check_mobility()
      } else {
        clear_mobility()
      }
    } else if(!is_unit_clicked() & game$map$unit_clicked != "") {
      move_reactive()
    }
  
})


# observe map_dblclick ----------------------------------------------------

observeEvent(input$map_dblclick, {
  
  if(game$map$unit_clicked!="" & is_unit_dblclicked()){
    attack_reactive()
  }
  
})

# observe end_turn --------------------------------------------------------

observeEvent(input$end_turn, {
  
  turn_end()
  
})

# observe AI --------------------------------------------------------------

observe({
  
  if(!is.null(game$map)){
    if(game$map$player == 1 & game$map$end_game == 0){
      action <- lookaheadAI()
      
      u1_name <- game$map$units$name[game$map$units$id == action$pid_start]

      if(action$aid == ""){ # only move
        move(action$pid_start, action$pid_move)
      } else if(action$pid_move == ""){ # only attack
        attack(action$pid_start, action$aid)
      } else { # move and attack
        move(action$pid_start, action$pid_move)
        attack(action$pid_move, action$aid)
      }

      turn_end()
    }
  }
  
})