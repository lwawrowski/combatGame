# lookaheadAI -------------------------------------------------------------

lookaheadAI <- function(current_player = game$map$player, units = game$map$units, pid = NULL){
  
  # action
  eval_action <- NULL
  
  # units which can be utilized
  if(is.null(pid)){
    player_units <- units %>%
      dplyr::filter(plnum==current_player)
  } else {
    player_units <- units %>%
      dplyr::filter(id==pid)
  }
  
  # possible actions
  eval_function_df <- data.frame(pid_start=character(0),
                                 pid_move=character(0),
                                 aid=character(0),
                                 evalf=numeric(0))
  
  for(u in 1:nrow(player_units)){
    
    current_id <- as.character(player_units$id[u])
    
    ur <- c(units$range_min[units$id==current_id], units$range_max[units$id==current_id])
    
    units_range <- units_in_range(current_id, units, ur, current_player)
    
    if(!is.null(units_range)){
      
      evalf_df_u <- data.frame(pid_start=current_id,
                               pid_move="",
                               aid=units_range,
                               evalf=0)
      
      for(a in 1:nrow(evalf_df_u)){
        # calculations for evaluation function
        hp <- check_hp(as.character(evalf_df_u$pid_start[a]), as.character(evalf_df_u$aid[a]), units)
        hp_aid <- units$hp_c[units$id==evalf_df_u$aid[a]]
        chance_list <- obstacles_chance(as.character(evalf_df_u$pid_start[a]), as.character(evalf_df_u$aid[a]), units)
        chance <- chance_list$chance
        
        # max potential revenge hp
        revenge_hp <- check_hp(as.character(evalf_df_u$aid[a]), as.character(evalf_df_u$pid_start[a]), units)
        
        # 2 x range
        ur2 <- ur*c(1,2)
        units_2range <- units_in_range(current_id, units, ur2, current_player)
        
        # number of opponents
        num_opp <- length(opponents_in_range(as.character(evalf_df_u$pid_start[a]), units, current_player))
        
        # is boss
        is_boss <- units$name[units$id==current_id] == "boss"
        
        # possible boss attack
        boss_attack <- possible_boss_attack(as.character(evalf_df_u$aid[a]), units, current_player)
        
        # evaluation function
        evalf_df_u$evalf[a] <- chance*100 + hp + as.numeric(hp > hp_aid)*100 + length(units_range)*4 + length(units_2range)*2 - 
          revenge_hp - num_opp*2 - as.numeric(is_boss)*300 + boss_attack*300
        
        }
      
      eval_function_df <- rbind(eval_function_df, evalf_df_u)
      
    }
    
    # possible other hexes
    unit_mobility_hexs <- mobility_hexs(id_hex = current_id, un = units)
    new_hexs <- sample(names(unit_mobility_hexs), 9, prob = unit_mobility_hexs)
    
    # new hexes
    for(h in 1:length(new_hexs)){
      
      units_temp <- units
      
      temp_id <- new_hexs[h]
      
      units_temp$id[units_temp$id==current_id] <- temp_id
      
      units_temp_range <- units_in_range(temp_id, units_temp, ur, current_player)
      
      if(!is.null(units_temp_range)){
        
        evalf_df_u <- data.frame(pid_start=current_id,
                                 pid_move=temp_id,
                                 aid=units_temp_range,
                                 evalf=0)
        
        for(a in 1:nrow(evalf_df_u)){
          
          hp <- check_hp(as.character(evalf_df_u$pid_move[a]), as.character(evalf_df_u$aid[a]), units_temp)
          hp_aid <- units$hp_c[units$id==evalf_df_u$aid[a]]
          chance_list <- obstacles_chance(as.character(evalf_df_u$pid_move[a]), as.character(evalf_df_u$aid[a]), units_temp)
          chance <- chance_list$chance
          
          revenge_hp <- check_hp(as.character(evalf_df_u$aid[a]), as.character(evalf_df_u$pid_move[a]), units_temp)
          
          ur2 <- ur*c(1,2)
          units_temp_2range <- units_in_range(temp_id, units_temp, ur2, current_player)
          
          num_opp <- length(opponents_in_range(as.character(evalf_df_u$pid_move[a]), units_temp, current_player))
          
          is_boss <- units$name[units$id==current_id] == "boss"
          
          boss_attack <- possible_boss_attack(as.character(evalf_df_u$aid[a]), units, current_player)
          
          evalf_df_u$evalf[a] <- chance*100 + hp + as.numeric(hp > hp_aid)*100 + length(units_temp_range)*4 + 
            length(units_temp_2range)*2 - 
            revenge_hp - num_opp*2 - as.numeric(is_boss)*300 + boss_attack*300
          
        }
        
        eval_function_df <- rbind(eval_function_df, evalf_df_u)
        
      }
      
    }
    
    
  }
  
  if(nrow(eval_function_df) > 0){
    
    eval_action <- eval_function_df %>%
      arrange(desc(evalf)) %>%
      slice(1)
    
  } else {
    
    # move
    
    if(length(log$data) > 0){
      
      log_data <- bind_rows(log$data) %>%
        dplyr::filter(pl==current_player) %>%
        select(u1_id) %>%
        group_by(u1_id) %>%
        count() %>%
        ungroup()
      
      if(nrow(log_data) > 0){
        
        pid <- player_units %>%
          select(unit_id, name, hp_c, price, id) %>%
          mutate(u1_id=as.character(unit_id)) %>%
          left_join(., log_data, by="u1_id") %>%
          mutate(n=ifelse(is.na(n),0,n)) %>%
          arrange(n, desc(hp_c), desc(price)) %>%
          dplyr::filter(name!="boss") %>%
          slice(1) %>%
          .$id
        
      } else {
        
        pid <- player_units %>%
          dplyr::filter(name!="boss") %>%
          arrange(desc(hp_c), desc(price)) %>%
          slice(1) %>%
          .$id
        
        if(length(pid)==0){
          
          pid <- player_units %>%
            arrange(desc(hp_c), desc(price)) %>%
            slice(1) %>%
            .$id
          
        }
        
      }
      
    } else {
      
      pid <- player_units %>%
        dplyr::filter(name!="boss") %>%
        arrange(desc(hp_c), desc(price)) %>%
        slice(1) %>%
        .$id
      
      if(length(pid)==0){
        
        pid <- player_units %>%
          arrange(desc(hp_c), desc(price)) %>%
          slice(1) %>%
          .$id
        
      }
    }
    
    # sample mobility hex
    pid_mobility <- mobility_hexs(id_hex = pid, un = units)
    new_hex <- sample(names(pid_mobility), 1, prob = pid_mobility)
    
    eval_action <- data.frame(pid_start=pid,
                              pid_move=new_hex,
                              aid="")
    
  }
  
  return(eval_action)
  
}