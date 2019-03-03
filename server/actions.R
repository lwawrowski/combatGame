# distance_reduction ------------------------------------------------------

distance_reduction <- function(min_range, max_range, acc, distance){
  
  a <- c(min_range,1)
  b <- c(max_range,acc/10)
  
  betas <- linear_model(a,b)
  
  y <- betas[2]*distance+betas[1]
  
  return(y)
}

# check_hp ----------------------------------------------------------------

check_hp <- function(pid, aid, units){
  
  u1 <- units[units$id==pid,]
  u2 <- units[units$id==aid,]
  
  hp_base <- 35*1.1^(u1$attack-u2$defence)
  hp_diff <- 1.005^(u1$hp_c-u2$hp_c)
  
  dist <- dist_matrix[pid,aid]
  
  range <- ifelse(dist %in% u1$range_min:u1$range_max, 1, 0)
  
  hp <- hp_base * hp_diff * range
  
  return(hp)
}

# attack_hp ---------------------------------------------------------------

attack_hp <- function(pid, aid, units){
  
  hp <- check_hp(pid, aid, units = units)
  
  u1 <- units[units$id==pid,]
  
  obst_chance_list <- obstacles_chance(pid, aid, units)
  obst_chance <- obst_chance_list$chance
  
  global_chance <- as.logical(rbinom(1, 1, obst_chance))
  
  accuracy <- runif(n = 1, min = 0, max = 100)
  
  if(global_chance){ 
    
    if(accuracy > u1$acc_max){
      acc_final <- 1.3
    } else if(accuracy < u1$acc_min) {
      acc_final <- 0.3
    } else {
      acc_final <- round(runif(n=1, min = 0.95, max = 1.05),2)
    }
    
    hp_final <- round(hp*acc_final)
    
    attack_result <- list(hp = hp_final, aid_final = aid, type = "type45")
    
  } else {
    
    obst_chance <- data.frame(id=c(obst_chance_list$type12, obst_chance_list$type45),
                              type=c(rep("12",length(obst_chance_list$type12)), rep("45",length(obst_chance_list$type45))))
    
    if(nrow(obst_chance) > 0){
      
      obst_sampled <- sample_n(obst_chance, 1)
      
      if(obst_sampled$type == "12"){ 
        
        acc_final <- 0
        
        hp_final <- round(hp*acc_final)
        
        attack_result <- list(hp = hp_final, aid_final = aid, type = "type45")
        
      } else {
        
        acc_final <- round(runif(n=1, min = 0.9, max = 1.1),2)
        
        hp_final <- round(hp*acc_final)
        
        attack_result <- list(hp = hp_final, aid_final = as.character(obst_sampled$id), type = "type45")
        
      }
      
    } else { 
      
      acc_final <- 0.5
      
      hp_final <- round(hp*acc_final)
      
      attack_result <- list(hp = hp_final, aid_final = aid, type = "type45")
      
    }
    
  }
  
  return(attack_result)
  
}

# mobility_hexs -----------------------------------------------------------

mobility_hexs <- function(id_hex, mobility = NULL, un){
  id_hex <- as.character(id_hex)
  if(is.null(mobility)){
    mobility <- as.numeric(un$mobil[un$id==id_hex])
  } 
  hexs <- dist_matrix[id_hex,dist_matrix[id_hex,] <= mobility]
  hexs_noids <- hexs[!(names(hexs) %in% game$map$obst$id)]
  return(hexs_noids)
}

# range_hexs --------------------------------------------------------------

range_hexs <- function(id_hex, unit_range){
  range_min <- unit_range[1]
  range_max <- unit_range[2]
  hexs <- names(which(dist_matrix[id_hex,] >= range_min & dist_matrix[id_hex,] <= range_max))
  return(hexs)
}

# units_in_range ----------------------------------------------------------

units_in_range <- function(id_hex, units, unit_range, cp){
  player <- -cp
  in_range_hexs <- range_hexs(id_hex, unit_range)
  ids <- as.character(units$id[units$plnum==player])
  in_range <- ids[ids %in% in_range_hexs]
  num_in_range <- length(in_range)
  if(num_in_range==0){
    return(NULL)
  } else {
    return(in_range)
  }
}

# opponents_in_range ------------------------------------------------------

opponents_in_range <- function(pid, un, cp){
  
  player <- -cp
  opps_in_range <- character()
  ids <- as.character(un$id[un$plnum==player])
  
  for(i in 1:length(ids)){
    unit_range <- c(un$range_min[un$id==ids[i]], un$range_max[un$id==ids[i]])
    unit_in_range <- units_in_range(ids[i], un, unit_range, player)
    num_opp_i <- sum(pid %in% unit_in_range)
    
    if(num_opp_i > 0){
      opps_in_range <- c(opps_in_range, ids[i])
    }
  }
  
  return(opps_in_range)
  
}

# possible_boss_attack ----------------------------------------------------

possible_boss_attack <- function(aid, un, cp){
  
  boss_attack <- 0
  
  id_boss <- un$id[un$name == "boss" & un$plnum == cp]
  
  if(length(id_boss) > 0){
    player <- -cp
    
    unit_range <- c(un$range_min[un$id==aid], un$range_max[un$id==aid])
    unit_in_range <- units_in_range(aid, un, unit_range, player)
    boss_attack <- sum(id_boss %in% unit_in_range)
    
  }
  
  return(boss_attack)
  
}

# check_mobility ----------------------------------------------------------

check_mobility <- reactive({
  
  if(is_unit_current_player()){
    
    id_clicked <- check_clicked_id()
    
    if(game$map$curr_mobility[1] == 0 | id_clicked == game$map$last_move){
      
      if(game$map$curr_mobility[1] != 0){
        mobility <- game$map$curr_mobility[2]
      } else {
        mobility <- game$map$units$mobil[game$map$units$id==id_clicked]
      }
      
      hexs <- names(mobility_hexs(id_clicked, mobility, game$map$units))
      
      map_df <- game$map$map_df 
      
      # update map
      map_df[map_df$id==id_clicked, c("type")] <- "6"
      map_df[map_df$id %in% hexs, c("type")] <- "7"
      
      game$map$map_df <- map_df
      game$map$unit_clicked <- id_clicked
      game$map$curr_mobility[2] <- mobility
    }
    
  }
  
})

# clear_mobility ----------------------------------------------------------

clear_mobility <- reactive({
  game$map$map_df[game$map$map_df$type=="7", c("type")] <- ""
  game$map$map_df[game$map$map_df$type=="6", c("type")] <- ifelse(game$map$player == -1,"4","5")
  game$map$unit_clicked <- ""
  
}) 


# move --------------------------------------------------------------------

move <- function(start, final){
  
  start <- as.character(start)
  final <- as.character(final)
  
  # new data
  game$map$map_df[game$map$map_df$id==final,c("type")] <- game$map$map_df[game$map$map_df$id==start,c("type")]
  game$map$map_ct[game$map$map_ct$id==final,c("name")] <- game$map$map_ct[game$map$map_ct$id==start,c("name")]
  game$map$units[game$map$units$id==start,c("id")] <- final
  
  # clean previous place
  game$map$map_df[game$map$map_df$id==start,c("type")] <- ""
  game$map$map_ct[game$map$map_ct$id==start,c("name")] <- ""
  
  # log
  dist <- as.numeric(dist_matrix[start, final])

  a <- game$map$turn_log[3]

  df_log <- data.frame(game_id=log$start,
                       turn=game$map$turn_log[1],
                       pl=game$map$player,
                       type="move",
                       s_hex=start,
                       f_hex=final,
                       u1_name=as.character(game$map$units$name[game$map$units$id==final]),
                       u2_name="",
                       dist=as.character(dist),
                       u1_hp=as.character(game$map$units$hp_c[game$map$units$id==final]),
                       u2_hp="",
                       hp="",
                       p_chance="")

  log$data[[paste0("a",a)]] <- df_log %>%
    mutate_if(is.factor, as.character)

  game$map$turn_log[3] <- a + 1
  
  # update obstacles
  game_obst <- game$map$obst %>%
    dplyr::filter(type %in% 1:2)
  
  game$map$obst <- obstacles_update(game$map$units, game_obst)
}


# move_reactive -----------------------------------------------------------

move_reactive <- reactive({
  
  if(!is_unit_clicked() & game$map$unit_clicked != ""){
    
    new_position <- check_clicked_id()
    
    map_df <- game$map$map_df
    map_ct <- game$map$map_ct
    map_un <- game$map$units
    
    if(game$map$curr_mobility[1] != 0){
      mobility <- game$map$curr_mobility[2]
    } else {
      mobility <- game$map$units$mobil[game$map$units$id==game$map$unit_clicked]
    }
    
    possible_move <- new_position %in% names(mobility_hexs(game$map$unit_clicked, mobility, game$map$units))
    
    if(possible_move){
      
      move(game$map$unit_clicked, new_position)
      
      dist <- as.numeric(dist_matrix[game$map$unit_clicked, new_position])
      curr_mobility <- game$map$curr_mobility[2]
      game$map$curr_mobility <- c(1, curr_mobility - dist)
      
      game$map$unit_clicked <- new_position
      game$map$last_move <- new_position
      
      game$map$map_df[game$map$map_df$type=="7", c("type")] <- ""
      check_mobility()
      
    }
    
  }
  
})


# attack ------------------------------------------------------------------

attack <- function(pid, aid){
  
  pid <- as.character(pid)
  aid <- as.character(aid)
  
  attack_list <- attack_hp(pid, aid, game$map$units)
  
  hp_attack <- attack_list$hp
  
  if(attack_list$type == "type45"){
    aid <- attack_list$aid_final
  }
  
  hp_id <- game$map$units$hp_c[game$map$units$id==aid]
  
  game$map$units$hp_c[game$map$units$id==aid] <- hp_id - hp_attack
  
  # log
  chance <- obstacles_chance(pid, aid, game$map$units)
  
  a <- game$map$turn_log[3]
  
  df_log <- data.frame(game_id=log$start,
                       turn=game$map$turn_log[1],
                       pl=game$map$player,
                       type="attack",
                       s_hex=pid,
                       f_hex=aid,
                       u1_name=as.character(game$map$units$name[game$map$units$id==pid]),
                       u2_name=as.character(game$map$units$name[game$map$units$id==aid]),
                       dist=as.character(dist_matrix[pid,aid]),
                       u1_hp=as.character(game$map$units$hp_c[game$map$units$id==pid]),
                       u2_hp=as.character(game$map$units$hp_c[game$map$units$id==aid]),
                       hp=as.character(hp_attack),
                       p_chance=as.character(chance$chance))
  
  log$data[[paste0("a",a)]] <- df_log %>%
    mutate_if(is.factor, as.character)
  
  game$map$turn_log[3] <- a + 1
  
  # eliminate units
  
  if(hp_attack >= hp_id){
    
    game$map$map_df[game$map$map_df$id==aid, c("type")] <- "" 
    game$map$map_ct[game$map$map_ct$id==aid, c("name")] <- "" 
    game$map$units <- game$map$units[!game$map$units$id==aid,]
    
    game_obst <- game$map$obst %>%
      dplyr::filter(type %in% 1:2)
    
    game$map$obst <- obstacles_update(game$map$units, game_obst)
    
  }
  
  game$map$shot <- game$map$shot + 1

  pid_name <- game$map$units$name[game$map$units$id == pid]
  
  p1_units <- game$map$units %>%
    dplyr::filter(player=="p1")
  
  p2_units <- game$map$units %>%
    dplyr::filter(player=="p2")
  
  if(nrow(p1_units)==0){
    showModal(modalDialog(title = "GAME OVER", "You lose!", footer = modalButton("Close")))
  } else if(nrow(p2_units)==0){
    showModal(modalDialog(title = "GAME OVER", "You win!", footer = modalButton("Close")))
  }
  
  if(nrow(p1_units)==0 | nrow(p2_units)==0){
    game$map$end_game <- 1
  }
  
}

# attack_reactive ---------------------------------------------------------

attack_reactive <- reactive({
  
  if(!is.null(game$map)){
    
    if(game$map$unit_clicked != "" & is_unit_dblclicked()){
      
      dblclicked_unit <- check_dblclicked_id()
      
      ur <- c(game$map$units$range_min[game$map$units$id==game$map$unit_clicked], game$map$units$range_max[game$map$units$id==game$map$unit_clicked])
      
      # without friendly fire
      # possible_attack <- dblclicked_unit %in% units_in_range(game$map$unit_clicked, game$map$units, ur, game$map$player)
      
      # friendly fire
      possible_attack <- dblclicked_unit %in% c(units_in_range(game$map$unit_clicked, game$map$units, ur, game$map$player),
                                                units_in_range(game$map$unit_clicked, game$map$units, ur, -game$map$player))
      
      if(possible_attack){
        
        attack(game$map$unit_clicked, dblclicked_unit)
        
        u_name <- game$map$units$name[game$map$units$id==game$map$unit_clicked]
        
        turn_end()
        
      }
      
    }
    
  }
  
})


# turn_end ----------------------------------------------------------------

turn_end <- function(){

  clear_mobility()
  
  t <- game$map$turn_log[1]
  game$map$turn_log[1] <- t + 1
  game$map$curr_mobility <- c(0,0)
  game$map$last_move <- "" 
  game$map$player <- -game$map$player
}