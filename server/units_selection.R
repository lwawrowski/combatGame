# units selection

units_players <- reactiveValues(player1 = NULL, player2 = NULL)

# draw units --------------------------------------------------------------

# points to use
points <- 50

draw_units <- function(points = 50){
  
  units_draw <- units
  
  units_drawned <- data.frame()
  
  points_left <- points
  sum <- 0
  
  while(points_left >= 6){
    
    unit_draw <- units_draw %>%
      dplyr::filter(price <= points_left) %>%
      sample_n(1)
    
    units_drawned <- rbind(units_drawned, unit_draw)
    
    points_left <- points_left - as.numeric(unit_draw$price)
    
    sum <- sum(units_drawned$price) 
    
    # only one boss
    general <- units_drawned %>%
      dplyr::filter(name == "boss") %>%
      count() %>%
      .$n
    
    if(general != 0 & nrow(units_draw[units_draw$name=="boss",])!=0){
      units_draw <- units_draw[units_draw$name!="boss",]
    }
  }
  
  units_drawned$name <- paste0(units_drawned$name, 1:nrow(units_drawned))
  
  return(units_drawned)
}

units_players$player1 <- draw_units(points)
units_players$player2 <- draw_units(points)

# draw units hex ----------------------------------------------------------

draw_units_hex <- function(){
  
  u1_units <- nrow(units_players$player1)
  u2_units <- nrow(units_players$player2)
  
  # add ID
  units_p1 <- data.frame(units_players$player1, player=rep("p1", u1_units), unit_id=paste0("p1id",1:u1_units), plnum=-1)
  units_p2 <- data.frame(units_players$player2, player=rep("p2", u2_units), unit_id=paste0("p2id",1:u2_units), plnum=1)
  units_battle <- rbind(units_p1, units_p2)
  
  # draw hex
  hex_battle <- data.frame(unit_id=c(paste0("p1id",1:u1_units),paste0("p2id",1:u2_units)),
                           id=c(sample(paste0("ID", 1:16), u1_units),sample(paste0("ID", 342:357), u2_units)))
  
  units_battle_hex <- merge(units_battle, hex_battle, by="unit_id")
  
  units_battle_hex <- units_battle_hex %>%
    mutate(hp_c=hp,
           id=as.character(id))
  
  return(units_battle_hex)
  
}