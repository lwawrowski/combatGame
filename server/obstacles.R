# linear_model ------------------------------------------------------------

linear_model <- function(a,b){
  
  x1 <- a[1]
  y1 <- a[2]
  
  x2 <- b[1]
  y2 <- b[2]
  
  b0 <- (y1*x2-y1*x1-y2*x1+y1*x1)/(x2-x1)
  b1 <- (y2-y1)/(x2-x1)
  
  betas <- round(c(b0, b1),4)
  
  return(betas)
}


# obstacles_update --------------------------------------------------------

obstacles_update <- function(un, obst_game){
  
  obst_player <- un %>%
    mutate(type=ifelse(plnum==-1,4,5)) %>%
    select(type,id)
  
  obst <- rbind(obst_game, obst_player)
  
  return(obst)
  
}


# obstacles_chance --------------------------------------------------------

obstacles_chance <- function(pid, aid, un){
  
  obst_chance_type <- list(chance = 0, type12 = NULL, type45 = NULL)
  
  
  obst_p <- data.frame(order=c(rep(1,5), rep(2,5)), 
                       type=c(rep(1:5,2)), 
                       prob=c(1,0.75,1,1,1,0.5,0.25,0.75,0.75,0.75))
  
  ct <- maps$grid_ct %>%
    mutate_if(is.numeric, round, 4) %>%
    rename(x=lat, y=long)
  
  d <- ct[ct$id %in% c(pid, aid),]
  
  if(nrow(d) == 2){
    
    vline <- FALSE
    if(d$x[1] == d$x[2]){
      d$x[1] <- d$x[1] + 0.0001
      vline <- TRUE
    }
    
    betas <- linear_model(c(d$x[1], d$y[1]),c(d$x[2], d$y[2]))
    
    if(!vline){
      ctm <- ct %>%
        mutate(lwr=betas[2]*x+betas[1]-0.7,
               upr=betas[2]*x+betas[1]+0.7,
               otl=y > lwr & y < upr & x >= d$x[1] & x <= d$x[2]) # on the line
      
    } else {
      ctm <- ct %>%
        mutate(otl=y >= d$y[1] & y <= d$y[2] & x >= d$x[1] - 0.0001 & x <= d$x[2]) # on the line
    }
    
    ids <- ctm$id[ctm$otl==TRUE]
    
    if(pid != ids[1]){
      ids <- rev(ids)
    }
    
    ids_obst <- ids[ids %in% game$map$obst$id]
    ids_obst <- ids_obst[!(ids_obst %in% c(pid, aid))]
    
    if(length(ids_obst) > 0){
      ids_type <- game$map$obst[game$map$obst$id %in% ids_obst,]
      ids_type12 <- as.character(ids_type$id[ids_type$type %in% 1:2])
      ids_type45 <- as.character(ids_type$id[ids_type$type %in% 4:5])
      
      obst_chance_type$type12 <- ids_type12
      obst_chance_type$type45 <- ids_type45
    }
    
    # reduction of chance according to distance
    
    dist <- dist_matrix[pid,aid]
    
    dist_red <- 0
    
    u1 <- un[un$id==pid,]
    
    if(dist %in% u1$range_min:u1$range_max){
      dist_red <- (distance_reduction(u1$range_min, u1$range_max, u1$acc, dist))^(1/3)
    }
    
    pid_chance <- (100 - un$acc_min[un$id==pid])/100 * dist_red
    
    if(length(ids_obst)!=0){
      
      # distance from first obstacles
      
      pid_dist <- dist_matrix[pid, ids_obst[1]]
      aid_dist <- dist_matrix[aid, ids_obst[1]]
      
      if(pid_dist <= aid_dist){ 
        
        obst_game_ids <- game$map$obst %>%
          dplyr::filter(id %in% ids_obst) %>%
          mutate(order=ifelse(id==ids_obst[1],1,2)) %>%
          left_join(., obst_p, by=c("order", "type"))
        
        obst_chance_type$chance <- prod(obst_game_ids$prob, na.rm = T) * pid_chance
        
      } else { 
        
        obst_game_ids <- game$map$obst %>%
          dplyr::filter(id %in% ids_obst) %>%
          mutate(order=2) %>%
          left_join(., obst_p, by=c("order", "type"))
        
        obst_chance_type$chance <- prod(obst_game_ids$prob, na.rm = T) * pid_chance
        
      }
      
    } else { # no obstacles
      
      obst_chance_type$chance <- pid_chance
      
    }
    
  }
  
  # rounded values
  obst_chance_type$chance <- round(obst_chance_type$chance,2)
  
  return(obst_chance_type)
  
}
