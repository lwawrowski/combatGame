# check_clicked_id --------------------------------------------------------

check_clicked_id <- reactive({
  
  x <- input$map_click$x
  y <- input$map_click$y
  
  if(is.numeric(x) | is.numeric(y)){
    
    clicked_point <- data.frame(lon=as.numeric(y), lat=as.numeric(x))
    point_to_sp   <- SpatialPoints(clicked_point, proj4string=CRS(proj4string(game$map$map_sp)))
    
    hex_id <- gContains(game$map$map_sp, point_to_sp, byid = T) %>%
      as.data.frame %>%
      gather(id, contains)
    
    if(sum(hex_id$contains)!=0){
      id <- hex_id$id[hex_id$contains==T]
    } else {
      id <- FALSE
    }
    
    return(id)
  }
  
})


# check_hovered_id --------------------------------------------------------

check_hovered_id <- reactive({
  
  x <- input$map_hover$x
  y <- input$map_hover$y
  
  if(is.numeric(x) | is.numeric(y)){
    
    hovered_point <- data.frame(lon=as.numeric(y), lat=as.numeric(x))
    point_to_sp   <- SpatialPoints(hovered_point, proj4string=CRS(proj4string(game$map$map_sp)))
    
    hex_id <- gContains(game$map$map_sp, point_to_sp, byid = T) %>%
      as.data.frame %>%
      gather(id, contains)
    
    if(sum(hex_id$contains)!=0){
      id <- hex_id$id[hex_id$contains==T]
    } else {
      id <- FALSE
    }
    
    return(id)
    
  }
  
  
})


# check_dblclicked_id -----------------------------------------------------

check_dblclicked_id <- reactive({
  
  x <- input$map_dblclick$x
  y <- input$map_dblclick$y
  
  if(is.numeric(x) | is.numeric(y)){
    
    clicked_point <- data.frame(lon=as.numeric(y), lat=as.numeric(x))
    point_to_sp   <- SpatialPoints(clicked_point, proj4string=CRS(proj4string(game$map$map_sp)))
    
    hex_id <- gContains(game$map$map_sp, point_to_sp, byid = T) %>%
      as.data.frame %>%
      gather(id, contains)
    
    if(sum(hex_id$contains)!=0){
      id <- hex_id$id[hex_id$contains==T]
    } else {
      id <- FALSE
    }
    
    return(id)
    
  }
  
})

# is_unit_clicked ---------------------------------------------------------

is_unit_clicked <- reactive({
  
  result <- FALSE
  
  if(!is.null(game$map)){
    
    id <- check_clicked_id()
    
    if(is.character(id)){
      
      result <- id %in% game$map$units$id
      
    }
  
  }
  
  return(result)
  
})


# is_unit_hovered ---------------------------------------------------------

is_unit_hovered <- reactive({
  
  result <- FALSE
  
  if(!is.null(game$map)){
    
    id <- check_hovered_id()
    
    if(is.character(id)){
      
      result <- id %in% game$map$units$id
      
    }
    
  }
  
  return(result)
  
})


# is_unit_dblclicked ------------------------------------------------------

is_unit_dblclicked <- reactive({
  
  result <- FALSE
  
  if(!is.null(game$map)){
    
    id <- check_dblclicked_id()
    
    if(is.character(id)){
      
      result <- id %in% game$map$units$id
      
    }
    
  }
  
  return(result)
  
})


# is_unit_current_player --------------------------------------------------

is_unit_current_player <- reactive({
  
  result <- FALSE
  
  if(!is.null(game$map)){
    
    id <- check_clicked_id()
    
    if(is.character(id)){
      
      player <- game$map$units$plnum[game$map$units$id==id]
      
      result <- player == game$map$player
      
    }
    
  }
  
  return(result)
  
})