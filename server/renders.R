# wyświetlanie elementów

# show map ----------------------------------------------------------------

show_map <- reactive({
  
  df <- game$map$map_df %>%
    mutate(type=factor(type, 
                       labels = c("1", "2", "4", "5", "6", "7", ""),
                       levels = c("1", "2", "4", "5", "6", "7", ""), 
                       ordered = T))
  
  colors <- c("#636363", "#bdbdbd", "#fb8072", "#ffffb3", "#fdb462",  "#8dd3c7", "#e5f5e0")
  names(colors) <- levels(df$type)
  
  g <- ggplot(df, aes(x=lat, y=long, group = id)) + 
    geom_polygon(aes(fill=type)) +
    geom_text(data = game$map$map_ct, aes(x=lat, y=long, label=name)) +
    scale_fill_manual(values = colors, name = "", 
                      labels = c("High obstacles", "Low obstacles", "Player 1", "Player 2", "Selected unit", "Mobility", ""), # opisy poziomów 
                      drop = FALSE) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom") + 
    guides(fill = guide_legend(nrow = 1))
  
  g
})


# renderUI ----------------------------------------------------------------

output$end_turn <- renderUI({
  
  if(!is.null(game$map)){
    
    actionLink("end_turn",
                 "END TURN")
    
  }
})

# render map --------------------------------------------------------------

output$map <- renderPlot({
  if(!is.null(game$map)){
    show_map() 
  }
  
})

# render info -------------------------------------------------------------

output$current_move <- renderInfoBox({
  
  if(!is.null(game$map)){
    player <- game$map$player
    current_player <- ifelse(player==-1, "Player 1", "Player 2")
  } else {
    current_player <- ""
  }
  
  infoBox("Player", current_player, icon = icon("gamepad"),color = "light-blue")
}) 

output$current_turn <- renderInfoBox({
  infoBox("Turn", game$map$turn_log[1], icon = icon("list-ol"),color = "olive")
})  

output$current_hp <- renderInfoBox({
  
  hp_info <- ""
  
  if(!is.null(game$map)){
    if(is_unit_hovered()){
      hp_info <- game$map$units$hp_c[game$map$units$id==check_hovered_id()]
    } 
  }
  
  infoBox("Unit HP", hp_info, icon = icon("heart"),color = "purple")
})

output$potential_hp <- renderInfoBox({
  
  p_hp <- ""
  
  if(!is.null(game$map)){
    if(is_unit_hovered() & game$map$unit_clicked != ""){
      hp_info <- check_hp(game$map$unit_clicked, check_hovered_id(), game$map$units)
      
      if(hp_info==0){
        p_hp <- "Out of range"
      } else {
        p_hp <- paste0("[", round(0.9*hp_info),";",round(1.1*hp_info),"]")
      }
    }
  }
  
  infoBox("Possible hit", p_hp, icon = icon("thumbs-down"), color = "orange")
})

output$chance <- renderInfoBox({
  
  chance_info_text <- ""
  
  if(!is.null(game$map)){
    if(is_unit_hovered() & game$map$unit_clicked != ""){
      chance_info <- obstacles_chance(game$map$unit_clicked, check_hovered_id(), game$map$units)
      chance_info_text <- paste0(round(chance_info$chance*100),"%")
    }
  }
  
  infoBox("Chance", chance_info_text, icon = icon("bullseye"), color = "blue")
})


output$info <- renderTable({
  
  if(!is.null(game$map)){
    
    log <- bind_rows(log$data)
    log
    
  }
  
})