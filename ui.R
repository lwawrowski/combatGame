dashboardPage(
  dashboardHeader(title = "COMBAT"),
  dashboardSidebar(disable = T,
                   sidebarMenu(
                     menuItem("Game", tabName = "game", icon = icon("th"))
                   )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "game",
        fluidRow(
          box(
            id = "box_map",
            width = 10,
            plotOutput(
              "map",
              width = "1000px",
              height = "600px",
              click = "map_click",
              dblclick = "map_dblclick",
              hover = "map_hover"
            )
          ),
          infoBox(title = actionLink("newgame", "NEW GAME"), subtitle = uiOutput("end_turn"), 
                  icon = icon("cog"), color = "blue", width = 2),
          infoBoxOutput("current_move", width = 2),
          infoBoxOutput("current_turn", width = 2),
          infoBoxOutput("current_hp", width = 2),
          infoBoxOutput("potential_hp", width = 2),
          infoBoxOutput("chance", width = 2)
        ),
        
        # other outputs -----------------------------------------------------------
        
        fluidRow(
          box(width = 10, tableOutput("info")))
      ))
  )
)