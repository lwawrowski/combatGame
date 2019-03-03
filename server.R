shinyServer(function(input, output) {
   
  for (file in list.files("server")) {
    source(file.path("server", file), local = TRUE)
  }
  
})