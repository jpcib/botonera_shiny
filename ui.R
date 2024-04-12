Sys.setlocale("LC_CTYPE", "C.UTF-8")

ui <- fluidPage(  
  navbarPage("Botonera", theme = shinytheme("lumen"),
             tabPanel("Tablero", fluid = TRUE,
                      div(class = "pull-right", shinyauthr::logoutUI(id = "logout",label = "Cerrar sesión")),
                      shinyauthr::loginUI(id = "login",
                                          title = "Inicio de sesión",
                                          user_title = "Nombre de usuario",
                                          pass_title = "Contraseña",
                                          login_title = "Iniciar sesión",
                                          error_message = "Usuario o contraseña invalido."
                      ),
                      shinyjs::useShinyjs(),
                      uiOutput("ui")
             )
  )
)