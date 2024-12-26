library(shiny)
library(shinythemes)
library(htmlwidgets)
library(shinyWidgets)
library(DT)
library(dplyr)
library(sodium)

base<- read.csv('datos.csv', encoding = 'UTF-8')

user_base <- 'RISLibrary'
#base<- base %>% select(c(1, 2, 5, 6, 8, 9))

#base <- base %>% mutate(Estado= 'Disponible') %>% 
#  rename(Clase= Materia, Año= A..o, Colección= Colecci..n) %>% 
#  mutate(Presta= 'Name', Comunidad= 'Comunity', ID= 'Number', Tel= 'Number')

#base<- base %>% select()

ui <- fluidPage(
  theme = shinytheme('journal'), 
  setBackgroundColor(
    color = c("#F7FFF7", "#3FAA3F"),
    gradient = "linear",
    direction = "bottom"
  ),
    titlePanel(title = div(tags$img(src = "LogoTARISL.png",height=77,width=125), 
                           'Biblioteca Territorio Ancestral Resguardo Indígena San Lorenzo'), 
               windowTitle= "Territorio Ancestral Resguardo Indígena San Lorenzo"),
    sidebarLayout(
        sidebarPanel(width = 2,
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"),
          tags$style(
            HTML(".nav-tabs > li > a {
                    border: 1px solid #ddd;
                    background-color: #FA9491;
                    color: #0A0A0A;
                    }")),
          tabsetPanel(
            tabPanel(HTML('<strong>Catalogo</strong>'), 
                     dataTableOutput('catalogo')
                     ),
            
            tabPanel(HTML('<strong>Prestamos</strong>'),
                     passwordInput("password", "Password:"),
                     actionButton("go", "Go"),
                     dataTableOutput('prestamo'))
          )
          
        )
    )
)

server <- function(input, output) {
  #base<- reactiveVal(base)
  base1<- base %>% select(1:7)
  
  
  output$catalogo<- renderDataTable(base1, 
                                    filter= 'top',
                                    rownames=FALSE,
                                    options=list(autoWidth = TRUE,
                                                 columnDefs = list(list(width = '125px', 
                                                                        targets = c(1, 3))),
                                                 initComplete = JS(
                                                   "function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': 
                                         '#F2D323', 'color': '#0A0A0A'});",
                                                   "}"
                                                 ), 
                                                 pageLength = 5
                                    ))
  
  BaseAdmin<- reactive({
    if(input$password == user_base){
    req(input$go)
    base
  }})
  
  output$prestamo<- renderDataTable(BaseAdmin(), 
                                    filter= 'top',
                                    rownames=FALSE,
                                    editable= TRUE,
                                    options=list(autoWidth = TRUE,
                                                 columnDefs = list(list(width = '125px', 
                                                                        targets = c(1, 3))),
                                                 initComplete = JS(
                                                   "function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': 
                                         '#F2D323', 'color': '#0A0A0A'});",
                                                   "}"
                                                 ), 
                                                 pageLength = 5
                                    ))
  
  proxy= dataTableProxy('prestamo')
  
  observeEvent(input$prestamo_cell_edit, {
    info = input$prestamo_cell_edit
    str(info)
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    base[i, j] <<- DT::coerceValue(v, base[i, j])
    replaceData(proxy, base, resetPaging = FALSE, rownames = FALSE) 
    write.csv(base, 'datos.csv', row.names = F)
  })
    
  
  
  
  
}

shinyApp(ui = ui, server = server)
