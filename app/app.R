library(shiny)
library(rio)
library(tidyverse)
library(glue)


comunidades = import("comunidades_lkp.rds")
provincias = import("provincias_lkp.rds")

provincias_check = import("resultado_formato.rds")
data_import = import("bienes_iglesia.rds") %>% tibble() %>%
  select(-table, - localidad)

count_top = function(df, var, n=5){
  
  df %>%
    mutate({{var}} := str_trim({{var}})) %>%
    group_by({{var}}) %>%
    summarise(Notas = n(), .groups = 'drop') %>%
    arrange(desc(Notas)) %>%
    filter(row_number()<=5)
  
    # mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n, other_level = "Otros")) %>%
    # group_by({{var}}) %>%
    # summarise(Notas = n(), .groups = 'drop') 
  
  
  
}



ui <- fluidPage(
  title = "Bienes de la Iglesia: Buscador",

#estilo --------------------------------------------------------------------
  tags$head(
    # Note the wrapping of the string in HTML()
    #font-family: 'Yusei Magic', sans-serif;
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Noto+Sans+JP&display=swap');
      
      body {

        width: 100%;
        padding:10px 120px;
        font-family: 'Noto Sans JP', sans-serif;
        width: inherit;
      }
     
      .shiny-input-container {
        color: #474747;
      }
      
      .aviso{ 
      color: #AB0613;
      font-weight: 100;
      font-style: italic;
      margin-top: -20px;
      
      }
      
      
      .nota{
      font-size: 80%;
      color: #9999A6;
      }
      
      .azul_guardian{
      color : #052962;
      
      } 
      
      label{
      
      color: #052962
      }
      
      tr.dtrg-group.dtrg-start.dtrg-level-0 > td {
      color: white;
      background-color: #052962;
      
      } 
      
      th{
  
  color: #052962;  
  }
      
      @media only screen and (max-width: 750px) {
  h3 {
    font-size: 18px;
  }
  
  body{
  padding:15px;
  
  }
  
  #table_bienes{
  
  font-size:10px !important;
  }
  
  
  
  
  
      }
  
  
/*
@media (min-width: 768px){}
.col-sm-3 {
    width: 33%;
}
*/



                    
                    
                    "
                    
                    
    ))
  ),

#Introduccion -----------------------------------------------------------------
  h1('Buscador de los bienes inmatriculados por la Iglesia'),
  HTML('<a href ="http://www.andresarau.com" target="_blank"> ANDRÉS ARAU PONTONES</a> - 28 de febrero del 2021'),
  br(),br(),
  HTML('El <a href ="https://www.lamoncloa.gob.es/consejodeministros/resumenes/Paginas/2021/160221-cministros.aspx"
  target="_blank">
  Gobierno de España</a> publicó un listado de 34,961 
       propiedades que la Iglesia católoca registró a su nombre entre 
       2004 y 2015. El listado completo fue publicado
       en dos documentos de más de 3,000 páginas y en formato ".pdf".
       
       Utilizando técnicas de <i>Data Scrapping</i> que utilizo
       en mi trabajo habitual, me dí a la tarea de convertir estos datos
       a un formato más accesible para su análisis.
       
       El repositario con el código utilizado para transformar
       los datos está disponible en <a href="https://github.com/araupontones/bienes_ig"
       target="_blank">GitHub</a>. Si tienes algún
       comentario puedes contactarme via <a href="https://twitter.com/AndresArau"
       target="_blank">twitter</a>.
         
         
         <br><br>
         <p>Los documentos oficiales en su formato original pueden
      están disponibles <a href="http://ep00.epimg.net/descargables/2021/02/16/81f680e3671ec19edb395114f640972c.pdf"
      target="_blank">aquí</a> y 
       <a href="http://ep00.epimg.net/descargables/2021/02/16/070845446d6851c3122a7a09570e31b5.pdf"
       target="_blank">aquí</a>.</p>
       
       <br>
       <p><i class = "nota">*El formato de los documentos originales es muy inconsistente entre sus diferentes páginas. Aunque el
       código utilizado para transformar los datos fue efectivo, existen algunos detalles aún por mejorar. Por ejemplo,
       algunos títulos de los inmuebles estan cortados o incompletos, esto se debe a que el código utilizado
      para transformar estos datos de formato .pdf a un formato más amigable no logró capturar el nombre completo de
       algunas observaciones. Los nombres incluidos en las tablas de resumen no fueron alterados
       para mantener la consistencia con el documento original.</i></p>
       
       
       '
       
         
        
  ),
  


  hr(),

  fluidRow(
    column(4,selectInput("i_comunidad", "Comunidad Autónoma", comunidades)),
    column(4,selectInput("i_provincia", "Provincia", choices = NULL))
  ),
  
  fluidRow(htmlOutput('text_resumen')),
  
  fluidRow(
    
    column(3,tableOutput("municipios")),
    column(3,tableOutput("registro")),
    column(3,tableOutput("titulares")),
    column(3,tableOutput("tipo"))
    
    
  ), 
  
  hr(),
  
  fluidRow(
    
    selectInput("i_certificacion", "Tipo de certificación", choices = c("Eclesiástica", "Otra")),
    
    column(12, DT::DTOutput("table_bienes"), class = "movie-list")
  )
  
)

server <- function(input, output, session) {
  
  #original data
  bienes <- reactive(
    
    data_import
  )
  
  data_check <- reactive(
    
    provincias_check %>%
      filter(provincia == input$i_provincia)
  )
  
  #data comunidades
  data_comunidades <- reactive(
    
    data_import %>% filter(comunidad == input$i_comunidad)
    
  )
  
  #update lista de provincias
  observeEvent(data_comunidades(),{
    choices <- unique(data_comunidades()$provincia)
    updateSelectInput(session, "i_provincia", choices = choices)
    
  })
  
  #data de provincia (la que va en la tabla)
  data_provincia <- reactive({
    
    req(input$i_provincia)
    
    data_comunidades() %>% filter(provincia == input$i_provincia)
    
  })
  
  #texto resumen
  
  
  
  output$text_resumen <- renderUI({
    
   
    #numero de notas simples
    notas = data_provincia() %>%
      summarise(n = n()) %>%
      pull(n)
    
    #con titulo distinto
    titulo_distinto = data_check()[1,8]
    distinto_perc = paste0("(",round(titulo_distinto/notas *100,1),"%)")
    
    
    #diferencia por transformacion
    diferencia = data_check()[1,1]
    
    if(diferencia >0 | diferencia < 0){
      
      aviso= glue({'*En el caso de {input$i_provincia}, 
        existe una diferencia de {diferencia} observaciones 
        entre los datos reportados en el documento oficial 
        y los datos transformados en estas tablas. Estoy trabajando en identificar el origen de esta diferencia.'})
    } else{
      
      aviso = ""
    }
    
    message("The value of input$count is ", input$i_provincia)
    HTML(glue('<h2 class = "azul_guardian">{input$i_provincia}</h2>
              <h3>{notas} notas simples de las cuales <b>{titulo_distinto} {distinto_perc} </b>
              tienen título distinto de certificación eclesiástica.</h3>
              <p>A continuación se muestran los <b>5 municipios, registros, titulares y tipos de 
              inmuebles</b> con más notas simples reportadas dentro de esta provinica. La totalidad de
              las notas simples de {input$i_provincia} es presentada debajo de estas tablas.
              </p><br>
              <p class = "aviso">{aviso}</p>'))
    
    
  })
  
  
  #tables resumen
  #changes when selected changes
  
  
  
  
  output$registro <- renderTable({


    count_top(data_provincia(), registro) %>% 
      rename(Registro = registro)

  }, width = "100%")
  
  output$municipios <- renderTable({
    
    
    count_top(data_provincia(), Municipio)
    
  }, width = "100%")
  
  
  output$tipo <- renderTable({
    
    
    count_top(data_provincia(), Tipo)
    
  }, width = "100%")
  
  
  output$titulares <- renderTable({
    
    
    count_top(data_provincia(), Titular)
    
  }, width = "100%")
  
  
  #table
  output$table_bienes <- DT::renderDataTable({
    
    #c("ECLESIÁSTICA", "DISTITNA", "OTRA")
    
    
    
    
    if(input$i_certificacion == "Eclesiástica") {

      datos_table = data_provincia() %>%
        filter(distinto_de_cetificacion == 'NO')%>% 
        rename(`Título distinto` =  8)


    } else if(input$i_certificacion == "Otra") {

      datos_table = data_provincia() %>%
        filter(distinto_de_cetificacion != 'NO')
    }
      
    
    datos_table = datos_table %>%
      rename(Orden = orden,
             `Título` = Titulo)
      
    names(datos_table)[8] <-"Templo y dependencias complementarias"
    names(datos_table)[10] <-"Título distinto de certificación eclesiástica"
    
    print(names(datos_table))
    
    DT::datatable(
      datos_table,
      extensions = c('RowGroup'),
      options = list(
        rowGroup = list(dataSrc = 2),
        language = list(
          info = "Mostrando _START_ a _END_ de _TOTAL_ entries",
          search = "Buscar",
          infoEmpty = "No existen observaciones en esta categoría",
          lengthMenu =  "Mostrar _MENU_ notas simples",
          paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
        
        columnDefs = list(list(visible = F, targets = c(0,1,2,3,7,9,10))),
        pageLength = 25,
        scrollX = T
        
      ),
      
      rownames= FALSE
      
      
    )
    
    
    
  })
  
}

shinyApp(ui, server)



