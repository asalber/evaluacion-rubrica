# Author: Alfredo Sánchez Alberca (asalber@ceu.es)

library(shiny)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(knitr)
library(kableExtra)
library(e1071)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
    useShinyjs(), 
    # Application title
    titlePanel("Evaluación por rúbrica"),
    navbarPage(title = NULL,
        # Rubric creation menu
        tabPanel("RÚBRICA",
                 fileInput("items.file",
                           "Elegir un fichero CSV con los ítems de la rúbrica",
                           multiple = F,
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv'),
                           buttonLabel = "Explorar...",
                           width = '400px'),
                 textOutput("text1"),
                 DT::dataTableOutput("itemsTable"),
                 # Download rubric items
                 uiOutput("downloadRubricButton")
        ),
        # Data loading menu
        tabPanel("CARGA DE DATOS", 
                 fileInput("data.file",
                           "Elegir un fichero CSV con las puntuaciones de la rúbrica",
                           multiple = F,
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv'),
                           buttonLabel = "Explorar...",
                           width = '400px'),
                 # Data table
                 dataTableOutput("dataTable")),
            # Grades computation menu
        tabPanel("NOTAS",
                 h2('Notas'),
                 # Grades table
                 dataTableOutput("gradesTable"),
                 # Download grades button
                 downloadButton("downloadGrades", "Descargar notas", class = "btn-primary"),
        ),
        # Statistics
        tabPanel("ESTADÍSTICAS",
                 h2('Estadísticos descriptivos'),
                 fluidRow(
                    # Statistics
                    column(2, tableOutput("stats")),
                    # Histogram
                    column(8, align="center", plotOutput('histogram', width = "800px"), offset = 0)
                 )
        ),
        # Student assessment report
        tabPanel("INFORMES", 
                 # Download reports button
                 downloadButton("downloadReports", "Descargar informes", class = "btn-primary"),
                 flowLayout(
                     # Select input student
                     uiOutput("selectStudent", inline = T),  
                     # Grade
                     htmlOutput("grade", inline = T)),
                 # Evaluación table
                 tableOutput("studentReport"),
                 column(12, align="center", plotOutput('boxplot', width = "800px"), offset = 0),
                 htmlOutput("Comments")
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    data.items <- reactive({
        inFile <- input$items.file
        if (is.null(inFile))
            return(NULL)
        data <- read_csv(inFile$datapath)
        return(data)
    })
    
    observeEvent(input$items.file, {
        output$text1 <- renderText('Selecciona los items que deseas para la rúbrica.')
        output$downloadRubricButton <- renderUI(
            downloadButton("downloadRubric", "Descargar rúbrica", class = "btn-primary")
        )
    })
    
    output$itemsTable <-  DT::renderDataTable(data.items(), 
                                              options = list(
                                                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                  pageLength = 20, 
                                                  autoWidth = FALSE))
    
    
    # Downdoad rubric
    output$downloadRubric <- downloadHandler(
        filename = "rubrica.csv",
        content = function(file) {
            rubric <- as_tibble(data.items()[input$itemsTable_rows_selected, ])
            rubric <- rubric %>%
                pivot_wider(id_cols=c(), names_from = Ítem, values_from = Peso) %>%
                add_column(Apellidos = "PESO", Nombre = "", `Nombre de usuario` = "", Presentado = "") %>%
                relocate(Apellidos, Nombre, `Nombre de usuario`, Presentado)
            write.csv(rubric, file, row.names = F)
        }
    )
    
    dataset <- reactive({
        inFile <- input$data.file
        if (is.null(inFile))
            return(NULL)
        data <- read_csv(inFile$datapath)
        
        # Extract the weights of the questions
        weights <- data %>%
            filter(Apellidos == "PESO") %>%
            select(-c(Nombre, Apellidos, `Nombre de usuario`, Presentado, Comentarios)) %>%
            pivot_longer(cols = everything(), names_to = "Ítem", values_to = "Peso")
        # Pivot data table
        data <- data %>%
            filter(Apellidos != "PESO") %>%
            pivot_longer(cols = -c(Nombre, Apellidos, `Nombre de usuario`, Presentado, Comentarios), names_to = "Ítem", values_to = "Evaluación") %>%
            # Combine with weights
            left_join(weights, by = "Ítem") 
    })
    
    
    # Show loaded data
    output$dataTable <- DT::renderDataTable(dataset(), 
                                        options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                       pageLength = 20,
                                                       autoWidth = FALSE))
    
    # Compute grades
    grades <- reactive({
        req(input$data.file)
        data <- dataset()
        # Compute the grades
        grades <- data %>%
            # Replace NAs by 0
            replace_na(list(Evaluación = 0)) %>%
            # Add a new columns with the Puntos
            mutate(Puntos = if_else(Presentado == "S", Peso * Evaluación, NaN)) %>%
            # Group by student
            group_by(Apellidos, Nombre, `Nombre de usuario`) %>%
            # Compute the grade
            summarise(Nota = round(sum(Puntos) / sum(Peso) * 10, 1))
        return(grades)
    })

    # Show grades
    output$gradesTable <- DT::renderDataTable(grades(), 
                                              options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                             pageLength = 20, 
                                                             autoWidth = FALSE))

    # Downdoad grades
    output$downloadGrades <- downloadHandler(
        filename = "notas.csv",
        content = function(file) {
            grade <- grades() %>% replace_na(list(Nota = ''))
            write.csv(grade, file, row.names = F)
        }
    )
    
    # Show grades histogram
    output$histogram <- renderPlot({
        ggplot(grades(), aes(x=Nota)) + 
            geom_histogram(breaks=seq(0, 10, by = 1), fill=rgb(5, 161, 230, maxColorValue = 255), color='white') +
            labs(title="Distribución de notas") +
            labs(y="Estudiantes") + 
            scale_x_continuous(breaks=seq(0, 10, 1)) +
            theme(title = element_text(size=18),
                axis.title=element_text(size=16),
                axis.text = element_text(size=14))
    })
    
    # Show statistics
    output$stats <- renderTable(
        as_tibble(grades()) %>% 
            filter(!is.na(Nota)) %>% 
            summarise(n = n(), 
                      Min = min(Nota), 
                      Max=max(Nota), 
                      Media = mean(Nota), 
                      Desv.Est.=sd(Nota), 
                      C1 = quantile(Nota, 0.25), 
                      C2 = quantile(Nota, 0.5), 
                      C3 = quantile(Nota, 0.75), 
                      Asimetría = skewness(Nota), 
                      Curtosis = kurtosis(Nota)) %>%
            pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor")
    )
    
    # Show select input for students
    output$selectStudent <- renderUI(
        selectInput("student","Seleccionar estudiante", choices=
                        as.character(unique(unlist(dataset()$Apellidos))))
    )

    # Get selected student data
    data.student <- eventReactive(input$student, {
        data <- dataset()
        data.student <- data %>% filter(Apellidos == input$student) %>%
            # Convert Conseguido to numeric
            mutate(Evaluación = as.numeric(Evaluación)) %>%
            # Replace NAs by 0
            replace_na(list(Evaluación = 0)) %>%
            # Add a new columns with the Puntos
            mutate(Puntos = Peso * Evaluación) %>%
            # Add a new column with Achivement
            mutate(Conseguido = recode(Evaluación, "0" = "No", "0.5" = "Parcialmente", "1.0" = "Si"))
        return(data.student)
    })

    # Show grade
    output$grade <- renderUI({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(Apellidos == input$student) %>% pull(Nota)
        h3(paste("Nota: ", round(grade, 1)))
    })

    # Show student Puntoss
    output$studentReport <- function() {
        req(input$student)
        data.student <- data.student()
        data.student %>%
            mutate(Conseguido = cell_spec(Conseguido, "html", color = if_else(Conseguido == "Si", "green", if_else(Conseguido == "Parcialmente", "orange", "red")))) %>%
            select(Ítem, Peso, Conseguido, Puntos) %>%
            kable(format = "html", escape = F, align = c("l", "c", "c", "c")) %>%
            kable_styling(bootstrap_options = c("striped"), full_width = F)
    }

    # Box plot
    output$boxplot <- renderPlot({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(Apellidos == input$student) %>% pull(Nota)
        boxplot(grades$Nota, horizontal = T, col = rgb(5, 161, 230, maxColorValue = 255), main = "Distribución de notas")
        text(x = grade, y = 1.1, labels = "Tú")
        points(grade, 1, col = "red", pch = 19)
    })
    
    # Show Comments
    output$Comments <- renderUI({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(Apellidos == input$student) %>% pull(Nota)
        data.student <- data.student()
        tagList(h3("Comentarios"), p(if_else(is.na(data.student$Comentarios[1]), '', data.student$Comentarios[1])))
    })
    
    # Generate reports
    output$downloadReports <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "evaluaciones.zip",
        
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tmpdir <- tempdir()
            tempReport <- file.path(tmpdir, "evaluacion.Rmd")
            file.copy("evaluacion.Rmd", tempReport, overwrite = TRUE)
            data <- dataset()
            files <- c()
            setwd(tmpdir)
            withProgress(message = 'Generando informes de evaluación', value = 0, {
                n <- length(unique(data$`Nombre de usuario`))
                for (student in unique(data$`Nombre de usuario`)){
                    data.student <- data %>% 
                        filter(`Nombre de usuario` == student)  %>%
                        # Convert Conseguido to numeric
                        mutate(Evaluación = as.numeric(Evaluación)) %>%
                        # Replace NAs by 0
                        replace_na(list(Evaluación = 0)) %>%
                        # Add a new columns with the Puntos
                        mutate(Puntos = Peso * Evaluación) %>%
                        # Add a new column with Achivement
                        mutate(Conseguido = recode(Evaluación, "0" = "No", "0.5" = "Parcialmente", "1.0" = "Si"))
                    if (data.student$Presentado[1] == "S"){
                        student.name <- paste0(data.student$Apellidos[1], ', ', data.student$Nombre[1])
                        output.file <-  paste0("evaluacion ", student.name, ".html")
                        files <- c(files, paste0(output.file))
                        params <- list(data.student = data.student, grades = grades())
                        rmarkdown::render(tempReport,
                                          output_file = output.file,
                                          output_dir = tmpdir,
                                          params = params, 
                                          envir = new.env(parent = globalenv()))
                    }
                    # send.assessment(i,emails[emails$Name == i,]$`Nombre de usuario`)
                    # Sys.sleep(10)
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = student.name)
                }
            })
            zip(file, files)
        },
        contentType = "application/zip"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
