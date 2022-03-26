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
                 fluidRow(
                    # Exam name
                    column(6, textInput('examName', 'Nombre del examen', value = paste0("Examen-", Sys.Date()))),
                    # Load items
                    column(6, fileInput("items.file",
                               "Elegir un fichero CSV con los Items de la rúbrica",
                               multiple = F,
                               accept=c('text/csv', 
                                        'text/comma-separated-values,text/plain', 
                                        '.csv'),
                               buttonLabel = "Explorar...",
                               width = '400px'))
                    ),
                 textOutput("text1"),
                 # Items table
                 DT::dataTableOutput("itemsTable"),
                 # Rubric summary table
                 tableOutput("itemsSummary"),
                 # Download rubric items
                 uiOutput("downloadRubricButton")
        ),
        tabPanel("ALUMNOS",
                 # Load students
                 fileInput("students.file",
                           "Elegir un fichero CSV con los alumnos del grupo (formato Blackboard).",
                           multiple = F,
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv'),
                           buttonLabel = "Explorar...",
                           width = '500px'),
   
                 # Students table
                 DT::dataTableOutput("studentsTable"),
                 # Download rubric items
                 uiOutput("downloadRubricTemplateButton")
        ),
        
        # Data loading menu
        tabPanel("CORRECCIÓN", 
                 fileInput("assessment.file",
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
                 # Assessment table
                 tableOutput("studentReport"),
                 column(12, align="center", plotOutput('boxplot', width = "800px"), offset = 0),
                 htmlOutput("comments")
        )
    )
)

# Define server logic required to draw a histogram
library(logging)
basicConfig()
options(shiny.error = function() { 
  logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", ")) })


server <- function(input, output) {
  # Log via logging
  printLogJs <- function(x, ...) {
    logjs(x)
    T
  }

  addHandler(printLogJs)
  
    # Encoding items
    encoding.items <- reactive({
        encoding <- unlist(guess_encoding(input$items.file$datapath))[1]
        return(encoding)
    })
    
    # Load items data set
    data.items <- reactive({
        inFile <- input$items.file
        if (is.null(inFile))
            return(NULL)
        # Get the encoding of the file (specially for Windows systems)
        encoding <- encoding.items()
        if (encoding == "ISO-8859-1")
            data <- read_csv2(inFile$datapath, locale = locale(encoding = encoding))
        else 
            data <- read_csv(inFile$datapath, locale = locale(encoding = encoding))
        return(data)
    })
    
    # Show text and download button
    observeEvent(input$items.file, {
        output$text1 <- renderText('Selecciona los Items que deseas para la rúbrica.')
        output$downloadRubricButton <- renderUI(
            downloadButton("downloadRubric", "Descargar rúbrica", class = "btn-primary")
        )
    })
    
    # Show items table
    output$itemsTable <- renderDataTable(data.items(), 
                                         options = list(
                                                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                  pageLength = 10, 
                                                  autoWidth = FALSE))
    
    selected.items <- eventReactive(input$itemsTable_rows_selected, {
        data.items()[input$itemsTable_rows_selected, ]
    })
    
    # Items summary
    output$itemsSummary <- renderTable({
        if (length(input$itemsTable_rows_selected)){
            selected.items() %>% 
                group_by(Tema) %>%
                summarise(`Peso Acumulado`= sum(Peso))
        }
    })
    
    # Download rubric
    output$downloadRubric <- downloadHandler(
        filename = paste0(input$examName, "-rubrica.csv"),
        content = function(file) {
            rubric <- selected.items() %>%
                pivot_wider(id_cols=c(), names_from = Item, values_from = Peso) %>%
                add_column(Apellidos = "PESO", Nombre = "", `Nombre de usuario` = "", Presentado = "") %>%
                relocate(Apellidos, Nombre, `Nombre de usuario`, Presentado)
            encoding <- encoding.items()
            if (encoding == "ISO-8859-1")
                write_csv2(rubric, file)
            else
                write_csv(rubric, file)
        }
    )
    
    # Load items data set
    data.students <- reactive({
        inFile <- input$students.file
        if (is.null(inFile))
            return(NULL)
        data <- read_csv(inFile$datapath)
        return(data %>% select(c(Apellidos, Nombre, `Nombre de usuario`)))
    })
    
    # Show students table
    output$studentsTable <-  DT::renderDataTable(data.students(),
                                              options = list(
                                                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                  pageLength = 15, 
                                                  autoWidth = FALSE))
    
    # Show download button
    observeEvent(input$items.file, {
        output$downloadRubricTemplateButton <- renderUI(
            downloadButton("downloadRubricTemplate", "Descargar plantilla de rúbrica", class = "btn-primary")
        )
    })
    
    # Download rubric
    output$downloadRubricTemplate <- downloadHandler(
        filename = paste0(input$examName, "-plantilla.csv"),
        content = function(file) {
          rubric <- selected.items() %>%
                pivot_wider(id_cols=c(), names_from = Item, values_from = Peso) %>%
                add_column(Comentarios = "", Apellidos = "PESO", Nombre = "", `Nombre de usuario` = "", Presentado = "") %>%
                relocate(Apellidos, Nombre, `Nombre de usuario`, Presentado) %>%
                # Add students
                add_row( data.students()) %>%
                mutate_all(~replace_na(., ""))
            encoding <- encoding.items()
            if (encoding == "ISO-8859-1")
                write_csv2(rubric, file)
            else
                write_csv(rubric, file)
        }
    )
    
    # Encoding correction
    encoding.assessment <- reactive({
        encoding <- unlist(guess_encoding(input$assessment.file$datapath))[1]
        return(encoding)
    })
    
    # Load correction data set
    data.assessment <- reactive({
        inFile <- input$assessment.file
        if (is.null(inFile))
            return(NULL)
        encoding <- encoding.assessment()
        if (encoding == "ISO-8859-1")
            data <- read_csv2(inFile$datapath, locale = locale(encoding = encoding))
        else 
            data <- read_csv(inFile$datapath, locale = locale(encoding = encoding))
        # Convert the comments column to character (if not when there are no comments the column is loaded as logical)
        data <- data %>% mutate(Comentarios = as.character(Comentarios))
        # Extract the weights of the questions
        weights <- data %>%
            filter(Apellidos == "PESO") %>%
            select(-c(Nombre, Apellidos, `Nombre de usuario`, Presentado, Comentarios)) %>%
            pivot_longer(cols = everything(), names_to = "Item", values_to = "Peso")
        # Pivot data table
        data <- data %>%
            filter(Apellidos != "PESO") %>%
            pivot_longer(cols = -c(Nombre, Apellidos, `Nombre de usuario`, Presentado, Comentarios), names_to = "Item", values_to = "Evaluación") %>%
            # Combine with weights
            left_join(weights, by = "Item")
        return(data)
    })
    
    
    # Show loaded data
    output$dataTable <- DT::renderDataTable(data.assessment(), 
                                        options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                       pageLength = 15,
                                                       autoWidth = FALSE))
    
    # Compute grades
    grades <- reactive({
        req(input$assessment.file)
        data <- data.assessment()
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
                                                             pageLength = 15, 
                                                             autoWidth = FALSE))

    # Downdoad grades
    output$downloadGrades <- downloadHandler(
        filename = paste0(input$examName, "-notas.csv"),
        content = function(file) {
            grade <- grades() %>% replace_na(list(Nota = ''))
            write_csv(grade, file)
        }
    )
    
    # Show grades histogram
    output$histogram <- renderPlot({
        ggplot(grades(), aes(x=Nota)) + 
            geom_histogram(breaks=seq(0, 10, by = 1), fill=rgb(5, 161, 230, maxColorValue = 255), color='white') +
            labs(title=paste("Distribución de notas", input$examName)) +
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
                        as.character(unique(unlist(data.assessment() %>% filter(Presentado == "S") %>% pull(Apellidos)))))
    )

    # Get selected student data
    data.student <- eventReactive(input$student, {
        data <- data.assessment()
        data.student <- data %>% filter(Apellidos == input$student) %>%
            # Convert Conseguido to numeric
            mutate(Evaluación = as.numeric(Evaluación)) %>%
            # Replace NAs by 0
            replace_na(list(Evaluación = 0)) %>%
            # Add a new columns with the Puntos
            mutate(Puntos = Peso * Evaluación) %>%
            # Add a new column with Achivement
            mutate(Conseguido = recode(Evaluación, "0" = "No", "0.25" = "Parcialmente", "0.5" = "Parcialmente", "0.75" = "Parcialmente", "1.0" = "Si"))
        return(data.student)
    })

    # Show grade
    output$grade <- renderUI({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(Apellidos == input$student) %>% pull(Nota)
        h3(paste("Nota: ", round(grade, 1)))
    })

    # Show student Score
    output$studentReport <- function() {
        req(input$student)
        data.student <- data.student()
        data.student %>%
            mutate(Conseguido = cell_spec(Conseguido, "html", color = if_else(Conseguido == "Si", "green", if_else(Conseguido == "Parcialmente", "orange", "red")))) %>%
            select(Item, Peso, Conseguido, Puntos) %>%
            kable(format = "html", escape = F, align = c("l", "c", "c", "c")) %>%
            kable_styling(bootstrap_options = c("striped"), full_width = F)
    }

    # Box plot
    output$boxplot <- renderPlot({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(Apellidos == input$student) %>% pull(Nota)
        boxplot(grades$Nota, horizontal = T, col = rgb(5, 161, 230, maxColorValue = 255), main = "Distribución de notas", yaxt="n", ylim = c(0,10))
        axis(1, at = 0:10)
        text(x = grade, y = 1.1, labels = "Tú")
        points(grade, 1, col = "red", pch = 19)
    })
    
    # Show comments
    output$comments <- renderUI({
        req(input$student)
        data.student <- data.student()
        tagList(h3("Comentarios"), p(ifelse(is.na(data.student$Comentarios[1]), '', data.student$Comentarios[1])))
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
            data <- data.assessment()
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
                        mutate(Conseguido = recode(Evaluación, "0" = "No", "0.25" = "Parcialmente", "0.5" = "Parcialmente", "0.75" = "Parcialmente", "1.0" = "Si"))
                    if (data.student$Presentado[1] == "S"){
                        student.name <- paste0(data.student$Apellidos[1], ', ', data.student$Nombre[1])
                        output.file <-  paste0("evaluacion ", student.name, ".html")
                        files <- c(files, paste0(output.file))
                        params <- list(title = input$examName, data.student = data.student, grades = grades())
                        rmarkdown::render(tempReport,
                                          output_file = output.file,
                                          output_dir = tmpdir,
                                          params = params, 
                                          envir = new.env(parent = globalenv()))
                    }
                    # send.assessment(i,emails[emails$Name == i,]$`Nombre de usuario`)
                    # Sys.sleep(10)
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = student)
                }
            })
            zip(file, files)
        },
        contentType = "application/zip"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)



