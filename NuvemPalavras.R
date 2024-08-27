library(shiny)
library(shinythemes)
library(shinymeta)
library(wordcloud2)
library(tm)
library(colourpicker)
library(tidyverse)
library(readr)
library(htmlwidgets)


codigo_fonte <- "NuvemPalavras.R"
how_use <- ("Como_Usar.html")

ui <- fluidPage(theme = shinytheme("sandstone"),
  
  # Define título da janela
  titlePanel(
    windowTitle = "Nuvem de Palavras",
    title = "Nuvem de Palavras"
  ),
  
  # adicionar icones linkedin e github
  div(
    style = "display: flex; justify-content: space-between;",
    h4("Autor: Geovane Fernandes da Silva"),
    div(
      tags$a(href = "https://www.linkedin.com/in/geovanefernandes/", target = "_blank",
             tags$img(src = "https://cdn-icons-png.flaticon.com/512/174/174857.png", height = "20px", style = "margin-right: 10px;")
      ),
      tags$a(href = "https://github.com/GeovaneFS", target = "_blank",
             tags$img(src = "https://cdn-icons-png.flaticon.com/512/25/25231.png", height = "20px")
      )
    )
  ),
  
  tabsetPanel(
    
    # Aba para criação do wordcloud
    tabPanel(
      title = ("Nuvem de Palavras"),
      sidebarLayout(
        sidebarPanel(width = 3,
          
          # Selecionar o idioma
          selectInput(
            inputId = "idioma",
            label = "Selecione o idioma da sua nuvem",
            choices = c("Português" = "pt", "English" = "en", "Español" = "es"),
            selected = "pt"
          ),
          hr(),
          
          # Selecionar a fonte das palavras
          radioButtons(
            inputId = "tipo_texto",
            label = "Selecione a fonte das suas palavras",
            choices = c("Vou digitar meu texto" = "proprio",
                        "Carregar um arquivo" = "arquivo")
          ),
          hr(),
          
          # Digitar o texto
          conditionalPanel(
            condition = "input.tipo_texto == 'proprio'",
            textAreaInput(
              inputId = "texto",
              label = "Digite seu texto aqui",
              value = "",
              rows = 7,
            )
          ),
          
          # Carregar arquivo
          conditionalPanel(
            condition = "input.tipo_texto == 'arquivo'",
            fileInput(
              inputId = "arquivo",
              label = "Carregar arquivo de texto (.txt ou .csv)",
              accept = c(".txt", ".csv")
            )
          ),
          hr(),
          
          # Remover palavras específicas
          checkboxInput(
            inputId = "remover_especificas",
            label = "Remover palavras específicas?",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.remover_especificas == 1",
            textAreaInput(
              inputId = "palavras_especificas",
              label = "Digite as palavras que deseja remover (uma por linha)",
              rows = 5
            )
          ),
          
          hr(),
          
         # Quantidade de palavras na nuvem
          sliderInput(
            inputId = "quantidade_palavras",
            label = "Quantidade de palavras na nuvem",
            min = 5,
            max = 200,
            value = 50,
            step = 1
          ),
          hr(),
          
          # Fonte
          selectInput(
            inputId = "fonte",
            label = "Mudar Fonte",
            choices = c("Arial", "Times New Roman", "Courier New", 
                        "Verdana", "Georgia", "Comic Sans MS",  
                        "Lucida Console", "Trebuchet MS", 
                        "Arial Black", "MS Sans Serif"),
            selected = "Arial"
          ),
          hr(),
         
         # paleta de cores da palavras
         selectInput(
           inputId = "paleta_cores",
           label = "Escolha a paleta de cores",
           choices = c("Cores Claras" = "random-light", 
                       "Cores Escuras" = "random-dark"),
           selected = "random-dark"
         ),
         hr(),
         
         # formato da nuvem
         selectInput(
           inputId = "formato_nuvem",
           label = "Formato da Nuvem",
           choices = c("Predefinido" = "default",
                       "Estrela" = "star",
                       "Pentagono" = "pentagon",
                       "Triangulo" = "triangle"
                       ),
           selected = "default"
         ),
         hr(),
         
          # Cor do fundo
          colourInput(
            inputId = "cor_fundo",
            label = "Cor do fundo",
            value = "white"
          ),
         hr(),
          
        ),
        
        mainPanel(
          fluidRow(
            column(10,
              div(
                style = "height: 100vh; display: flex; flex-direction: column;",
                wordcloud2Output("wordcloud", height = "100%", width = "100%")
              )
            ),
            column(2,
              div(
                style = "align-self: flex-start; margin: 10px;",
                downloadButton("downloadPlot", "", icon = icon("download"))
              )
            )
          )
        )
      )
    ),
    
    # Aba com tabela de frequência das palavras
    tabPanel(
      title = "Tabela de Frequência",
      h3("Tabela de Frequência da Nuvem de Palavras"),
      br(),
      mainPanel(
        column(10,tableOutput("tabela_frequencia")),
      )
    ),
    
    #Aba com o código fonte do aplicativo
    tabPanel(
      title = "Código Fonte",
      h3("Código Fonte do Aplicativo"),
      br(),
      tags$pre(
        verbatimTextOutput("codigo_fonte")
          )
        ),
    
    #Aba como usar o aplicativo
    tabPanel(
      title = "Como Usar",
      inputId = "como_usar",
      h3("Como Usar o Aplicativo"),
      br(),
      includeHTML(how_use)
      
    ),
    
    # Aba sobre o aplicativo
    tabPanel(
      title = "Sobre",
      h3("Sobre o aplicativo"),
      br(),
      p("Este aplicativo foi desenvolvido na linguagem R como parte da disciplina GES-109 
      (Organização e Apresentação de Dados) do curso de Estatística (Ênfase em Ciência de Dados e Big Data)
      da Universidade Federal de Lavras,
      tendo como intuito facilitar a criação de nuvens de palavras."),
      p("Sendo este para livre uso e reprodução, tanto para comunidade acadêmica
        quanto externa."),
      br(),
      p("Desenvolvido por Geovane Fernandes da Silva.")
    )
  )
)


server <- function(input, output) {
  
  # checa e define a fonte de dados
  
  fonte_dados <- reactive({
    if (input$tipo_texto == "proprio") {
      texto <- input$texto
    } else if (input$tipo_texto == "arquivo") {
      texto <- input_arquivo()
    
    }
    return (texto)
  })
  
  # checagem se arquivo esta vazio
  
  input_arquivo <- reactive({
    if (is.null(input$arquivo)){
      return ("")
    }
    
    ext <- tools::file_ext(input$arquivo$name)
    
    # checa a extensão do arquivo
    
    if (ext == ".csv"){
      text <- read_csv(input$arquivo$datapath, encoding = "UTF-8")
      text <- paste(text[[1]], collapse = " ")
    } else {
      text <- readLines(input$arquivo$datapath)
      text <- paste(text, collapse = " ")
    }
    return(text)
    
  })
  
  criar_nuvem <- metaReactive({
    texto <- fonte_dados()
    num_palavras <- input$quantidade_palavras
    background <- input$cor_fundo
    fonte <- input$fonte
    formato <- input$formato_nuvem
    paleta <- input$paleta_cores
    
    if (is.character(texto)) {
      palavras <- Corpus(VectorSource(texto))
      palavras <- tm_map(palavras, stripWhitespace)
      palavras <- tm_map(palavras, content_transformer(tolower))
      palavras <- tm_map(palavras, removePunctuation)
      palavras <- tm_map(palavras, removeNumbers)
      palavras <- tm_map(palavras, removeWords, stopwords(input$idioma))
      
      if (input$remover_especificas) {
        palavras_remover <- unlist(strsplit(input$palavras_especificas, "\n"))
        palavras <- tm_map(palavras, removeWords, palavras_remover)
      }
      
      dados <- TermDocumentMatrix(palavras)
      matriz <- as.matrix(dados)
      texto <- sort(rowSums(matriz), decreasing = TRUE)
      texto <- data.frame(word = names(texto), freq = as.numeric(texto))
    }
    
    texto <- head(texto, n = num_palavras)
    if (nrow(texto) == 0) {
      return(NULL)
    }
    
    colnames(texto) <- c("Palavra", "Frequência")
    output$tabela_frequencia <- renderTable({
      tibble(texto)
    })
    
   wordcloud2(texto, backgroundColor = background, fontFamily = fonte,
                 shape = formato, color = paleta)
    
  })
    

  output$wordcloud <- renderWordcloud2({
    criar_nuvem()
    })
  
  # armazena a nuvemcriada em .html
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("wordcloud_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      nuvem <- criar_nuvem()
      saveWidget(nuvem, file)
    }
  )
  
  # Renderiza o código fonte do aplicativo
  output$codigo_fonte <- renderPrint({
    cat(readLines(codigo_fonte), sep = "\n")
    })
  
  # Renderiza o arquivo como usar
  output$how_use <- renderPrint({
    cat(readLines(how_use), sep = "\n")
  })
  
}

shinyApp(ui = ui, server = server)

