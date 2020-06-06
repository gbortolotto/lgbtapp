library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(wesanderson)
library(RColorBrewer)
library(shinydashboard)
#library(plotly)

header <- dashboardHeader(titleWidth = 380)
anchor <- tags$div(href='LGBT_Rainbow_Flag',
                 tags$img(src='LGBT_Rainbow_Flag.png', height='30', width='50'),
                 "Denúncias DISQUE 100 - LGBT")

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: black }"))),
  anchor,
  class = 'name')


ui <- dashboardPage(skin="black",
                    header,
  #dashboardHeader(title = "Denúncias DISQUE 100 - LGBT", titleWidth = 350,
  #                tags$li(div(href = 'LGBT_Rainbow_Flag',
  #                          img(src = 'LGBT_Rainbow_Flag.png',
  #                             height = "30px"),
  #                          style = "padding-top:10px; padding-bottom:10px;margin-right:10px;"),
  #                        class = "dropdown")),
  dashboardSidebar(
    sidebarMenu(
    menuItem('Dados por estado', tabName='dados-estado', icon=icon('map')),
    menuItem('Perfil da vítima', tabName='perfil-vitima', icon=icon('user-friends'), startExpanded=F,
             menuSubItem('Por sexo', tabName='perfil-sexo',icon=NULL),
             menuSubItem('Por gênero', tabName='perfil-genero',icon=NULL),
             menuSubItem('Por idade', tabName='perfil-idade',icon=NULL),
             menuSubItem('Por raça', tabName='perfil-raca',icon=NULL),
             menuSubItem('Por tipo de deficiência', tabName='perfil-deficiencia', icon=NULL)
             ),
    menuItem('Tipo de denúncia', tabName='tipo-denuncia',icon=icon('bullhorn')),
    menuItem('Sobre', tabName='sobre',icon=icon('info-circle')))
  ),
  dashboardBody(
    tabItems(
      tabItem("dados-estado",fluidRow(
              box(width=12,
                title = "Número de casos por estado",
                "Os dados correspondem ao período de janeiro de 2011 a julho de 2019",
                solidHeader = TRUE,
                collapsible = FALSE,
                status="success"
              ),
              box(width=12,
                  status="success",
                  plotlyOutput("mes_uf",height = "600px")
              ))),
      tabItem("perfil-vitima", "Widgets tab content"),
      tabItem("perfil-sexo", fluidRow(
        box(width=12,
            title = "Sexo",
            "Os dados correspondem ao período de janeiro de 2011 a julho de 2019",
            solidHeader = TRUE,
            collapsible = FALSE,
            status="danger"
        ),
        box(width=12,
            status="danger",
            plotlyOutput("pms")
        ))),
              
      tabItem("perfil-genero", fluidRow(
        box(width=12,
            title = "Gênero",
            "Os dados correspondem ao período de janeiro de 2011 a julho de 2019",
            solidHeader = TRUE,
            collapsible = FALSE,
            status="danger"
            
        ),
        box(width=12,
            status="danger",
            plotlyOutput("pmg")
        ))),
              
      tabItem("perfil-idade", fluidRow(
        box(width=12,
            title = "Idade",
            "Os dados correspondem ao período de janeiro de 2011 a julho de 2019",
            solidHeader = TRUE,
            collapsible = FALSE,
            status="danger"
            
        ),
        box(width=12,
            status="danger",
            plotlyOutput("pmi",height = "456px")
        ))),
      
      tabItem("perfil-raca", fluidRow(
        box(width=12,
            title = "Raça",
            "Os dados correspondem ao período de janeiro de 2011 a julho de 2019",
            solidHeader = TRUE,
            collapsible = FALSE,
            status="danger"
            
        ),
        box(width=12,
            status="danger",
            plotlyOutput("pmr")
        ))),
      tabItem("perfil-deficiencia", fluidRow(
        box(width=12,
            title = "Deficiência",
            "Os dados correspondem ao período de janeiro de 2011 a julho de 2019",
            solidHeader = TRUE,
            collapsible = FALSE,
            status="danger"
        ),
        box(width=12,
            status="danger",
            plotlyOutput("pmd")
        ))),
      tabItem("tipo-denuncia",fluidRow(
              box(width=12,
              title = "Tipos de violência",
              "Os dados correspondem ao período de janeiro de 2011 a julho de 2019",
              solidHeader = TRUE,
              collapsible = FALSE,
              status="primary"
                                  
      ), 
              tabBox(width=12,
                         tabPanel("Abuso financeiro", plotlyOutput("abuso")),
                         tabPanel("Discriminação", plotlyOutput("discriminacao")),
                         tabPanel("Negligência", plotlyOutput("negligencia")),
                         tabPanel("Tortura", plotlyOutput("tortura")),
                         tabPanel("Trabalho escravo", plotlyOutput("escravo")),
                         tabPanel("Tráfico de pessoas", plotlyOutput("trafico")),
                         tabPanel("Violência física", plotlyOutput("fisica")),
                         tabPanel("Violência institucional", plotlyOutput("institucional")),
                         tabPanel("Violência psicolõgica", plotlyOutput("psicologica")),
                         tabPanel("Violência sexual", plotlyOutput("sexual")),
                         tabPanel("Outros", plotlyOutput("outros"))))
      ),
      tabItem("sobre", box(width=12,
                           title = "Sobre",
                           
                           
                           p("Esta plataforma visa permitir uma melhor visualização dos dados disponibilidados pelo Ministério da Família e Direitos Humanos referentes às denúncias da população LGBT no serviço", strong("Disque 100,"), "agora chamado de ",strong("Disque Direitos Humanos."),br(),br(),"Os dados correspondem ao período de janeiro de 2011 a julho de 2019. Por abrangerem apenas seis meses, o total de casos denunciados em 2019 é menor do que o visto nos anos anteriores.",br(),br(), "Esses dados podem ser acessados ",a("nesta página.", href = "https://www.gov.br/mdh/pt-br/acesso-a-informacao/ouvidoria/balanco-disque-100"),br(),br(), "Em junho de 2020, o Ministério da Família e Direitos Humanos modificou a forma de apresentar esses dados. Não há mais tabelas específicas para cada tipo de denúncia (em grupos como Crianças e Adolescentes, Pessoas idosas etc). Os dados agora são reunidos em um pdf único que pode ser acessado ",a("neste link.", href="https://www.gov.br/mdh/pt-br/acesso-a-informacao/ouvidoria/Relatorio_Disque_100_2019_.pdf")),
                           solidHeader = TRUE,
                           collapsible = FALSE,
                           status="warning"
      ),)
    ),
    tags$head(tags$style(HTML('
        /* logo */
        .skin-black .main-header .logo {
                              background-color: #000000;
                              color: #FFF;
                              border-right-color: #000;
                              border-left-color: #000;
                              }

        /* navbar (rest of the header) */
        .skin-black .main-header .navbar {
                              background-color: #000000;
                              color: #FFF;
                              }        

         .skin-black .main-header .navbar .sidebar-toggle {
                              color: #FFF;
                              border-right-color: #000;
                              border-left-color: #000;}          
        /* main sidebar */
        .skin-black .main-sidebar {
                              background-color: #101010;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #202020;
                              }

        /* other links in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #101010;
                              color: #FFF;
        }
                              
        /* other links in the sidebarmenu when hovered */
        .skin-black .main-header .logo:hover{
                              background-color: #000;
                              }
         .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #202020;
                              }
        /* toggle button when hovered  */                    
         .skin-black .main-header .navbar .sidebar-toggle:hover{
                              background-color: #202020;
                              }                              
 
                              ')))
  ) # body
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$mes_uf <- renderPlotly({
    input$newplot
    nb.cols <- 28
    mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
    mes_uf <- readRDS(file.path(getwd(),"data/casos_mes_uf.rds"))
    plot_ly()  %>%
      add_trace(data=mes_uf, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)
    
  })
  
  
  output$pmd <- renderPlotly({
    input$newplot

    pmd <- readRDS(file.path(getwd(),"/data/perfil_mes_deficiencia.rds"))
    plot_ly()  %>%
      add_trace(data=pmd, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Deficiência`)
    
  })
  
  output$pmg <- renderPlotly({
    input$newplot
    
    pmg <- readRDS(file.path(getwd(),"/data/perfil_mes_genero.rds"))
    plot_ly()  %>%
      add_trace(data=pmg, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Gênero`)
    
  })
  
  output$pmi <- renderPlotly({
    input$newplot
    
    pmi <- readRDS(file.path(getwd(),"/data/perfil_mes_idade.rds"))
    plot_ly()  %>%
      add_trace(data=pmi, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Idade`)
    
    })
  
  output$pmr <- renderPlotly({
    input$newplot
    
    pmr <- readRDS(file.path(getwd(),"/data/perfil_mes_raca.rds"))
    plot_ly()  %>%
      add_trace(data=pmr, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Raça`)
    
    #ggplot(pmr, aes(x=Ano, y=Casos,color=`Raça`, group=`Raça`)) + geom_point()+geom_line() 
    
  })
  
  output$pms <- renderPlotly({
    input$newplot
    
    pms <- readRDS(file.path(getwd(),"/data/perfil_mes_sexo.rds"))
    
    plot_ly()  %>%
      add_trace(data=pms, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Sexo`)
    
  })
  
  output$abuso <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[1]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`) })
  
  output$discriminacao <- renderPlotly({
    input$newplot
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[2]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`) 
    })
  output$negligencia <- renderPlotly({
    input$newplot
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[3]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)
  })
  output$tortura <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[4]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)
  })
  output$escravo <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[5]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)  
    })
  output$trafico  <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[6]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)
        })
  output$fisica <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[7]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)
        })
  output$institucional <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[8]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)
      })
  output$psicologica <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[9]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)
        })
  output$sexual <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[10]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)
        })
  output$outros <- renderPlotly({
    input$newplot
    
    tipos_violacao <- readRDS(file.path(getwd(),"/data/tipos_violencia.rds"))
    df <- tipos_violacao[[11]]
    colnames(df) <-  c("Estado", "Ano", 'Casos')
    plot_ly()  %>%
      add_trace(data=df, x = ~Ano, y = ~Casos,mode = "lines+markers",type="scatter",linetype = ~`Estado`)  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)


