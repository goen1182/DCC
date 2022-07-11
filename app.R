#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)

# 학번 가져오기
survey_2 <- read.csv("num.csv", fileEncoding = "utf-8")
survey_2
base1 <- data.frame(survey_2)

raffle <- function(data, cnt){
    out <- data[sample(nrow(data), replace = FALSE, cnt),]
    return(out)
}

datas <- data.frame(num = raffle(survey_2, 10))

df <- dplyr::tibble(data.frame(num = raffle(survey_2, 10)))
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2차 온라인 설문조사 추첨"),
    
    helpText("추첨 버튼을 클릭해주세요"),
    
    fluidRow(
        column(3,
               actionButton("action", label = "추첨"),
               # DT::dataTableOutput("table_2")
               ),
    ),

    mainPanel(
        DT::DTOutput(outputId = "table"),
        DT::DTOutput(outputId = "table_2")
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data1 <- data.frame(raffle(datas, 10))
    mod_df <- shiny::reactiveValues(x = data1)
    # helptext 다음
    
    
    shiny::observeEvent(input$action, {
        mod_df$x <- mod_df$x %>%
                dplyr::tibble(num = input$num)

    })
    
    proxy <- DT::dataTableProxy('table_2')
    
    shiny::observe({
                DT::replaceData(proxy, mod_df$x)
    })
    output$table_2 <- DT::renderDataTable(
        DT::datatable({
            data <- base1
        }))
    }

# Run the application 
shinyApp(ui = ui, server = server)


