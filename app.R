library(shiny)
library(dplyr)
library(stringr)

ui <- fluidPage(
  titlePanel("Jay's Scrabble Spelling Bee Solver"),
  sidebarLayout(
    sidebarPanel(
      selectInput("center_letter", "Center letter:", choices = letters, selected = "a"),
      selectInput("outside_letter1", "First outside letter", choices = letters, selected = "a"),
      selectInput("outside_letter2", "Second outside letter", choices = letters, selected = "a"),
      selectInput("outside_letter3", "Third outside letter", choices = letters, selected = "a"),
      selectInput("outside_letter4", "Fourth outside letter", choices = letters, selected = "a"),
      selectInput("outside_letter5", "Fifth outside letter", choices = letters, selected = "a"),
      selectInput("outside_letter6", "Sixth outside letter", choices = letters, selected = "a")
    ),
    mainPanel(
      tableOutput("solutions")
    )
  )
)

server <- function(input, output) {
  original <- read.csv("C:\\Users\\jwest\\Documents\\Scrabble\\nwl2023.csv")
  letters_vec <- letters
  
  output$solutions <- renderTable({
    center_letter <- input$center_letter
    outside_letters <- c(input$outside_letter1, input$outside_letter2, input$outside_letter3,
                         input$outside_letter4, input$outside_letter5, input$outside_letter6)
    
    forbidden_letters <- setdiff(letters_vec, c(center_letter, outside_letters))
    
    spelling_bee_solutions <- original %>%
      filter(nchar(word) >= 4) %>%
      filter(str_detect(word, center_letter)) %>%
      { 
        for (forbidden_letter in forbidden_letters) {
          . <- filter(., !str_detect(word, forbidden_letter))
        }
        .
      }
    
    spelling_bee_solutions
  })
}

shinyApp(ui = ui, server = server)
