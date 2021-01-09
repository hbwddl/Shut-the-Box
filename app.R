## A shiny app implementing the "shut the box" dice game that I like

library(shiny)

#Initialize some values
max.on.board <- 9
num.dice <- 2

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shut the Box"),

    # Input for:
            #Rules
            helpText("RULES: Roll the dice, then select which numbers from the 'box' (here, it's a list) you will keep. Kept numbers must have the same sum as the dice. When you keep numbers, they are removed from the 'box'. Your score is the sum of the remaining numbers in the 'box'. The goal is to have a score of zero."),
            #Rolling dice
            fluidRow(column(width=4,actionButton("roll.dice","Roll Dice"))),
            #Display dice
            fluidRow(column(width=2,imageOutput("dice1")),column(width=2,imageOutput("dice2"))),
            #Display sum of Dice
            fluidRow(column(width=2,"Sum of Dice:"),column(width=1,textOutput("dice.sum"))),
            #Display board
            fluidRow(checkboxGroupInput("board","Game Board",1:9,inline=T)),
            #Display current sum of choices
            fluidRow(column(width=2,"Sum of Choices:"),column(width=1,textOutput("choice.sum"))),
            #Display score
            fluidRow(column(width=2,"Total Score:"),column(width=1,textOutput("scoretext"))),
            #Saving numbers
            fluidRow(column(width=4,actionButton("keep.nums","Keep Numbers"))),
            #Display warning if they don't add up
            fluidRow(column(width=4,textOutput("warntext"))),
            #Reset game
            fluidRow(column(width=4,actionButton("restart","Restart")))
        )

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    dice.sum <- reactiveVal(0)
    nums.on.board <- reactiveVal(1:max.on.board)
    total.score <- reactiveVal(sum(1:max.on.board))
    
    output$scoretext <- renderText(total.score())
    
    #Game state
    #0 <- beginning of game
    #1 <- rolling dice
    #2 <- saving dice
    #3 <- winning
    #4 <- losing
    game.state <- reactiveVal(0)
    
    observeEvent(input$roll.dice,{
        if(game.state() == 2){
            output$warntext <- renderText("Select numbers to keep")
        } 
        if(game.state() == 1 | game.state() == 0){
            ##Check whether you lost
            dice.roll <- rollDice(n.dice=num.dice)
            
            dice.sum(sum(dice.roll))
            
            output$dice.sum <- renderText(dice.sum())
            
            dice1path <- paste0("Dice-",dice.roll[[1]],".png")
            dice2path <- paste0("Dice-",dice.roll[[2]],".png")
            
            output$dice1 <- renderImage(list(src=dice1path),deleteFile = F)
            output$dice2 <- renderImage(list(src=dice2path),deleteFile = F)
            
            if(!(checkNumbers(dice.sum(),nums.on.board()))){
                output$warntext <- renderText("You lose.")
                game.state(4)
            }
            
            output$choice.sum <- renderText(sum(as.numeric(input$board)))
            
            game.state(2)
        }
    })
    
    #Saving the dice rolls
    observeEvent(input$keep.nums,{
        if(game.state() == 0){
            output$warntext <- renderText("You haven't rolled the dice yet")
        }
        
        if(game.state() == 1){
            output$warntext <- renderText("Roll dice again")
        }
        
        if(game.state() == 2){
            if(sum(as.numeric(input$board)) == dice.sum()){
                output$warntext <- renderText("")
                
                nums.on.board(setdiff(nums.on.board(),as.numeric(input$board)))
                updateCheckboxGroupInput(session=session,inputId="board",label="Game Board",choices=nums.on.board(),inline=T)
            
                total.score(total.score() - sum(as.numeric(input$board)))
                
                output$scoretext <- renderText(total.score())
                
                if(total.score() == 0){
                    output$warntext <- renderText("YOU WIN!!")
                    game.state(3)
                }
                game.state(1)
            } else{
                output$warntext <- renderText("Choices don't add up to the sum of the dice")
            }
        }

    })
    
    observeEvent(input$restart,{
        session$reload()
    })

    ### Some useful functions
    ##Roll x n-sided dice
    rollDice <- function(n.dice=2,n.sides=6){
        dice.list <- sample(1:n.sides,n.dice,replace=T)
        
        return(dice.list)
    }
    
    ##Check if the numbers on the board will sum to the dice
    checkNumbers <- function(a.number,num.on.board=1:9){
        is.allowed <- FALSE
        
        i <- 1
        while(a.number >= sum(1:i) && i <= max(num.on.board)){
            i <- i+1
        }
        max.num.sum <- i - 1
        
        for(i in 1:max.num.sum){
            #Get all available sums
            available.combos <- expand.grid(data.frame(matrix(data=rep(num.on.board,i),ncol=i)))
            
            #Find rows without duplicates
            non.dup <- (unlist(lapply(apply(available.combos,1,unique),length)) == i)
            
            #Remove rows that are not equal to number
            correct.sums <- (rowSums(available.combos) == a.number)
            
            if(sum(non.dup & correct.sums) > 0){
                is.allowed <- TRUE
            }
        }
        
        return(is.allowed)
    }
}

# Run the application 
shinyApp(ui = ui, server = server)