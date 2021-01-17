library(shiny)
library(shinyjs)

shinyUI(navbarPage("Guess the regression line!",
                   
    tabPanel("App",
        shinyjs::useShinyjs(),
        sidebarLayout(
            sidebarPanel(
                selectInput("diff", "difficulty:", c("easiest", "easy", "medium", "hard"), selected = "hard"),
                actionButton("reset.all", "reset"),
                br(),
                br(), 
                numericInput("slope.guessed", label = "guess slope", min = -5, max = 20, step = 0.2, value = 5),
                numericInput("inter.guessed", label = "guess intercept", min = -5, max = 20, step = 0.2, value = 5),
                
                actionButton("guess", "guess!"),
                actionButton("hint2", "hint"),
                actionButton("solve", "solution"),
                br(),
                shinyjs::hidden(p(id = "hint1", " press reset to start again..."))
            ),
    
            mainPanel(
                plotOutput("lmPlot", height = "350px"),
                
                # shinyjs::hidden() seems not to work for tabsetPanel => hide on startup in server.R
                tabsetPanel(type = "tabs", id = "eval", 
                            tabPanel("Summary", 
                                     htmlOutput("summary"),
                                     br(),
                                     tableOutput("coeffs")),
                            tabPanel("x vs. residuals", 
                                     plotOutput("residuals", height = "325px")),
                            tabPanel("q-q plot", 
                                     plotOutput("qqplot", height = "325px"))
                )
            )
        )
    ),
    navbarMenu("Documentation",
        tabPanel("General",
            h4("General"),
            p("In this shiny app the user is supposed to guess the slope and the 
              intercept of the regression line for the shown data. The data is
              created randomly at runtime, following the equation y = b0 + b1*x + eps
              , where eps is the introduced noice and depends on the mean of the
              random x variable and the selected difficulty. The user can ask for hints, 
              where for both the slope and the intercept a interval is provided - 
              the actual b0 and b1 values lie within this interval. With each given
              hint the range of the intervals is narrowed down, 
              hence guessing the approximate slope and intercept becomes
              more easy. The user can provide several guesses. The game ends with 
              a press on \"solve\" and the correct regression line is revealed. Have fun!"), br()),
        tabPanel("Goal of the game",
            h4("Goal of the game"),
            p("The goal is to guess in as few as possible attempts a regression line
              that is as close as possible to the line fit of the linear regression 
              model and to achieve an positive R-squared with the guessed line, as a 
              negative R-squared indicates a model that is worse than just using the
              mean value of the data to predict each point."), br()),
        tabPanel("Difficulty",
            h4("Difficulty"),
            p("The difficulty setting is used to determine the amount of the added noice
              to the x-y correlation. The x values are initialized randomly and
              y is then given by y = b0 + b1 * x + eps and the random values b0 and b1,
              while for each point a random eps-value is added. ebs follows a normal 
              distribution, whose standard deviation is related to the difficulty setting.
              Harder difficulty level results in higher noice and hence lower signal-
              to-noice ratio making it harder to guess the slope and the intercept."),
            p("Switching the difficulty will result in resetting the game. Progress
              will be lost and new data points will be created."), br()), 
        tabPanel("Guess",
            h4("How to guess"),
            p("In order to guess, the user simply has to specify the slope value and the intercept
              value in the corresponding input boxes and hit the \"guess!\" button. 
              The resulting line is then constructed from the provided slope and intercept
              and drawn in the plot together with the data points. If the guessed 
              line is out of the plottet range, this is indicated by a blue arrow on 
              either the upper or the lower side of the plot. The user can repeat
              guessing any number of times simply by adjusting the slope and the 
              intercept values and pressing the \"guess!\" button again. 
              The line is then updated in the plot."), br()),
        tabPanel("Hints",
            h4("How to get hints"),
            p("Guessing a good line to fit the data just by eyeballing is hard and
              even several tries might not yield the desired line. To get a hint 
              on the actual slope and intercept, that were used to construct
              the random data, the user can press the \"hint\" button. Then, in the
              lower right corner a range for both the slope and the intercept is 
              provided, in which the actual slope and intercept lie. But be careful,
              the interval is not symmetric around the actual values - this would 
              be to easy. With each requested hint, the interval range is narrowed
              down."), br()),
        tabPanel("Solve",
            h4("Solution and result"),
            p("Once the user is confident with their guessed line, the solution
              can be shown by pressing the \"solve\" button. The line derived 
              from the linear regression model is then drawn in red. A summary is
              then shown below the x-y-plot giving info on the guessed line, the
              linear regression line and the actual slope and intercept that were
              used to construct the data. Furthermore, a \"x vs. residuals plot \" 
              and a \"q-q-plot\" is provided to compare the guessed line with the
              linear regression model."), 
            p("Solving is only possible after at least one guess was provided by 
              the user."),
            p("After the \"solve\" button is pressed, guessing is not possible 
              anymore. The game has ended. To restart the game with a new set 
              of data simply click the \"reset\" button"), br()),
        tabPanel("Reset",
            h4("Resetting the game"),
            p("The game can be reset at any given time simply by clicking the 
              \"reset\" button. This will result in the creation of new random
              data and removing any drawn lines in the x-y-plot."), br()),
        tabPanel("Plots",
            h4("x-y-plot"),
            p("This is the standard plot in the upper right part. The current 
              set of data is plotted and guessed lines will be drawn in blue as soon 
              as the \"guess!\" button is pressed. Also, the fitted line 
              derived form the linear regression model is drawn in red once
              the \"solve\" button was pressed."), br(),
            
            h4("x vs. residuals plot"),
            p("Once the \"solve\" button was clicked, this plot will be created.
              It shows both the residuals (y - y_fitted) of the guessed line and
              the linear regression (lm) line. Corresponding points are connected 
              with a dashed line."), br(),
            
            h4("q-q-plot"),
            p("Once the \"solve\" button was clicked, this plot will be created.
              The residual quantiles are plotted against the theoretical quantiles
              of a Gaussian distribution. A line indicates the Gaussian distribution.
              The residuals of the guessed line can be compared with those of the
              linear regression model (lm)."))
    )
))
