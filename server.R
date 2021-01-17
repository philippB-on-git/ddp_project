library(shiny)
library(ggplot2)

shinyServer(function(input, output, session) {
    dat     <- reactiveValues()
    
    # reinit() resets the app status and re-initializes random data
    reinit  <- function() {
        shinyjs::hide("eval")
        shinyjs::hide("summary")
        shinyjs::enable("guess")
        shinyjs::enable("hint2")
        shinyjs::enable("solve")
        shinyjs::hide("hint1")
        
        fctr <- switch(input$diff,
                       easiest = 1000,
                       easy = runif(n = 1, min = 25, max = 50), 
                       medium = runif(n = 1, min = 8, max = 15),
                       hard = runif(n = 1, min = 1, max = 5)
                       )

        dat$cnt <- 0
        dat$hnt <- 0
        dat$x   <- rnorm(n = 100, mean = 10, sd = 2)
        dat$b1  <- runif(n = 1, min = 0.1, max = 9.9)
        dat$b0  <- runif(n = 1, min = 0, max = 10)
        dat$eps <- rnorm(n = 100, sd = (isolate(mean(dat$x) * dat$b1)) / fctr)
        dat$y   <- isolate(dat$b1 * dat$x + dat$b0 + dat$eps)
 
        dat$guess <- F
        dat$solve <- F
    }

    # Events that cause the app to re-initialize
    observeEvent(c(input$reset.all, input$diff), {
        reinit()
    }, ignoreNULL = FALSE)
    
    # "guess!" button
    observeEvent(input$guess, {
        if (!is.na(input$inter.guessed) && !is.na(input$slope.guessed)) {
            dat$guess <- T
            dat$cnt <- dat$cnt + 1
        }
        
        if (is.na(input$slope.guessed)) {
            updateNumericInput(session, "slope.guessed", value = 5)
            showNotification("Slope must not be empty.", type = "error", duration = 2)
        }
        
        if (is.na(input$inter.guessed)) {
            updateNumericInput(session, "inter.guessed", value = 5)
            showNotification("Intercept must not be empty.", type = "error", duration = 2)
        }
    })
    
    # "hint" button
    observeEvent(input$hint2, {
        # use a random shift to make hint interval asymmetric around the true value
        shift <- sample(seq(from = 0.5, to = 1.5, by = 0.1), 1)
        b1.int  <- round(dat$b1 + c(shift - 2, shift) * dat$b1/(1 + dat$hnt), digits = 2)
        b0.int  <- round(dat$b0 + c(shift - 2, shift) * dat$b0/(1 + dat$hnt), digits = 2)
        
        dat$hnt <- dat$hnt + 1
        showNotification(paste0("Slope is within [", b1.int[1], ",", b1.int[2], "]",
                                " and intercept is within [", b0.int[1], ",", b0.int[2], "]"), 
                         type = "message",
                         duration = 25)
    })
    
    # "solve" button
    observeEvent(input$solve, {
        if (dat$cnt == 0) {
            showNotification("Please guess slope and intercept first.", type = "default")
            return()
        }
        
        shinyjs::disable("guess")
        shinyjs::disable("hint2")
        shinyjs::disable("solve")
        shinyjs::show("hint1")
        shinyjs::show("eval")
        shinyjs::show("summary")
        dat$solve <- T
    })
    
    # x-y-plot
    output$lmPlot <- renderPlot({
        input$guess
        input$solve
        
        g <- ggplot(mapping = aes(x = dat$x, y = dat$y)) + 
            geom_point() + 
            labs(x = "x", y = "y")
        m <- lm(dat$y ~ dat$x)
        
        if (dat$solve)
            # draw lm-line
            g <- g + geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], col = "#F8766D")
        
        if (dat$guess) {
            # draw guessed line
            g <- isolate(g + geom_abline(slope = input$slope.guessed, intercept = input$inter.guessed, col = "#00BFC4", linetype = "dashed"))
            
            # indicate with arrows if guessed line is out of plot range
            g.layout <- ggplot_build(g)$layout
            x.min <- g.layout$panel_scales_x[[1]]$range$range[1]
            x.rng <- g.layout$panel_scales_x[[1]]$range$range[2] - x.min
            y.max <- g.layout$panel_scales_y[[1]]$range$range[2]
            y.rng <- y.max - g.layout$panel_scales_y[[1]]$range$range[1]
            
            y0.guess <- isolate(input$inter.guessed + input$slope.guessed * x.min)
            y1.guess <- isolate(input$inter.guessed + input$slope.guessed * (x.min + x.rng))
            
            if (((y0.guess > y.max - y.rng * 0.002) && isolate(input$slope.guessed) >= 0) || 
                ((y1.guess > y.max - y.rng * 0.002) && isolate(input$slope.guessed) < 0)) {
                g <- g + geom_segment(aes(x = x.min + 0.5 * x.rng, y = y.max - 0.04 * y.rng, 
                                          xend = x.min + 0.5 * x.rng, yend = y.max - 0.005 * y.rng), 
                                      colour='blue', size=1, 
                                      arrow = arrow(length = unit(0.02, "npc"), type="closed"))
            } else if (((y1.guess < y.max - y.rng * 0.998) && isolate(input$slope.guessed) >= 0) ||
                       ((y0.guess < y.max - y.rng * 0.998) && isolate(input$slope.guessed) < 0)){
                g <- g + geom_segment(aes(x = x.min + 0.5 * x.rng, y = y.max - 0.96 * y.rng, 
                                          xend = x.min + 0.5 * x.rng, yend = y.max - 0.995 * y.rng), 
                                      colour='blue', size=1, 
                                      arrow = arrow(length = unit(0.02, "npc"), type="closed"))
            }
        }
        g
    })
    
    # linear model
    m    <- eventReactive(dat$solve, lm(dat$y ~ dat$x))
    
    # residuals of guessed line
    res  <- eventReactive(dat$solve, dat$y - input$inter.guessed + input$slope.guessed * dat$x)
    
    # R-squared of guessed line
    r.sq <- eventReactive(res(), 1 - sum(res()^2)/sum((dat$y - mean(dat$y))^2))
    
    df   <- eventReactive(c(m(), res()), {
        tmp <- rbind(data.frame(x = dat$x, 
                                y = m()$residuals, 
                                gr = rep("actual residual", times = length(dat$x))), 
                     data.frame(x = dat$x, 
                                y = res(), 
                                gr = rep("guess residual", times = length(dat$x))))
    })
    
    # x-vs.-residuals-plot
    output$residuals <- renderPlot({
        if (!dat$solve) return()
        
        r   <- ggplot(mapping = aes(x, y), data = df()) + 
            geom_line(mapping = aes(group = x), linetype = "dashed", alpha = 0.15) +
            geom_point(mapping = aes(color = gr), 
                       size = rep(c(1,2), each = length(dat$x)), 
                       shape = rep(c(3,19), each = length(dat$x))) + 
            geom_hline(yintercept = 0) +
            labs(color = "", y = "residuals") +
            theme(legend.position = "top")
        
        r
        
    })
    
    # q-q-plot
    output$qqplot <- renderPlot({
        if (!dat$solve) return()
        
        q   <- ggplot(mapping = aes(sample = y, color = gr), data = df()) + 
            stat_qq() + stat_qq_line() +
            labs(color = "", y = "residuals") +
            theme(legend.position = "top")
        
        q
        
    })
    
    # summary output
    output$summary <- renderUI({
        if (!dat$solve) return()
        
        isolate({
            if (r.sq() < 0) {
                msg <- "Your guess is worse than the trivial model."
            } else if (r.sq() < 0.2) {
                msg <- "Better than nothing..."
            } else if (r.sq() < 0.4) {
                msg <- "Not good, not bad..."
            } else if (r.sq() < 0.6) {
                msg <- "Good job!"
            } else {
                msg <- "Outstanding guess!"
            }
            
            HTML(paste("", 
                       paste("You guessed a fit with an R-squared of", 
                             round(r.sq(), digits = 2), 
                             "in", dat$cnt, ifelse(dat$cnt == 1, "try", "tries"), 
                             "with", dat$hnt, ifelse(dat$hnt == 1, "hint.", "hints."), msg),
                       sep = "<br/>"))
        })
    })
    
    # comparison output for actual, lm-fitted and guessed slope and intercept
    output$coeffs <- renderTable({
        if (!dat$solve) return()
        
        coef <- data.frame()
        coef[1,1] <- "intercept"
        coef[2,1] <- "slope"
        coef[3,1] <- "SSE"
        coef[4,1] <- "R-squared"
        names(coef)[1] <- ""
        
        isolate({
            coef[1, "created"] <- dat$b0
            coef[2, "created"] <- dat$b1
            coef[3, "created"] <- sum(dat$eps^2)
            coef[4, "created"] <- 1 - sum((dat$y - dat$b0 - dat$b1 * dat$x)^2)/sum((dat$y - mean(dat$y))^2)
            
            coef[1, "lm fit"] <- m()$coefficients[1]
            coef[2, "lm fit"] <- m()$coefficients[2]
            coef[3, "lm fit"] <- sum(m()$residuals^2)
            coef[4, "lm fit"] <- summary(m())$r.squared
            
            coef[1, "guessed"] <- input$inter.guessed
            coef[2, "guessed"] <- input$slope.guessed
            coef[3, "guessed"] <- sum(res()^2)
            coef[4, "guessed"] <- r.sq()
        })
        
        coef
    })
})
