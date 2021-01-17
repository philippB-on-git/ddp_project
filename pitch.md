Developing Data Products - Project Assignment
========================================================
author: Philipp B.
date: 2021-01-17
autosize: true

Introducing "Guess the regression line!"
========================================================

When starting to learn statistics and data science methods it is often quite helpful to start developing a basic understanding of data and to get a feel for relations in data as well as correlations between variables.  
  
  
This shiny webapp provides a simple game in which the user can try to guess the slope and the intercept of the linear regression line. The data is created randomly and constructed sucht that x and y are correlated with some noice added to the realtion based on the selected difficulty.

Background and data creation
========================================================

The data is created by using the relation **y = b0 + b1 * x + eps**, where the vector **x** is initialzed with random values following a normal distribution. The two values **b0** and **b1** are randomly created yielding the intercept and the slope respectively. The vector **eps** is then created using a normal distribution with **mean = 0** and the standard deviation depending on the mean of the **x** vector and the selected difficulty (in this example **fctr <- 6**):




```r
x   <- rnorm(n = 100, mean = 10, sd = 2)
b01 <- runif(n = 2, min = 0, max = 10)
eps <- rnorm(n = 100, sd = mean(x) * b01[2] / fctr)
y   <- b01[1] + b01[2] * x + eps
```

Guess the line to fit the created data
========================================================

Suppose after a first glance at the data the user guesses **5** and **3** for the intercept and the slope respectively. The guess is drawn in the dashed blue line while the fit from the linear regression is indicated by the red line.

<img src="pitch-figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />


Comparing residuals of guess with linear model
========================================================




```r
ggplot(mapping = aes(x, y.residual)) + 
    geom_line(mapping = aes(group = x), linetype = "dashed", alpha = 0.15) +
    geom_point(mapping = aes(color = gr)) + geom_hline(yintercept = 0) 
```

<img src="pitch-figure/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />
