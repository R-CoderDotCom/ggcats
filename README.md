# ggcats
The geom you always wished for adding cats to ggplot2. This package is part of the memeverse.
The source code of this package is based on geom_image from ggimage.

+ Follow me on [Twitter](https://twitter.com/RCoderWeb)
+ Follow me on [Facebook](https://www.facebook.com/RCODERweb)
+ Visit my [R programming site](https://r-coder.com/)


## Installation
```r
# install.packages("remotes")
remotes::install_github("R-CoderDotCom/ggcats@main")
```


## Available cats

There are 15 cats available:

```r
"nyancat" (default), "bongo", "colonel", "grumpy", "hipster", "lil_bub", "maru",
"mouth", "pop", "pop_close", "pusheen", "pusheen_pc", "toast", "venus" and "shorineko"
```

## Some examples

```r
grid <- expand.grid(1:5, 3:1)

df <- data.frame(x = grid[, 1],
                 y = grid[, 2],
                 image = c("nyancat", "bongo", "colonel", "grumpy", "hipster",
                           "lil_bub", "maru", "mouth", "pop", "pop_close", 
                           "pusheen", "pusheen_pc", "toast", "venus", "shorineko"))
                           
library(ggplot2)
ggplot(df) +
 geom_cat(aes(x, y, cat =image), size = 5) +
    xlim(c(0.25, 5.5)) + 
    ylim(c(0.25, 3.5))
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/105848548-67473e00-5fdf-11eb-8f8b-fff9860b6171.png">
</p>


```r
ggplot(mtcars) +
  geom_cat(aes(mpg, wt), cat = "nyancat", size = 5)
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/105848781-c86f1180-5fdf-11eb-8468-813a41235292.png">
</p>


```r
ggplot(mtcars) +
  geom_cat(aes(mpg, wt, size = cyl), cat = "toast")
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/105849119-416e6900-5fe0-11eb-904e-6dc30be87546.png">
</p>


I took the most part of the following code from [Jonathan Hersh](https://twitter.com/DogmaticPrior).

```r
library(Ecdat)
data(incomeInequality)

library(tidyverse)
library(ggcats)
library(gganimate)


 dat <-
   incomeInequality %>%
   select(Year, P99, median) %>%
   rename(income_median = median,
          income_99percent = P99) %>%
   pivot_longer(cols = starts_with("income"),
                names_to = "income",
                names_prefix = "income_")


dat99 <- dat[dat$income == "99percent", ]
datmedian <- dat[dat$income == "median", ]

dat$cat <- rep(NA, 132)

dat$cat[which(dat$income == "median")] <- "nyancat"
dat$cat[which(dat$income == "99percent")] <- rep(c("pop_close", "pop"), 33)

ggplot(dat, aes(x = Year, y = value, group = income, color = income)) +
   geom_line(size = 2) +
   ggtitle("ggcats, a core package of the memeverse") +
   geom_cat(aes(cat = cat), size = 5) +
   xlab("Cats") +
   ylab("Cats") +
   theme(legend.position = "none",
         plot.title = element_text(size = 20),
         axis.text = element_blank(),
         axis.ticks = element_blank()) +
   transition_reveal(Year)
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/105854010-9ad99680-5fe6-11eb-9ee0-c42e9e257d48.gif">
</p>


