title: "HW3"
author: "Math 181B"
date: "Friday of Week 3, 04/13/2022"
output: pdf_document
urlcolor: blue
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Question 1:

```{r}
x <- c(22.8, 10.2, 20.8, 27, 19.2, 9, 14.2, 19.8, 14.5, 14.8)
y <- c(23.5, 31.0, 19.5, 26.2, 26.5, 25.2, 24.5, 23.8, 27.8, 22)
n <- length(x)
m <- length(y)
sq.x <- var(x)
sq.y <- var(y)
fdist1 <- min(sq.x/sq.y, sq.y/sq.x)
fdist1
fdist2 <- max(sq.x/sq.y, sq.y/sq.x)
fdist2

#now calculate the f test statistic
ftest <- (sq.y)^2 / (sq.x)^2
ftest

# we also can calculate the pval
pval <- pf(fdist1, n - 1, m - 1, lower = TRUE) + pf(fdist2, n - 1, m - 1, lower = FALSE)
pval
```

# part B
```{r}
Xbar <- mean(x)
Ybar <- mean(y)
tscoretest <- (Xbar - Ybar) / sqrt(sq.x/n + sq.y/m)
theta <- sq.x/sq.y
thefreedom <- (theta + n/m)^2/(theta^2/(n - 1) + (n/m)^2 /(m - 1))
pval2 <- 2 * pt(abs(tscoretest), df = thefreedom, lower.tail = FALSE)
pval2
```


# Question 2a
```{r}
binom.test(x = 53, n = 91, p = 0.7, alternative = "less")
