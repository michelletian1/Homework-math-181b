---
title: "homework 4"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
x = c(45, 34, 32, 15, 13, 14)
n = 153
p = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)
exp.n = n*p
d = 0.922
pval = 1 - pchisq(d, df = 5)
pval
```

# Question 2
```{r}
table = c(251,264,234,283,226,244,269,241,276,274,263,243,254,276,241,232,260,248,284,253,265,235,259,279,256,256,254,256,250,269,240,261,263,262,259,230,268,284,259,261,268,268,264,271,263,259,294,259,263,278,267,293,247,244,250,266,286,263,274,253,281,286,266,249,255,233,245,266,265,264,257,261,252,254,266)

n = 75
thmean = mean(table)
standardD = sqrt(var(table))

x=1:8
x[1] = sum(table >= 220 & table < 230) 
x[2] = sum(table >= 230 & table < 240) 
x[3] = sum(table >= 240 & table < 250)  
x[4] = sum(table >= 250 & table < 260)
x[5] = sum(table >= 260 & table < 270)
x[6] = sum(table >= 270 & table < 280)
x[7] = sum(table >= 280 & table < 290)
x[8] = sum(table >= 290 & table < 300) 

p=1:8
countingprob = function(firstN, secondM, themean, standardD){
  result = countingprob(secondM, mean = themean, sd = standardD) - pnorm(firstN, mean = themean, sd = standardD)
  return(result)
}
p[1] = countingprob(220,230, themean = themean, standardD = standardD)
p[2] = countingprob(230,240, themean = themean, standardD = standardD)
p[3] = countingprob(240,250, themean = themean, standardD = standardD)
p[4] = countingprob(250,260, themean = themean, standardD = standardD)
p[5] = countingprob(260,270, themean = themean, standardD = standardD)
p[6] = countingprob(270,280, themean = themean, standardD = standardD)
p[7] = countingprob(280,290, themean = themean, standardD = standardD)
p[8] = countingprob(290,300, themean = themean, standardD = standardD)

d = 0
for (i in 1:8) {
  d = d + ((x[i]-n*p[i])^2)/(n*p[i])
}

p-val = 1 - pchisq(d, df =  7)
print(pval)


```

