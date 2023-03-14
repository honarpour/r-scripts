library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

library(dplyr)
heights <- setDT(heights)

head(heights)

average <- mean(heights$height, na.rm = TRUE)
average

ind <- heights[, height > average]
ind
total = sum(ind == TRUE)
total

indF <- heights[, height > average & sex == 'Female']
indF
totalF = sum(indF == TRUE)
totalF

proportionF <- mean(heights$sex == 'Female')
proportionF

minH <- min(heights$height)
minH

matched <- match(minH, heights$height)
matched

matchedSex <- top_n(heights[matched], 10)
matchedSex

maxH <- max(heights$height)
maxH

x <- 50:82
x

notHeights <- sum(!x %in% heights$height)
notHeights

heights2 <- heights[, ht_cm := height * 2.54]
heights2
heights2[18]
mean(heights2$ht_cm)

females <- heights[sex == 'Female']
females
mean(females$ht_cm)
