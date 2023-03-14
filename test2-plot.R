library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic, olive$palmitoleic)

hist(olive$eicoseno)

boxplot(palmitic~region, data = olive)