rm(list=ls())

## -------------------------------------------------------------------------------------------------------------------------------
library(readr)
titanic <- read_csv("https://raw.githubusercontent.com/aldosolari/IPE/refs/heads/main/docs/titanic.csv")

View(titanic)

dim(titanic)

sopravvissuti = titanic$survived
table(sopravvissuti)
table(sopravvissuti)/length(sopravvissuti)
tasso_sopravvivenza = (table(sopravvissuti)/length(sopravvissuti))[2]

classe = titanic$class
table(classe)
table(classe)/length(classe)

table(classe, sopravvissuti)

prop.table(table(classe, sopravvissuti),1)
tasso_sopravvivenza_classe = prop.table(table(classe, sopravvissuti),1)[,2]

round(tasso_sopravvivenza_classe*100,1)

barplot(tasso_sopravvivenza_classe, 
        main = "Tasso di sopravvivenza per classe",
        xlab="Classe", ylab="% di sopravvissuti")
abline(h=tasso_sopravvivenza)

## -------------------------------------------------------------------------------------------------------------------------------

rm(list=ls())
## -------------------------------------------------------------------------------------------------------------------------------

draws <- read_delim("https://www.stat.uchicago.edu/~stigler/draws.txt", 
                    delim = "\t", escape_double = FALSE, 
                    col_names = FALSE, trim_ws = TRUE)
View(draws)

draws[c(3749,3769),]

numeri = unlist(draws)

table(numeri)

frequenza_attesa = length(numeri)/90

plot(table(numeri), 
     main = "Frequenza dei numeri estratti (1758-1834)",
     xlab = "Numero", ylab="Frequenza")
abline(h=frequenza_attesa)

draws_ordered = t(apply(draws,1,sort))

which(table(apply(draws_ordered, 1, function(x) paste(x,collapse=" "))) >= 2)
