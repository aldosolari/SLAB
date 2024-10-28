rm(list=ls())

## -------------------------------------------------------------------------------------------------------------------------------

# importare i dati
titanic <- read.csv("https://raw.githubusercontent.com/aldosolari/IPE/refs/heads/main/docs/titanic.csv")

# dimensione della tabella dei dati 
dim(titanic)

# la "testa" della tabella
head(titanic) 

# variabile sopravvissuti
sopravvissuti = titanic$survived
# frequenze assolute
table(sopravvissuti)
# frequenze relative
table(sopravvissuti)/length(sopravvissuti)
# tasso di sopravvivenza
tasso_sopravvivenza = (table(sopravvissuti)/length(sopravvissuti))[2]
tasso_sopravvivenza

# variabile classe
classe = titanic$class
table(classe)
table(classe)/length(classe)

# tabella di contingenza
table(classe, sopravvissuti)

# frequenze relative condizionate a classe
prop.table(table(classe, sopravvissuti),1)
# tasso di sopravvivenza per classe
tasso_sopravvivenza_classe = prop.table(table(classe, sopravvissuti),1)[,2]
round(tasso_sopravvivenza_classe*100,1)

# diagramma a barre
barplot(tasso_sopravvivenza_classe, 
        main = "Tasso di sopravvivenza per classe",
        xlab="Classe", ylab="% di sopravvissuti")
abline(h=tasso_sopravvivenza)

# diagramma a barre con base proporzionale alla frequenza
plot(as.factor(sopravvissuti) ~ classe)

## -------------------------------------------------------------------------------------------------------------------------------

rm(list=ls())
## -------------------------------------------------------------------------------------------------------------------------------

# importare i dati
library(readr)
draws <- read_delim("https://www.stat.uchicago.edu/~stigler/draws.txt", 
                    delim = "\t", escape_double = FALSE, 
                    col_names = FALSE, trim_ws = TRUE, 
                    show_col_types = FALSE)
head(draws)

draws[c(3749,3769),]

numeri = unlist(draws)

table(numeri)

frequenza_attesa = length(numeri)/90

plot(table(numeri), 
     main = "Frequenza dei numeri estratti (1758-1834)",
     xlab = "Numero", ylab="Frequenza")
abline(h=frequenza_attesa)

draws_ordered = t(apply(draws,1,sort))
head(draws_ordered)

which(table(apply(draws_ordered, 1, function(x) paste(x,collapse=" "))) >= 2)

N = nrow(draws)
c = choose(90,5)

# probability of at least one match
pbirthday(n = N, classes=c)
# approximation
1 - exp(-(N^2)/(2*c))
