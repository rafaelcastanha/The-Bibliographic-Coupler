{

#THE COUPLER
#ler arquivo txt tabulado com cabeçalho. Arquivo de itens citados ou uma lista de itens qualquer (documentos, autores, periódicos, DOI, keywords)
#Verifique se o arquivo está na pasta do diretório do R
#corpus é o arquivo com todas listas de referências

#PACOTES

install.packages("RVenn")
install.packages("dplyr")

#bibliotecas 

library(dplyr)
library(RVenn)

#Arquivo

corpus<-read.table("Org.txt", header = TRUE, sep = "\t")

#Corpus para dataframe

corpus<-as.data.frame(corpus)

#remover espaços e vazios

corpus<-corpus
corpus[corpus==""|corpus==" "|corpus=="   "]<-NA

#Contagem de itens citados por lista

citados<-function(x){return(length(which(!is.na(x))))}
itens_citados<-apply(X=corpus,FUN=citados,MARGIN=c(1,2))
df1<-as.data.frame(itens_citados, header=TRUE)
df2<-colSums(df1)
df2<-as.data.frame(df2, header=TRUE)
df2<-tibble::rownames_to_column(df2, "VALUE")
colnames(df2)[1]<-"Docs"
colnames(df2)[2]<-"refs"
references<-df2

#Transformação em objeto Venn

corpus_aba<-Venn(corpus)

#Intersecção Pareada: identificação das unidades de acoplamento)

ABA<-overlap_pairs(corpus_aba)

#Unidades por acoplamento

stack(ABA)

#Intensidades de ABA

df<-as.data.frame(table(stack(ABA)))
int_aba<-aggregate(Freq ~ ind, data = df, FUN = sum)
Freq_ABA<-data.frame(do.call("rbind",strsplit(as.character(int_aba$ind),"...",fixed=TRUE)))
Freq_ABA["ABA"]<-int_aba$Freq

m2=merge(Freq_ABA,df2,by.x="X2",by.y="Docs",all.x=TRUE)
m1=merge(m2,df2,by.x="X1",by.y="Docs",all.x=TRUE)
colnames(m1)[4]<-"refs_X2"
colnames(m1)[5]<-"refs_X1"
Freq_ABA<-m1 %>% select(X1,X2,"refs_X1","refs_X2","ABA")

#Normalizações 

novacoluna<-c("Saltons_Cosine")
Freq_ABA[,novacoluna]<-Freq_ABA$ABA/sqrt(Freq_ABA$refs_X1*Freq_ABA$refs_X2)
novacoluna_2<-c("Jaccard_Index")
Freq_ABA[,novacoluna_2]<-Freq_ABA$ABA/(Freq_ABA$refs_X1+Freq_ABA$refs_X2-Freq_ABA$ABA)

#Comandos

#corpus = visualiza todo corpus analisado
#references = número de referencia por lista
#ABA = unidades de acoplamento
#Freq_ABA = Intensidade de acoplamento por intersecção e normalizações por Coseno de Salton e Índice de Jaccard

}