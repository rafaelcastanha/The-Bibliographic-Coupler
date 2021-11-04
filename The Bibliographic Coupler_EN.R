{

#THE BIBLIOGRAFIC COUPLER
#read tabbed txt file with header. File of cited items or a list of any items (documents, authors, journals, DOI, keywords)
#Check if the file is in the R directory folder
#corpus is the file with all reference lists

#Packages

install.packages("RVenn")
install.packages("dplyr")

#Library

library(dplyr)
library(RVenn)

#corpus

corpus<-read.table("Org.txt", header = TRUE, sep = "\t")

#Corpus to dataframe

corpus<-as.data.frame(corpus)

#remove blank spaces

corpus<-corpus
corpus[corpus==""|corpus==" "|corpus=="   "]<-NA

#item per list

citados<-function(x){return(length(which(!is.na(x))))}
itens_citados<-apply(X=corpus,FUN=citados,MARGIN=c(1,2))
df1<-as.data.frame(itens_citados, header=TRUE)
df2<-colSums(df1)
df2<-as.data.frame(df2, header=TRUE)
df2<-tibble::rownames_to_column(df2, "VALUE")
colnames(df2)[1]<-"Docs"
colnames(df2)[2]<-"refs"
references<-df2

#corpus to venn

corpus_aba<-Venn(corpus)

#Intersect pairs (coupling units)

ABA<-overlap_pairs(corpus_aba)

#Intensidades de ABA

stack(ABA)
df<-as.data.frame(table(stack(ABA)))
int_aba<-aggregate(Freq ~ ind, data = df, FUN = sum)
Freq_ABA<-data.frame(do.call("rbind",strsplit(as.character(int_aba$ind),"...",fixed=TRUE)))
Freq_ABA["ABA"]<-int_aba$Freq

m2=merge(Freq_ABA,df2,by.x="X2",by.y="Docs",all.x=TRUE)
m1=merge(m2,df2,by.x="X1",by.y="Docs",all.x=TRUE)
colnames(m1)[4]<-"refs_X2"
colnames(m1)[5]<-"refs_X1"
Freq_ABA<-m1 %>% select(X1,X2,"refs_X1","refs_X2","ABA")

#Normalizations (Salton's Cosine & Jaccard Index)

novacoluna<-c("Saltons_Cosine")
Freq_ABA[,novacoluna]<-Freq_ABA$ABA/sqrt(Freq_ABA$refs_X1*Freq_ABA$refs_X2)
novacoluna_2<-c("Jaccard_Index")
Freq_ABA[,novacoluna_2]<-Freq_ABA$ABA/(Freq_ABA$refs_X1+Freq_ABA$refs_X2-Freq_ABA$ABA)

#Comands

#references = references number per list
#ABA = coupling units
#Freq_ABA = coupling strenght

}