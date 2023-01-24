{

#THE BIBLIOGRAFIC COUPLER
#read tabbed txt file with header. File of cited items or a list of any items (documents, authors, journals, DOI, keywords)
#the user must choose a file to process
#Use the file test

#Packages

install.packages("RVenn")
install.packages("dplyr")
install.packages("igraph")

#Library

library(dplyr)
library(RVenn)
library("igraph")

#file choose

corpus<-read.table(file.choose(), header = FALSE, sep = "\t", quote="\"")

colnames(corpus)<-corpus[1,]
corpus<-corpus[(-1),]
hd<-gsub("\\.$","",names(corpus))
colnames(corpus)<-hd

#Corpus to dataframe

corpus<-as.data.frame(corpus)

#remover blank spaces

corpus<-corpus
corpus[corpus==""|corpus==" "|corpus=="   "]<-NA

empty_columns<-sapply(corpus, function(x) all(is.na(x)|x==""))
corpus<-corpus[,!empty_columns]

#item per list

citados<-function(x){return(length(which(!is.na(x))))}
itens_citados<-apply(X=corpus,FUN=citados,MARGIN=c(1,2))
df1<-as.data.frame(itens_citados, header=TRUE)
df2<-colSums(df1)
df2<-as.data.frame(df2, header=TRUE)
df2<-tibble::rownames_to_column(df2, "VALUE")
colnames(df2)[1]<-"units"
colnames(df2)[2]<-"refs"
references<-df2

#Corpus to Venn object

corpus_aba<-Venn(corpus)

#Coupling units

ABA<-overlap_pairs(corpus_aba)
Coupling<-ABA

#Unidades por acoplamento

stack(ABA)

#Coupling frequency

df<-as.data.frame(table(stack(ABA)))
int_aba<-aggregate(Freq ~ ind, data = df, FUN = sum)
Freq_Coupling<-data.frame(do.call("rbind",strsplit(gsub("....", "...",as.character(int_aba$ind), fixed=TRUE),"...",fixed=TRUE)))
Freq_Coupling["Coupling"]<-int_aba$Freq

m2=merge(Freq_Coupling,df2,by.x="X2",by.y="units",all.x=TRUE)
m1=merge(m2,df2,by.x="X1",by.y="units",all.x=TRUE)
colnames(m1)[4]<-"refs_X2"
colnames(m1)[5]<-"refs_X1"
Freq_Coupling<-m1 %>% select(X1,X2,"refs_X1","refs_X2","Coupling")

#Normalizations

novacoluna<-c("Saltons_Cosine")
Freq_Coupling[,novacoluna]<-Freq_Coupling$Coupling/sqrt(Freq_Coupling$refs_X1*Freq_Coupling$refs_X2)
novacoluna_2<-c("Jaccard_Index")
Freq_Coupling[,novacoluna_2]<-Freq_Coupling$Coupling/(Freq_Coupling$refs_X1+Freq_Coupling$refs_X2-Freq_Coupling$Coupling)

#Coupling Network

net_list<-filter(Freq_Coupling, Coupling>0)
links<-data.frame(source=c(net_list$X1), target=c(net_list$X2))
network_ABA<-graph_from_data_frame(d=links, directed=F)

 #Matrix
      

      #Coupling Matrix
      
      mtx<-as_adjacency_matrix(network_ABA)
      E(network_ABA)$weight<-net_list$Coupling
      mtx_ad<-as_adjacency_matrix(network_ABA, attr="weight")
      mtx_adj<-as.data.frame(as.matrix(mtx_ad))
      mtx_adj<-tibble::rownames_to_column(mtx_adj, " ")
      
      dt<-stack(corpus)
      dt2<-table(dt$values[row(dt[-1])], unlist(dt[-1]))
      
	mtx_cit<-t(dt2)
      mtx_cocit<-(t(mtx_cit) %*% mtx_cit)
      mtx_cocit<-as.table(mtx_cocit)
      
      #Co-citation Matrix

#Citation Matrix

mtx_cit<-as.data.frame.matrix(mtx_cit)
mtx_cit<-tibble::rownames_to_column(mtx_cit, " ")

      
      mtx_cocit_df<-as.data.frame(mtx_cocit)
      links_cocit<-data.frame(source=c(mtx_cocit_df$Var1), target=c(mtx_cocit_df$Var2))
      network_cocit<-graph_from_data_frame(d=links_cocit, directed=T)
      E(network_cocit)$weight<-mtx_cocit_df$Freq
      mtx_adj_cocit<-as_adjacency_matrix(network_cocit, attr="weight")
      mtx_adj_cocit_df<-as.data.frame(as.matrix(mtx_adj_cocit))
      mtx_adj_cocit_df<-tibble::rownames_to_column(mtx_adj_cocit_df, " ")
	mtx_cct<-mtx_adj_cocit_df
      

#Comands

#corpus
#references = references per list
#Coupling = coupling units
#Freq_Coupling = Coupling frequency and normalizations
#plot(network_ABA) = coupling network without normalizations
#plot(network_ABA, edge.width=c(net_list$ABA)) = coupling network with Coupling's edge
#plot(network_ABA, edge.width=c(net_list$Saltons_Cosine)) = coupling network with Salton's Cosine's edge
#plot(network_ABA, edge.width=c(net_list$Jaccard_Index)) = coupling network with Jaccard Index's edge
#mtx_adj = Coupling Matrix
#mtx_cit = Citation Matrix (boolean)
#mtx_cct = Cocitation Matrix

}
