# The-Bibliographic-Coupler
R's rotine to calculate the bibliographic coupling and indentify the coupling units
#
1) Abra R (Open R ou R-Studion)
2) Execute o código (run the code)
3) Excute (run): "references", "Coupling" e "Freq_Coupling", "mtx_cit", "mtx_adj", "mtx_cct"

4) Para construir as redes de acoplamento bibliográfico utilize:

#plot(network_ABA) = rede de de acoplamento não normalizada (coupling network without normalizations)
#plot(network_ABA, edge.width=c(net_list$ABA)) = (rede de acoplamento valorada por Coupling) coupling network with Coupling's edge
#plot(network_ABA, edge.width=c(net_list$Saltons_Cosine)) = (rede de acoplamento valorada pelo Cosseno de Salton) coupling network with Salton's Cosine's edge
#plot(network_ABA, edge.width=c(net_list$Jaccard_Index)) = (rede de acoplamento valorada pelo Índice de Jaccard) coupling network with Jaccard Index's edge

5) Assista o vídeo explicativo da ferramenta em: https://youtu.be/f95I_gc6vi8 (video of the coupler)

7) Aplicativo (shiny app) da ferramenta: https://rafaelcastanha.shinyapps.io/thecoupler (shinyapp of the coupler)

8) email: rafael.castanha@unesp.br
