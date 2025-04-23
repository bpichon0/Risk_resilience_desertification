rm(list=ls())
source("./Functions.R")

## Preparing bibliometric data ----
# Loading the output of WOS
d=readxl::read_xls("./data/savedrecs.xls")
colnames(d)=c("Publication_type","Authors","KEEP","Title","Journal","Type_article","Author_keyword","Keyword_plus","Abstract","Citations","DOI")
d$Title=toupper(d$Title)


#Transform it into bibliometrix object (~tibble)
M = convert2df("./data/savedrecs.bib", dbsource = "wos", format = "bibtex")

remove_titles=c("SURFACE ALBEDO AND DESERTIFICATION","SURFACE ALBEDO AND DESERTIFICATION",
                "NO DESERTIFICATION MECHANISM","NO DESERTIFICATION MECHANISM")   

#adding information whether or not keeping the article

M$KEEP=unlist(sapply(1:nrow(M),function(x){
  if (any( d$Title==M$TI[x]) & M$DT[x]!="CORRECTION" & M$TI[x] %!in% remove_titles){
    return(d$KEEP[which(d$Title==M$TI[x])])
  }else if (any(d$DOI==M$DI[x])  & M$DT[x]!="CORRECTION" & M$TI[x] %!in% remove_titles){
    return(d$KEEP[which(d$DOI==M$DI[x])])
  }else{return(NA)}
}))

#filtering out old articles or correction papers 
M_fil=dplyr::filter(M, PY>2000,DT %in% c("REVIEW","ARTICLE","ARTICLE; EARLY ACCESS","ARTICLE; PROCEEDINGS PAPER","LETTER"))

M_fil$TI[is.na(M_fil$KEEP)]
M_fil$KEEP[is.na(M_fil$KEEP)]=c(1,0)
M_fil=M_fil%>%dplyr::filter(., KEEP==1)
saveRDS(M_fil[,-ncol(M_fil)],"./data/Filtered_biblio.rds")




## Cocitation analyses ----

M_fil=readRDS("./data/Filtered_biblio.rds")
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";") #network cocitation

set.seed(123)
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, 
                edges.min=3,label=F,verbose = F,label.n=20,remove.isolates = T)

prepare_graph=Layout_communities(net,switch = T)


g=prepare_graph$graph
V(g)$size = V(g)$size
V(g)$color = prepare_graph$vertex.color
E(g)$color = prepare_graph$edge.color
E(g)$width = E(g)$width
tg = as_tbl_graph(g)%>%
  mutate(top5 = rank(-size) <= 9)


layout_df = as.data.frame(prepare_graph$layout)
colnames(layout_df) = c("x", "y")
layout_df$cluster=as.numeric(as.factor(prepare_graph$vertex.color)) 

# Cluster label positions = centroid of nodes in each cluster
cluster_labels=tibble(label=c("Climate and human-driven \n desertification","Resilience indicators",
                              "   Desertification risk \n assessment","Review and \n vegetation dynamics"),
                      x=c(10.2,4,-9,-7.5),
                      y=c(6.8,-11,-6.2,11),
                      fill_=c("#B8D6DA","#D9F4B6","#E19FB5","#EABDB4")
                      )


V(tg)$name[1:12]=c("Reynolds, 2007","Kéfi, 2007","Geist, 2004","Salvati, 2011","Helldén, 2008","Schlesinger, 1990","Rietkerk, 2004",
                  "Basso, 2000","Contador, 2009","d'Odorico, 2013","Vogt, 2011","Verón, 2006")

p_graph=ggraph(tg, layout = "manual", x = prepare_graph$layout[,1], y = prepare_graph$layout[,2]) +
  geom_edge_link(aes(color = I(color), width = .7*width), show.legend = FALSE) +
  scale_edge_width_identity() +
  geom_node_point(aes(color = I(color), size = 1.2*size), show.legend = FALSE) +
  scale_size_identity() +
  geom_node_label(
    data = function(x) dplyr::filter(x, top5),
    aes(label = name),
    color = "black",         
    fill = "white",          
    label.size = 0.5,        
    label.r = unit(0.15, "lines"),  
    size = 5,alpha=.6             
  ) +theme_void()+
  geom_node_label(
    data = cluster_labels,
    aes(x = x, y = y, label = label,fill=fill_),
    color = "black",
    label.size = 0.5,   
    family = "NewCenturySchoolbook",
    label.r = unit(0.5, "lines"),  
    size = 6.5,alpha=.6             
  )+
  geom_node_label(
    data = NULL,
    aes(x = 1.7, y = 16.5, label = "Co-Citation network on drylands resilience and desertification risk "),
    color = "white",
    fill="black",
    label.size = 1,   
    family = "NewCenturySchoolbook",
    label.padding = unit(.5, "lines"),
    label.r = unit(0.3, "lines"),  
    size = 7.5             
  )+
  ylim(-13,16.5)+xlim(-13,16)+
  scale_fill_manual(values=cluster_labels$fill_)+
  guides(fill="none")



d=tibble()
number_com=length(prepare_graph$community_membership)
for (k in 1:length(E(g))){ #n edges
  
  tail_k=tail_of(g,es = 1:length(E(g)))[k]
  head_k=head_of(g,es = 1:length(E(g)))[k]
  
  cluster_head=as.numeric(na.omit(sapply(1:number_com,function(x){
    if (names(head_k) %in% prepare_graph$community_membership[[x]]){
      return(x)
    }else{
      return(NA)
    }
  })))
  cluster_tail=as.numeric(na.omit(sapply(1:number_com,function(x){
    if (names(tail_k) %in% prepare_graph$community_membership[[x]]){
      return(x)
    }else{
      return(NA)
    }
  })))
  d=rbind(d,tibble(Tail=names(tail_k),
                   Head=names(head_k),
                   Cluster_head=cluster_head,
                   Cluster_tail=cluster_tail))
}

d2=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::summarise(., .groups = "keep",
                   frac_between=sum(Cluster_head!=Cluster_tail)/length(Cluster_head)
  )%>%
  mutate(., frac_within=1-frac_between)%>%
  melt(., id.vars=c("Cluster_head"))%>%
  mutate(., Cluster_head=recode_factor(Cluster_head,
                                       "2"="Review & \n vegetation dynamics",
                                       "3"="Desertification \n risk assessment",
                                       "4"="Resilience \n indicators",
                                       "1"="Climate and human-\n driven desertification"))


d3=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::count(Cluster_tail)%>%
  dplyr::group_by(., Cluster_head)%>%
  mutate(., n=n/sum(n))%>%
  mutate(., Cluster_head=recode_factor(Cluster_head,
                                       "2"="Review & \n vegetation dynamics",
                                       "3"="Desertification \n risk assessment",
                                       "4"="Resilience \n indicators",
                                       "1"="Climate and human-\n driven desertification"))%>%
  mutate(., Cluster_tail=recode_factor(Cluster_tail,
                                       "2"="Review & \n vegetation dynamics",
                                       "3"="Desertification \n risk assessment",
                                       "4"="Resilience \n indicators",
                                       "1"="Climate and human-\n driven desertification"))




p21=ggplot(d2)+
  geom_bar(aes(x=as.factor(Cluster_head),y=value,fill=variable),position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("lightgrey","lightblue"),
                    labels=c("Between","Within"))+
  guides(fill=guide_legend(ncol=2))+
  labs(fill="",x="",y="Fraction of co-citation")+
  theme(axis.text.x = element_text(angle=60,hjust = 1,size=15),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=15),axis.title.y = element_text(size=15))


p22=ggplot(d3)+
  geom_bar(aes(x=as.factor(Cluster_head),
               y=n,fill=as.factor(Cluster_tail)),
           position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("Resilience \n indicators"="#C4EF8D",
                             "Review & \n vegetation dynamics"="#E2A192",
                             "Desertification \n risk assessment"="#D27191",
                             "Climate and human-\n driven desertification"="#7FB5BD"))+
  guides(fill=guide_legend(nrow=4))+
  labs(fill="",x="",y="Fraction of co-citation")+
  theme(axis.text.x = element_text(angle=60,hjust = 1,size=15),
        axis.text.y = element_text(size=15),legend.text = element_text(size=15),
        axis.title.y = element_text(size=15))


ggsave("./Figures/Cocitation_within_between.pdf",
       ggarrange(p_graph,
                 ggarrange(p21,#+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()),
                           p22+guides(fill="none"),ncol=2,labels = letters[2:3],align = "hv",font.label = list(size = 22)),
                 nrow=2,heights = c(1.2,1),labels = c("a",""),font.label = list(size = 22)),
       width = 12,height = 16)

## Same but with more nodes and SBM to cluster ----

M_fil=readRDS("./data/Filtered_biblio.rds")
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";")
set.seed(123)
net=networkPlot(NetMatrix, n = 100, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, 
                edges.min=3,label=F,verbose = F,label.n=20,remove.isolates = T)

igraph_matrix=as.matrix(igraph::as_adjacency_matrix(net$graph)>0)
keep_names=rownames(igraph_matrix)
igraph_matrix=matrix(as.numeric(igraph_matrix),nrow = dim(igraph_matrix)[1],dim(igraph_matrix)[2])
rownames(igraph_matrix)=colnames(igraph_matrix)=keep_names

set.seed(123)
mySimpleSBM = igraph_matrix %>% 
  estimateSimpleSBM("poisson", dimLabels = 'tree', estimOptions = list(verbosity = 1, plot = F))



prepare_graph=Layout_communities(net,switch = F,
                                 communities = tibble(Name=keep_names,Cluster=mySimpleSBM$memberships),
                                 community_colors = c("#7FB5BD","#D27191","#C4EF8D", "#E2A192" , "#FDB462"))


g=prepare_graph$graph
V(g)$size = V(g)$size
V(g)$color = prepare_graph$vertex.color
E(g)$color = prepare_graph$edge.color
E(g)$width = E(g)$width
tg = as_tbl_graph(g)%>%
  mutate(top5 = rank(-size) <= 9)


layout_df = as.data.frame(prepare_graph$layout)
colnames(layout_df) = c("x", "y")
layout_df$cluster=as.numeric(as.factor(prepare_graph$vertex.color)) 


V(tg)$name[1:12]=c("Reynolds, 2007","Kéfi, 2007","Geist, 2004","Salvati, 2011","Helldén, 2008","Schlesinger, 1990","Rietkerk, 2004",
                   "Basso, 2000","Contador, 2009","d'Odorico, 2013","Vogt, 2011","Verón, 2006")

# Cluster label positions = centroid of nodes in each cluster
cluster_labels=tibble(label=c("Climate and human-driven \n desertification","Resilience indicators",
                              "   Desertification risk \n assessment","Review and \n vegetation dynamics")[c(4,1,2,3)],
                      x=c(10.2,4,-9,-7.5),
                      y=c(6.8,-11,-6.2,11),
                      fill_=c("#B8D6DA","#D9F4B6","#E19FB5","#EABDB4")[c(4,1,2,3)])

p_graph=ggraph(tg, layout = "manual", x = prepare_graph$layout[,1], y = prepare_graph$layout[,2]) +
  geom_edge_link(aes(color = I(color), width = .7*width), show.legend = FALSE) +
  scale_edge_width_identity() +
  geom_node_point(aes(color = I(color), size = 1.2*size), show.legend = FALSE) +
  scale_size_identity() +
  geom_node_label(
    data = function(x) dplyr::filter(x, top5),
    aes(label = name),
    color = "black",         
    fill = "white",          
    label.size = 0.5,        
    label.r = unit(0.15, "lines"),  
    size = 5,alpha=.6             
  ) +theme_void()+
  geom_node_label(
    data = cluster_labels,
    aes(x = x, y = y, label = label,fill=fill_),
    color = "black",
    label.size = 0.5,
    family = "NewCenturySchoolbook",
    label.r = unit(0.5, "lines"),  
    size = 6.5,alpha=.6)+             
  geom_node_label(
    data = NULL,
    aes(x = 1.7, y = 16.5, label = "Co-Citation network on drylands resilience and desertification risk "),
    color = "white",
    fill="black",
    label.size = 1,   
    family = "NewCenturySchoolbook",
    label.padding = unit(.5, "lines"),
    label.r = unit(0.3, "lines"),  
    size = 7.5             
  )+
  ylim(-13,16.5)+xlim(-13,16)+
  
  scale_fill_manual(values=rep("grey",4))+
  guides(fill="none")


d=tibble()
for (k in 1:length(E(g))){ #n edges
  
  tail_k=tail_of(g,es = 1:length(E(g)))[k]
  head_k=head_of(g,es = 1:length(E(g)))[k]
  
  cluster_head=mySimpleSBM$memberships[which(keep_names==names(head_k))]
  cluster_tail=mySimpleSBM$memberships[which(keep_names==names(tail_k))]
  
  d=rbind(d,tibble(Tail=names(tail_k),
                   Head=names(head_k),
                   Cluster_head=cluster_head,
                   Cluster_tail=cluster_tail))
}

d2=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::summarise(., .groups = "keep",
                   frac_between=sum(Cluster_head==Cluster_tail)/length(Cluster_head)
  )%>%
  mutate(., frac_within=1-frac_between)%>%
  melt(., id.vars=c("Cluster_head"))%>%
  mutate(., Cluster_head=recode_factor(Cluster_head,
                                       "1"="Review & \n vegetation dynamics",
                                       "2"="Desertification \n risk assessment",
                                       "3"="Resilience \n indicators",
                                       "4"="Climate and human-\n driven desertification"))

d3=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::count(Cluster_tail)%>%
  dplyr::group_by(., Cluster_head)%>%
  mutate(., n=n/sum(n))%>%
  mutate(., Cluster_head=recode_factor(Cluster_head,
                                       "1"="Review & \n vegetation dynamics",
                                       "2"="Desertification \n risk assessment",
                                       "3"="Resilience \n indicators",
                                       "4"="Climate and human-\n driven desertification"))%>%
  mutate(., Cluster_tail=recode_factor(Cluster_tail,
                                       "1"="Review & \n vegetation dynamics",
                                       "2"="Desertification \n risk assessment",
                                       "3"="Resilience \n indicators",
                                       "4"="Climate and human-\n driven desertification"))

p21=ggplot(d2)+
  geom_bar(aes(x=as.factor(Cluster_head),y=value,fill=variable),position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("lightgrey","lightblue"),
                    labels=c("Between","Within"))+
  guides(fill=guide_legend(ncol=2))+
  labs(fill="",x="",y="Fraction of co-citation")+
  theme(axis.text.x = element_text(angle=60,hjust = 1,size=15),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=15),axis.title.y = element_text(size=15))

p22=ggplot(d3)+
  geom_bar(aes(x=as.factor(Cluster_head),
               y=n,fill=as.factor(Cluster_tail)),
           position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("Resilience \n indicators"="#C4EF8D",
                             "Review & \n vegetation dynamics"="#E2A192",
                             "Desertification \n risk assessment"="#D27191",
                             "Climate and human-\n driven desertification"="#7FB5BD"))+
  guides(fill=guide_legend(nrow=4))+guides(fill="none")+
  labs(fill="",x="",y="Fraction of co-citation")+
  # scale_x_discrete(labels=c("Review & \n vegetation dynamics",
  #                           "Desertification \n risk assessment",
  #                           "Resilience \n indicators",
  #                           "Climate and human-\n driven desertification")[c(3,2,4,1)])+
  theme(axis.text.x = element_text(angle=60,hjust = 1,size=15),
        axis.text.y = element_text(size=15),legend.text = element_text(size=15),
        axis.title.y = element_text(size=15))

ggsave("./Figures/Cocitation_within_between_extended.pdf",
       ggarrange(p_graph,
                 ggarrange(p21,#+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()),
                           p22+guides(fill="none"),ncol=2,labels = letters[2:3],align = "hv",font.label = list(size = 22)),
                 nrow=2,heights = c(1.2,1),labels = c("a",""),font.label = list(size = 22)),
       width = 12,height = 16)


## Keyword cooccurrence ----

M_fil$SR=M_fil$SR_FULL
NetMatrix = biblioNetwork(M_fil, analysis = "co-occurrences", network = "keywords", sep = ";")
set.seed(123)
net=networkPlot(NetMatrix, normalize="association", n = 60,
                Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE,
                size=10, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,
                label.n=50,edges.min=3,remove.isolates = T,label=T,verbose = F)

prepare_graph=Layout_communities(net,switch = F,seed_=1,
                                 community_colors = brewer.pal(12,"Set3"))
                                 
g=prepare_graph$graph
V(g)$size = V(g)$size
V(g)$color = prepare_graph$vertex.color
E(g)$color = prepare_graph$edge.color
E(g)$width = E(g)$width

nodes_to_remove=which(V(g)$color %in% c("#B3DE69","#FDB462","#80B1D3"))

g=delete.vertices(g,nodes_to_remove) #visual purpose

tg = as_tbl_graph(g)%>%
  mutate(top5 = rank(-size) <= 15)

layout_df = as.data.frame(prepare_graph$layout[-nodes_to_remove,])
colnames(layout_df) = c("x", "y")
layout_df$cluster=as.numeric(as.factor(prepare_graph$vertex.color[-nodes_to_remove])) 

firstup = function(x) {
  substr(x, 1, 1) = toupper(substr(x, 1, 1))
  x
}


V(tg)$top5[which(V(tg)$name %in% c("resilience","catastrophic shifts","vegetation patterns","early-warning signals","regime shifts"))]=T
V(tg)$name=firstup(V(tg)$name)

p_graph=ggraph(tg, layout = "manual", x = prepare_graph$layout[-nodes_to_remove,1], y = prepare_graph$layout[-nodes_to_remove,2]) +
  geom_edge_link(aes(color = I(color), width = .7*width), show.legend = FALSE) +
  scale_edge_width_identity() +
  geom_node_point(aes(color = I(color), size = 1.2*size), show.legend = FALSE) +
  scale_size_identity() +
  geom_node_label(
    data = function(x) dplyr::filter(x, top5),
    aes(label = name),
    color = "black",         
    fill = "white",          
    label.size = 0.5,        
    label.r = unit(0.15, "lines"),  
    size = 5,alpha=.6             
  ) +theme_void()

pdf("./Figures/Network_themes.pdf", width = 12, height = 9)
print(p_graph)
dev.off()


M_fil=readRDS("./data/Filtered_biblio.rds")
set.seed(123)
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, 
                edges.min=3,label=F,verbose = T,label.n=20,remove.isolates = T)



  
  
Map=thematicMap(M_fil, field = "ID", n = 150, minfreq = 8,
                stemming = FALSE, size = 0.4, n.labels=6, repel = TRUE)
pdf("./Figures/Map_themes.pdf", width = 8, height = 6)
plot(Map$map)
dev.off()


