rm(list=ls())
source("./Packages.R")

d=readxl::read_xls("./data/savedrecs.xls")
colnames(d)=c("Publication_type","Authors","KEEP","Title","Journal","Type_article","Author_keyword","Keyword_plus","Abstract","Citations","DOI")
d$Title=toupper(d$Title)

M = convert2df("./data/savedrecs.bib", dbsource = "wos", format = "bibtex")

remove_titles=c("SURFACE ALBEDO AND DESERTIFICATION","SURFACE ALBEDO AND DESERTIFICATION",
                "NO DESERTIFICATION MECHANISM","NO DESERTIFICATION MECHANISM")   

M$KEEP=unlist(sapply(1:nrow(M),function(x){
  if (any( d$Title==M$TI[x]) & M$DT[x]!="CORRECTION" & M$TI[x] %!in% remove_titles){
    return(d$KEEP[which(d$Title==M$TI[x])])
  }else if (any(d$DOI==M$DI[x])  & M$DT[x]!="CORRECTION" & M$TI[x] %!in% remove_titles){
    return(d$KEEP[which(d$DOI==M$DI[x])])
  }else{return(NA)}
}))

M_fil=dplyr::filter(M, PY>2000,DT %in% c("REVIEW","ARTICLE","ARTICLE; EARLY ACCESS","ARTICLE; PROCEEDINGS PAPER","LETTER"))

M_fil$TI[is.na(M_fil$KEEP)]
M_fil$KEEP[is.na(M_fil$KEEP)]=c(1,0)
M_fil=M_fil%>%dplyr::filter(., KEEP==1)
saveRDS(M_fil[,-ncol(M_fil)],"./data/Filtered_biblio.rds")


M_fil=readRDS("./data/Filtered_biblio.rds")
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, 
                edges.min=3,label=F,verbose = T,label.n=20,remove.isolates = T)

cocit_colors = c("#7FB5BD","#E2A192","#D27191", "#C4EF8D" , "black")

graph_net=net$graph

V(graph_net)$color=sapply(1:length(V(graph_net)$name),function(x){
  if (V(graph_net)$name[x] %in% net$community_obj[[1]]){
    color_ID=1
  }else if (V(graph_net)$name[x] %in% net$community_obj[[2]] | 
            V(graph_net)$name[x] %in% net$community_obj[[4]]){
    color_ID=2
  }else if (V(graph_net)$name[x] %in% net$community_obj[[3]]){
    color_ID=3
  }else {
    color_ID=4
  }
  return(cocit_colors[color_ID])
})

past_col=c("#E41A1C40","#B3B3B340","#377EB840","#4DAF4A40","#984EA340")

E(graph_net)$color=sapply(E(graph_net)$color,function(x){
  
  if (x==past_col[1]){
    return(cocit_colors[1])
  }else if (x==past_col[2]){
    return("lightgrey")
  }else if (x==past_col[3] | x==past_col[5]){
    return(cocit_colors[2])
  }else if (x==past_col[4]){
    return(cocit_colors[3])
  }else {
    return(cocit_colors[4])
  }
})



pdf("./Figures/Network_cocitation.pdf", width = 10.3, height = 6.5)
plot(graph_net,lty=1)
dev.off()



M_fil$SR=M_fil$SR_FULL
NetMatrix = biblioNetwork(M_fil, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", n = 50,
                Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE,
                size=10, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,
                label.n=50,edges.min=3,remove.isolates = T,label=F)


pdf("./Figures/Network_themes.pdf", width = 10.3, height = 6.5)
plot(net$graph,lty=1,layout=layout_with_fr(net$graph))
dev.off()




M_fil=readRDS("./data/Filtered_biblio.rds")
set.seed(123)
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, 
                edges.min=3,label=F,verbose = T,label.n=20,remove.isolates = T)



d=tibble()

for (k in 1:length(E(net$graph))){ #n edges
 
  tail_k=tail_of(net$graph,es = 1:length(E(net$graph)))[k]
  head_k=head_of(net$graph,es = 1:length(E(net$graph)))[k]
  
  cluster_head=as.numeric(na.omit(sapply(1:5,function(x){
    if (names(head_k) %in% net$community_obj[[x]]){
      return(x)
    }else{
      return(NA)
    }
  })))
  cluster_tail=as.numeric(na.omit(sapply(1:5,function(x){
    if (names(tail_k) %in% net$community_obj[[x]]){
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



d$Cluster_head[d$Cluster_head==1]=3
d$Cluster_tail[d$Cluster_tail==1]=3

d2=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::summarise(., .groups = "keep",
                   frac_between=sum(Cluster_head==Cluster_tail)/length(Cluster_head)
                   )%>%
  mutate(., frac_within=1-frac_between)%>%
  melt(., id.vars=c("Cluster_head"))

d3=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::count(Cluster_tail)%>%
  dplyr::group_by(., Cluster_head)%>%
  mutate(., n=n/sum(n))


p1=ggplot(d2)+
  geom_bar(aes(x=as.factor(Cluster_head),y=value,fill=variable),position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("lightgrey","lightblue"),
                    labels=c("Between","Within"))+
  guides(fill=guide_legend(nrow=4))+
  labs(fill="",x="",y="Fraction of co-citation")+
  scale_x_discrete(labels=c("Resilience \n indicators","Review & \n vegetation dynamics",
                            "Climate and human-\n driven desertification","Desertification \n risk assessment")[c(3,2,4,1)])+
  theme(axis.text.x = element_text(angle=60,hjust = 1,size=15),
        axis.text.y = element_text(size=15),
        legend.text = element_text(size=15),axis.title.y = element_text(size=15))


p2=ggplot(d3)+
  geom_bar(aes(x=as.factor(Cluster_head),
               y=n,fill=as.factor(Cluster_tail)),
           position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("#7FB5BD","#E2A192","#D27191", "#C4EF8D")[c(3,2,4,1)],
                    labels=c("Resilience indicators","Review & vegetation dynamics",
                             "Climate and human-driven desertification","Desertification risk assessment")[c(3,2,4,1)])+
  guides(fill=guide_legend(nrow=4))+
  labs(fill="",x="",y="Fraction of co-citation")+
  scale_x_discrete(labels=c("Resilience \n indicators","Review & \n vegetation dynamics",
                            "Climate and human-\n driven desertification","Desertification \n risk assessment")[c(3,2,4,1)])+
  theme(axis.text.x = element_text(angle=60,hjust = 1,size=15),
        axis.text.y = element_text(size=15),legend.text = element_text(size=15),
        axis.title.y = element_text(size=15))


ggsave("./Figures/Cocitation_within_between.pdf",
       ggarrange(ggplot()+theme_void(),
                 ggarrange(p1,#+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()),
                           p2,ncol=2,labels = letters[2:3],align = "hv"),nrow=2,heights = c(1,1.3),labels = c("a","")),
       width = 12,height = 16)

  
  
Map=thematicMap(M_fil, field = "ID", n = 150, minfreq = 8,
                stemming = FALSE, size = 0.4, n.labels=6, repel = TRUE)
pdf("./Figures/Map_themes.pdf", width = 8, height = 6)
plot(Map$map)
dev.off()



## SBM ----


M_fil=readRDS("./data/Filtered_biblio.rds")
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 100, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, 
                edges.min=3,label=F,verbose = F,label.n=20,remove.isolates = T)

igraph_matrix=as.matrix(igraph::as_adjacency_matrix(net$graph)>0)
igraph_matrix=matrix(as.numeric(igraph_matrix),nrow = dim(igraph_matrix)[1],dim(igraph_matrix)[2])



plotMyMatrix(igraph_matrix, dimLabels =c('tree'))
mySimpleSBM = igraph_matrix %>% 
  estimateSimpleSBM("bernoulli", dimLabels = 'tree', estimOptions = list(verbosity = 1, plot = F))

list_member=list()
id=1
for (k in unique(mySimpleSBM$memberships)){
  list_member[[id]]=sapply(which(mySimpleSBM$memberships==k),function(x){
    return(rownames(as_adjacency_matrix(net$graph))[x])
  })
  id=id+1
}
list_member


num_communities = length(unique(mySimpleSBM$memberships))
layout_matrix = matrix(NA, nrow = vcount(graph), ncol = 2)
graph = net$graph

community_centers = matrix(0, nrow = num_communities, ncol = 2)
for (i in seq_len(num_communities)) {
  angle = 2 * pi * (i - 1) / num_communities
  radius = 7 
  community_centers[i, ] = c(cos(angle), sin(angle)) * radius
}

for (comm in seq_len(num_communities)) {
  nodes_in_comm = which(mySimpleSBM$memberships == comm)
  subgraph = induced_subgraph(graph, vids = nodes_in_comm)
  
  degrees = degree(subgraph)
  center_idx = which.max(degrees)
  center_node = nodes_in_comm[center_idx]
  
  layout_matrix[center_node, ] = community_centers[comm, ]
  
  others = setdiff(nodes_in_comm, center_node)
  n_others = length(others)
  radius = 2  
  if (n_others > 0) {
    angles = seq(0, 2 * pi, length.out = n_others + 1)[-1]
    radius = seq(1, 3 , length.out = n_others + 1)[-1]
    radius=sample(radius)
    circle_coords = t(sapply(1:length(angles), function(a) {
      community_centers[comm, ] + c(cos(angles[a]), sin(angles[a])) * radius[a]
    }))
    layout_matrix[others, ] = circle_coords
  }
}


num_communities = length(unique(mySimpleSBM$memberships))
community_colors = brewer.pal(min(num_communities, 12), "Set3")  

# Assign colors to nodes based on their community
node_colors = community_colors[mySimpleSBM$memberships]

edge_colors = sapply(E(graph), function(e) {
  src = ends(graph, e)[1]
  tgt = ends(graph, e)[2]
  if (mySimpleSBM$memberships[which(rownames(as_adjacency_matrix(net$graph))==src)] == 
      mySimpleSBM$memberships[which(rownames(as_adjacency_matrix(net$graph))==tgt)]) {
    community_colors[mySimpleSBM$memberships[which(rownames(as_adjacency_matrix(net$graph))==src)]]
  } else {
    rgb(0.5, 0.5, 0.5, alpha = 0.1) #light grey
  }
})

V(graph)$label=rownames(as_adjacency_matrix(graph))


d=tibble()
for (k in 1:length(E(graph))){ #n edges
  
  tail_k=tail_of(graph,es = 1:length(E(graph)))[k]
  head_k=head_of(graph,es = 1:length(E(graph)))[k]
  
  cluster_head=mySimpleSBM$memberships[which(rownames(as_adjacency_matrix(net$graph))==names(head_k))]
  cluster_tail=mySimpleSBM$memberships[which(rownames(as_adjacency_matrix(net$graph))==names(tail_k))]
  
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
  melt(., id.vars=c("Cluster_head"))

d3=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::count(Cluster_tail)%>%
  dplyr::group_by(., Cluster_head)%>%
  mutate(., n=n/sum(n))



p2=ggplot(d3)+
  geom_bar(aes(x=as.factor(Cluster_head),
               y=n,fill=as.factor(Cluster_tail)),
           position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=community_colors)+
  labs(fill="",x="",y="Fraction of co-citation")+
  theme(axis.text.x = element_text(angle=60,hjust = 1,size=15),
        axis.text.y = element_text(size=15),legend.text = element_text(size=15),
        axis.title.y = element_text(size=15))

pdf("./Figures/Co_citation_extended.pdf",width = 9,height = 9)
plot(graph,
      layout = layout_matrix,
      vertex.color = node_colors,
      edge.color = edge_colors,
      edge.curved = 0,
      edge.lty = 1)

print(ggarrange(p1,p2,ncol=2))
dev.off()

