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




## Keyword co-occurrence ----

seed_ID=78
M_fil=readRDS("./data/Filtered_biblio.rds")

M_fil$SR=M_fil$SR_FULL
NetMatrix = biblioNetwork(M_fil, analysis = "co-occurrences", network = "keywords", sep = ";")
NetMatrix=NetMatrix[-which(rownames(NetMatrix) %in% toupper(c("ecosystems","inner-mongolia","productivity","gis","erosion","time"))),
                    -which(rownames(NetMatrix) %in% toupper(c("ecosystems","inner-mongolia","productivity","gis","erosion","time")))] #isolated nodes

set.seed(seed_ID)
net=networkPlot(NetMatrix, normalize="association", n = 100,
                Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE,
                size=10, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,
                label.n=50,edges.min=1,remove.isolates = T,label=T,verbose = T)


igraph_matrix=as.matrix(igraph::as_adjacency_matrix(net$graph)>0)
keep_names=rownames(igraph_matrix)
igraph_matrix=matrix(as.numeric(igraph_matrix),nrow = dim(igraph_matrix)[1],dim(igraph_matrix)[2])
rownames(igraph_matrix)=colnames(igraph_matrix)=keep_names

set.seed(seed_ID)
mySimpleSBM = igraph_matrix %>% 
  estimateSimpleSBM("poisson", dimLabels = 'tree', estimOptions = list(verbosity = 1, plot = F))

prepare_graph=Layout_communities(net,switch = F,seed_ = seed_ID,max_radius = 5.5,
                                 community_colors = c(brewer.pal(4,"Accent")))

prepare_graph$community_membership                                 
g=prepare_graph$graph
V(g)$size = V(g)$size
V(g)$color = prepare_graph$vertex.color
E(g)$color = prepare_graph$edge.color
E(g)$width = E(g)$width

tg = as_tbl_graph(g)%>%
  mutate(top5 = rank(-size) <= 30)

layout_df = as.data.frame(prepare_graph$layout)
colnames(layout_df) = c("x", "y")
layout_df$cluster=as.numeric(as.factor(prepare_graph$vertex.color)) 

V(tg)$top5[which(V(tg)$name %in% c("resilience","catastrophic shifts","vegetation patterns","early-warning signals","regime shifts"))]=T
V(tg)$name=firstup(V(tg)$name)

p_graph=ggraph(tg, layout = "manual", 
               x = prepare_graph$layout[,1], 
               y = prepare_graph$layout[,2]) +
  geom_edge_link(aes(color = I(color), width = .3*width), show.legend = FALSE) +
  scale_edge_width_identity() +
  geom_node_point(aes(color = I(color), size = 1*size), show.legend = FALSE) +
  scale_size_identity() +
  geom_node_label(
    data = NULL,
    aes(x = 0, y = 16.5, label = "Co-occurrence of keywords"),
    color = "white",
    fill="black",
    label.size = 1,   
    family = "NewCenturySchoolbook",
    label.padding = unit(.5, "lines"),
    label.r = unit(0.3, "lines"),  
    size = 7.5             
  )+
  geom_node_label(
    data = function(x) dplyr::filter(x, top5),
    aes(label = name),
    color = "black",         
    fill = "white",          
    label.size = 0.5,        
    label.r = unit(0.15, "lines"),  
    size = 5,alpha=.6             
  ) +theme_void()+ylim(-15,17)



pdf("./Figures/Network_themes.pdf", width = 10, height = 8)
p_graph
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




## Cocitation papers ----

M_fil=readRDS("./data/Filtered_biblio.rds")
seed_ID=9
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";")
set.seed(seed_ID)
net=networkPlot(NetMatrix, n = 100, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=T, labelsize=1,edgesize = 10, 
                edges.min=2,label=F,verbose = T,label.n=20,remove.isolates = T)

igraph_matrix=as.matrix(igraph::as_adjacency_matrix(net$graph)>0)
keep_names=rownames(igraph_matrix)
igraph_matrix=matrix(as.numeric(igraph_matrix),nrow = dim(igraph_matrix)[1],dim(igraph_matrix)[2])
rownames(igraph_matrix)=colnames(igraph_matrix)=keep_names

set.seed(seed_ID)
mySimpleSBM = igraph_matrix %>% 
  estimateSimpleSBM( dimLabels = 'tree', estimOptions = list(verbosity = 1, plot = F))

member_ship=tibble(groups=mySimpleSBM$memberships)%>%
  mutate(., groups=recode_factor(groups,""))

prepare_graph=Layout_communities(net,switch = F,seed_ = seed_ID,max_radius = 5,
                                 # communities = tibble(Name=keep_names,Cluster=mySimpleSBM$memberships),
                                 community_colors = c("#D27191","#7FB5BD", "#C4EF8D" , "#E2A192",'#2A2D7D'))

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

V(tg)$name[which(V(tg)$name %in% c("scheffer m 2009","huang jp 2020"))]=c("Scheffer, 2009","Huang, 2020")
V(tg)$top5[which(V(tg)$name %in% (c("Helldén, 2008")))]=F
V(tg)$top5[which(V(tg)$name %in% (c("Scheffer, 2009","Huang, 2020")))]=T

# Cluster label positions = centroid of nodes in each cluster
cluster_labels=tibble(label=c("Reviews and \n vegetation dynamics",
                              "Human and climatic \n drivers of desertification",
                              "  Desertification risk \n assessment",
                              "Resilience indicators"),
                      x=c(11,9,-9,-7.5),
                      y=c(5,-11,-5,12),
                      fill_=c("#EABDB4","#D9F4B6","#B8D6DA","#E19FB5"))

p_graph=ggraph(tg, layout = "manual", x = prepare_graph$layout[,1], y = prepare_graph$layout[,2]) +
  geom_edge_link(aes(color = I(color), width = .4*width), show.legend = FALSE) +
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
    fill =c("#E19FB5","#EABDB4","#D9F4B6","#B8D6DA"),
    label.size = 0.5,
    family = "NewCenturySchoolbook",
    label.r = unit(0.5, "lines"),  
    size = 6.5,alpha=.6)+             
  xlim(-13,16)+
  scale_fill_manual(values=rep("grey",4))+
  guides(fill="none")

communities = tibble(Name=gsub("-1","",net$cluster_res$vertex),Cluster=net$cluster_res$cluster)

M_fil=readRDS("./data/Filtered_biblio.rds")
M_fil$SR=M_fil$SR_FULL

M_fil$Changed_name=sapply(1:nrow(M_fil),function(x){
  full_name=M_fil$SR_FULL[x]
  full_name=gsub(",","", full_name)
  full_name=gsub("(\\d{4}).*", "\\1", full_name)
  full_name=tolower(full_name)
  return(full_name)
})

all_keywords=tibble()
for (cluster_ID in 1:4){
  
  M_fil2=M_fil%>%dplyr::filter(., Changed_name %in% unique(communities$Name[which(communities$Cluster==cluster_ID)]))
  NetMatrix = biblioNetwork(M_fil2, analysis = "co-occurrences", network = "keywords", sep = ";")
  NetMatrix=sort(rowSums(NetMatrix),decreasing = T)
  Keyword_data=data.frame(Keyword=firstup(tolower(names(NetMatrix))),
                          Number_occ=as.numeric(NetMatrix))%>%
    dplyr::arrange(., desc(Number_occ))
  all_keywords=rbind(all_keywords,Keyword_data%>%
                       mutate(Name_cluster=cluster_ID))
}


communities%>%sample_n(20)

blue_clust=all_keywords%>%
  dplyr::filter(., Name_cluster==1)

blue_clust=blue_clust[-which(blue_clust$Keyword %in% c("Lessons","Ecological knowledge","Science")),]

p1=ggplot(blue_clust%>%
            mutate(., angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
            slice_head(.,n=12),
          aes(label = Keyword, 
              size = Number_occ,
              color = Number_occ,
              angle = angle))+
  ggwordcloud::geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  scale_color_gradient(low = "#D27191", high = "#8A1A3F")


green_clust=all_keywords%>%
  dplyr::filter(., Name_cluster==2)

green_clust=green_clust[-which(green_clust$Keyword %in% c("Arid ecosystems","Species","Grasslands"
                                              ,"Community","Competition","Facilitation",
                                              "Disturbance","Dynamics","Landscape","Species response")),]

green_clust$Number_occ[which(green_clust$Keyword=="Desertification")]=
  green_clust$Number_occ[which(green_clust$Keyword=="Desertification")]+
  green_clust$Number_occ[which(green_clust$Keyword=="Global desertification")]

green_clust$Number_occ[which(green_clust$Keyword=="Diversity")]=
  green_clust$Number_occ[which(green_clust$Keyword=="Diversity")]+
  green_clust$Number_occ[which(green_clust$Keyword=="Richness")]

green_clust$Number_occ[which(green_clust$Keyword=="Vegetation patterns")]=
  green_clust$Number_occ[which(green_clust$Keyword=="Vegetation patterns")]+
  green_clust$Number_occ[which(green_clust$Keyword=="Patch size distribution")]+
  green_clust$Number_occ[which(green_clust$Keyword=="Patchiness")]+
  green_clust$Number_occ[which(green_clust$Keyword=="Self-organized patchiness")]
  
green_clust$Number_occ[which(green_clust$Keyword=="Indicator")]=
  green_clust$Number_occ[which(green_clust$Keyword=="Suitable indicator")]+
  green_clust$Number_occ[which(green_clust$Keyword=="Indicator")]

green_clust$Number_occ[which(green_clust$Keyword=="Desertification")]=
  green_clust$Number_occ[which(green_clust$Keyword=="Desertification")]+
  green_clust$Number_occ[which(green_clust$Keyword=="Global desertification")]

green_clust=green_clust[-which(green_clust$Keyword %in% c("Global desertification","Suitable indicator","Self-organized patchiness"
                                              ,"Patchiness","Patch size distribution","Richness")),]


p2=ggplot(green_clust%>%
            dplyr::arrange(., desc(Number_occ))%>%
            mutate(., angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
            slice_head(.,n=12),
          aes(label = Keyword, 
              size = Number_occ,
              color = Number_occ,
              angle = angle))+
  ggwordcloud::geom_text_wordcloud_area() +
  scale_size_area(max_size = 30) +
  theme_minimal() +
  scale_color_gradient(low = "#7FB5BD", high = "#114586")


red_clust=all_keywords%>%
  dplyr::filter(., Name_cluster==3)

red_clust=red_clust[-which(red_clust$Keyword %in% c("Areas","Carpathian","Mountains","Neutrality","Southern","Agri basin"
                                          ,"Classification","Example","Forest","Gis","Landscape","Resources","Systems")),]

red_clust$Number_occ[which(red_clust$Keyword=="Desertification risk")]=
  red_clust$Number_occ[which(red_clust$Keyword=="Desertification risk")]+
  red_clust$Number_occ[which(red_clust$Keyword=="Risk")]

red_clust$Keyword[which(red_clust$Keyword=="Reference evapotranspiration")]="Evapotranspiration"

# red_clust=red_clust[-which(red_clust$Keyword %in% c("Risk")),]



p3=ggplot(red_clust%>%
            dplyr::arrange(., desc(Number_occ))%>%
            mutate(., angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
            slice_head(.,n=12),
          aes(label = Keyword, 
              size = Number_occ,
              color = Number_occ,
              angle = angle))+
  ggwordcloud::geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  scale_color_gradient(low = "#A5CC73", high = "#476B19")



orange_clust=all_keywords%>%
  dplyr::filter(., Name_cluster==4)

orange_clust=orange_clust[-which(orange_clust$Keyword %in% c("Grassland","Impact","Satellite","Arid grazing lands","Central australia")),]

orange_clust$Number_occ[which(orange_clust$Keyword=="Trend analysis")]=
  orange_clust$Number_occ[which(orange_clust$Keyword=="Trend analysis")]+
  orange_clust$Number_occ[which(orange_clust$Keyword=="Time-series")]
orange_clust=orange_clust[-which(orange_clust$Keyword %in% c("Time-series")),]

p4=ggplot(orange_clust%>%
            dplyr::arrange(., desc(Number_occ))%>%
            mutate(., angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
            slice_head(.,n=12),
          aes(label = Keyword, 
              size = Number_occ,
              color = Number_occ,
              angle = angle))+
  ggwordcloud::geom_text_wordcloud_area() +
  scale_size_area(max_size = 25) +
  theme_minimal() +
  scale_color_gradient(low = "#E2A192", high = "#96321B")


p_title1=ggraph(tibble(),layout = "manual",x=1,y=1) +
  geom_node_label(
    data = NULL,
    aes(x = 0, y = 0, label = "Co-Citation network of articles"),
    color = "white",
    fill="black",
    label.size = 1,   
    family = "NewCenturySchoolbook",
    label.padding = unit(.5, "lines"),
    label.r = unit(0.3, "lines"),  
    size = 7.5             
  )

p_title2=ggraph(tibble(),layout = "manual",x=1,y=1) +
  geom_node_label(
    data = NULL,
    aes(x = 0, y = 0, label = "Main keywords"),
    color = "white",
    fill="black",
    label.size = 1,   
    family = "NewCenturySchoolbook",
    label.padding = unit(.5, "lines"),
    label.r = unit(0.3, "lines"),  
    size = 7.5             
  )

p_tot=ggarrange(p_title1+theme_void(),
                p_graph,
                p_title2+theme_void(),
                ggarrange(p2,p1,p3,p4,ncol=2,nrow=2),
                nrow=4,heights = c(.1,1,.1,.9),
                labels = c("","a","","b"),font.label = list(size=20))

ggsave("./Figures/New_main_figure.pdf",p_tot,width = 10,height = 14)


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
                                       "1"="Review & \n vegetation dynamics",
                                       "2"="Desertification \n risk assessment",
                                       "3"="Resilience \n indicators",
                                       "4"="Human and climatic \n drivers of desertification"))

d3=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::count(Cluster_tail)%>%
  dplyr::group_by(., Cluster_head)%>%
  mutate(., n=n/sum(n))%>%
  mutate(., Cluster_head=recode_factor(Cluster_head,
                                       "1"="Review & \n vegetation dynamics",
                                       "2"="Desertification \n risk assessment",
                                       "3"="Resilience \n indicators",
                                       "4"="Human and climatic \n drivers of desertification"))%>%
  mutate(., Cluster_tail=recode_factor(Cluster_tail,
                                       "1"="Review & \n vegetation dynamics",
                                       "2"="Desertification \n risk assessment",
                                       "3"="Resilience \n indicators",
                                       "4"="Human and climatic \n drivers of desertification"))

p2=ggplot(d2%>%
             mutate(., Cluster_head2=rep(1:4,2)))+
  geom_bar(aes(x=Cluster_head2,y=value,fill=variable),position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("lightgrey","lightblue"),
                    labels=c("Between","Within"))+
  guides(fill=guide_legend(nrow=2))+
  scale_x_continuous(labels = d2$Cluster_head[1:4],breaks = 1:4)+
  labs(fill="",x="",y="Fraction of co-citation")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))


p3=ggplot(d3%>%
             mutate(., Cluster_head2=as.numeric(Cluster_head)))+
  geom_bar(aes(x=Cluster_head2,
               y=n,fill=as.factor(Cluster_tail)),
           position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("Resilience \n indicators"="#7FB5BD",
                             "Human and climatic \n drivers of desertification"="#E2A192",
                             "Desertification \n risk assessment"="#C4EF8D",
                             "Review & \n vegetation dynamics"="#D27191"))+
  guides(fill=guide_legend(nrow=2))+
  scale_x_continuous(labels = d2$Cluster_head[1:4],breaks = 1:4)+
  labs(fill="",x="",y="Fraction of co-citation")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))

p1=ggplot(mySimpleSBM$connectParam$mean%>%
            melt(.)%>%
            mutate(., Var1=recode_factor(Var1,
                                         "1"="Review & \n vegetation dynamics",
                                         "2"="Desertification \n risk assessment",
                                         "3"="Resilience \n indicators",
                                         "4"="Human and climatic \n drivers of desertification"))%>%
            mutate(., Var2=recode_factor(Var2,
                                         "1"="Review & \n vegetation dynamics",
                                         "2"="Desertification \n risk assessment",
                                         "3"="Resilience \n indicators",
                                         "4"="Human and climatic \n drivers of desertification")))+
  geom_tile(aes(x=Var1,Var2,fill=value))+
  theme_classic()+
  geom_text(aes(x=Var1,Var2,label=round(value,2)),size=3)+
  scale_fill_gradient2(low="white",mid="grey",high = "grey40",midpoint = .25,na.value = "white")+
  theme(axis.text.x = element_text(angle=60,hjust=1))+
  labs(x="",y="",fill=TeX("$\\alpha_{ii}, \\alpha_{ij}$"))

  


ggsave("./Figures/Properties_cocitation.pdf",
                 # ggarrange(p2,
                 #           #p2,
                 #           ggarrange(ggplot()+theme_void(),p3,ggplot()+theme_void(),ncol=3,widths = c(.2,1,.2)),
                 #           nrow=2,labels = letters[1:2],font.label = list(size = 22)),
       p3,
       width = 6,height = 5)



## Same but with/less more nodes and SBM to cluster ----

M_fil=readRDS("./data/Filtered_biblio.rds")
seed_ID=9
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";")

set.seed(seed_ID)
net=networkPlot(NetMatrix, n = 120, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=T, labelsize=1,edgesize = 10, 
                edges.min=3,label=F,verbose = F,label.n=20,remove.isolates = T)

set.seed(seed_ID)

igraph_matrix=as.matrix(igraph::as_adjacency_matrix(net$graph)>0)
keep_names=rownames(igraph_matrix)
igraph_matrix=matrix(as.numeric(igraph_matrix),nrow = dim(igraph_matrix)[1],dim(igraph_matrix)[2])
rownames(igraph_matrix)=colnames(igraph_matrix)=keep_names

set.seed(seed_ID)
mySimpleSBM = igraph_matrix %>% 
  estimateSimpleSBM( dimLabels = 'tree', estimOptions = list(verbosity = 1, plot = F))

prepare_graph=Layout_communities(net,switch = F,seed_ = seed_ID,max_radius = 5,
                                 communities = tibble(Name=keep_names,Cluster=mySimpleSBM$memberships),
                                 community_colors = c("#2A2D7D","#C4EF8D","#7FB5BD", "#D27191" , "#E2A192","pink","yellow"))
g=prepare_graph$graph
V(g)$size = V(g)$size
V(g)$color = prepare_graph$vertex.color
E(g)$color = prepare_graph$edge.color
E(g)$width = E(g)$width
tg = as_tbl_graph(g)%>%
  mutate(top5 = rank(-size) <= 10)


layout_df = as.data.frame(prepare_graph$layout)
colnames(layout_df) = c("x", "y")
layout_df$cluster=as.numeric(as.factor(prepare_graph$vertex.color)) 

V(tg)$name[1:13]=c("Reynolds, 2007","Kéfi, 2007","Geist, 2004","Salvati, 2011","Helldén, 2008","Schlesinger, 1990","Rietkerk, 2004",
                   "Basso, 2000","Contador, 2009","d'Odorico, 2013","Vogt, 2011","Verón, 2006","Jafari, 2016")

V(tg)$name[which(V(tg)$name %in% c("scheffer m 2009","huang jp 2020"))]=c("Scheffer, 2009","Huang, 2020")
V(tg)$top5[which(V(tg)$name %in% (c("Helldén, 2008")))]=F
V(tg)$top5[which(V(tg)$name %in% (c("Scheffer, 2009","Huang, 2020","Jafari, 2016")))]=T

p_graph=ggraph(tg, layout = "manual", x = prepare_graph$layout[,1], y = prepare_graph$layout[,2]) +
  geom_edge_link(aes(color = I(color), width = .4*width), show.legend = FALSE) +
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
  xlim(-13,16)+
  scale_fill_manual(values=rep("grey",4))+
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

d3=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::count(Cluster_tail)%>%
  dplyr::group_by(., Cluster_head)%>%
  mutate(., n=n/sum(n))%>%
  mutate(., Cluster_head=recode_factor(Cluster_head,
                                       "1"="General papers desertification",
                                       "2"="Resilience \n indicators",
                                       "3"="Desertification \n risk assessment",
                                       "4"="Human and climatic \n drivers of desertification",
                                       "5"="Human and climatic \n drivers of desertification-2",
                                       "6"="Desertification \n risk assessment-2"))%>%
  mutate(., Cluster_tail=recode_factor(Cluster_tail,
                                       "1"="General papers desertification",
                                       "2"="Resilience \n indicators",
                                       "3"="Desertification \n risk assessment",
                                       "4"="Human and climatic \n drivers of desertification",
                                       "5"="Human and climatic \n drivers of desertification-2",
                                       "6"="Desertification \n risk assessment-2"))

p=ggplot(d3%>%
            mutate(., Cluster_head2=as.numeric(Cluster_head)))+
  geom_bar(aes(x=Cluster_head,
               y=n,fill=as.factor(Cluster_tail)),
           position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("Human and climatic \n drivers of desertification"="#D27191",
                             "Human and climatic \n drivers of desertification-2"="pink",
                             "Desertification \n risk assessment"="#C4EF8D",
                             "Desertification \n risk assessment-2"="#E2A192",
                             "Resilience \n indicators"="#7FB5BD",
                             "General papers desertification"="#2A2D7D"))+
  guides(fill=guide_legend(nrow=2))+
  labs(fill="",x="",y="Fraction of co-citation")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))

ggsave("./Figures/Sensitivity_network_SI.pdf",ggarrange(p_graph,p,heights = c(1,1),nrow=2,labels = letters[1:2]),width = 9,height = 15)


### SMALER NUMBER OF NODES

M_fil=readRDS("./data/Filtered_biblio.rds")
seed_ID=9
NetMatrix = biblioNetwork(M_fil, analysis = "co-citation", network = "references", sep = ";")

set.seed(seed_ID)
net=networkPlot(NetMatrix, n = 80, Title = "Co-Citation Network", type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=T, labelsize=1,edgesize = 10, 
                edges.min=2,label=F,verbose = F,label.n=20,remove.isolates = T)

set.seed(seed_ID)

prepare_graph=Layout_communities(net,switch = F,seed_ = seed_ID,max_radius = 5,
                                 community_colors = c("#7FB5BD","#D27191","#C4EF8D", "#E2A192" , "black"))
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
                   "Basso, 2000","Contador, 2009","d'Odorico, 2013","Vogt, 2011","Verón, 2006","Jafari, 2016")

V(tg)$name[which(V(tg)$name %in% c("scheffer m 2009","huang jp 2020"))]=c("Scheffer, 2009","Huang, 2020")
V(tg)$top5[which(V(tg)$name %in% (c("Helldén, 2008")))]=F
V(tg)$top5[which(V(tg)$name %in% (c("Scheffer, 2009","Huang, 2020")))]=T

p_graph=ggraph(tg, layout = "manual", x = prepare_graph$layout[,1], y = prepare_graph$layout[,2]) +
  geom_edge_link(aes(color = I(color), width = .4*width), show.legend = FALSE) +
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
  xlim(-13,16)+
  scale_fill_manual(values=rep("grey",4))+
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

d3=d%>%
  dplyr::group_by(., Cluster_head)%>%
  dplyr::count(Cluster_tail)%>%
  dplyr::group_by(., Cluster_head)%>%
  mutate(., n=n/sum(n))%>%
  mutate(., Cluster_head=recode_factor(Cluster_head,
                                       "1"="Review & \n vegetation dynamics",
                                       "2"="Desertification \n risk assessment",
                                       "3"="Resilience \n indicators",
                                       "4"="Human and climatic \n drivers of desertification"))%>%
  mutate(., Cluster_tail=recode_factor(Cluster_tail,
                                       "1"="Review & \n vegetation dynamics",
                                       "2"="Desertification \n risk assessment",
                                       "3"="Resilience \n indicators",
                                       "4"="Human and climatic \n drivers of desertification"))


p=ggplot(d3%>%
           mutate(., Cluster_head2=as.numeric(Cluster_head)))+
  geom_bar(aes(x=Cluster_head,
               y=n,fill=as.factor(Cluster_tail)),
           position="fill", stat="identity")+
  the_theme2+
  scale_fill_manual(values=c("Resilience \n indicators"="#C4EF8D",
                             "Human and climatic \n drivers of desertification"="#E2A192",
                             "Desertification \n risk assessment"="#D27191",
                             "Review & \n vegetation dynamics"="#7FB5BD"))+
  guides(fill=guide_legend(nrow=2))+
  labs(fill="",x="",y="Fraction of co-citation")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))

ggsave("./Figures/Sensitivity_network_SI_small.pdf",ggarrange(p_graph,p,heights = c(1,1),nrow=2,labels = letters[1:2]),width = 9,height = 15)

