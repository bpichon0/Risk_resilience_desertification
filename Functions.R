x = c("tidyverse", "ggpubr", "sf", "simecol","reshape2",
      "sp","rgeos","scico","khroma","GGally","ggrepel","maps",
      "tsibble","raster","geodata","car","latex2exp","jtools",
      "ggpubr","bibliometrix","igraph","sbm","RColorBrewer",
      "ggraph","tidygraph")

# to install the packages: lapply(x, install.packages, character.only = TRUE)
lapply(x, require, character.only = TRUE)
the_theme2 = theme_classic() + theme(
  legend.position = "bottom",
  panel.border = element_rect(colour = "black", fill=NA),
  strip.background = element_rect(fill = "transparent",color="transparent"),
  strip.text.y = element_text(size = 10, angle = -90),
  strip.text.x = element_text(size = 10),title = element_text(size=8),
  axis.title.y=element_text(size = 10),
  axis.title.x=element_text(size = 10),
  #legend.box="vertical",
  legend.text = element_text(size = 10), text = element_text(family = "NewCenturySchoolbook")
)
`%!in%` = Negate(`%in%`)

Layout_communities=function(net,switch=F,communities=NULL,seed_=123,
                            community_colors = NULL){
  
  if (is.null(community_colors)){
    community_colors=c("#7FB5BD","#E2A192","#D27191", "#C4EF8D" , "#FDB462")
  }
  
  if (is.null(communities)){
    communities=tibble(Name=net$cluster_res$vertex,Cluster=net$cluster_res$cluster)
  }
  
  if (switch){
    communities$Cluster[which(communities$Cluster==1)]=2
    communities$Cluster=communities$Cluster-1
  } 
  graph_net=net$graph
  
  list_member=list()
  id=1
  for (k in unique(communities$Cluster)){
    list_member[[id]]=sapply(which(communities$Cluster==k),function(x){
      return(communities$Name[x])
    })
    id=id+1
  }

  num_communities = length(unique(communities$Cluster))
  layout_matrix = matrix(NA, nrow = vcount(graph_net), ncol = 2)
  
  community_centers = matrix(0, nrow = num_communities, ncol = 2)
  for (i in unique(communities$Cluster)) {
    angle = 2 * pi * (i - 1) / num_communities
    radius = 9 
    community_centers[i, ] = c(cos(angle), sin(angle)) * radius
  }
  
  for (comm in unique(communities$Cluster)) {
    name_nodes_in_comm = communities$Name[which(communities$Cluster == comm)]
    
    name_nodes_in_comm=which(rownames(as_adjacency_matrix(net$graph)) %in% name_nodes_in_comm)
    subgraph = induced_subgraph(graph_net, vids = name_nodes_in_comm)
    
    degrees = degree(subgraph)
    center_idx = which.max(degrees)
    center_node = name_nodes_in_comm[center_idx]
    
    layout_matrix[center_node, ] = community_centers[comm, ]
    
    others = setdiff(name_nodes_in_comm, center_node)
    n_others = length(others)
    radius_max = 5  
    if (n_others > 0) {
      angles = seq(0, 2 * pi, length.out = n_others + 1)[-1]
      radius = seq(1, radius_max , length.out = n_others + 1)[-1]
      set.seed(seed_)
      radius=sample(radius)
      circle_coords = t(sapply(1:length(angles), function(a) {
        community_centers[comm, ] + c(cos(angles[a]), sin(angles[a])) * radius[a]
      }))
      layout_matrix[others, ] = circle_coords
    }
  }
  
  
  num_communities = length(unique(communities$Cluster))

  
  # Assign colors to nodes based on their community
  node_colors = sapply(1:vcount(graph_net),function(x){
    return(community_colors[communities$Cluster[which(communities$Name==names(V(graph_net))[x])]])
  })
  
  edge_colors = sapply(E(graph_net), function(e) {
    src = ends(graph_net, e)[1]
    tgt = ends(graph_net, e)[2]
    if (communities$Cluster[which(communities$Name==src)] == 
        communities$Cluster[which(communities$Name==tgt)]) {
      community_colors[communities$Cluster[which(communities$Name==src)]]
    } else {
      rgb(0.5, 0.5, 0.5, alpha = 0.1) #light grey
    }
  })
  
  
  V(graph_net)$label=names(V(net$graph))
  V(graph_net)$size=V(net$graph)$size
  
  plot(graph_net,
       layout = layout_matrix,
       vertex.color = node_colors,
       edge.color = edge_colors,
       edge.curved = 0,
       edge.lty = 1)
  
  return(list(graph_net=graph_net,
              community_membership=list_member,
              layout = layout_matrix,
              vertex.color = node_colors,
              edge.color = edge_colors,
              edge.curved = 0,
              edge.lty = 1))
}
