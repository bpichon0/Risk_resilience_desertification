rm(list=ls())
source("./Functions.R")

dir.create("./Figures",showWarnings = F)

# Comparing resilience indicators in simulations and in the data ----

d_obs=read.table("./data/Empirical_data.csv",sep=";")
d_sim=read.table("./data/All_simulations.csv",sep=";")

p01=ggplot(d_sim)+
  geom_point(aes(x=AC_ini_space,y=AC_ini_time),fill="lightblue",color="grey30",size=1,shape=21)+
  the_theme2+
  labs(x="Temporal autocorrelation (space)",y="Temporal autocorrelation (time)")

p02=ggplot(d_sim%>%mutate(., Dist_desert=as.numeric(cut(.$Dist_desert,breaks=7)))%>%
         dplyr::group_by(., Dist_desert)%>%
         dplyr::summarise(., .groups = "keep",mean_AC=mean(AC_ini_time),sd_AC=sd(AC_ini_time),AC_space_mean=mean(AC_ini_space),AC_space_sd=sd(AC_ini_space)))+
  geom_pointrange(aes(x=AC_space_mean,xmin=AC_space_mean-AC_space_sd,xmax=AC_space_mean+AC_space_sd,y=mean_AC,ymin=mean_AC-sd_AC,ymax=mean_AC+sd_AC),fill="lightblue",color="grey30",size=1,shape=21,width=.1)+
  the_theme2+
  labs(x="Spatial autocorrelation",y="Temporal autocorrelation")

p03=ggplot(d_obs)+
  geom_point(aes(x=moran_I,y=AC_mean),fill="lightblue",color="grey30",size=1,shape=21)+
  geom_smooth(aes(x=moran_I,y=AC_mean),fill="lightblue",color="grey30",method = "lm")+
  the_theme2+
  labs(x="Spatial autocorrelation",y="Temporal autocorrelation")


p1=ggplot(d_sim%>%mutate(., Dist_desert=as.numeric(cut(.$Dist_desert,breaks=7)))%>%
            dplyr::group_by(., Dist_desert)%>%
            dplyr::summarise(., .groups = "keep",mean_AC=mean(AC_ini_time),sd_AC=sd(AC_ini_time)))+
  geom_pointrange(aes(x=Dist_desert,y=mean_AC,ymin=mean_AC-sd_AC,ymax=mean_AC+sd_AC),fill="lightblue",color="grey30",size=1,shape=21,width=.1)+
  the_theme2+
  labs(x="Distance to a desertification point \n (in the model)",y="Temporal autocorrelation")+
  scale_x_continuous(breaks=c(7,1),labels=c("Far","Close"))


p2=ggplot(d_obs)+
  geom_point(aes(x=log(Mean_Dist),y=AC_mean),fill="lightblue",color="grey30",size=1,shape=21)+
  geom_smooth(aes(x=log(Mean_Dist),y=AC_mean),fill="lightblue",color="grey30",method = "lm")+
  the_theme2+
  labs(x="Distance to a desertification point \n (estimated in the data)",y="Temporal autocorrelation")+
  scale_x_continuous(breaks=c(-5,-2),labels=c("Close","Far"))

p3=ggplot(d_sim%>%mutate(., Dist_desert=as.numeric(cut(.$Dist_desert,breaks=7)))%>%
            dplyr::group_by(., Dist_desert)%>%
            dplyr::summarise(., .groups = "keep",mean_AC=mean(SD_ini_time),sd_AC=sd(SD_ini_time)))+
  geom_pointrange(aes(x=Dist_desert,y=mean_AC,ymin=mean_AC-sd_AC,ymax=mean_AC+sd_AC),fill="lightblue",color="grey30",size=1,shape=21,width=.1)+
  the_theme2+
  labs(x="Distance to a desertification point \n (in the model)",y="Average SD")+
  scale_x_continuous(breaks=c(7,1),labels=c("Far","Close"))

p4=ggplot(d_obs)+
  geom_point(aes(x=log(Mean_Dist),y=SD_mean),fill="lightblue",color="grey30",size=1,shape=21)+
  the_theme2+
  labs(x="Distance to a desertification point \n (estimated in the data)",y="Average SD")+
  scale_x_continuous(breaks=c(-5,-2),labels=c("Close","Far"))

p02_title=p02+ggtitle("Model")+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(size = 12, margin = margin(b = -25,l = 150),color = "white"))+
  annotate("rect",xmin=.17,xmax=.25,ymin=.96,ymax=.98,alpha=1,color="black",fill = "black")

p03_title=p03+ggtitle("Data")+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(size = 12, margin = margin(b = -25,l = 195),color = "white"))+
  annotate("rect",xmin=.4,xmax=.55,ymin=.82,ymax=.9,alpha=1,color="black",fill = "black")

p_label1=ggplot(NULL)+geom_node_label(
  data = NULL,
  aes(x = .2, y = 1.2, label = "Empirical data"),
  color = "white",
  fill="black",
  label.size = 1,   
  family = "NewCenturySchoolbook",
  label.padding = unit(.5, "lines"),
  label.r = unit(0.5, "lines"),  
  size = 5             
)+theme_void()

p_label2=ggplot(NULL)+geom_node_label(
  data = NULL,
  aes(x = .2, y = 1.2, label = "Theoretical model"),
  color = "white",
  fill="black",
  label.size = 1,   
  family = "NewCenturySchoolbook",
  label.padding = unit(.5, "lines"),
  label.r = unit(0.5, "lines"),  
  size = 5             
)+theme_void()


p_tot=ggarrange(
  ggarrange(p_label2,p_label1,ncol=2),
  ggarrange(p02,p03,ncol=2,labels = letters[1:2]),
  ggarrange(p1,p2,ncol=2,labels = letters[3:4]),
  nrow=3,heights = c(.15,1,1))

ggsave("./Figures/Model_data_temporal_spatial.pdf",p_tot,width = 7,height = 7)



# Resilience and future projections ----

climatic_data=tibble()
d_obs=read.table("./data/Empirical_data.csv",sep=";")

model_id=1
for (model_clim in list.files("./data/Climate/")){
  
  nc_data_aridity=rast(paste0("./data/Climate/",model_clim))
  dryland_sf = st_as_sf(d_obs, coords = c("Longitude", "Lattitude"), crs = 4326)
  MAT_values = raster::extract(nc_data_aridity[[1]], dryland_sf,method="bilinear")[,-1]
  MAP_values = raster::extract(nc_data_aridity[[12]], dryland_sf,method="bilinear")[,-1]
  
  climatic_data=rbind(climatic_data,tibble(MAP=MAP_values,MAT=MAT_values,Site_ID=paste0(d_obs$Site_ID,"_",d_obs$Image))%>%
                        add_column(., model=model_id))
  model_id=model_id+1
}


#compute mean MAP, MAT
mean_trend=climatic_data%>%
  dplyr::group_by(., Site_ID)%>%
  dplyr::summarise(., .groups = "keep",MAP=mean(MAP),MAT=mean(MAT))


d_obs=dplyr::arrange(d_obs,paste0(Site_ID,"_",Image))
d_obs$MAP_future=mean_trend$MAP
d_obs$MAT_future=mean_trend$MAT


p1=ggplot(d_obs)+
  geom_point(aes(x=log(Mean_Dist),y=MAP_future-MAP_current,fill=MAP_future-MAP_current),color="grey30",size=2,shape=21)+
  the_theme2+
  scale_fill_viridis_c(option = "G",breaks=c(-100,100,300))+
  labs(x="Distance to desertification point",y="Future change in precipitation \n (future - current)")+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  scale_x_continuous(breaks = c(-5,-1),labels=c("Close","Far"))


p2=ggplot(d_obs)+
  geom_point(aes(x=log(Mean_Dist),y=MAT_future-MAT_current,fill=MAT_future-MAT_current),color="grey30",size=2,shape=21)+
  the_theme2+
  scale_fill_viridis_c(option = "F")+
  labs(x="Distance to desertification point",y="Future change in temperature \n (future - current)")+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  scale_x_continuous(breaks = c(-5,-1),labels=c("Close","Far"))




p3=ggplot(d_obs)+
  # geom_rect(data=tibble(xmin=c(-Inf,median(d_obs$AC_mean,na.rm = T)/2,-Inf,median(d_obs$AC_mean,na.rm = T)/2),
  #                       xmax=c(median(d_obs$AC_mean,na.rm = T)/2,Inf,median(d_obs$AC_mean,na.rm = T)/2,Inf),
  #                       ymin=c(-Inf,-Inf,median(d_obs$MAP_future-d_obs$MAP_current,na.rm = T)/2,median(d_obs$MAP_future-d_obs$MAP_current,na.rm = T)/2),
  #                       ymax=c(median(d_obs$MAP_future-d_obs$MAP_current,na.rm = T)/2,median(d_obs$MAP_future-d_obs$MAP_current,na.rm = T)/2,Inf,Inf),
  #                       label=letters[1:4]),
  #           aes(xmin = xmin,xmax=xmax,ymin=ymin,ymax = ymax,fill=label),lwd=1,alpha=.2)+
  geom_point(aes(x=AC_mean,y=MAP_future-MAP_current,color=MAP_future-MAP_current),size=2)+
  the_theme2+
  scale_color_viridis_c(option = "G",breaks=c(-100,100,300))+
  scale_fill_manual(values = c("lightgreen","orange","grey","red"))+
  labs(x="Temporal indicator of resilience",y="Future change in precipitation \n (future - current)")+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  scale_x_continuous(breaks = c(0,.6),labels=c("Low","High"))


p4=ggplot(d_obs)+
  geom_point(aes(x=AC_mean,y=MAT_future-MAT_current,fill=MAT_future-MAT_current),color="grey30",size=2,shape=21)+
  the_theme2+
  scale_fill_viridis_c(option = "F")+
  labs(x="Temporal indicator of resilience",y="Future change in temperature \n (future - current)")+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  scale_x_continuous(breaks = c(0,.6),labels=c("Low","High"))



ggsave("./Figures/Risk_future_clim.pdf",
       ggarrange(ggarrange(p1+theme(legend.title = element_text(size=10),
                                    axis.title = element_text(size=13))+
                             labs(fill="Change in precipitation \n (Future - Current)"),
                           p3+theme(legend.title = element_text(size=10),
                                    axis.title = element_text(size=13),axis.title.y = element_blank())+
                             labs(fill="Change in temperature \n (Future - Current)"),ncol=2,
                           common.legend = T,legend = "bottom",align = "hv"),
                 ggarrange(p2+theme(legend.title = element_text(size=10),
                                    axis.title = element_text(size=13))+labs(fill="Change in precipitation \n (Future - Current)"),
                           p4+theme(legend.title = element_text(size=10),
                                    axis.title = element_text(size=13),axis.title.y = element_blank())+
                             labs(fill="Change in temperature \n (Future - Current)"),ncol=2,common.legend = T,legend = "bottom",align = "hv"),nrow=2),
       width = 7,height = 8)











set.seed(417)
library(plotly)

p1=plot_ly(data=d_obs,x=~log(Mean_Dist), y=~Resistance_month_drought, 
           z=~MAP_current-MAP_future, type="scatter3d", mode="markers", 
           color=~(MAP_future-MAP_current),colors = "RdGy")%>%
  layout(yaxis = list(title = 'Temporal indicator of resilience (autocorrelation)'),
         xaxis = list(title = 'Spatial indicator of resilience (autocorrelation)'),
         legend = list(title=list(text='<b> Future change in temperature </b>')))

p1=plot_ly(data=d_obs,x=~log(Mean_Dist), y=~Resistance_month_drought, 
           z=~MAP_current-MAP_future, type="scatter3d", mode="markers", 
           color=~(MAP_future-MAP_current),colors = "RdGy")%>%
  layout(yaxis = list(title = 'Temporal indicator of resilience (autocorrelation)',showline= T, linewidth=2, linecolor='black', mirror = T, showticklabels = F),
         xaxis = list(title = 'Spatial indicator of resilience (autocorrelation)',showline= T, linewidth=2, linecolor='black', mirror = T, showticklabels = F),
         legend = list(title=list(text='<b> Future change in temperature </b>')))



fig2 <-  plot_ly(data = iris ,x =  ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = 'scatter', mode = 'markers')%>%
  layout(title = 'Manually Specified Labels', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Sepal Length (cm)'), 
         yaxis = list(title = 'Sepal Width (cm)'), legend = list(title=list(text='<b> Species of Iris </b>')))
fig2

library("gg3D")


ggplot(d_obs, aes(x=moran_I, y=AC_mean, z=MAP_future, color=log(MAP_future))) + 
  theme_void() +
  axes_3D() +
  stat_3D()


library("plot3D")

pdf("./Risk_3D_1.pdf",width = 7,height = 7)
par(mfrow=c(1,1))
scatter3D(d_obs$AC_mean,d_obs$moran_I,d_obs$MAP_current-d_obs$MAP_future,cex=2,
          pch = 16,theta = 45, phi =30,col=brewer.pal(n = 8, name = "BrBG"),
          grid=TRUE, box=T, clab = c("Current-Future", "precipitations"),
          xlab = c("\n Temporal indicator resilience \n High resilience            Low resilience" ),
          ylab ="\n Spatial indicator resilience \n High resilience            Low resilience", zlab = "Current-Future precipitations")

dev.off()
pdf("./Risk_3D_2.pdf",width = 7,height = 14)
par(mfrow=c(2,1))

scatter3D(d_obs$Resistance_month_drought,
          log(d_obs$Mean_Dist),d_obs$MAT_current-d_obs$MAT_future,cex=2,
          labels = rownames(d_obs),
          pch = 16,theta = 45, phi =30,col=brewer.pal(n = 8, name = "RdGy"),
          grid=TRUE, box=T, clab = c("Current-Future", "temperature"),
          xlab = c("\n Drought resistance \n Low resistance            High resistance" ),
          ylab ="\n Distance to desertification point \n Close                             Far", zlab = "Current-Future temperature")
scatter3D(d_obs$Recovery_drought,
          log(d_obs$Mean_Dist),d_obs$MAT_current-d_obs$MAT_future,cex=2,
          labels = rownames(d_obs),
          pch = 16,theta = 45, phi =30,col=brewer.pal(n = 8, name = "RdGy"),
          grid=TRUE, box=T, clab = c("Current-Future", "temperature"),
          xlab = c("\n Drought recovery time \n Slow recovery            Fast recovery" ),
          ylab ="\n Distance to desertification point \n Close                             Far", zlab = "Current-Future temperature")
dev.off()


d_obs$AC_mean


d_pair1=d_obs[,c("Mean_Dist","Resistance_month_drought","Recovery_drought","AC_mean","moran_I")]%>%
  mutate(.,Mean_Dist=log(Mean_Dist))

colnames(d_pair1)=c("Dist. to \n desertif. point","Drought resistance","Drought \n recovery time","Temporal autocorr.","Spatial autocorr.")
p_pairs1=ggpairs(
  d_pair1
)+the_theme2

ggsave("./Pairs_resilience.pdf",p_pairs1,width = 7,height = 7)

d_pair1=d_obs[,c("Mean_Dist","Resistance_month_drought","Recovery_drought","AC_mean","moran_I")]%>%
  mutate(.,Mean_Dist=log(Mean_Dist))

colnames(d_pair1)=c("Dist. to \n desertif. point","Drought resistance","Drought \n recovery time","Temporal autocorr.","Spatial autocorr.")
p_pairs1=ggpairs(
  d_pair1
)+the_theme2
p_(pm)
