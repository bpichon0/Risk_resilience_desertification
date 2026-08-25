rm(list=ls())
source("./Functions.R")

dir.create("./Figures",showWarnings = F)



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
  labs(x="Temporal resilience indicator",y="Future change in precipitation \n (future - current)")+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  scale_x_continuous(breaks = c(0,.6),labels=c("Low","High"))


p4=ggplot(d_obs)+
  geom_point(aes(x=AC_mean,y=MAT_future-MAT_current,fill=MAT_future-MAT_current),color="grey30",size=2,shape=21)+
  the_theme2+
  scale_fill_viridis_c(option = "F")+
  labs(x="Temporal resilience indicator",y="Future change in temperature \n (future - current)")+
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










library("plot3D")

pdf("./Risk_3D_1.pdf",width = 7,height = 7)
par(mfrow=c(1,1))

d_obs_fil=dplyr::filter(d_obs,(Field_cover-Image_cover)**2 < 0.01)

scatter3D(d_obs_fil$AC_mean,d_obs_fil$moran_I,d_obs_fil$MAP_current-d_obs_fil$MAP_future,cex=2,
          pch = 16,theta = 45, phi =30,col=brewer.pal(n = 8, name = "BrBG"),
          grid=TRUE, box=T, clab = c("Current-Future", "precipitation"),
          xlab = c("\n Temporal resilience indicator \n High resilience            Low resilience" ),
          ylab ="\n Spatial resilience indicator \n High resilience            Low resilience", zlab = "Current-Future precipitation")

dev.off()
pdf("./Risk_3D_2.pdf",width = 7,height = 14)
par(mfrow=c(2,1))

scatter3D(d_obs_fil$Resistance_month_drought,
          log(d_obs_fil$Mean_Dist),d_obs_fil$MAT_current-d_obs_fil$MAT_future,cex=2,
          labels = rownames(d_obs_fil),
          pch = 16,theta = 45, phi =30,col=brewer.pal(n = 8, name = "RdGy"),
          grid=TRUE, box=T, clab = c("Current-Future", "temperature"),
          xlab = c("\n Drought resistance \n Low resistance            High resistance" ),
          ylab ="\n Distance to desertification point \n Close                             Far", zlab = "Current-Future temperature")
scatter3D(d_obs_fil$Recovery_drought,
          log(d_obs_fil$Mean_Dist),d_obs_fil$MAT_current-d_obs_fil$MAT_future,cex=2,
          labels = rownames(d_obs_fil),
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



p=ggplot(d_obs)+
  geom_point(aes(x=Field_cover,Image_cover,fill=(Field_cover-Image_cover)**2 > 0.01),color="black",shape=21)+
  geom_abline(intercept = 0,slope = 1,color="black",linetype = 1,lwd=2)+
  geom_smooth(aes(x=Field_cover,Image_cover),color="pink",method = "lm",se = F,lwd=2)+
  the_theme2+
  scale_fill_manual(values=c("lightblue","red"))+
  labs(x="Field cover",y="Image cover")+guides(fill="none",linetype="none")
ggsave("./Figures/Performance_Kmeans.pdf",p,width = 5,height = 4)
