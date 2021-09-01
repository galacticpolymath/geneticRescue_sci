require(pacman)
p_load(ggmap)

#bounds for map
bbox<-c(left=-81.75,right=-81.25,bottom=28.25,top=28.65)

#Save files for multiple zoom levels
for(z in 11:14){
mapData<-get_stamenmap(bbox,zoom=z,"toner-background")
g<-ggmap(mapData)+xlab("")+ylab("")+theme(axis.text=element_blank(),axis.ticks=element_blank())
ggsave(paste0("assets/Orlando_outskirts_z=",z,".png"),g)
}

#Attribution: Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
