require(ggplot2);require(ggmap);require(dplyr);require(ggrepel);require(galacticPubs)
deaths<-read.csv("data/Florida_Panther_Mortality.csv") %>% dplyr::filter(.,!CAUSE%in% c("Illegally shot (not fatal)","Capture related"))
deaths$cause<-recode(deaths$CAUSE,`Vehicular trauma`="Vehicle",ISA="Other",Unknown="Other",`Infectious disease - Other`="Disease",`Infectious disease - Pseudorabies`="Disease",` `= "Other", Predation="Other",`Infectious disease - FeLV`="Disease",`Degenerative diseases`="Disease")
deaths$id<-1:nrow(deaths)
range(deaths$X,na.rm = T)
range(deaths$Y,na.rm = T)
quantile(deaths$Y,na.rm=T)

#these are the lat/lon view limits for the overall map
b<-data.frame(left=-82,right=-80.5,bottom=25.9,top=26.95)


#How much to inset quadrant labels
k<-0.6
#divide overall map into quadrants
regions<-data.frame(  quad=c("  I","II","III","IV"),
                      region=1:4,
                      lab_x=c(b$right,b$left,b$left,b$right),
                      lab_y=c(b$top,b$top,b$bottom,b$bottom),
                      lab_hjust=c(1+k,0-k,0-k,1+k),
                      lab_vjust=c(1+k,1+k,0-k,0-k),
                      xmin=c(mean(c(b$left,b$right)),b$left,b$left,mean(c(b$left,b$right))),
                      xmax=c(b$right,mean(c(b$left,b$right)),mean(c(b$left,b$right)),b$right),
                      ymin=c(mean(c(b$bottom,b$top)),mean(c(b$bottom,b$top)),b$bottom,b$bottom),
                      ymax=c(b$top,b$top,mean(c(b$bottom,b$top)),mean(c(b$bottom,b$top)))
                      )
#filter data to match map
deaths2 <- deaths %>% dplyr::filter(X>=b$left& X<=b$right & Y>=b$bottom & Y<=b$top)
#Add Region column
deaths2$region<-sapply(1:length(deaths2$X),function(i){
                  x<-deaths2$X[i]
                  y <- deaths2$Y[i]
                  indx<-which(x>=regions$xmin & x<=regions$xmax &
                          y>=regions$ymin & y<=regions$ymax )
                  regions$region[indx]
                  })

#Sample Size:
nrow(deaths2)
table(deaths2$cause)

#create subset for labeling and tabulation
head(deaths2)
d_17_20<-subset(deaths2,YEAR>=2017&YEAR<2021&cause=="Vehicle")
nrow(d_17_20)
#reassign ID for subset that will be tabulated for students
d_17_20$id
d_17_20$id<-1:nrow(d_17_20)
d_17_20$id
#Get map
mapData<-get_stamenmap(unlist(b),zoom=10,"toner")

g<-ggmap(mapData)+
  geom_point(data=deaths2,aes(x=X,y=Y,shape=cause))+
  geom_rect(data=regions,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),linetype="solid",color="gray50",fill="transparent",inherit.aes=F)+
  geom_text(data=regions,aes(x=lab_x,y=lab_y,label=quad,hjust=lab_hjust,vjust=lab_vjust),family="sans",fontface=1,color="gray30",size=12)+
  galacticEdTools::theme_galactic()+
  theme(panel.border=element_rect(color="#363636",fill="transparent"),axis.ticks = element_line(colour="#363636"),plot.caption=element_text(size=15))+
  labs(x="Longitude",y="Latitude",title="Florida Panther Deaths, All Regions (1972-2020)",caption="SOURCE: FL Fish & Wildlife Conservation Commission")+
  scale_shape_manual(values=c(8,10,5,4),name="Cause of Death")
g

gpsave("collisions/deaths_all-causes_1972-2020.png",width=5,height=3.5)

## Now create function to make individual region maps
map_region<-function(reg,ttl,zoom=13){
  #get map data
  b2<-data.frame(left=regions$xmin[reg],right=regions$xmax[reg],bottom=regions$ymin[reg],top=regions$ymax[reg])
  mapdat<-get_stamenmap(unlist(b2),zoom=zoom,maptype = "toner")
  # mapdat<-get_googlemap(c(mean(c(b2$left,b2$right)),mean(c(b2$top,b2$bottom))),zoom=zoom,maptype="roadmap",color="bw",
  #                        style = c(feature = "all", element = "labels"))
  #
  df.1<-subset(deaths2,deaths2$cause=="Vehicle"&deaths2$region==reg)
  g<-ggmap(mapdat)+
  geom_point(data=df.1,aes(x=X,y=Y,shape=cause))+
  galacticEdTools::theme_galactic(text.cex = 1.4)+
  theme(panel.border=element_rect(color="#363636",fill="transparent"),axis.ticks = element_line(colour="#363636"),
        plot.caption=element_text(lineheight=0.4),legend.margin=margin(0,0,0,0))+
  labs(x="Longitude",y="Latitude",title=ttl,caption="Deaths since 2017 are labeled with ID numbers\nSOURCE: FL Fish & Wildlife Conservation Commission")+
  scale_shape_manual(values=c(4),name="Cause of Death")+
    ggrepel::geom_label_repel(data=d_17_20,aes(x=X,y=Y,label=id),seed = 30,max.time=2,label.padding = .1 ,box.padding=.5,direction="both",max.overlaps=30, fontface=2, size=8,col="gray40")
g

}

(m1<-map_region(1,"FL Panther Vehicle Deaths: Quadrant I",zoom=10))
gpsave("collisions/deaths_vehicles_Region1_1972-2020.png",plot=m1,width=5,height=3.5,dpi = 400)

 m2<-map_region(2,"FL Panther Vehicle Deaths: Quadrant II",zoom=10)
gpsave("collisions/deaths_vehicles_Region2_1972-2020.png",plot=m2,width=5,height=3.5,dpi = 400)

m3<-map_region(3,"FL Panther Vehicle Deaths: Quadrant III",zoom=10)
gpsave("collisions/deaths_vehicles_Region3_1972-2020.png",plot=m3,width=5,height=3.5,dpi = 400)

m4<-map_region(4,"FL Panther Vehicle Deaths: Quadrant IV",zoom=10)
gpsave("collisions/deaths_vehicles_Region4_1972-2020.png",plot=m4,width=5,height=3.5,dpi = 400)

 #Output subset of data

write.csv(d_17_20,"data/collisions_2017-2020.csv")
