require(readxl);require(writexl);require(ggplot2);require(lubridate);require(dplyr)
p_surv<-read_xlsx("data/Florida-panther-survival-data.xlsx",2)
g_surv <- read_xlsx("data/guppy-survival.xlsx",2)
p_pop<-read_xlsx("data/Florida-panther-pop-size-and-heterozygosity_1985-2013.xlsx",2)
g_pop<-read_xlsx("data/guppy-pop-size-and-heterozygosity.xlsx",2)

x.axis<-data.frame(x=1985:1994,labels="")
labeled<-seq(1985,1994,3)
x.axis$labels[match(labeled,x.axis$x)]<-labeled

showAll<-function(tbl_df){
  N=nrow(tbl_df)
  print(tbl_df,n=N)
}

scale_x_cust=function(limits,breaks=1,labels=NULL){
  if(missing(limits)){stop("Supply limits")}
  if(breaks!=1){
    if(length(breaks)==1){
      breaks<-seq(limits[1],limits[2],breaks)
      }
    }else{
      breaks<-limits[1]:limits[2]
      }

  x_axis <- data.frame(x=breaks,labs=ifelse(is.null(labels),breaks,""))
  if(!is.null(labels)){
  x_axis$labs[match(labels,x_axis$x)] <- labels
  }

  ggplot2::scale_x_continuous(limits=limits,labels=x_axis$labs,breaks=breaks)
}

# Panther plots -----------------------------------------------------------
# 1985-1994
ggplot(p_pop,aes(Year,MinPopEst))+geom_point()+ggGalactic(font.cex=1.5)+ylim(0,150)+theme(axis.ticks.length.x = unit(5,"pt"))+ylab("Minimum Population Est.")+scale_x_cust(limits=c(1985,1994),labels=seq(1985,1994,3))
ggsave("assets/panther-population_before.png",width=5,height=3.5)


#1985-2013
#Full X-axis, missing after genetic rescue
p_pop %>% subset(.,Year<1996) %>%
ggplot(.,aes(Year,MinPopEst))+geom_vline(xintercept=1995,col=gpColors("hy"),linetype=2)+
  geom_point()+ggGalactic(font.cex=1.5)+theme(axis.ticks.length.x = unit(5,"pt"))+ylab("Minimum Population Est.")+
  scale_x_cust(limits=c(1985,2013),labels=seq(1985,2013,5))+
  scale_y_continuous(limits=c(0,150))+
  annotate("rect",xmin=1995.15,xmax=2009,ymin=5,ymax=15,fill="white",alpha=.7)+
  annotate("text",x=1995.35,y=10,label="Genetic Rescue: Texas pumas introduced",hjust=0,col=gpColors("hy"),size=8)
ggsave("assets/panther-population_before-after.png",width=5,height=3.5)

#Full X-axis, including after genetic rescue
ggplot(p_pop,aes(Year,MinPopEst))+geom_vline(xintercept=1995,col=gpColors("hy"),linetype=2)+
  geom_point()+ggGalactic(font.cex=1.5)+theme(axis.ticks.length.x = unit(5,"pt"))+ylab("Minimum Population Est.")+
  scale_x_cust(limits=c(1985,2013),labels=seq(1985,2013,5))+
  scale_y_continuous(limits=c(0,150))+
  annotate("rect",xmin=1995.15,xmax=2009,ymin=5,ymax=15,fill="white",alpha=.7)+
  annotate("text",x=1995.35,y=10,label="Genetic Rescue: Texas pumas introduced",hjust=0,col=gpColors("hy"),size=8)
ggsave("assets/panther-population_before+after.png",width=5,height=3.5)



# Guppy plots -------------------------------------------------------------
g_pop2<-g_pop %>% mutate(month2=as.Date(Month))
#pre intervention
ggplot(g_pop2,aes(month2,PopSize))+geom_point()+ggGalactic(font.cex=1.5)+ylim(0,50)+theme(axis.ticks.length.x = unit(5,"pt"))+ylab("Population Size")+scale_x_date(date_labels="%b",limits=c(g_pop2$month2[1],g_pop2$month2[11]))+xlab("2009")
ggsave("assets/guppy-population_before.png",width=5,height=3.5)
g_pop2 %>% showAll()

## after intervention
# Full X-axis, Missing post rescue data
g_pop2 %>% subset(.,Stream=="Taylor"&month2<as_date("2009-03-31")) %>%
ggplot(.,aes(month2,PopSize))+
  geom_vline(xintercept=as_date("2009-3-15"),col=gpColors("hy"),linetype=2)+
  geom_point()+
  ggGalactic(font.cex=1.5)+
  theme(axis.ticks.length.x = unit(5,"pt"))+
  ylab("Population Size")+
  scale_x_date(date_labels="%Y",limits=as_date(c("2009-01-01","2011-06-01")))+
  xlab("Year")+ylim(0,1150)+
  annotate("rect",xmin=as_date("2009-4-5")-1,xmax=as_date("2010-6-1")-1,ymin=950,ymax=1150,fill="white",alpha=.7)+
  annotate("text",x=as_date("2009-4-5"),y=1050,label="Genetic Rescue: New guppies introduced",hjust=0,col=gpColors("hy"),size=8)
ggsave("assets/guppy-population_before-after.png",width=5,height=3.5)

# Full X-axis, Missing post rescue data
g_pop2 %>% subset(.,Stream=="Taylor") %>%
  ggplot(.,aes(month2,PopSize))+
  geom_vline(xintercept=as_date("2009-3-15"),col=gpColors("hy"),linetype=2)+
  geom_point()+ggGalactic(font.cex=1.5)+theme(axis.ticks.length.x = unit(5,"pt"))+
  ylab("Population Size")+
  scale_x_date(date_labels="%Y")+xlab("Year")+ylim(0,1150)+
  annotate("rect",xmin=as_date("2009-4-5")-1,xmax=as_date("2010-6-1")-1,ymin=950,ymax=1150,fill="white",alpha=.7)+
  annotate("text",x=as_date("2009-4-5"),y=1050,label="Genetic Rescue: New guppies introduced",hjust=0,col=gpColors("hy"),size=8)
ggsave("assets/guppy-population_before+after.png",width=5,height=3.5)
