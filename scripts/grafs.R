require(pacman)
p_load(readxl,ggplot2,ggh4x,lubridate,dplyr,gganimate,galacticPubs,galacticEdTools,av,cowplot)
p_surv<-read_xlsx("data/Florida-panther-survival-data.xlsx",2)
g_surv <- read_xlsx("data/guppy-survival.xlsx",2)
p_pop<-read_xlsx("data/Florida-panther-pop-size-and-heterozygosity_1985-2013.xlsx",2)
g_pop<-read_xlsx("data/guppy-pop-size-and-heterozygosity.xlsx",2)

# x.axis<-data.frame(x=1985:1994,labels="")
# labeled<-seq(1985,1994,3)
# x.axis$labels[match(labeled,x.axis$x)]<-labeled

showAll<-function(tbl_df){
  N=nrow(tbl_df)
  print(tbl_df,n=N)
}

#Don't need this code anymore ggh4x has a much more elegant solution!

# scale_x_cust=function(limits,breaks=1,labels=NULL,...){
#   if(missing(limits)){stop("Supply limits")}
#   if(breaks!=1){
#     if(length(breaks)==1){
#       breaks<-seq(limits[1],limits[2],breaks)
#       }
#     }else{
#       breaks<-limits[1]:limits[2]
#       }
#
#   x_axis <- data.frame(x=breaks,labs=ifelse(is.null(labels),breaks,""))
#   if(!is.null(labels)){
#   x_axis$labs[match(labels,x_axis$x)] <- labels
#   }
#
#   ggplot2::scale_x_continuous(limits=limits,labels=x_axis$labs,breaks=breaks,...)
# }

# Panther plots -----------------------------------------------------------

pTitle <- "Minimum FL Panther Population Estimate"
pYlab <- "Panthers"

#Define custom theme for all these plots
rescue_theme<-theme_galactic(text.cex=1.4,grid.wt.min = 0.2,grid.wt.maj = 1)

# 1985-1994
ggplot(p_pop, aes(Year, MinPopEst)) +
  geom_point(size = 2) +
  theme_galactic(
    text.cex = c(1, 1.7, 2, 1.7),
    grid.wt.min = 0.2,
    grid.wt.maj = 1,
    pad.outer = c(5, 5, 5, 5),
    pad.title = 0
  ) +
  ylim(0, 50) + theme(
    axis.ticks.length.x = unit(5, "pt"),
    plot.background = element_rect(fill = "transparent",colour="transparent"),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    )
  ) + ylab(pYlab) + ggtitle(pTitle) +
  #add minor ticks (who'd've thought this would be so complicated?!)
  scale_x_continuous(
    limits = c(1985, 1994),
    minor_breaks = 1985:1994,
    breaks = seq(1985, 1994, 1)
  ) +
  #theme(axis.ticks.length.x=unit(0.5,"cm"),plot.title=element_text(face="plain",size=30))+
  ggtitle("")
ggsave("assets/panther-population_before.png",width=5,height=3.5,bg="transparent")




#1985-2013
#Full X-axis, missing after genetic rescue
p_pop %>% subset(.,Year<1996) %>%
ggplot(.,aes(Year,MinPopEst))+geom_vline(xintercept=1995,col=gpColors("hy"),linetype=2)+
  geom_point(size=2.5)+
  rescue_theme+
  ylab("Minimum Population Est.")+
  scale_y_continuous(limits=c(0,150))+
  # annotate("rect",xmin=1995.15,xmax=2009,ymin=5,ymax=15,fill="white",alpha=.7)+
  annotate("text",x=1995.35,y=10,label="New pumas introduced",hjust=0,col=gpColors("hy"),size=8)+
  #add minor ticks (who'd've thought this would be so complicated?!)
  scale_x_continuous(limits=c(1985,2013),minor_breaks=seq(1985,2013,1))+
  # theme(axis.ticks.length.x=unit(0.5,"cm"),ggh4x.axis.ticks.length.minor = rel(0.3))+
  ggtitle("Genetic Rescue of Florida Panthers")
ggsave("assets/panther-population_before-after.png",width=5,height=3.5)

#Full X-axis, including after genetic rescue
G_ppop <- ggplot(p_pop,aes(Year,MinPopEst))+geom_vline(xintercept=1995,col=gpColors("hy"),linetype=2)+
  geom_point()+
  rescue_theme+
  ylab("Minimum Population Est.")+
   scale_x_continuous(limits=c(1985,2013),minor_breaks=seq(1985,2013,1))+
  scale_y_continuous(limits=c(0,150))+
  # annotate("rect",xmin=1995.15,xmax=2009,ymin=5,ymax=15,fill="white",alpha=.7)+
  annotate("text",x=1995.35,y=10,label="New individuals introduced",hjust=0,col=gpColors("hy"),size=8)+
  ggtitle("Genetic Rescue of Florida Panthers")
G_ppop
ggsave("assets/panther-population_before+after.png",G_ppop,width=5,height=3.5)

G_ppop+theme(plot.background=element_rect(fill="transparent",colour=NA))+ggtitle("")
ggsave("assets/panther-population_before+after_transpBG.png",width=5,height=3.5,bg="transparent")

#Make version with blank axis labels
G_ppop+blank_labs()
ggsave("assets/panther-population_before+after_BLANK-LABS.png",width=5,height=3.5)





# Panther Heterozygosity ~ Year (all data)
G_phet <- p_pop%>%
  ggplot(.,aes(Year,Heterozygosity))+
  geom_vline(xintercept=1995,col=gpColors("hy"),linetype=2)+
  geom_point()+
  rescue_theme+
  theme(axis.ticks.length.x = unit(5,"pt"))+
  ylab(expression(atop(bold("Genetic Variation"),"(Heterozygosity)")))+
  scale_x_continuous(limits=c(1986,2007),minor_breaks=1986:2007)+
  xlab("Year")+
  ylim(0,1)+
  # annotate("rect",xmin=1995.15,xmax=2005.8,ymin=.04,ymax=.06,fill="white",alpha=.7)+
  annotate("text",x=1995.35,y=.05,label="New individuals introduced",hjust=0,col=gpColors("hy"),size=7)+
  ggtitle("Genetic Rescue of Florida Panthers")
G_phet
ggsave("assets/panther-heterozygosity_before+after.png",G_phet,width=5,height=3.5)

#make blank version
G_phet+blank_labs()
ggsave("assets/panther-heterozygosity_before+after_BLANK-LABS.png",width=5,height=3.5)

# Guppy plots -------------------------------------------------------------
g_pop2<-g_pop %>% mutate(month2=as.Date(Month))
#pre intervention
Gpop<-ggplot(g_pop2,aes(month2,PopSize))+geom_point()+
  rescue_theme+
  ylim(0,50)+
  ylab("Population Size")+
  scale_x_date(date_labels="%b",limits=c(g_pop2$month2[1],g_pop2$month2[11]),minor_breaks="1 month")+
  xlab("2009")+
  guides(x="axis_minor")+theme(axis.ticks.length.x=unit(0.5,"cm"),ggh4x.axis.ticks.length.minor = rel(0.3))
Gpop
ggsave("assets/guppy-population_before.png",Gpopwidth=5,height=3.5)
g_pop2 %>% showAll()

## after intervention
# Full X-axis, Missing post rescue data
g_rescue<-data.frame(x=c(as_date("2009-4-5"),as_date("2009-4-5")),y=c(950,1050),use=c("rect","text"))
g_rescue_rect<-data.frame(xmin=c(as_date("2009-4-5"),as_date("2009-4-5")),y=c(950,1050))
g_pop2$Year<-factor(sprintf("%.4s",g_pop2$Month))

#Define date formatter function
dte_formatter <- function(x) {
  #formatter for axis labels: J12, F12, M12, etc...
  mth <- paste0(substr(format(x, "%b"),1,3),"-")
  yr <- format(x, "%y")
  paste0(mth, yr)
}

g_rescue1<-g_pop2 %>% subset(.,Stream=="Caigual"&month2<=as_date("2009-4-1")) %>%
ggplot(.,aes(month2,PopSize))+
  geom_vline(xintercept=as_date("2009-3-15"),col=gpColors("hy"),linetype=2)+
  geom_point()+
  rescue_theme+
  scale_x_date(date_breaks="3 months",labels=dte_formatter,date_minor_breaks="1 month",limits=as_date(c("2009-1-1","2011-6-1"))) +
    theme(axis.text.x = element_text(angle=90, vjust=.5))+
  ylab("Population Size")+
  xlab("Month")+ylim(0,1150)+
  # geom_rect(data=g_rescue_rect,aes(x,y),fill="blue",alpha=.7)+
  # geom_label(data=g_rescue,aes(x,y),label="New individuals introduced",hjust=0,col=gpColors("hy"),size=8)+
  guides(x="axis_minor")+
  ggtitle("Genetic Rescue of Trinidadian Guppies")

g_rescue1
ggsave("assets/guppy-population_before-after.png",width=5,height=3.5)




# Guppy PopSize ~ Year (all data)
g_pop2_T<- g_pop2 %>% subset(.,Stream=="Caigual")
g_rescue2<-g_pop2_T %>%
  ggplot(.,aes(month2,PopSize))+
  geom_vline(xintercept=as_date("2009-3-15"),col=gpColors("hy"),linetype=2)+
  geom_point()+
  rescue_theme+
  scale_x_date(date_breaks="3 months",labels=dte_formatter,date_minor_breaks="1 month") +
    theme(axis.text.x = element_text(angle=90, vjust=.5))+
  ylab("Population Size")+
  ylim(0,1150)+
  xlab("Month")+
  # annotate("rect",xmin=as_date("2009-4-5")-1,xmax=as_date("2010-3-1")-1,ymin=950,ymax=1150,fill="white",alpha=.7)+
  annotate("text",x=as_date("2009-4-5"),y=1050,label="New individuals introduced",hjust=0,col=gpColors("hy"),size=8)+
  guides(x="axis_minor")+theme(axis.ticks.length.x=unit(0.5,"cm"),ggh4x.axis.ticks.length.minor = rel(0.3))+
  ggtitle("Genetic Rescue of Trinidadian Guppies")#+labs(caption="Population: Caigual")
g_rescue2
ggsave("assets/guppy-population_before+after.png",width=5,height=3.5)
#Make a blank version
g_rescue2+blank_labs()
ggsave("assets/guppy-population_before+after_BLANK-LABS.png",width=5,height=3.5)


# Guppy Heterozygosity ~ Year (all data)
g_rescue_het <- g_pop2_T %>%
   subset(.,complete.cases(Heterozygosity)) %>%
  ggplot(.,aes(month2,Heterozygosity))+
  geom_vline(xintercept=as_date("2009-3-15"),col=gpColors("hy"),linetype=2)+
  geom_point()+theme(axis.ticks.length.x = unit(5,"pt"))+
  ylab(expression(atop(bold("Genetic Variation"),"(Heterozygosity)")))+
  #galactic styling
  rescue_theme+
  scale_x_date(date_breaks="2 months",labels=dte_formatter,date_minor_breaks="1 month") +
    theme(axis.text.x = element_text(angle=90, vjust=.5),
          axis.ticks.length.x = unit(5,"pt"))+
  xlab("Month")+ylim(0,1)+
  # annotate("rect",xmin=as_date("2009-4-5")-1,xmax=as.Date("2009-12-11")-1,ymin=.01,ymax=.12,fill="white",alpha=.7)+
  annotate("text",x=as_date("2009-3-25"),y=.05,label="New individuals introduced",hjust=0,col=gpColors("hy"),size=7)+
  guides(x="axis_minor")+theme(axis.ticks.length.x=unit(0.5,"cm"),ggh4x.axis.ticks.length.minor = rel(0.3))+
  ggtitle("Genetic Rescue of Trinidadian Guppies")
g_rescue_het
 ggsave("assets/guppy-heterozygosity_before+after.png",width=5,height=3.5)

#Make a blank version
g_rescue_het+blank_labs()
 ggsave("assets/guppy-heterozygosity_before+after_BLANK-LABS.png",width=5,height=3.5)
 #
# # Guppy PopSize ~ Heterozygosity (all data)
# # This is not necessarily a graph we'll show them...but shows that heterozygosity doesn't keep increasing population size; asymptote
# # heterozygosity at intervention date is 0.46
# incpt<-0.46
# g_pop2_T %>%
#   ggplot(.,aes(Heterozygosity,PopSize))+
#   geom_vline(xintercept=incpt,col=gpColors("hy"),linetype=2)+
#   geom_point()+theme_galactic(text.cex=1.5)+theme(axis.ticks.length.x = unit(5,"pt"))+
#   ylab("Population Size")+
#   xlab("Genetic Variation (Heterozygosity)")+ylim(0,1150)+
#   annotate("rect",xmin=incpt-.02,xmax=max(g_pop2_T$Heterozygosity,na.rm=T)-.01,ymin=950,ymax=1150,fill="white",alpha=.7)+
#   annotate("text",x=incpt,y=1050,label="New guppies introduced",hjust=0,col=gpColors("hy"),size=8)+
#   guides(x="axis_minor")+theme(axis.ticks.length.x=unit(0.5,"cm"),ggh4x.axis.ticks.length.minor = rel(0.3))+
#   ggtitle("Genetic Rescue of Trinidadian Guppies")
# # ggsave("assets/guppy-population_before+after.png",width=5,height=3.5)



# panther population crash animation --------------------------------------

dNt<-function(r,N){r*N*(1-N)}

Nt <- function(r, N0, t, k) {
  out<-list(N=N0,t=1)
  for (i in 1:(t-1)) {
    out$t[i + 1]<-i+1
    # population at next time step is population at current time + pop growth
    out$N[i + 1] <- out$N[i] + r*out$N[i]  *  (1- (out$N[i])/k)
  }
  as_tibble(out)
}





simulateDecline<-function(N_start,N_end,t_start,t_end,t_intvl=1,exponent=.014,noise_mag=1,noise_var=1,asymp=0){

  out<-list(t=t_start,N=N_start)
  t_vec<-seq(t_start,t_end,t_intvl)
  delta<-(N_start-N_end)/length(t_vec)
  noise_mag2<-delta*noise_mag
  noise_var2<-delta*noise_var
  exponent_relative<-10^-(nchar(round((N_start-N_end),-2))-2 )*exponent
  for(i in 1:(length(t_vec)-1)){
    out$t[i+1]<-t_vec[i+1]
    out$N[i+1]<-out$N[i]*exp(-exponent_relative*i)-abs(jitter(sample(1:noise_mag2,1),sample(1:noise_var2,1)))
    print(i)
    if(out$N[i+1]<N_end){out$N[i+1] <- max(rnorm(1,mean=N_end,sd=noise/10),asymp)}
  }
  out$N[length(out$N)]<-N_end
  as_tibble(out)
  }
set.seed(23)
pop_crash_1900<-simulateDecline(50e3,1111,1500,1900,t_intvl=5,exponent=.000005,asymp=1000,noise_mag=.4,noise_var=1.75)

pop_crash_1985<-simulateDecline(1111,24,1900,1985,t_intvl=5,exponent=5,asymp=20,noise_mag=.8,noise_var=1.75)

pop_crash_2013<-p_pop %>% select(Year,MinPopEst) %>% rename(t=Year,N=MinPopEst) %>% mutate(dataType="real data")

pop_crash0<-bind_rows(pop_crash_1900,pop_crash_1985)
pop_crash0$dataType<-"simulated"
pop_crash <- bind_rows(pop_crash0,pop_crash_2013)
polygon_bottom<-tibble(t=pop_crash$t,N=rep(0,nrow(pop_crash)),dataType="simulated")
pop_crash_polygon<-bind_rows(polygon_bottom,pop_crash) %>% arrange(N,t)


crash_animation<-
  ggplot(pop_crash,aes(t,N,group=t)) +
  geom_polygon(inherit.aes=F,data=pop_crash_polygon,aes(t,N),fill="#5e5e5e")+
  geom_line(size=1.5,alpha=.85,aes(group=dataType,col=dataType))+
  theme_galactic(text.cex=1.5,grid.wt.min = 0,grid.wt.maj = .2)+theme(axis.text.y=element_text(margin=margin(l=25)),axis.text.x=element_text(margin=margin(b=25)))+
  xlab("Year")+ylab("Panther Population")+scale_color_manual(values=c("#cb1f8e","#363636"))+theme(legend.position=c(.8,.875),legend.title=element_blank())+guides(color=guide_legend(override.aes=list(size=3)))+
transition_reveal(t)

animate(crash_animation,rewind=F,renderer=av_renderer(file="assets/panther_pop_crash.mp4"),width=1290,height=800,units="px",nframes=160,fps=20)

