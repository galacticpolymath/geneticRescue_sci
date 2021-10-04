require(magick);require(dplyr);require(ggplot2);require(cowplot);require(gganimate)
gup0<-image_read("assets/guppy.png")
gup<-image_scale(gup0,"50x")

count_breaks<-c(1:10,seq(20,90,10),seq(100,1000,100))

make_count_anim_df<-function(count_breaks,n_cols=10){
  # n_cols
  n_states<-length(count_breaks)
  if(length(n_cols)==1){n_cols=rep(n_cols,n_states)}

  L<-lapply(1:n_states,function(i){
    n_i<-count_breaks[i]
    n_cols<-n_cols[i]
    x <- sapply(1:n_i, function(ii_x) {ifelse(ii_x%%n_cols==0,n_cols,ii_x%%n_cols)})
    y <- sapply(1:n_i, function(ii_y) {ifelse(ii_y%%n_cols==0,ii_y%/%n_cols,ii_y%/%n_cols+1)})
    # y-min because y axis is flipped to go from top to bottom
    tibble(x=x,y=y,x.max=max(x),y.min=min(y),y.max=max(y),N=n_i,group=i)
  })
  df<-do.call(rbind,L)
  lab_rows<-sapply(unique(df$N),function(x) match(x,df$N)[1])
  df$label=df$labX=df$labY=df$labYoffset=NA
  df$label[lab_rows]<-paste0("count = ",format(df$N[lab_rows],big.mark=","))
  df$labX[lab_rows]=df$x.max[lab_rows]#sapply(lab_rows,function(x) {unlist(ifelse(df[x,"N"]<10,(df[x,"x.max"]+.1),(df[x,"x.max"]+df[x,"x.max"]/10)))})
  df$labY[lab_rows]=unlist(1-(df[lab_rows,"y.max"]/10))
  #create what will be an invisible label on the otherside to balance the scale
  df$labYoffset[lab_rows]=unlist(df[lab_rows,"y.max"]+(df[lab_rows,"y.max"]/10))
  #Autoscale pt size
  # Test autoscale function
   d<-data.frame(x=1:50,y=5*exp(-.15*(1:50)+1.5)+2.5)
   ggplot(d,aes(x=x,y=y,size=y))+geom_point()+scale_size_continuous(range=range(d$y))
  #
  df$pts<-10*exp(-.15*df$N+1.5)+1.5
    #old step function
    #ifelse(df$N<400,5*exp(-.25*df$N+1.5),.05*exp(-.0009*df$N)+.02)
  df

}

df<-make_count_anim_df(c(1:10,seq(20,90,10),seq(100,1000,100),seq(2000,10e3,1e3)),  n_cols=c(rep(10,28),seq(20,100,10)))

state_len<-c(1,rep(.2,length(unique(df$group))-2),1)
##########################3
 #Text counter at top
 g_counter<-df%>% ggplot(.,aes(x=.5,y=.5,group=group))+
   lims(x=c(-0.5,1.5),y=c(0,1))+
   geom_text(x=.5,y=0,aes(label=label,hjust="inward",vjust="inward"),size=30)  +
  # geom_text(aes(x=labX,y=labYoffset,label=label,vjust="inward"),color="white")+
  galacticEdTools::theme_galactic(base.theme="void",grid.col = "transparent",border.col="transparent",axis.text.col="transparent")+
  labs(x="",y="")   +
  ggplot2::theme(plot.margin=margin(10,10,0,10,unit="pt"))+
  gganimate::transition_states(group,state_length=state_len)

 a_top<-gganimate::animate(g_counter,start_pause=20,duration=10,
                                        fps=20,end_pause=30,
                                        #width=1290,height=200,
                                        renderer=magick_renderer(loop=T))

#########################
# MAIN GRAPH
 g<-df%>% subset(N<100) %>% ggplot(.,aes(x=x,y=y,group=group,size=pts))+
    geom_point(show.legend = F)+
    scale_y_reverse(expand=expansion(mult=.1))+#expand=expansion(0)
    scale_x_continuous(expand=expansion(mult=.1))+
    scale_size_continuous(range=range(df$pts))+
    #geom_text(aes(x=labX,y=labY,label=label,vjust="inward"))+
    # geom_text(aes(x=labX,y=labYoffset,label=label,vjust="inward"),color="white")+
    theme_void()+
    theme(plot.margin=margin(0,10,10,10,unit="pt"))+
    gganimate::transition_states(group,state_length=state_len,transition_length=1)+
    gganimate::view_follow()
 #animate the graph
 a_bottom<-gganimate::animate(g,start_pause=20,duration=10,fps=20,end_pause=30)
#,width=1290,height=880

 nframes<-length(attributes(a_bottom)$frame_vars$frame)
 message("Combining text and graph animations")
 a_comb<-pbapply::pblapply(1:nframes,function(i){
   magick::image_append(c(a_top[i],a_bottom[i]),stack=T)
 })
 out<-do.call(c,a_comb)

 #output mp4
 gganimate::anim_save("assets/10k_guppies.gif",out)
 #gganimate::shadow_mark(past = TRUE, future = F, colour = 'grey') +



