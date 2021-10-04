require(magick);require(dplyr);require(ggplot2);require(cowplot);require(gganimate)
require(showtext);require(ggimage)
font_add_google("Montserrat")
showtext_auto()

make_count_anim_df<-function(count_breaks,n_cols=10){
  # n_cols
  n_states<-length(count_breaks)
  if(length(n_cols)==1){n_cols=rep(n_cols,n_states)}

  L<-lapply(1:n_states,function(i){
    n_i<-count_breaks[i]
    n_cols<-n_cols[i]
    #special arrangement for counts less than or equal to 10
    if(n_i<=10){
    x<-switch(n_i,`1`=c(1),`2`=c(1,1),`3`=c(1,1,1),`4`=c(1,2,1,2),`5`=c(1,2,1,2,1.5),
              `6`=c(1:3,1:3),`7`=c(1:3,1:3,2),`8`=c(1:4,1:4),`9`=c(1:4,1:4,2.5),`10`=c(1:5,1:5))
    y<-switch(n_i,`1`=1,`2`=c(1,2),`3`=c(1,2,3),`4`=c(1,1,2,2),`5`=c(1,1,2,2,1.5),
              `6`=c(1,1,1,2,2,2),`7`=c(1,1,1,2,2,2,1.5),`8`=c(1,1,1,1,2,2,2,2),`9`=c(1,1,1,1,2,2,2,2,1.5),
              `10`=c(rep(1,5),rep(2,5)))
    #scalable arrangement for higher numbers
    }else{
    x <- sapply(1:n_i, function(ii_x) {ifelse(ii_x%%n_cols==0,n_cols,ii_x%%n_cols)})
    y <- sapply(1:n_i, function(ii_y) {ifelse(ii_y%%n_cols==0,ii_y%/%n_cols,ii_y%/%n_cols+1)})
    }
    # y-min because y axis is flipped to go from top to bottom
    tibble(x=x,y=y,x.max=max(x),y.min=min(y),y.max=max(y),N=n_i,group=format(N,big.mark=","))
  })
  df<-do.call(rbind,L)

  #AUTOSCALE pt and Image size
  # Test autoscale function
  #1:10,20,30,40,50,60,70,80,90,100,
  steps=c(200,300,400,500,600,700,800,900,1000,10000)
  d<-data.frame(x=steps,y=3*exp(-.005*steps+1.5)+.5)
   ggplot(d,aes(x=x,y=y,size=y))+geom_point()+scale_size_continuous(range=range(d$y))+scale_x_log10()
  #scale of points (for large numbers)
  df$pt.size<-8*exp(-.005*df$N+1.5)+2
  #scale of images (for smaller numbers (under 200))
  df$img.size<-5*exp(-.35*df$N+1.5)+1.15
    #old step function
    #ifelse(df$N<400,5*exp(-.25*df$N+1.5),.05*exp(-.0009*df$N)+.02)
  df

}

df<-make_count_anim_df(c(1:10,seq(20,90,10),seq(100,1000,100),seq(2000,10e3,1e3)),  n_cols=c(rep(10,28),seq(20,100,10)))

# ##########################3
#  #Text counter at top
#  g_counter<-df%>% ggplot(.,aes(x=.5,y=.5,group=group))+
#    lims(x=c(-0.5,1.5),y=c(0,1))+
#    geom_text(x=.5,y=0,aes(label=label,hjust="inward",vjust="inward"),size=30)  +
#   # geom_text(aes(x=labX,y=labYoffset,label=label,vjust="inward"),color="white")+
#   galacticEdTools::theme_galactic(base.theme="void",grid.col = "transparent",border.col="transparent",axis.text.col="transparent")+
#   labs(x="",y="")   +
#   ggplot2::theme(plot.margin=margin(10,10,0,10,unit="pt"))+
#   gganimate::transition_states(group,state_length=state_len)
#
#  a_top<-gganimate::animate(g_counter,start_pause=20,duration=10,
#                                         fps=20,end_pause=30,
#                                         #width=1290,height=200,
#                                         renderer=magick_renderer(loop=T))
df$img<-"assets/gup_silhouette.png"


#########################
# MAIN GRAPH
max.img.size<-max(df$img.size)
df$rel_img.size<-df$img.size/max.img.size
#output lower numbers as images
 g<-df %>% subset(N<=300) %>% ggplot(.,aes(x=x,y=y,group=group))+
    ggimage::geom_image(aes(image=img,x=x,y=y,size=I(rel_img.size*.8)))+
    #geom_point(show.legend = F)+
    scale_y_reverse(expand=expansion(mult=.05,add=.5))+#expand=expansion(0)
    scale_x_continuous(expand=expansion(mult=.1,add=.5))+
    #scale_size_continuous(range=range(df$pt.size))+
    labs(x="",y="",title='{format(as.numeric(closest_state),big.mark=",")}')+
    theme_void()+
    theme(plot.margin=margin(80,10,10,10,unit="pt"),
          plot.title=element_text(family="Montserrat",size=140,hjust = 0.5,face=2))+
    gganimate::transition_states(N,wrap=F)+
    gganimate::view_follow()
 #animate the graph and save as MP4
 gganimate::animate(g,start_pause=20,end_pause=10,duration=10,fps=20,
                    width=1200,height=1200,rewind=F,
                    renderer=av_renderer(file="assets/10k_guppies_<=300.mp4"))

#Output higher numbers as pt.size
df2<-subset(df,N>=300)
df2$rel_pt.size<-df2$pt.size/max(df2$pt.size)*2
g2<-df2 %>% ggplot(.,aes(x=x,y=y,group=group,size=pt.size))+
    geom_point(show.legend = F,pch=21,fill="#363636",aes(stroke=rel_pt.size),col="#cb1f8e")+
    scale_y_reverse(expand=expansion(mult=.05,add=.5))+#expand=expansion(0)
    scale_x_continuous(expand=expansion(mult=.1,add=.5))+
    scale_size_continuous(range=range(df2$pt.size))+
    labs(x="",y="",title='{format(as.numeric(closest_state),big.mark=",")}')+
    theme_void()+
    theme(plot.margin=margin(80,10,10,10,unit="pt"),
          plot.title=element_text(family="Montserrat",size=140,hjust = 0.5,face=2))+
    gganimate::transition_states(N,wrap=F)+
    gganimate::view_follow()
 #animate the graph and save as MP4
 gganimate::animate(g2,start_pause=20,end_pause=30,duration=10,fps=20,width=1200,height=1200,renderer=
                           av_renderer(file="assets/10k_guppies_>=300.mp4"))


 #,width=1290,height=880

 # nframes<-length(attributes(a_bottom)$frame_vars$frame)
 # message("Combining text and graph animations")
 # a_comb<-pbapply::pblapply(1:nframes,function(i){
 #   magick::image_append(c(a_top[i],a_bottom[i]),stack=T)
 # })
 # out<-do.call(c,a_comb)

 #output mp4
 gganimate::anim_save("assets/10k_guppies.gif",g)




