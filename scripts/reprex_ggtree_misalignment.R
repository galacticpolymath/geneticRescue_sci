#USEFUL SO posts
# https://stackoverflow.com/questions/35781950/r-alignment-of-axis-labels-with-expressions
# https://stackoverflow.com/questions/48195587/how-to-set-a-standard-label-size-in-ggplots-geom-label


#GGTREE github
# https://github.com/YuLab-SMU/ggtree/issues

nwk <- "((((bufonidae, dendrobatidae), ceratophryidae), (centrolenidae, leptodactylidae)), hylidae);"
require(ggtree)
x = read.tree(text = nwk)
ggtree(x) + xlim(NA, 7) + ylim(NA, 6.2)+geom_tiplab(geom="label",offset=1,hjust=0)

longestString<-max(nchar(x$tip.label))
filler<-sapply(nchar(x$tip.label),function(x) paste0(rep("~",longestString-x),collapse=""))
doubledecker<-paste0(x$tip.label,"\n(Common~Name",1:6,")")
x2<-x
x2$tip.label<-doubledecker
ggtree(x2) + xlim(NA, 7) + ylim(NA, 6.2)+geom_tiplab(geom="label",offset=.6,hjust=0.5,parse=F)

