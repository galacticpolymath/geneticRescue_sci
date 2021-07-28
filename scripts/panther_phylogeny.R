cats<-showPhylo(speciesNames=c("puma","leopard","jaguar","domestic cat"),nameType="c",picSaveDir = "assets/panther_imgs",pic="cust")
cats
ggsave("assets/panther_phylogeny.jpg")
