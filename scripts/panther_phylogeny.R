# panther phylogeny
pacman::p_load(BiocManager,tidytree,ggtree,rotl,galacticPubs,taxize)
devtools::install_github("phylotastic/datelife")
#currently there's an incompatibility of ggtree with dplyr 1.0.6
remove.packages("dplyr")
devtools::install_version("dplyr",version="1.0.5")
require(dplyr);require(datelife)

common_names<-c("lion","ocelot","puma","leopard","jaguar","domestic cat")

sci_names<-common_names %>% comm2sci() %>% unlist()
tol_taxa<-tnrs_match_names(sci_names,do_approximate_matching = F)
tol_taxa$common_name<-sci2comm(tol_taxa$unique_name)
cat_tree<-tol_taxa%>% ott_id() %>% tol_induced_subtree(label="name")

tipIndx<-match(cat_tree$tip.label,gsub(" ","_",tol_taxa$unique_name))

cat_tree$tip.label<-paste0("atop(bolditalic(",tol_taxa$unique_name[tipIndx],"),(",tol_taxa$common_name[tipIndx],"))") %>% gsub(" ","~",.)
ggtree(cat_tree)+geom_tiplab(offset=3,vjust=0.5,hjust=.5,parse=T)+xlim(0,7)+geom_hilight(node=5,fill="gold")

#how to see node numbers
+geom_text(aes(label=node))

tree_dates<-get_datelife_result(cat_tree)
phinal<-summarize_datelife_result(tree_dates)
plot_phylo_all(phinal)

# #All extant cats
# tol_subtree(tnrs_match_names("Felidae")$ott_id) %>% plot()


taxonomy_taxon_info(tol_taxa$ott_id)
tree<-read.tree("data/panther_phylo_newick.txt")

ggtree::ggtree(tree,size=2,col=gpColors("gal"))+ggGalactic()+ggplot2::theme_void()
