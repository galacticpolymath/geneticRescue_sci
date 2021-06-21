showPhylo<-function(speciesNames,nameType="common",pic="phylopic"){
  #Try to match common names to scientific names, and stop if there's no match
if(nameType=="common"){
  sci_search<-taxize::comm2sci(speciesNames,simplify=FALSE)
  #replace missings with NA
  sci_search[which(sapply(sci_search,length)==0)]<-NA
  sci_search<-unlist(sci_search)
  noms<-data.frame(common_name=names(sci_search),scientific_name=unlist(sci_search),row.names = NULL)
  #print scientific and common names for error checking
  message("\n",rep("-",35),"\n Here's what we found\n",rep("-",35))
  print(noms)
  message(rep("-",35))
  #if
  tip<-if(length(noms$scientific_name)==sum(is.na(noms$scientific_name))){"Are these common names or did you mean to set nameType='sci'?"}else{"Try a different common name."}

  #if no matches ask if they meant to input scientific names
  if(sum(is.na(noms$scientific_name))>0){stop("No match for: \n   -",paste0( noms$common_name[which(is.na(noms$scientific_name))],collapse="\n   -" ),"\n\n*",tip)}
}else{
  sci_search<-speciesNames
}

#Now search for matches to scientific names in Open Tree of Life
tol_taxa<-tnrs_match_names(sci_search,do_approximate_matching = F)
print(tol_taxa[,c(1:2)])
#if there are no matches, ask if they meant to input common names
if(sum(is.na(tol_taxa$unique_name))==nrow(tol_taxa)){stop("\n *Are these scientific names or did you mean to set nameType='common'?")}

###########################################################################
# Add common names --------------------------------------------------------
#If nameType is sci, look up common names, otherwise skip this
if(nameType!="common"){
#assign common names to tol_taxa
common_names1<-taxize::sci2comm(tol_taxa$unique_name,simplify=FALSE,db="ncbi")

#try to fill in blanks with EOL common names if there are missings
common_names2 <- sapply(1:length(common_names1),function(i) {
  if(length(common_names1[[i]])==0){
  namez<-taxize::sci2comm(tol_taxa$unique_name[i],simplify=FALSE,db="eol")[[1]]
  namez2 <- subset(namez,namez$language=="en")$vernacularname[1]
  if(is.null(namez2)){namez2<-"no common name found"}
  namez2
  }else{common_names1[[i]]}
  })
#if common names were supplied initially, use those
}else{common_names2<-speciesNames}


# tidying/flagging extinct ------------------------------------------------
#pull out "extinct" flag to add qualifier for extinct taxa
tol_taxa$extinct<-ifelse(grepl("extinct",tol_taxa$flags,fixed=T)," \"*Extinct*\"","")
#make consistent common name capitalization and add extinction flag if appropriate
tol_taxa$common_name<-paste0("(",tools::toTitleCase(common_names2),")",tol_taxa$extinct)


###########################################################################
# Make tree from scientific names in tol_taxa -----------------------------

tryCatch(
  tree<-tol_induced_subtree(ott_id(tol_taxa),label="name"),error=function(e) message("\n! Tree Build FAILED\n* The Tree of Life Open Taxonomy system doesn't work super well with extinct organisms sometimes. Try removing them from your set.")
  )

#make an index to go between tree tips and tol_taxa object
tipIndx<-match(tree$tip.label,gsub(" ","_",tol_taxa$unique_name))
tree$tip.label.backup<-tree$tip.label
tree$tip.label<-gsub(" ","~",paste0("atop(bolditalic(",tol_taxa$unique_name[tipIndx],"),",tol_taxa$common_name[tipIndx],")") )


# Look up and cache phylopic image UIDs in an efficient manner ------------
  if(pic="phylopic"){
    #check for cached phylopic UIDs, cuz this is slooooow
    tmpfile_uid<-fs::path(tempdir(),"phyloUIDcache",ext="rds")
    if(!file.exists(tmpfile_uid)){
    pic_uid<-ggimage::phylopic_uid(tree$tip.label.backup)
    saveRDS(pic_uid,tmpfile_uid)
    }else{
    #if we've already cached phylo info, compare new names and see if we can just tack on a few more
    pic_uid_cached<-readRDS(tmpfile_uid)
    noncached_taxa<-tree$tip.label.backup[which(is.na(match(tree$tip.label.backup,pic_uid_cached$name)))]
    if(length(noncached_taxa)==0){
      pic_uid<-pic_uid_cached
      }else{
      #lookup and append the missing taxa to cache
        message("\nLooking up Phylopic IDs for taxa not already cached:\n  -",paste0(noncached_taxa,collapse="\n  -"))
      pic_uid_new<-ggimage::phylopic_uid(noncached_taxa)
      pic_uid<-rbind(pic_uid_cached,pic_uid_new)
      saveRDS(pic_uid,tmpfile_uid)
      }
    }
#rewrite tip labels so we have bold italic scientific name over parenthetical common name


ggtree(tree)+xlim(0,5+(length(speciesNames)))+geom_tiplab(geom='label',vjust=0.5,hjust=0,parse=T,offset=2,align=1,label.padding=unit(.5,"lines"))+
  geom_tiplab(image=pic_uid$uid,geom="phylopic",color="slategrey",size=.07,offset=0.5)
#+length(speciesNames)/20))
#+geom_hilight(node=5,fill="gold")
}


speciesNames <- c("Florida manatee","giraffe","barn swallow","ocelot","domestic cat","leopard","platypus","swifts")
showPhylo(speciesNames,nameType="common")

#input
speciesNames<- c("Tachycineta bicolor","Homo","Hirundo rustica","Hirundo aethiopica","Zonotrichia leucophrys","Sturnus vulgaris","Tyrannosaurus rex","rattus norvegicus","Stegosaurus")
showPhylo(speciesNames,nameType="sci")
