getPhyloNames_noCache<-function(speciesNames,nameType){
      taxize_options(taxon_state_messages=T)
      message("Looking for ",switch(nameType,sci="common",common="scientific")," names:\n")
      outNames<-switch(nameType,
                        common={pbapply::pbsapply(speciesNames,function(x){
                          tmp<-suppressMessages(taxize::comm2sci(x)) #taxize is a noisy package...suppressing all feedback
                          if(length(tmp[[1]])==0){tmp<-"no sci. name found"}
                          message("\n  -", x,"  =  ",tmp,"\n")
                          unlist(tmp)
                          })
                        },
                        sci={pbapply::pbsapply(speciesNames,function(x){
                            tmp<-suppressMessages(taxize::sci2comm(x))#taxize is a noisy package...suppressing all feedback
                            #if common name not found in NCBI, try in EOL
                            if(length(tmp[[1]])==0){
                              tmpList<-taxize::sci2comm(x,simplify=FALSE,db="eol")[[1]]
                              tmp<-subset(tmpList,tmpList$language=="en")$vernacularname[1]
                              if(is.null(tmp)){tmp<-"no common name found"}
                            }
                            message("\n  -", x,"  =  ",tmp,"\n")
                            unlist(tmp)
                          })
                        })
    noms<-data.frame(common_name=switch(nameType,sci=outNames,common=speciesNames),scientific_name=switch(nameType,sci=speciesNames,common=outNames),row.names = NULL)

invisible(noms)
}

list.of.packages<-c("WikipediR","rvest")
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(WikipediR); require(rvest)

# GetWikipic def ----------------------------------------------------------
#x= search string (i.e. a title of a Wikipedia page); or a vector of search string(s)
#width= desired width in pixels (220 px thumbnail by default)
#savedest= save destination; wd by default
getWikiPic<-function(x,width=220,picSaveDir=tempdir(),quiet=T,openDir=F){
  message("\n",rep("-",50),"\n  Downloading Wikipedia Pics\n",rep("-",50))
  dir.create(picSaveDir,showWarnings=!quiet)
  imgs<-pbapply::pblapply(x, function (ttl,...){
    savefilename<-fs::path(picSaveDir,paste0(gsub(" ","_",ttl),"_",width,"px"),ext=ext)
          #Check if exists. Don't download if it does
          if(!file.exists(savefilename)){
              d<-page_info("en","wikipedia",page=ttl,clean_response=T,ext="jpeg")
              url<-d[[1]]$fullurl
              wikipage<-rvest::session(url)
              imginfo<-wikipage %>% rvest::html_elements("tr:nth-child(2) img")
              img.url0<- imginfo[1] %>% rvest::html_attr("src")
              img.url<-paste0("https:",img.url0)
              if(width!=220){img.url<-gsub("/220px-",paste0("/",width,"px-"),img.url)}
              download.file(img.url,savefilename,quiet=quiet)
              message("\n Img saved for ",ttl,": ",basename(img.url),"\n")#tell user original filename (or error)
          }else{message("\n **Skipping ",ttl,"; already downloaded.**\n")}
          savefilename
      },width,picSaveDir)#End lapply

  #If requested,open the containing folder
  if(openDir){system(paste0("open ",picSaveDir))}
  out<-data.frame(search_term=x,img_loc=unlist(imgs),row.names=NULL)
  invisible(out)
}#End function






#############################
### GetPhyloNames BEGIN
getPhyloNames<-function(speciesNames,nameType,clearCache=F){
#check for cached species names, cuz taxize is slooooow
  tmpfile_names<-fs::path(tempdir(),"phylonamescache",ext="rds")

  #Delete cache file if requested
  if(clearCache){unlink(tmpfile_names);message("\n@cache cleared\n")}

    #If there's no cache, look things up
    if(!file.exists(tmpfile_names)){
      taxa_final<-getPhyloNames_noCache(speciesNames,nameType)
      test1=T #We'll consider saving this to cache

    #if there is a cache, see if it needs to be updated with new rows
    }else{
      taxa_cached<-readRDS(tmpfile_names)
      message("\nChecking cached species records\n")
      species_missing<-speciesNames[which(is.na(match(speciesNames,taxa_cached[,switch(nameType,
                                                                                sci="scientific_name",
                                                                                common="common_name")])))]
      #subset cached by the requested species records
      if(length(species_missing)==0){
        taxa<-taxa_cached
        test1=F
      }else{
        taxa_new<-getPhyloNames_noCache(species_missing,nameType)
        taxa<-rbind(taxa_cached,taxa_new)
        #I'll wait till later to write RDS, b/c I want to see if these are valid entries
        test1=T
      }
      goodRows<-match(speciesNames,taxa[,switch(nameType,sci="scientific_name",common="common_name")])
      taxa_final<-taxa[goodRows,]
    }

    #Everything below is regardless of whether cache existed or not

      #Do some error checking
      #if all of common or scientific names contain "found" as in were not found, suggest changing nameType,
      #or if some of the records have the same scientific and common name

      #Output results to user
      message("\n",rep("-",35),"\n Taxonomic Name Results\n",rep("-",35))
      print(taxa_final)
      message(rep("-",35))

      #Error checking
      if(nrow(taxa_final)==sum(grepl("found",taxa_final[,1]))|
         nrow(taxa_final)==sum(grepl("found",taxa_final[,2]))){stop("\nSomething's weird here. Did you set the right nameType?\n")}

      #warn if sci and common names match
      if(sum((taxa_final[,1]==taxa_final[,2]))>0){warning("Double check output. You've got some matching scientific and common names. Did you supply the correct nameType?")
        test2=F
        }else{test2=T}

    #Warn about individual no matches
    if(sum(grepl("found",taxa_final))>0){
      noMatch.indx<-which(apply(taxa_final,c(1,2),function(x) grepl("found",x)),arr.ind=T)
      noMatch<-taxa_final[noMatch.indx[,"row"],ifelse(noMatch.indx[,"col"]==1,2,1)[1]]
      warning("No match for: \n   -",paste0( noMatch,collapse="\n   -" ),"\n")
      test3=F
    }else{test3=T}

      #save to cache if all 3 tests pass
      if(test1&test2&test3){
        saveRDS(taxa,tmpfile_names)
        message("\n@cache updated")
        }else{
          if(test1==F){message("\n@Records already in cache")
            }else{message("\n@not saved to cache (because of potential errors)")}
        }

      invisible(taxa_final)

}



showPhylo<-function(speciesNames,nameType,dateTree=T,labelOffset=0.15,pic="wiki",dotsConnectText=F,picSize=.08,picSaveDir=paste0(tempdir(),"/showPhylo"),openDir=F,xAxisMargin=20,textScalar=1,phyloThickness=1.5,phyloColor="#363636",textCol="#363636"){
    if(missing(nameType)){stop("\nPlease supply a nameType; either 'sci' or 'common'")}
    # 1. Lookup, error check, & compile a df of sci and common names --------------
    spp<-getPhyloNames(speciesNames,nameType)

    #Now search for matches to scientific names in Open Tree of Life
    message("\n Trying to match scientific names with Open Tree of Life")
    tol_taxa<-tnrs_match_names(spp$scientific_name,do_approximate_matching = F)
    message(rep("-",35),"\nOTL matching results\n",rep("-",35))
    print(tol_taxa[,])
    message(rep("-",35))
    #if there are no matches, throw error
    if(sum(is.na(tol_taxa$unique_name)>0)){stop("\n *Some species records not matched. Try changing your search terms.")}

    # tidying/flagging extinct ------------------------------------------------
    #pull out "extinct" flag to add qualifier for extinct taxa
    tol_taxa$extinct<-ifelse(grepl("extinct",tol_taxa$flags,fixed=T)," \"*Extinct*\"","")
    #make consistent common name capitalization and add extinction flag if appropriate
    tol_taxa$common_name<-paste0("(",tools::toTitleCase(spp$common_name),")",tol_taxa$extinct)


    ###########################################################################
    # Make tree from scientific names in tol_taxa -----------------------------

    tryCatch(
      tree<-suppressWarnings(tol_induced_subtree(ott_id(tol_taxa),label="name")),error=function(e) message("\n! Tree Build FAILED\n* The Tree of Life Open Taxonomy system doesn't work super well with extinct organisms sometimes. Try removing them from your set."))



    # Dating the tree ---------------------------------------------------------
    if(dateTree){
      tree_final<-datelife::datelife_search(tree,summary_format="phylo_median")
    }else{tree_final<-tree}


    # This modifies tip.labels destructively ----------------------------------
    #make an index to go between tree tips and tol_taxa object
    tree_final$tip.label.backup<-tree_final$tip.label
    tipIndx<-match(tree_final$tip.label.backup,gsub(" ","_",tol_taxa$unique_name))
    tree_final$tip.label<-gsub(" ","~",paste0("atop(bolditalic(",tol_taxa$unique_name[tipIndx],"),",tol_taxa$common_name[tipIndx],")") )


    # Look up and cache phylopic image UIDs in an efficient manner ------------
      if(pic=="phylopic"){
        #check for cached phylopic UIDs, cuz this is slooooow
        tmpfile_uid<-fs::path(tempdir(),"phyloUIDcache",ext="rds")
        if(!file.exists(tmpfile_uid)){
        pic_uid<-ggimage::phylopic_uid(tree_final$tip.label.backup)
        saveRDS(pic_uid,tmpfile_uid)
        }else{
        #if we've already cached phylo info, compare new names and see if we can just tack on a few more
        pic_uid_cached<-readRDS(tmpfile_uid)
        noncached_taxa<-tree_final$tip.label.backup[which(is.na(match(tree_final$tip.label.backup,pic_uid_cached$name)))]
        if(length(noncached_taxa)==0){
          pic_uid_final<-pic_uid_cached[match(tree_final$tip.label.backup,pic_uid_cached$name),]
          }else{
          #lookup and append the missing taxa to cache
            message("\nLooking up Phylopic IDs for taxa not already cached:\n  -",paste0(noncached_taxa,collapse="\n  -"))
          pic_uid_new<-ggimage::phylopic_uid(noncached_taxa)
          pic_uid<-rbind(pic_uid_cached,pic_uid_new)
          saveRDS(pic_uid,tmpfile_uid)
          #now filter out to just the relevant ones
          pic_uid_final<-pic_uid[match(tree_final$tip.label.backup,pic_uid$name),]
          }
        }
      }

    if(pic=="wiki"){
      wikiPics<-getWikiPic(tree_final$tip.label.backup)
    }


    # Plot that beautiful tree :) ---------------------------------------------
    g0<-ggtree(tree_final,size=phyloThickness,color=phyloColor)
    timescale<-ggplot2::layer_scales(g0)$x$get_limits()[2]
    timescale_rounded <- ceiling(timescale/10)*10
    yscale<-ggplot2::layer_scales(g0)$y$get_limits()
    textOffset=labelOffset*timescale
    picOffset=textOffset/2
    backgroundRec<-data.frame(xmin=timescale+picOffset-(picSize*timescale*.7),xmax=timescale+picOffset+(picSize*timescale*.7),
                              ymin=yscale[1]-.5,ymax=yscale[2]+.5)

    #Define custom theme to override a lot of ggtree's styling (if we want to plot)
    theme_phylo<-ggplot2::theme(plot.margin=ggplot2::margin(0,0,30,0,unit="pt"),axis.line.x=element_line(color=1),
                                axis.ticks.x=element_line(color=1),axis.ticks.length.x=unit(3,"pt"),
                                axis.title.x=element_text(margin=margin(xAxisMargin,0,5,0),face="bold",size=26*textScalar),
                                axis.text.x=element_text(color=1,size=18*textScalar),
                                panel.border=element_blank())

    #Rescale to have a 50% buffer on the right to add text
    g0+theme_phylo+scale_x_continuous(breaks=seq(timescale,0,-timescale/10),
                                      limits=c(0,timescale*1.6),labels=round(seq(0,timescale,timescale/10)))+
      ggplot2::coord_cartesian(ylim=yscale,clip='off')+
      annotate("text",x=timescale/2,y=yscale[1]-yscale[2]*.135, hjust=0.5,label="Millions of Years Ago (Ma)")+

      #Add text labels
      geom_tiplab(geom='label',vjust=0.5,hjust=0,parse=T,offset=textOffset,align=dotsConnectText,
                  color="black",label.padding=unit(.5,"lines"))+

      #add semitransparent rectangle between dotted line and phylopic
      #geom_rect(inherit.aes=F,data=backgroundRec,aes(xmin=xmin,ymin=ymin, xmax=xmax,ymax=ymax),fill="white",alpha=.7)+
      {
        if(pic=="phylopic"){
          geom_tiplab(image=pic_uid_final$uid,geom="phylopic",color="gray20",hjust=0.5,
                      size=picSize,offset=picOffset,alpha=1)}else{}
      }+{
        if(pic=="wiki"){
          geom_tiplab(image=wikiPics$img_loc,geom="image",size=picSize,offset=picOffset,alpha=1,hjust=0.5,asp=1)
        }else{}
      }
}
#+length(speciesNames)/20))
#+geom_hilight(node=5,fill="gold")



speciesNames <- c("Florida manatee","giraffe","barn swallow","ocelot","domestic cat","leopard","platypus","swifts")
showPhylo(speciesNames,nameType="common")

#input
speciesNames<- c("Tachycineta bicolor","Homo","Hirundo rustica","Hirundo aethiopica","Zonotrichia leucophrys","Sturnus vulgaris","Tyrannosaurus rex","rattus norvegicus","Phoenicoparrus jamesi")
showPhylo(speciesNames,nameType="sci",pic="wiki")

showPhylo(c("domestic cat","human","puma","leopard","jaguar"),"common",pic="wiki",picSize=.12,labelOffset=.26)
ggsave("cats.png")
