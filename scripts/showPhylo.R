getPhyloNames_noCache<-function(speciesNames,nameType,quiet=T){
      taxize::taxize_options(taxon_state_messages=quiet)
    #allow for abbreviated nameType specification
      if(substr(nameType,1,1)=="s"){nameType <- "sci"}else{nameType <- "common"}
      message("Looking for ",switch(nameType,sci="common",common="scientific")," names in NCBI:\n")
      outNames<-switch(nameType,
                        common={pbapply::pbsapply(speciesNames,function(x){
                          tmp<-if(quiet){suppressMessages(taxize::comm2sci(x)) }else{taxize::comm2sci(x)}
                          #if common name not found in NCBI, try in EOL
                            if(length(tmp[[1]])==0){
                              tmpList<-if(quiet){suppressMessages(taxize::comm2sci(x,simplify=FALSE,db="eol")[[1]])
                                      }else{taxize::comm2sci(x,simplify=FALSE,db="eol")[[1]]}
                                tmp<-tmpList$name[1]

                              if(is.null(tmp)){tmp<-"no common name found"
                                }else{message("\n ! ",nameType," name for ",x," not found in NCBI; using top hit in EOL: ",tmp,"\n")}
                            }

                          if(length(tmp[[1]])==0){tmp<-"no sci. name found"}
                          message("\n  -", x,"  =  ",tmp,"\n")
                          unlist(tmp)
                          })
                        },
                        sci={pbapply::pbsapply(speciesNames,function(x){
                            tmp<-if(quiet){suppressMessages(taxize::sci2comm(x))}else{
                              taxize::sci2comm(x)}#taxize is a noisy package...suppressing all feedback
                            #if common name not found in NCBI, try in EOL
                            if(length(tmp[[1]])==0){
                              tmpList<-if(quiet){suppressMessages(taxize::sci2comm(x,simplify=FALSE,db="eol")[[1]])
                                      }else{taxize::sci2comm(x,simplify=FALSE,db="eol")[[1]]}
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
getWikiPic<-function(x,width=220,picSaveDir=tempdir(),quiet=T,openDir=F,clearCache=F){
  message("\n",rep("-",50),"\n  Downloading Wikipedia Pics\n",rep("-",50))
  if(clearCache){unlink(picSaveDir,recursive=T)}#delete cache if requested
  dir.create(picSaveDir,showWarnings=!quiet)
  imgs<-pbapply::pblapply(x, function (ttl,...){
    savefilename<-fs::path(picSaveDir,paste0(gsub(" ","_",ttl),"_",width,"px"),ext="jpeg")
          #Check if exists. Don't download if it does
          if(!file.exists(savefilename)){
              d<-WikipediR::page_info("en","wikipedia",page=ttl,clean_response=T,ext="jpeg")
              url<-d[[1]]$fullurl
              wikipage<-if(quiet){suppressWarnings(rvest::session(url))}else{rvest::session(url)}
              #check if there's a wiki page for this
              if(wikipage$response$status_code!=404){
                imginfo<-rvest::html_elements(wikipage,".infobox img")
                img.url0<- rvest::html_attr(imginfo[1] ,"src")
                img.url<-paste0("https:",img.url0)
                  if(width!=220){
                    img.url<-gsub("/220px-",paste0("/",width,"px-"),img.url)
                  }

                dlTest <- try(download.file(img.url,savefilename,quiet=quiet),silent=quiet)
                if(inherits(dlTest,"try-error")){
                  message("\n Img download failed for '",ttl,"'.\n")
                  savefilename <- NA
                  }else{ message("\n Img saved for ",ttl,": ",basename(img.url),"\n")}#tell user original filename (or error)
              }else{
                #if there is a 404 code
                savefilename <- NA
                message("\n!! No Wikipedia page found for ",ttl,"!!\n")
              }
          }else{message("\n  Skipping ",ttl,"; already downloaded.**\n")}
          savefilename
      },width,picSaveDir)#End lapply

  #If requested,open the containing folder
  if(openDir){system(paste0("open ",picSaveDir))}
  out<-data.frame(search_term=x,img_loc=unlist(imgs),row.names=NULL)
  invisible(out)
}#End function






#############################
### GetPhyloNames BEGIN
getPhyloNames<-function(speciesNames,nameType,clearCache=F,quiet=T){
#check for cached species names, cuz taxize is slooooow
  tmpfile_names<-fs::path(tempdir(),"phylonamescache",ext="rds")

  #Delete cache file if requested
  if(clearCache){unlink(tmpfile_names,recursive=T);message("\n@cache cleared\n")}

    #If there's no cache, look things up
    if(!file.exists(tmpfile_names)){
      taxa_final<-getPhyloNames_noCache(speciesNames,nameType,quiet=quiet)
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
        taxa_new<-getPhyloNames_noCache(species_missing,nameType,quiet=quiet)
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
        saveRDS(taxa_final,tmpfile_names)
        message("\n@cache updated")
        }else{
          if(test1==F){message("\n@Records already in cache")
            }else{message("\n@not saved to cache (because of potential errors)")}
        }

      invisible(taxa_final)

}



####################################################################################################################

showPhylo<-function(speciesNames,nameType,dateTree=T,labelOffset=0.3,aspectRatio=1,pic="wiki",dotsConnectText=F,picSize=.08,picSaveDir=paste0(tempdir(),"/showPhylo"),optPicWidth=200,picBorderWidth=10,picBorderCol="#363636",openDir=F,xAxisPad=.2,xTitlePad=20,textScalar=1,xTitleScalar=1,phyloThickness=1.2,phyloColor="#363636",textCol="#363636",plotMar=c(t=.02,r=.1,b=.02,l=.02),clearCache=F,quiet=T){
    if(missing(nameType)){stop("\nPlease supply the type of names you're providing; i.e. nameType= either 'sci' or 'common'")}
    #allow for abbreviated nameType specification
    if(substr(nameType,1,1)=="s"){nameType <- "sci"}else{nameType <- "common"}
    # 1. Lookup, error check, & compile a df of sci and common names --------------
    spp<-getPhyloNames(speciesNames,nameType,clearCache = clearCache,quiet=quiet)

    #Now search for matches to scientific names in Open Tree of Life
    message("\n Trying to match scientific names with Open Tree of Life")
    message("\n *You may be asked to choose a number if there are multiple matches.\n")
    tol_taxa<-rotl::tnrs_match_names(spp$scientific_name,do_approximate_matching = F)
    message(rep("-",35),"\nOTL matching results\n",rep("-",35))
    print(tol_taxa[,])
    message(rep("-",35))
    #if there are no matches, throw error
    if(sum(is.na(tol_taxa$unique_name)>0)){stop("\n *Some species records not matched. Try changing your search terms.")}

    # tidying/flagging extinct ------------------------------------------------
    #pull out "extinct" flag to add qualifier for extinct taxa
    tol_taxa$extinct<-ifelse(grepl("extinct",tol_taxa$flags,fixed=T)," \"*Extinct*\"","")
    #make consistent common name capitalization and add extinction flag if appropriate
    tol_taxa$common_name<-paste0(tools::toTitleCase(spp$common_name),tol_taxa$extinct)
    tol_taxa$searchNames.user<-speciesNames


    # Make tree from scientific names in tol_taxa -----------------------------
    tryCatch(
      tree<-if(quiet){suppressWarnings(rotl::tol_induced_subtree(rotl::ott_id(tol_taxa),label="name"))
            }else{rotl::tol_induced_subtree(rotl::ott_id(tol_taxa),label="name")},
      error=function(e) message("\n! Tree Build FAILED\n* The Tree of Life Open Taxonomy system doesn't work super well with extinct organisms sometimes. Try removing them from your set."))

    # Dating the tree ---------------------------------------------------------
    if(dateTree){
      #tryCatch({
        message(rep("-",55),"\n Attempting to scale the tree to divergence times...\n",rep("-",55))
        message(" Tip: If it takes more than a few seconds, it's probably going to fail.\n")
        tree_final<-datelife::datelife_search(tree,summary_format="phylo_median")

       #},error=function(e) {message("\n! Tree dating FAILED !\n Try setting quiet=F, removing taxa, or setting dateTree=F.\n")
        # stop()})
    }else{tree_final<-tree}

    # This modifies tip.labels destructively ----------------------------------
    #make an index to go between tree tips and tol_taxa object
    tree_final$tip.label.backup<-tree_final$tip.label
    tipIndx<-match(tree_final$tip.label.backup,gsub(" ","_",tol_taxa$unique_name))
    sci_tmp<-gsub(" ","~",tol_taxa$unique_name[tipIndx])
    com_tmp<-paste0("(",gsub(" ","~",tol_taxa$common_name[tipIndx]),")")
    tree_final$tip.label<-paste0("atop(bolditalic(",sci_tmp,"),",
                                              gsub("([^~()*]*'[^~()*]*)","\"\\1\"",fixed=F,com_tmp)    ,")")


    # Look up and cache phylopic image UIDs in an efficient manner ------------
      if(pic=="phylopic"){
        #check for cached phylopic UIDs, cuz this is slooooow
        tmpfile_uid<-fs::path(tempdir(),"phyloUIDcache",ext="rds")
        #delete cache if requested
        if(clearCache){unlink(tmpfile_uid,recursive=T)}

        if(!file.exists(tmpfile_uid)){
        message(rep("-",45),"\n  Looking for PhyloPics for your species...(slow)\n",rep("-",45))
        pic_uid<-do.call(rbind,  pbapply::pblapply(tree_final$tip.label.backup,function(x) ggimage::phylopic_uid(x)) )
        saveRDS(pic_uid,tmpfile_uid)
        }else{
        #if we've already cached phylo info, compare new names and see if we can just tack on a few more
        pic_uid_cached<-readRDS(tmpfile_uid)
        noncached_taxa<-tree_final$tip.label.backup[which(is.na(match(tree_final$tip.label.backup,pic_uid_cached$name)))]
        if(length(noncached_taxa)==0){
          pic_uid_final<-pic_uid_cached[match(tree_final$tip.label.backup,pic_uid_cached$name),]
          }else{
          #lookup and append the missing taxa to cache
          message(rep("-",45),"\n  Looking up Phylopic IDs for taxa not already cached:\n",rep("-",45))
          message("\n\n  -",paste0(noncached_taxa,collapse="\n  -"))
          pic_uid_new<-do.call(rbind,  pbapply::pblapply(noncached_taxa,function(x) ggimage::phylopic_uid(x)) )
          pic_uid<-rbind(pic_uid_cached,pic_uid_new)
          saveRDS(pic_uid,tmpfile_uid)
          #now filter out to just the relevant ones
          pic_uid_final<-pic_uid[match(tree_final$tip.label.backup,pic_uid$name),]
          }
        }
      }

# Get Wikipedia main pic --------------------------------------------------
    #initialize addIMg
    addImg=F
    if(pic=="wiki"){
      wikiPics<-getWikiPic(tree_final$tip.label.backup,picSaveDir = picSaveDir,clearCache=clearCache)
      wikiPics$name<-tree_final$tip.label.backup
      #If scientific name didn't come up with anything, try common name
      if(sum(is.na(wikiPics$img_loc))>0){
          missingImgs<-which(is.na(wikiPics$img_loc))
          common_names_in_order_of_tips<-gsub("\\(|\\)","",
                                              tol_taxa$common[match(tree_final$tip.label.backup,
                                                                    gsub(" ","_",tol_taxa$unique_name))])
          #replace search_term with common name
          wikiPics$search_term[missingImgs]<-common_names_in_order_of_tips[missingImgs]
          #search again
          message("Trying common name for missing species ")
          wikiPics[missingImgs,1:2]<-getWikiPic(wikiPics$search_term[missingImgs],picSaveDir = picSaveDir)
      }

      #Use image Magick to add border if requested
      if(picBorderWidth>0){
        wikiPics$img_loc_border<-sapply(wikiPics$img_loc,function(x){
          if(is.na(x)){
            NA
          }else{
            newfile_border<-fs::path(picSaveDir,paste0(gsub("^(.*)\\..*$","\\1",basename(x)), "_border.jpg"))
            img<-magick::image_read(x)
            img<-magick::image_border(img,picBorderCol,paste0(picBorderWidth,"%x",picBorderWidth,"%"))
            # #Rescale to desired pixel width
            # img<-magick::image_scale(img,optPicWidth)

            #Rescale to desired pixel width (This constrains height to the width value to prevent overlap!!)
            img<-magick::image_scale(img,paste0(optPicWidth,"x",optPicWidth))
            #write bordered file
            magick::image_write(img,newfile_border)
            newfile_border
          }
        })
        #output paths to bordered images
        imgLoc<-wikiPics$img_loc_border
        #If no border width requested, just plot originals
      }else{imgLoc<-wikiPics$img_loc}
      addImg <- T
    }




# Import custom images if supplied ----------------------------------------
    if(pic=="cust"){
      #check if images exist
      message(rep("-",45),"\n Checking for custom species images in picSaveDir=\n  > ",picSaveDir,"\n",rep("-",50))
      #Reorder tol_taxa to match tree as our Rosetta for matching file names
      tol_taxa.orderedByTree<-tol_taxa[match(tree_final$tip.label.backup,gsub(" ","_",tol_taxa$unique_name)),]
      #search_string
      img_files<-list.files(fs::path(picSaveDir),pattern="\\.png|\\.jpeg|\\.jpg")
      img_files.stndzd <- gsub(" |-","_",tolower(img_files))
      speciesNames.stndzd<-gsub(" |-","_",tolower(tol_taxa.orderedByTree$searchNames.user))
      IMGs <- sapply(speciesNames.stndzd,function(x){
        img_indx<-grep(x,img_files.stndzd)
        img_filename<-ifelse(length(img_indx)==0,NA,fs::path(picSaveDir,img_files[img_indx[1]]))#indx[1] is in case of multiple hits; take first
      })
      if(sum(is.na(IMGs)>0)){
        warning("Missing images for:\n -",paste0(names(IMGs)[which(is.na(IMGs))],collapse=" -"))
        }

      # Manipulate images to make them display faster and add a border if requested
      optImg_loc<-fs::path(picSaveDir,"opt_img_for_showPhylo")
      #Delete cached optimized photos if clearCache==T
      if(clearCache){unlink(optImg_loc,recursive=T)}

      #make optimized pic folder if it doesn't exist
      dir.create(optImg_loc,showWarnings = F)
      message(rep("-",45),"\n Optimizing custom images\n  Params:",
              "\n  |_ optPicWidth= ",optPicWidth,"px\n  |_ picBorderWidth= ",picBorderWidth,
              "\n  |_ picBorderCol= ",picBorderCol,"\n",rep("-",50),"\n")
      optimizedIMGs<-sapply(1:length(IMGs),function(i){
        x<-IMGs[i]
        if(is.na(x)){
          warning(" - ","!! ",names(IMGs[i])," IMAGE MISSING")
          NA
        }else{
          #preserves spaces, removes ext; Not sure if I should keep spaces, but respecting user's prefs
          baseName<-gsub("^(.*)\\..*$","\\1",basename(x))
          newfile<-fs::path(optImg_loc,paste0(baseName,"_",optPicWidth,"px"),ext="jpg")
          if(file.exists(newfile)){
            message(" - ",basename(newfile)," : Already exists")
          }else{
            #Work the image Magick
            img<-magick::image_read(x)

            #Rescale to desired pixel width
            img<-magick::image_scale(img,paste0(optPicWidth,"x",optPicWidth))

            #add border
            if(picBorderWidth>0){
              img<-magick::image_border(img,picBorderCol,paste0(picBorderWidth,"%x",picBorderWidth,"%"))
            }

            #write optimized file
            magick::image_write(img,newfile)
            message(" - ",basename(newfile)," : SAVED")
          }
          newfile
        }
      })
      addImg <- T
      imgLoc<-optimizedIMGs
       #If requested,open the containing folder
    if(openDir){system(paste0("open ",optImg_loc))}
    }#end custom image code



    # Plot that beautiful tree :) ---------------------------------------------
    #Define custom theme to override a lot of ggtree's styling (if we want to plot)
    theme_phylo<-ggplot2::theme(plot.margin=ggplot2::margin(plotMar,unit="npc"),
                                panel.border=ggplot2::element_blank())

    #Define basic tree plot before modifying in steps
    g0<-ggtree::ggtree(tree_final,size=phyloThickness,color=phyloColor)+theme_phylo

    #Extract some info from base plot
    timescale<-ggplot2::layer_scales(g0)$x$get_limits()[2]
    timescale_rounded <- ceiling(timescale/10)*10
    yscale<-ggplot2::layer_scales(g0)$y$get_limits()
    textOffset=labelOffset*timescale
    picOffset=textOffset/2
    backgroundRec<-data.frame(xmin=timescale+picOffset-(picSize*timescale*.7),xmax=timescale+picOffset+(picSize*timescale*.7),
                              ymin=yscale[1]-.5,ymax=yscale[2]+.5)



    #Rescale to have a 50% buffer on the right to add text
    g <- g0+ggplot2::scale_x_continuous(breaks=seq(timescale,0,-timescale/10),
                                      labels=round(seq(0,timescale,timescale/10)))  +
      ggplot2::coord_cartesian(ylim=c(yscale[1]-xAxisPad,yscale[2]),clip='off')+
      #Add text labels
      ggtree::geom_tiplab(geom='text',vjust=0.5,hjust=0,parse=T,offset=textOffset,align=dotsConnectText,
                  color=textCol,label.padding=ggplot2::unit(1,"lines"),size=6*textScalar)  +
      #ggplot2::coord_fixed(3.1)+

      #add semitransparent rectangle between dotted line and phylopic
      #geom_rect(inherit.aes=F,data=backgroundRec,aes(xmin=xmin,ymin=ymin, xmax=xmax,ymax=ymax),fill="white",alpha=.7)+
      {
        if(pic=="phylopic"){
          ggtree::geom_tiplab(image=pic_uid_final$uid,geom="phylopic",color=textCol,hjust=0.5,
                      size=picSize,offset=picOffset,alpha=1)}else{}
      }+{
        if(addImg){
          ggtree::geom_tiplab(image=imgLoc,geom="image",size=picSize,offset=picOffset,alpha=1,hjust=0.5,asp=1)
        }else{}
      }+{
        if(dateTree){
         ggplot2::xlab("Millions of Years Ago (Ma)")
        }else{}
      }

    #dateTree formatting has to be in 2 steps cuz aPPARENTLY you can add 2 layers in 1 if/then :(
    if(dateTree){
    g+ggplot2::theme(axis.ticks.x=ggplot2::element_line(color=phyloColor),
                  axis.ticks.length.x=ggplot2::unit(3,"pt"),
                  axis.title.x=ggplot2::element_text(margin=ggplot2::margin(xTitlePad,0,3,0),face="bold",size=26*textScalar),
                  axis.text.x=ggplot2::element_text(color=textCol,size=21*textScalar),
                  axis.line.x=ggplot2::element_line(color=phyloColor))
    }else{g}

}

#+geom_hilight(node=5,fill="gold")



speciesNames <- c("Florida manatee","giraffe","barn swallow","ocelot","domestic cat","leopard","platypus")
G1<-showPhylo(speciesNames,nameType="common",plotMar = c(t=0,r=.25,b=.05,l=0),picSize=.12)
G1
#input
speciesNames<- c("European starling","Anna's hummingbird","flamingo")
showPhylo(speciesNames,nameType="common",pic="wiki",dateTree =T ,picSize=.2,plotMar = c(t=0,r=.4,b=.05,l=0),labelOffset=0.35)
#,"Greater saber-toothed cat"
speciesNames<-c("domestic cat","puma","leopard","jaguar","cheetah")
showPhylo(speciesNames,"c",pic="cust",picSize=.2,labelOffset=.5,plotMar = c(t=.01,r=.23,b=.01,l=0),xTitlePad=10,xAxisPad=.2,picSaveDir = "assets/panther_imgs",textScalar=1.3)
ggsave("assets/cats.png")
ggtree::groupClade(p,node=6)+ggplot2::aes(color=group)+ggtree::geom_highlight(node=c(3,2,5))+geom_text(aes(label=node))
ggsave("cats.png")

