# Function to get data from Loisel et al 2014 data base on bulk density in peatlands
# Hosted by "Holocene Perspective on Peatland Biogeochemistry" 
# https://peatlands.lehigh.edu

# Author: gustaf.granath@gmail.com
# The function uses the packages rvest & RCurl to scrape the site

get_peat <- function(type = "all", only_meta = TRUE, metadata = NULL) {
  require(rvest)
  require(RCurl)
  if("peatMeta" %in% class(metadata)) {
    site.info <- metadata
  }
  
  if(!("peatMeta" %in% class(metadata)) & !(is.null(metadata))) {stop("You gave a metadata object that is not from the get_peat function.
                                                                      Run function with metadata = NUll")}
  if(!("peatMeta" %in% class(metadata))) { #get metadata
    
    if(type == "bog") {select="/taxonomy/term/1" }
    if(type == "rich") {select="/taxonomy/term/2" }
    if(type == "poor") {select="/taxonomy/term/3" }
    if(type == "perma") {select="/taxonomy/term/4" }
    if(type == "kettle") {select="/taxonomy/term/5" }
    if(type == "other") {select="/taxonomy/term/6" }
    if(type == "all") {select="/node?page=0" }
    
    url<- read_html(paste("https://peatlands.lehigh.edu", select, sep = ""))
    #get site name and site specific links for page 0
    selector_name <- ".node-title"
    site.names<-html_nodes(url, selector_name) %>%
      html_text() 
    site.links <- html_nodes(url, selector_name) %>% # get the CSS nodes
      sub(sprintf(".*%s(.*)%s.*", "\"", "\""), "\\1", .)
    #how many pages are there?
    selector_name <- ".pager-item"
    pages <- html_nodes(url, selector_name) %>%
      html_text()
    #get basic data (name and link) from the other web pages
    if(length(pages) > 0) {
      for (i in 1:length(pages)) {
        if(grepl("term", select)) {select.page = paste(select,"?page=", sep = "")} else 
        {select.page = paste("/node?page=", sep = "")}
        url<- read_html(paste("https://peatlands.lehigh.edu", select.page, i, sep=""))
        selector_name <- ".node-title"
        site.names.new <- html_nodes(url, selector_name) %>%
          html_text()
        site.names <- c(site.names, site.names.new)
        site.links.new <- html_nodes(url, selector_name) %>% # get the CSS nodes
          sub(sprintf(".*%s(.*)%s.*", "\"", "\""), "\\1", .)
        site.links <- c(site.links, site.links.new)
      }
    }
    site.info <- data.frame(site = site.names, links = paste("https://peatlands.lehigh.edu", site.links, sep = ""), 
                            country=NA, file=NA, contributor=NA, type=NA, basal_age=NA, No_dates=NA,
                            year_collected=NA, publication=NA, latitude=NA, longitude=NA, elevation=NA, comment=NA,stringsAsFactors = FALSE )
    
    #Now get the meta-data from the sites
    for (i in 1:nrow(site.info)) {
      url<- read_html(site.info[i, "links"])
      selector_name <- ".field-label"
      #get field label
      site.label <- html_nodes(url, selector_name) %>%
        html_text(trim=TRUE)
      #get filed data
      selector_name <- ".field-item"
      site.item <- html_nodes(url, selector_name) %>%
        html_text(trim=TRUE)
      #Check if there are comments and 
      #add comments (labelled ***) to data frame
      if(any(grepl("[***]", site.item))) {
        site.info[ i, "comment"] <- site.item[grep("[***]", site.item)]
        #remove comment from field
        site.item <- site.item[-grep("[***]", site.item)]
      } 
      # which fields are recorded. List fields and then check which fields are reported for the site
      fields <- c("country", "file", "contributor", "type", "basal", "dates", "collected", "public", "latitude", "longitude", "elevation")
      fields <- fields[sapply(fields, function (x) any(grepl(x, site.label, , ignore.case = TRUE)))]
      
      #match filed data with the right column of the data frame
      for (j in 1:length(fields)) {
        site.info[i, grep(fields[j], colnames(site.info), ignore.case = TRUE)] <- site.item[grep(fields[j], site.label, ignore.case = TRUE)]
      }
      print(i)
    }
    #remmove white space in basal age column
    site.info$basal_age <- unlist(lapply(strsplit(site.info$basal_age, split=" +"), function (x) paste(x[1], x[2], sep="")))
    # and now just convert only the numeric ones
    # find the numeric variables and convert them
    numvar <- apply(site.info, 2, function (x) all(!grepl("[a-zA-Z]", x)))
    site.info[, numvar] <- as.numeric(as.character(unlist(site.info[, numvar])))
    
    #REMOVE duplicate sites. E.g. Sundance can be found twice in the data set
    site.info <- site.info[!(duplicated(site.info$site)),]
    site.info <- droplevels(site.info) #may need to clean up
    closeAllConnections()
    
  }
  
  #start part two
  
  # Get CSV files with actual data
  if(!(isTRUE(only_meta))) { 
    
    for (i in 1:nrow(site.info)) {
      urldat <- getURL(paste("https://peatlands.lehigh.edu/sites/peatlands.lehigh.edu/files/", site.info[i,"file"], sep=""))
      peatdat <- read.table(textConnection(urldat), skip = 1, header =TRUE, sep =',', comment.char = "")
      #make blanks as NAs
      peatdat[peatdat==""] <- NA
      print(i)
      
      # FIX meta data first
      #first column with only NAs
      #keep columns before 
      meta <- peatdat[, 1:(which(apply(is.na(peatdat),2,all))[1]-1) ]
      #remove empty rows
      meta <- meta[-which(apply(is.na(meta),1,all)),]
      meta.long <- as.data.frame(t(meta), stringsAsFactors = FALSE)
      meta.long <- cbind(rownames(meta.long), meta.long) #fix site column
      colnames(meta.long) <- lapply(meta.long[1,], as.character)
      meta.long <- meta.long[-1,]
      rownames(meta.long) <- NULL #remove rownames
      #if mutiple samples fix so we get the same site name
      if(nrow(meta.long) > 1) {meta.long[2:nrow(meta.long), 1] <- meta.long[1,1]}
      
      # FIX peat prop data
      #first column with not only NAs after we have removed the meta data columns
      #keep 10 columns thereafter 
      peatdat <- peatdat[, -c(1:ncol(meta))]
      peatdat.prop <- peatdat[, which(!(apply(is.na(peatdat),2,all)))[1]:(which(!(apply(is.na(peatdat),2,all)))[1] + 9) ]
      #remove empty rows
      empty.row <- apply(is.na(peatdat.prop),1,all) #check which rows are empty
      if(sum(empty.row) > 0) {peatdat.prop <- peatdat.prop[-which(apply(is.na(peatdat.prop),1,all)),]}
      #change to proper column names and fix sites with wrong names
      colnames(peatdat.prop) <- c("depth","age_Loisel_et_al_2014", "bulk_density_g_cm3", "org_mat_percent", "org_mat_density_g_OM_cm3",  
                                  "C_content", "N_content", "org_C_density_g_C_cm3", "peat_type","peat_type_Loisel_et_al_2014")
      
      #check if more cores are available
      if(nrow(meta.long) > 1) {
        nrows <- numeric()
        for (j in 1:(nrow(meta.long)-1)) { # -1 b/c first core has no number on depth variable
          lab <- paste("depth.",j, sep = "")
          if(lab %in% colnames(peatdat)) { 
            peatdat.prop2 <- peatdat[, which(colnames(peatdat)==lab):(which(colnames(peatdat)==lab) + 9) ]
            colnames(peatdat.prop2) <- colnames(peatdat.prop)
            #remove empty rows
            empty.row <- apply(is.na(peatdat.prop2),1,all) #check which rows are empty
            if(sum(empty.row) > 0) {peatdat.prop2 <- peatdat.prop2[-which(apply(is.na(peatdat.prop2),1,all)),]}
            #bind together the cores
            if(j == 1) {peatdat.prop.ex <- peatdat.prop2} else
            {peatdat.prop.ex <- rbind(peatdat.prop.ex, peatdat.prop2)}
            nrows[j] <- nrow(peatdat.prop2)
          }
        }
        #merge meta data and peat prop data
        mm <- meta.long[rep(c(1:nrow(meta.long)), times=c(nrow(peatdat.prop),nrows)), ] #copy first rows to fill out df  
        peatdat.com <- cbind(rbind(peatdat.prop, peatdat.prop.ex), mm)
      }
      
      # One core. Merge meta data and peat prop data
      if(nrow(meta.long) == 1) {
        mm <- meta.long[rep(1, nrow(peatdat.prop)), ] #copy first row to fill out df  
        peatdat.com <- cbind(peatdat.prop, mm)
      }
      #merge the ith data set
      if(i == 1) {peatdat.com.all <- peatdat.com} else
      {peatdat.com.all <- rbind(peatdat.com.all, peatdat.com)}
    }
    
    # and now just convert only the numeric variables
    # find the numeric variables and convert them
    numvar <- apply(peatdat.com.all, 2, function (x) all(!grepl("[a-zA-Z]", x)))
    peatdat.com.all[, numvar] <- as.numeric(as.character(unlist(peatdat.com.all[, numvar])))
    peatdat.com.all <- droplevels(peatdat.com.all) #may need to clean up
    closeAllConnections()
  } 
  
  #what to return
  if(only_meta == TRUE) {
    class(site.info) <- append(class(site.info),"peatMeta")
    return(site.info)
  } else {
    class(peatdat.com.all) <- append(class(peatdat.com.all),"peatData")
    return(peatdat.com.all)
  }
  }
