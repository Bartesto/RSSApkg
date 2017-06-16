# utility functions

# function to get dates
# u_dates

path <- "W:/usgs/115078"
archive <- TRUE

path <- paste0("Z:/DOCUMENTATION/BART/R/R_DEV/devtest")
archive2 <- FALSE
pat <- ".jpeg"

setwd(path)

u_leapR <- function(path){
  allfiles <- list.files(path = path, recursive = TRUE)
  result <- allfiles[grepl("*pre.ers", allfiles)]
  result <- result[!grepl("^[a-zA-Z]", result)]
  #date for folder
  fold <- substr(result, 1, 8)
  fdate <- as.character(lubridate::ymd(fold))
  #date for image
  ldate <- as.character(lubridate::dmy(substr(result, 21, 26)))
  #find mismatch
  bad.fold.dates <- setdiff(fdate, ldate)
  if(length(bad.fold.dates) > 0){
    #correct the folder names and path
    corr.fold.dates <- lubridate::ymd(bad.fold.dates) - 1
    corr.fold <- gsub("-", "", as.character(corr.fold.dates))
    new.name <- paste0(path, "\\", corr.fold)
    #old folder names and path to correct
    bad.fold <- gsub("-", "", as.character(bad.fold.dates))
    old.name <- paste0(path, "\\", bad.fold)
    #rename folders
    file.rename(old.name, new.name)
  } else {
    cat("No leap date folder errors")
  }

}
u_dateR <- function(path, archive, pat = ".jpeg"){
  if(archive == TRUE){
    u_leapR(path)
    prefolds <- list.files(path = path, pattern = "*pre.ers$", recursive = TRUE)
    ind <- grepl("^[[:digit:]]", prefolds)
    prefolds <- prefolds[ind]
    datesdf <- data.frame(path = paste0(path, "/", prefolds),
                          folds = substr(prefolds, 1, 8),
                          dates = lubridate::ymd(substr(prefolds, 1, 8)),
                          stringsAsFactors = FALSE)
    return(datesdf)
  } else {
    #get jpeg dirs
    dirs <- list.dirs()
    dirs <- dirs[grepl("/jpegs_", dirs)]

    #create list
    qadlist <- vector("list", length(dirs))
    names(qadlist) <- dirs

    for(h in 1:length(dirs)){
      jpegs <- list.files(path = dirs[h], pattern = "*.jpeg")
      qadates <- substr(jpegs, 1, 10)
      qafolds <- stringr::str_replace_all(qadates, "[^[:alnum:]]", "")
      qadlist[[h]] <- paste0("holder", "/", qafolds)
    }
    # files <- list.files(path = path, pattern = pat)
    # datesdf <- data.frame(folds = gsub("-", "", substr(files, 1, 10)),
    #                       dates = lubridate::ymd(substr(files, 1, 10)),
    #                       stringsAsFactors = FALSE)
    return(qadlist)
  }
}
u_shpsplitR <- function(pathin = ".", pathout = "./site_vectors", layer, attrb){
  if(!file.exists("site_vectors")){ dir.create("site_vectors")}
  data <-  rgdal::readOGR(dsn = pathin, layer)
  sites <- as.character(unique(data@data[,attrb]))
  for(i in 1:length(sites)){
    tmp <- data[data@data[,attrb] == sites[i], ]
    rgdal::writeOGR(tmp, dsn = pathout, sites[i], driver = "ESRI Shapefile",
                    overwrite_layer = TRUE)
    write.table(layer, paste0(pathout, "/OriginShapeFile.txt"),
                row.names = FALSE, quote = FALSE, col.names = FALSE)
  }
  return(sites)
}
test <- u_dateR(path, archive)
test2 <- u_dateR(path2, archive2)

wdir <- "Z:/DOCUMENTATION/BART/R/R_DEV/devtest"
imdir <- "W:/usgs/108083"
#setwd(wdir)
layer <- "SWWMP_wetlands_10883_20170227"
attrb <- "WCode"
start <- "1/1/2000"
stop <- "01/06/2000"
combo <- c(5,4,2)
#buffer <- 2000

jpegR <- function(wdir=".", imdir, layer, attrb, start=NA, stop=NA, combo,
                  buffer = 2000){
  #split layer, get names
  shpnames <- u_shpsplitR(layer = layer, attrb = attrb)

  #get folder/date imagery info
  alldo <- u_dateR(path = imdir, archive = TRUE)
  suppressWarnings(start <- lubridate::dmy(start))
  suppressWarnings(stop <- lubridate::dmy(stop))
  todo <- if(!is.na(start) & !is.na(stop)){
    sub <- subset(alldo, dates >= start & dates <= stop)#start and stop
    sub
  } else if(is.na(start) & !is.na(stop)){
    sub <- subset(alldo, dates <= stop)#stop only
    sub
  } else if(!is.na(start) & is.na(stop)){
    sub <- subset(alldo, dates >= start)#start only
    sub
  } else {
    alldo
  }

  #grab CRS of rasters for transform of layers
  proj <- raster::crs(raster::raster(todo[1,1]))

  #make the jpegs
  for(i in seq_along(shpnames)){
    shp <- rgdal::readOGR(dsn = "./site_vectors", shpnames[i])
    shp_t <- sp::spTransform(shp, proj)
    ext <- raster::extent(shp_t) + buffer
    beg <- todo[1,2]
    end <- todo[length(todo[,2]), 2]
    folder <- paste0("jpegs_site_", shpnames[i], "_", beg, "-", end)
    if(!file.exists(folder)){ dir.create(folder)}
    for(j in seq_along(todo[,1])){
      date <- todo[j,"dates"]
      jname <- paste0(date, "-", paste(combo, collapse=""), ".jpeg")
      fname <- paste0("./", folder, "/", jname)
      img <- todo[j,"path"]
      rstack <- raster::stack(img)
      jpeg(filename = fname, width = 842, height = 870)
      raster::plotRGB(rstack, r = combo[1], g = combo[2], combo[3],
                      ext = ext, stretch = "lin")
      raster::plot(shp_t, add = TRUE, lwd = 2, border = "magenta")
      dev.off()
    }

  }

}

jpegR(wdir, imdir, layer, attrb, start, stop, combo, buffer = 2000)

##extract new
wdir <- "Z:/DOCUMENTATION/BART/R/R_DEV/devtest"
imdir <- "W:/usgs/108083"
option <- "i35"
attrb <- "WCode"
setwd(wdir)
extract(wdir, imdir, option, attrb)

extract <- function(wdir = ".", imdir, option, attrb){
  #function for right substring
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  #get jpeg folders
  jfolds <- list.files(path = wdir, pattern = "jpegs_site")

  #get vector folder
  vfold <- "./site_vectors"

  #copy shape files to relevant QA folders
  shpfiles <- list.files(path = vfold, pattern = "*.shp$")
  shpnames <- unlist(strsplit(shpfiles, split = "\\."))[c(TRUE,FALSE)]
  for(i in seq_along(shpnames)){
    shp <- shpnames[i]
    patt1 <- paste0("^", shp, "\\.")
    shps <- list.files(path = vfold, pattern = patt1)
    from <- paste0(vfold,"/", shps)
    patt2 <- paste0("site_", shp, "_")
    to <- paste0(wdir, "/", jfolds[grep(patt2, jfolds)])
    file.copy(from=from, to=to, recursive = FALSE, overwrite = TRUE,
              copy.mode = TRUE)
  }

  #get jpeg dates
  jlist <- vector("list", length(jfolds))
  names(jlist) <- jfolds

  for(j in seq_along(jfolds)){
    jpegs <- list.files(path = paste0("./", jfolds[j]), pattern = "*.jpeg")
    jdates <- substr(jpegs, 1, 10)
    ifolds <- stringr::str_replace_all(jdates, "[^[:alnum:]]", "")
    jlist[[j]] <- paste0(imdir, "/", ifolds)
  }

  #empty list for storage of all csvs at end
  resultslist <- vector("list", length(jfolds))

  #extract to csv
  for(k in seq_along(jlist)){
    #get site shape file
    shpk <- shpnames[k]
    sitesSHP <- rgdal::readOGR(dsn = paste0("./", names(jlist[k])), shpk)
    rnames <- as.character(sitesSHP@data[, attrb])

    #get beginning and end dates
    beg <- substrRight(jlist[[k]][1], 8)
    end <- substrRight(jlist[[k]][length(jlist[[k]])], 8)

    #make sure same projections (shape to raster)
    rfile <- list.files(path = jlist[[1]][1], pattern = "pre.ers", full.names = TRUE)[1]
    rastproj <- raster::crs(raster::raster(rfile))
    sitesSHPt <- sp::spTransform(sitesSHP, rastproj)

    #empty df for results and options vector
    results <- as.data.frame(matrix(ncol=length(rnames) + 1,
                                    nrow = length(jlist[k][[1]]),
                                    dimnames=list(NULL, c("date", rnames))),
                             stringsAsFactors = FALSE)
    options <- c("i35", "ndvi", "b1", "b2", "b3", "b4", "b5", "b6")


    #extract from imagery rasters
    for(m in seq_along(jlist[k][[1]])){

      #image file name
      imfile <- list.files(path = jlist[k][[1]][m], pattern = "pre.ers",
                           full.names = TRUE)

      #make sure only one pre and return complete path
      if(length(imfile) > 1){
        imdat <- imfile[grepl("USG", imfile)]
      } else { imdat <- imfile}

      #grab path row and date
      prow <- substr(imdat, 9, 14)
      date <- lubridate::ymd(substr(imdat, 16, 23))

      #extract by option
      if(option == "i35"){
        b3r <- raster::raster(imdat, band = 3)
        b5r <- raster::raster(imdat, band = 5)
        b3x <- raster::extract(b3r, sitesSHPt, fun = mean, na.rm = TRUE)
        b5x <- raster::extract(b5r, sitesSHPt, fun = mean, na.rm = TRUE)
        i35 <- (b3x + b5x)/2
        out <- i35[1,1]
      } else if (option == "ndvi"){
        b3r <- raster::raster(imdat, band = 3)
        b4r <- raster::raster(imdat, band = 4)
        b3x <- raster::extract(b3r, sitesSHPt, fun = mean, na.rm = TRUE)
        b4x <- raster::extract(b4r, sitesSHPt, fun = mean, na.rm = TRUE)
        ndvi <- (b4x - b3x)/(b4x + b3x)
        out <- ndvi[1,1]
      } else if (option == "b1"){
        b1r <- raster::raster(imdat, band = 1)
        b1x <- raster::extract(b1r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b1x[1,1]
      } else if (option == "b2"){
        b2r <- raster::raster(imdat, band = 2)
        b2x <- raster::extract(b2r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b2x[1,1]
      } else if (option == "b3"){
        b3r <- raster::raster(imdat, band = 3)
        b3x <- raster::extract(b3r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b3x[1,1]
      } else if (option == "b4"){
        b4r <- raster::raster(imdat, band = 4)
        b4x <- raster::extract(b4r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b4x[1,1]
      } else if (option == "b5"){
        b5r <- raster::raster(imdat, band = 5)
        b5x <- raster::extract(b5r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b5x[1,1]
      } else {
        b6r <- raster::raster(imdat, band = 6)
        b6x <- raster::extract(b6r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b6x[1,1]
      }

      results[m,1] <- as.character(date)
      results[m,2] <- out

    }

    ## write out results per shp file
    write.csv(file = paste0("./", jfolds[k], "/", prow,"_", option, "_", shpk,
                            "_", beg, "-", end, ".csv"),
              x = results, row.names = FALSE)
    resultslist[[k]] <- results

  }
  ## Combine all results in one data source ####################################

  #combine all data into 1 csv
  #get all poss image dates

  unlist(lapply(jlist, "[[", 1))
  last_elems <- unlist(lapply(jlist, head, n = 1L))
  endall <- sort(substrRight(last_elems, 8))

  allfolds <- suppressWarnings(lubridate::ymd(list.files(path = imdir)))
  alldates <- allfolds[!is.na(allfolds)]
  alldat <- data.frame(date = as.character(alldates), stringsAsFactors = FALSE)


  for(n in seq_along(resultslist)){
    alldat <- dplyr::left_join(alldat, resultslist[[n]], "date")
  }
  write.csv(x = alldat, file = paste0(prow, "_", option, "_QA_", beg, "-", end,
                                  ".csv"))
}


