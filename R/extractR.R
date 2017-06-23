#' Extracts mean values from Landsat imagery based on locations provided
#' by shape files.
#'
#'  \code{extractR} will extract mean pixel values from dates, determined from
#'  quality assessed jpegs, of Landsat raster imagery, from locations as
#'  provided by shape files. Common usage is for extracting vegetation indices
#'  from field work or calibration sites. When run it will:
#'   \enumerate{
#'       \item Copy the appropriate shape files to the appropriate quality
#'       assessed jpeg folders based on attribute id.
#'       \item Looks in each jpeg folder to determine appropriate image dates
#'       i.e. stores the dates of the remaining jpegs after quality assessment.
#'       \item Based on user input, extracts mean band or vegetation index
#'       value per site from the shape file.
#'       \item Writes mean values to a .csv file and stores each in appropriate
#'       jpeg folder.
#'       \item On completion it will join all extracted value data sets into one
#'       .csv file.
#'       }
#'
#' @param wdir Character string filepath to the working directory where the
#' original shape files are located. This working directory must contain a
#' folder named ./site_vectors" which contains the individual shape files for
#' extraction.
#' @param imdir Character string filepath to the path/row level for imagery.
#' @param option A character string indicating one of the following "i35",
#' "ndvi", "b1", "b2", "b3", "b4", "b5", "b6".
#' @param attrb A character string of the name of field in the attribute column
#' of the shape file that contains the unique identifier for the location.
#' This must be the same as was used to create the jpegs and is usually the site
#' ID.
#'
#' @return Creates individual .csv files for extracted values per shape file and
#' another that collates them all.
#'
#'
#' @author Bart Huntley, \email{bart.huntley@dpaw.wa.gov.au}
#'
#' For more details see  \url{https://rspaw.github.io/RSSApkg/index.html}
#' {the RSSApkg website}
#'
#' @examples
#' \dontrun{
#' extractR(wdir = "z:/working", imdir = "W:/usgs/115078", option = "i35", attrb = "ID")
#' }
#'
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom rgdal readOGR
#' @importFrom raster crs raster extract
#' @importFrom sp spTransform
#' @importFrom lubridate ymd
#' @importFrom dplyr left_join
#' @importFrom utils write.csv write.table
extractR <- function(wdir, imdir, option, attrb){
  #function for right substring
  substrRight <- function(x, n){
    substr(x, nchar(x) - n + 1, nchar(x))
  }
  #get jpeg folders
  jfolds <- list.files(path = wdir, pattern = "jpegs_site")

  #get vector folder
  vfold <- paste0(wdir, "/site_vectors")

  #copy shape files to relevant QA folders
  shpfiles <- list.files(path = vfold, pattern = "*.shp$")
  shpnames <- unlist(strsplit(shpfiles, split = "\\.")) [c(TRUE, FALSE)]
  for (i in seq_along(shpnames)){
    shp <- shpnames[i]
    patt1 <- paste0("^", shp, "\\.")
    shps <- list.files(path = vfold, pattern = patt1)
    from <- paste0(vfold, "/", shps)
    patt2 <- paste0("site_", shp, "_")
    to <- paste0(wdir, "/", jfolds[grep(patt2, jfolds)])
    file.copy(from = from, to = to, recursive = FALSE, overwrite = TRUE,
              copy.mode = TRUE)
  }

  #get jpeg dates
  jlist <- vector("list", length(jfolds))
  names(jlist) <- jfolds

  for (j in seq_along(jfolds)){
    jpegs <- list.files(path = paste0(wdir, "/", jfolds[j]), pattern = "*.jpg")
    jdates <- substr(jpegs, 1, 10)
    ifolds <- stringr::str_replace_all(jdates, "[^[:alnum:]]", "")
    jlist[[j]] <- paste0(imdir, "/", ifolds)
  }

  #empty list for storage of all csvs at end
  resultslist <- vector("list", length(jfolds))

  #extract to csv
  for (k in seq_along(jlist)){
    #get site shape file
    shpk <- shpnames[k]
    sitesSHP <- rgdal::readOGR(dsn = paste0(wdir, "/", names(jlist[k])), shpk)
    rnames <- as.character(sitesSHP@data[, attrb])

    #get beginning and end dates
    beg <- substrRight(jlist[[k]][1], 8)
    end <- substrRight(jlist[[k]][length(jlist[[k]])], 8)

    #make sure same projections (shape to raster)
    rfile <- list.files(path = jlist[[1]][1], pattern = "pre.ers",
                        full.names = TRUE)[1]
    rastproj <- raster::crs(raster::raster(rfile))
    sitesSHPt <- sp::spTransform(sitesSHP, rastproj)

    #empty df for results and options vector
    results <- as.data.frame(matrix(ncol = length(rnames) + 1,
                                    nrow = length(jlist[k][[1]]),
                                    dimnames = list(NULL, c("date", rnames))),
                             stringsAsFactors = FALSE)


    #extract from imagery rasters
    for (m in seq_along(jlist[k][[1]])){

      #image file name
      imfile <- list.files(path = jlist[k][[1]][m], pattern = "pre.ers",
                           full.names = TRUE)

      #make sure only one pre and return complete path
      if (length(imfile) > 1){
        imdat <- imfile[grepl("USG", imfile)]
      } else {
        imdat <- imfile
      }

      #grab path row and date
      prow <- substr(imdat, 9, 14)
      date <- lubridate::ymd(substr(imdat, 16, 23))

      #extract by option
      if (option == "i35"){
        b3r <- raster::raster(imdat, band = 3)
        b5r <- raster::raster(imdat, band = 5)
        b3x <- raster::extract(b3r, sitesSHPt, fun = mean, na.rm = TRUE)
        b5x <- raster::extract(b5r, sitesSHPt, fun = mean, na.rm = TRUE)
        i35 <- (b3x + b5x) / 2
        out <- i35[1, 1]
      } else if (option == "ndvi"){
        b3r <- raster::raster(imdat, band = 3)
        b4r <- raster::raster(imdat, band = 4)
        b3x <- raster::extract(b3r, sitesSHPt, fun = mean, na.rm = TRUE)
        b4x <- raster::extract(b4r, sitesSHPt, fun = mean, na.rm = TRUE)
        ndvi <- (b4x - b3x) / (b4x + b3x)
        out <- ndvi[1, 1]
      } else if (option == "b1"){
        b1r <- raster::raster(imdat, band = 1)
        b1x <- raster::extract(b1r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b1x[1, 1]
      } else if (option == "b2"){
        b2r <- raster::raster(imdat, band = 2)
        b2x <- raster::extract(b2r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b2x[1, 1]
      } else if (option == "b3"){
        b3r <- raster::raster(imdat, band = 3)
        b3x <- raster::extract(b3r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b3x[1, 1]
      } else if (option == "b4"){
        b4r <- raster::raster(imdat, band = 4)
        b4x <- raster::extract(b4r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b4x[1, 1]
      } else if (option == "b5"){
        b5r <- raster::raster(imdat, band = 5)
        b5x <- raster::extract(b5r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b5x[1, 1]
      } else {
        b6r <- raster::raster(imdat, band = 6)
        b6x <- raster::extract(b6r, sitesSHPt, fun = mean, na.rm = TRUE)
        out <- b6x[1, 1]
      }

      results[m, 1] <- as.character(date)
      results[m, 2] <- out

    }

    ## write out results per shp file
    write.csv(file = paste0(wdir, "/", jfolds[k], "/", prow, "_", option, "_",
                            shpk, "_", beg, "-", end, ".csv"), x = results,
              row.names = FALSE)
    resultslist[[k]] <- results

  }

  #combine all data into 1 csv
  #get all poss image dates and subset to match data range
  first_elems <- unlist(lapply(jlist, head, n = 1L))
  first_date <- lubridate::ymd(sort(substrRight(first_elems, 8))[1])
  last_elems <- unlist(lapply(jlist, tail, n = 1L))
  last_date <- lubridate::ymd(sort(substrRight(last_elems, 8),
                                   decreasing = TRUE))[1]


  allfolds <- suppressWarnings(lubridate::ymd(list.files(path = imdir)))
  alldates <- allfolds[!is.na(allfolds)]
  subdates <- alldates[alldates >= first_date & alldates <= last_date]
  alldat <- data.frame(date = as.character(subdates), stringsAsFactors = FALSE)


  for (n in seq_along(resultslist)){
    alldat <- dplyr::left_join(alldat, resultslist[[n]], "date")
  }
  write.csv(x = alldat, file = paste0(wdir, "/", prow, "_", option, "_QA_", beg,
                                      "-", end, ".csv"))
}
