#' Create small jpegs of satellite imagery.
#'
#' \code{jpegR} makes small jpeg images centered on and showing site locations
#' from a shape file. This function can be used as part of the cloud quality
#' check process prior to extracting satellite bands or indices. Although
#' designed to help the QA process the jpegs can be useful for quickly checking
#' many image dates for change (e.g. fires, vegetation removal, etc) or for
#' creating stills for animating. When run the function:
#' \enumerate{
#'     \item Obtains a list of imagery folders to access. In doing so it also
#'     checks for folder name errors (\code{\link{u_leapR}}) and only returns
#'     sun-corrected image folders.
#'     \item Splits a supplied shape file by unique attribute field entries,
#'     stores results in a folder and writes a text file containing the original
#'     shape file name (\code{\link{u_shpsplitR}}).
#'     \item Creates a small jpeg of the individual sites for each image date as
#'     per step 1. jpegs are based on band combination from user input and
#'     buffered by a desired distance.
#'     \item All outputs are placed in appropriately named folders per site in
#'     the working directory
#'     }
#'
#' @param wdir Character string filepath to the working directory where the
#' original shape file is located.
#' @param imdir Character string filepath to the path/row level for imagery.
#' @param layer Character string of the name of the shape file (no extension).
#' @param attrb Character string of the name of the field in the attribute
#' table of the shape file that contains the unique entries (e.g.site numbers).
#' @param start Character string representing start date of imagery search. Must
#' be in the form "dmY" or "dmy" and can include separators. Defaults to NA so
#' image search starts from first available image in the RSSA archive.
#' @param stop Character string representing end date of imagery search. Must
#' be in the form "dmY" or "dmy" and can include separators. Defaults to NA so
#' image search ends with latest available image in the RSSA archive.
#' @param combo Numerics representing bands to place in RGB channels of the jpeg.
#' Must be of the form c(N\emph{r}, N\emph{g}, N\emph{b}) where N\emph{r} is the
#' band number for the red channel etc.
#' @param buffer Numeric representing how much to buffer out the site to control
#' zoom level for jpegs. If imagery crs is projected, number represents metres.
#' If imagery is geodetic then the number represents degrees.
#'
#' @return Creates named folders, based on site location and dates of images
#' processed, containing small jpeg images for visual assessment.
#'
#' @author Bart Huntley, \email{bart.huntley@@dpaw.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' jpegR(wdir = "z:/working", imdir = "W:/usgs/115078", layer = "site_locations", attrb = "ID", start = "1/1/98", stop = "28-12-2009", combo = c(5,4,2), buffer = 2000)
#' }
#'
#' @export
#' @import raster
#' @importFrom rgdal readOGR
#' @importFrom lubridate dmy
#' @importFrom sp spTransform
#' @importFrom grDevices dev.off jpeg
jpegR <- function(wdir, imdir, layer, attrb, start=NA, stop=NA, combo,
                  buffer = 2000){
  #split layer, get names
  shpnames <- u_shpsplitR(pathin = wdir, layer = layer, attrb = attrb)

  #get folder/date imagery info
  alldo <- u_dateR(path = imdir, archive = TRUE)
  suppressWarnings(start <- lubridate::dmy(start))
  suppressWarnings(stop <- lubridate::dmy(stop))
  todo <- if (!is.na(start) & !is.na(stop)){
    sub <- subset(alldo, dates >= start & dates <= stop)#start and stop
    sub
  } else if (is.na(start) & !is.na(stop)){
    sub <- subset(alldo, dates <= stop)#stop only
    sub
  } else if (!is.na(start) & is.na(stop)){
    sub <- subset(alldo, dates >= start)#start only
    sub
  } else {
    alldo
  }

  #grab CRS of rasters for transform of layers
  proj <- raster::crs(raster::raster(todo[1, 1]))

  #make the jpegs
  for (i in seq_along(shpnames)){
    shp <- rgdal::readOGR(dsn = paste0(wdir, "/site_vectors"), shpnames[i])
    shp_t <- sp::spTransform(shp, proj)
    ext <- raster::extent(shp_t) + buffer
    beg <- todo[1, 2]
    end <- todo[length(todo[, 2]), 2]
    folder <- paste0(wdir, "/jpegs_site_", shpnames[i], "_", beg, "-", end)
    if (!file.exists(folder)){
      dir.create(folder)
      }
    for (j in seq_along(todo[, 1])){
      date <- todo[j, "dates"]
      jname <- paste0(date, "-", paste(combo, collapse = ""), ".jpg")
      fname <- paste0(folder, "/", jname)
      img <- todo[j, "path"]
      rstack <- raster::stack(img)
      jpeg(filename = fname, width = 842, height = 870)
      raster::plotRGB(rstack, r = combo[1], g = combo[2], combo[3],
                      ext = ext, stretch = "lin")
      raster::plot(shp_t, add = TRUE, lwd = 2, border = "magenta")
      dev.off()
    }

  }

}
