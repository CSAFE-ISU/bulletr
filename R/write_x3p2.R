#' @export
write_x3p = function( x , ... )
{
  UseMethod( "write_x3p" )
}

#' internal helper function
xml_set_text <- function(x, value) {
  if (is.null(value) || length(value) == 0) 
    value="N/A"
  xml2::xml_set_text(x, value)
}

#' Write an x3p file taking Lists: general.info, feature.info, matrix.info and matrix: surface.matrix as inputs
#' 
#' @param x Surface Matrix with the x y z values to be written (variable type: matrix)
#' @param file where should the file be stored?
#' @param header.info header info of x3p
#' @param general.info Setting the Values for the XML to a list
#' @param feature.info Setting the Values for the XML to a list
#' @param matrix.info Setting the Values for the XML to a list
#' @param profiley If FALSE, reorient the matrix to ensure a profile is taken is consistent with surface.matrix (input variable). The default value of Profiley is TRUE
#' 
#' @export
#' @import xml2
#' @importFrom utils zip
#' @importFrom digest digest
#' @method write_x3p default
#' 
#' @examples
#' \dontrun{
#'  # use all defaults:
#'  write_x3p(surface.matrix=surface.matrix, file="out.x3p", profiley = FALSE)
#'  
#' }
write_x3p.default<- function(x, file, header.info= x$header.info, general.info=NULL, feature.info=NULL, matrix.info=NULL,  profiley= TRUE)
{
  surface.matrix <- x
  if (is.null(general.info)) {
    cat("general info not specified, using template\n")
    general.info = internal$Record2
  }
  if (is.null(feature.info)) {
    cat("feature info not specified, using template\n")
    feature.info = internal$Record1

  }
  if (is.null(matrix.info)) {
    cat("matrix info not specified, using template\n")
    matrix.info = internal$Record3

  }
  
  feature.info$Axes$CX$Increment = header.info$obs_inc
  feature.info$Axes$CY$Increment = header.info$profile_inc
  
  matrix.info$MatrixDimension$SizeX = header.info$num_obs_per_profile
  matrix.info$MatrixDimension$SizeY = header.info$num_profiles
  
  # Function to assign values to the Record 2 in the main.XML
  record2.assign<- function(a1, record2.data){
    
    xml_set_text(xml_child(a1, search = "Record2/Date"), as.character(record2.data$Date) )
    xml_set_text(xml_child(a1, search = "Record2/Creator"), as.character(record2.data$Creator) )
    xml_set_text(xml_child(a1, search = "Record2/Instrument/Manufacturer"), as.character(record2.data$Instrument$Manufacturer) )
    xml_set_text(xml_child(a1, search = "Record2/Instrument/Model"), as.character(record2.data$Instrument$Model))
    xml_set_text(xml_child(a1, search = "Record2/Instrument/Serial"), as.character(record2.data$Instrument$Serial) )
    xml_set_text(xml_child(a1, search = "Record2/Instrument/Version"), as.character(record2.data$Instrument$Version) )
    xml_set_text(xml_child(a1, search = "Record2/CalibrationDate"), as.character(record2.data$CalibrationDate) )
    xml_set_text(xml_child(a1, search = "Record2/ProbingSystem/Type"), as.character(record2.data$ProbingSystem$Type))
    xml_set_text(xml_child(a1, search = "Record2/ProbingSystem/Identification"), as.character(record2.data$ProbingSystem$Identification))
    xml_set_text(xml_child(a1, search = "Record2/Comment"), as.character(record2.data$Comment))
    
  }
  
  # Function to assign values to the Record 1 in the main.XML
  record1.assign<- function(a1, record1.data){
    
    xml_set_text(xml_child(a1, search = "Record1/Revision"), as.character(record1.data$Revision) )
    xml_set_text(xml_child(a1, search = "Record1/FeatureType"), as.character(record1.data$FeatureType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CX/AxisType"), as.character(record1.data$Axes$CX$AxisType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CX/DataType"), as.character(record1.data$Axes$CX$DataType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CX/Increment"), as.character(record1.data$Axes$CX$Increment) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CX/Offset"), as.character(record1.data$Axes$CX$Offset) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CY/AxisType"), as.character(record1.data$Axes$CY$AxisType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CY/DataType"), as.character(record1.data$Axes$CY$DataType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CY/Increment"), as.character(record1.data$Axes$CY$Increment) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CY/Offset"), as.character(record1.data$Axes$CY$Offset) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/AxisType"), as.character(record1.data$Axes$CY$AxisType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/DataType"), as.character(record1.data$Axes$CZ$DataType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/Increment"), as.character(record1.data$Axes$CZ$Increment) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/Offset"), as.character(record1.data$Axes$CZ$Offset) )
  }
  
  # Function to assign values to the Record 3 in the main.XML
  record3.assign<- function(a1, record3.data){
    
    xml_set_text(xml_child(a1, search = "Record3/MatrixDimension/SizeX"), as.character(record3.data$MatrixDimension$SizeX) )
    xml_set_text(xml_child(a1, search = "Record3/MatrixDimension/SizeY"), as.character(record3.data$MatrixDimension$SizeY) )
    xml_set_text(xml_child(a1, search = "Record3/MatrixDimension/SizeZ"), as.character(record3.data$MatrixDimension$SizeZ) )
    xml_set_text(xml_child(a1, search = "Record3/DataLink/PointDataLink"), "bindat/data.bin")
  }
  # Storing the Working Dir path
  orig.path<- getwd()
  # Retrieving the Template XML file
  #data(template_writex3p, envir=environment())
  a1<- xml2::read_xml(paste0(find.package("bulletr", lib.loc=NULL, quiet = TRUE), "/templateXML.xml")) # gets messed up if it's stored as R object
  # Creating Temp directory and bin directory
  # 'File structure'
  dir.create("x3pfolder")
  dir.create("x3pfolder/bindata")

  # Change Working Dir 
  setwd(paste0(getwd(),"/x3pfolder"))
  new.wdpath<- getwd()
  # Assigning values to the Record 1 part of the XML
  record2.assign(a1, general.info)
  
  sizes<- c(matrix.info$MatrixDimension$SizeX, matrix.info$MatrixDimension$SizeY, matrix.info$MatrixDimension$SizeZ)
  sizes<- as.numeric(sizes)
  increments<- c(feature.info$Axes$CX$Increment, feature.info$Axes$CY$Increment, feature.info$Axes$CZ$Increment)
  increments<- as.numeric(increments)
  
  if (profiley) { #counter clock-wise rotation by 90 degrees
    sizes <- sizes[c(2, 1, 3)]
    increments <- increments[c(2, 1, 3)]
    
    #datamat <- t(datamat)
    surface.matrix <- t(apply(surface.matrix, 2, rev))
  }
  
  
  # Updating the list values
  matrix.info$MatrixDimension$SizeX<- as.character(sizes[1])
  matrix.info$MatrixDimension$SizeY<- as.character(sizes[2])
  matrix.info$MatrixDimension$SizeZ<- as.character(sizes[3])
  feature.info$Axes$CX$Increment<- as.character(1e-06*increments[1])
  feature.info$Axes$CY$Increment<- as.character(1e-06*increments[2])
  feature.info$Axes$CZ$Increment<- as.character(increments[3])
  
  # Updating the Records : main.xml.
  record3.assign(a1, matrix.info)
  record1.assign(a1, feature.info)
  

  # Writing the Surface Matrix as a Binary file
  writeBin(1e-6* as.vector((surface.matrix)), con = "bindata/data.bin")
  
  # Generating the MD% check sum
  chksum<- digest("bindata/data.bin", algo= "md5", serialize=FALSE, file=TRUE)
  xml_set_text(xml_child(a1, search = "Record3/DataLink/MD5ChecksumPointData"), chksum )
  
  # Assigning values to Record 4 in main.xml
  xml_set_text(xml_child(a1, search = "Record4/ChecksumFile"), "md5checksum.hex" )
  
  # Write the Main xml file
  write_xml(a1, "main.xml")
  
  # Writing the md5checksum.hex with checksum for the main.xml
  main.chksum<- digest("main.xml", algo= "md5", serialize=FALSE, file=TRUE)
  write(main.chksum, "md5checksum.hex")
    
  # Write the x3p file and reset path
  # create zipped file one level up, now get out and delete
  zip(zipfile = paste0("../",file), files = dir())
  setwd("./..")
  unlink("x3pfolder",recursive = TRUE)
  
  setwd(orig.path) 
  
}


#' Write an x3p file taking Lists: general.info, feature.info, matrix.info and matrix: surface.matrix as inputs
#' 
#' @param x x3pobject
#' @param file where should the file be stored?
#' @param profiley If FALSE, reorient the matrix to ensure a profile is taken is consistent with surface.matrix (input variable). The default value of Profiley is TRUE
#' 
#' @export
#' @import xml2
#' @importFrom digest digest
#' @importFrom utils zip
#' @method write_x3p x3p
#' 
#' @examples
#' \dontrun{
#'  # use all defaults:
#'  write_x3p(surface.matrix=surface.matrix, file="out.x3p", profiley = FALSE)
#'  
#' }
write_x3p.x3p<- function(x, file, profiley= TRUE) {
  write_x3p(x = x$surface.matrix, header.info= x$header.info, general.info=x$general.info, feature.info = x$feature.info,
            matrix.info = x$matrix.info, file=file, profiley=profiley)
}
  