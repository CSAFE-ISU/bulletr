#' Write an x3p file taking Lists: general.info, feature.info, matrix.info and matrix: surface.matrix as inputs
#' 
#' @param general.info Setting the Values for the XML to a list
#' @param feature.info Setting the Values for the XML to a list
#' @param matrix.info Setting the Values for the XML to a list
#' @param surface.matrix Surface Matrix with the x y z values to be written (variable type: matrix)
#' @param profiley If FALSE, reorient the matrix to ensure a profile is taken is consistent with surface.matrix (input variable). The default value of Profiley is TRUE
#' 
#' @export
#' @import xml2
#' @import digest 
#' @importFrom utils zip
#' 
#' @examples
#' \dontrun{
#'  write_x3p(general.info, feature.info,matrix.info,surface.matrix, profiley = FALSE)
#'  data(example_input_writex3p, envir=environment())
#' }

write_x3p<- function(general.info, feature.info, matrix.info, surface.matrix, profiley= TRUE)
{
  
  # Function to assign values to the Record 2 in the main.XML
  record2.assign<- function(a1, record2.data){
    
    xml_set_text(xml_child(a1, search = "Record2/Date"), as.character(record2.data$date) )
    xml_set_text(xml_child(a1, search = "Record2/Creator"), as.character(record2.data$creator) )
    xml_set_text(xml_child(a1, search = "Record2/Instrument/Manufacturer"), as.character(record2.data$manufacturer) )
    xml_set_text(xml_child(a1, search = "Record2/Instrument/Model"), as.character(record2.data$model))
    xml_set_text(xml_child(a1, search = "Record2/Instrument/Serial"), as.character(record2.data$serial) )
    xml_set_text(xml_child(a1, search = "Record2/Instrument/Version"), as.character(record2.data$version) )
    xml_set_text(xml_child(a1, search = "Record2/CalibrationDate"), as.character(record2.data$calibrationdate) )
    xml_set_text(xml_child(a1, search = "Record2/ProbingSystem/Type"), as.character(record2.data$type))
    xml_set_text(xml_child(a1, search = "Record2/ProbingSystem/Identification"), as.character(record2.data$identification))
    xml_set_text(xml_child(a1, search = "Record2/Comment"), as.character(record2.data$comment))
    
  }
  
  # Function to assign values to the Record 1 in the main.XML
  record1.assign<- function(a1, record1.data){
    
    xml_set_text(xml_child(a1, search = "Record1/Revision"), as.character(record1.data$revision) )
    xml_set_text(xml_child(a1, search = "Record1/FeatureType"), as.character(record1.data$featuretype) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CX/AxisType"), as.character(record1.data$CXAxisType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CX/DataType"), as.character(record1.data$CXDataType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CX/Increment"), as.character(record1.data$CXIncrement) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CX/Offset"), as.character(record1.data$CXOffset) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CY/AxisType"), as.character(record1.data$CYAxisType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CY/DataType"), as.character(record1.data$CYDataType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CY/Increment"), as.character(record1.data$CYIncrement) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CY/Offset"), as.character(record1.data$CYOffset) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/AxisType"), as.character(record1.data$CZAxisType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/DataType"), as.character(record1.data$CZDataType) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/Increment"), as.character(record1.data$CZIncrement) )
    xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/Offset"), as.character(record1.data$CZOffset) )
  }
  
  # Function to assign values to the Record 3 in the main.XML
  record3.assign<- function(a1, record3.data){
    
    xml_set_text(xml_child(a1, search = "Record3/MatrixDimension/SizeX"), as.character(record3.data$SizeX) )
    xml_set_text(xml_child(a1, search = "Record3/MatrixDimension/SizeY"), as.character(record3.data$SizeY) )
    xml_set_text(xml_child(a1, search = "Record3/MatrixDimension/SizeZ"), as.character(record3.data$SizeZ) )
    xml_set_text(xml_child(a1, search = "Record3/DataLink/PointDataLink"), "bindat/data.bin")
  }
  # Storing the Working Dir path
  orig.path<- getwd()
  # Retrieving the Template XML file
  data(template_writex3p, envir=environment())
  a1<- template
  # Creating Temp directory and bin directory
  #' File structure'
  dir.create("x3pfolder")
  dir.create("x3pfolder/bindata")
  
  # Change Working Dir 
  setwd(paste0(getwd(),"/x3pfolder"))
  new.wdpath<- getwd()
  # Assigning values to the Record 1 part of the XML
  record2.assign(a1, general.info)
  
  sizes<- c(matrix.info$SizeX, matrix.info$SizeY, matrix.info$SizeZ)
  sizes<- as.numeric(sizes)
  increments<- c(feature.info$CXIncrement, feature.info$CYIncrement, feature.info$CZIncrement)
  increments<- as.numeric(increments)
  
  # Reorienting the Surface Matrix according to Profiley Information  
  if (profiley == FALSE && sizes[2] > sizes[1]) {
    sizes <- sizes[c(2, 1, 3)]
    increments <- increments[c(2, 1, 3)]
  }
  
  # Updating the list values
  matrix.info$SizeX<- as.character(sizes[1])
  matrix.info$SizeY<- as.character(sizes[2])
  matrix.info$SizeZ<- as.character(sizes[3])
  feature.info$CXIncrement<- as.character(1e-06*increments[1])
  feature.info$CYIncrement<- as.character(1e-06*increments[2])
  feature.info$CZIncrement<- as.character(increments[3])
  
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
  setwd("./..")
  zip(zipfile = 'output.x3p', files = "x3pfolder")
  unlink("x3pfolder",recursive = TRUE)
  
  setwd(orig.path) 
  
}

