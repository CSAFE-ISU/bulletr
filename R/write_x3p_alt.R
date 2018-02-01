#' @export
write_x3p_alt = function( x , ... )
{
  UseMethod( "write_x3p_alt" )
}

# ##' internal helper function
# xml_set_text <- function(x, value) {
#   if (is.null(value) || length(value) == 0) 
#     value="N/A"
#   xml2::xml_set_text(x, value)
# }

# ##' List Empty field finder
# null_list_entry <- function(x){
#   if((length(x) == 0) || is.null(x[[1]])) 
#     x<- "N/A"#return(TRUE) 
#   else 
#     purrr::map(x, null_list_entry)
# }


#' Alternative Write function to write an x3p file taking Lists: general.info, feature.info, matrix.info and matrix: surface.matrix bulet_info as inputs
#' 
#' @param x Surface Matrix with the x y z values to be written (variable type: matrix)
#' @param file where should the file be stored?
#' @param header.info header info of x3p
#' @param general.info Setting the Values for the XML to a list
#' @param feature.info Setting the Values for the XML to a list
#' @param matrix.info Setting the Values for the XML to a list
#' @param bullet_info xml file from x3p file
#' @param profiley If FALSE, reorient the matrix to ensure a profile is taken is consistent with surface.matrix (input variable). The default value of Profiley is TRUE
#' @param template path to an xml file with meta information to be saved as part of the x3p file.
#' 
#' @export
#' @import xml2
#' @importFrom utils zip
#' @importFrom digest digest
#' @method write_x3p_alt default
#' 
#' @examples
#' \dontrun{
#'  # use all defaults:
#'  write_x3p_alt(surface.matrix=surface.matrix, file="out.x3p", profiley = FALSE)
#'
#' Examples to test various methods of using a specified template and without it.
#' a<- read_x3p(file.choose())
#' b<- read_dat(file.choose())
#' specified_template<- "./inst/shiny-examples/wrApp/defaultTemplateXML.xml"
#' # Write DAT file without specified template
#' write_x3p_alt(b, "test.x3p", profiley = T)
#' check<- read_x3p("./test.x3p")
#' # Write DAT file with specified template
#' write_x3p_alt(b, "test.x3p", profiley = T, template = specified_template)
#' check<- read_x3p("./test.x3p")
#' # Write x3p file with specified template
#' write_x3p_alt(a, "test.x3p", profiley = T, template = specified_template)
#' check<- read_x3p("./test.x3p")
#' # Write x3p file without specified template
#' write_x3p_alt(a, "test.x3p", profiley = T)
#' check<- read_x3p("./test.x3p")
#'  
#' }
write_x3p_alt.default<- function(x, file, header.info= x$header.info, general.info=x$general.info, feature.info=x$feature.info, matrix.info=x$matrix.info, bullet_info = x$bullet_info, profiley= TRUE, template=NULL)
{
  surface.matrix <- x
  
  ###############################################################################
  # The following section takes care of 4 cases in the following order
  # 1. DAT file no specified template
  #    Will use the information from a system default template
  #    (both the structure of the main.xml and the contents of the meta-info)
  #    * Vital information is duely subsituted using header.info
  # 2. DAT file for a specified template
  #    Will ue the information from a specified template
  #    (both the structure of the main.xml and the contents of the meta-info)
  #    * Vital information is duely subsituted using header.info
  # 3. x3p file for a specified template
  #    The main.xml structure and meta information will be as specified in the
  #    specified template.
  #    * Vital information is duely subsituted using header.info
  #    As long as the field with Vital information that pertain to the dimensions,
  #    increments are called the same and remain the same, any new fields added or 
  #    removed should still be added or removed in accordance with the specified
  #    template.
  # 4. x3p file for no specified template 
  #    The 4th case uses the general.info etc from the x3p file
  #    it also makes sure that the structure of the xml as read in using
  #    read_x3p is maintained. This takes care of the situation if there are
  #    more or less number of fields in the x3p in comparison to the default
  #    template.
  #    * Vital information is duely subsituted using header.info
  
  k<- 0 # Indicator Variable, when k = 1 used to pick out case 3 : x3p with specified template
  if(is.null(bullet_info)){ # When bullet_info = NULL for DAT files
      if (!is.null(template)) {  # Retrieving the Template XML file from path provided
        cat(" bullet_info = Not present, XMLstructure = Specified Template, Template = Specified Template\n  Meta_info comes from Specified Template\n")
        a1<- xml2::read_xml(template) 
      } else { # Retrieves template when no path is provided & bullet_info = NULL
        cat(" bullet_info = Not present, XMLstructure = default template, Template = default template\n  Meta_info comes from Default Template\n")
        a1 <- xml2::read_xml(system.file("templateXML.xml", package="bulletr"))
      }
      a1list<- as_list(a1)
      tmp<- as.relistable(a1list) # strucutre needed for compiling xml_document
  } else{ # When bullet_info is not null for x3p files
      if (!is.null(template)) {  # Retrieving the Template file from path provided
        cat(" bullet_info = present, XMLstructure = Specified template, Template = Specified template\n  Meta_info comes from Specified Template\n")
        a1<- xml2::read_xml(template) 
        a1list<- as_list(a1)
        tmp<- as.relistable(a1list)
        k <- 1 # Indicator so that general.info, feature.info, matrix.info read in from x3p file is not used.
        # Template fields are used for these three lists. The 'vital info' is still retained from the x3p read in and 
        # is duely substitued at the right places.
      } else {
        # Using Bullet_info template
        # Using read_x3p generated data for *.info lists
        cat(" bullet_info = present, XMLstructure = bullet_info, Template = Default template\n Meta_info comes from x3p input\n")
        
        # The following sets the fields to be used from the template
        a1<- xml2::read_xml(system.file("templateXML.xml", package="bulletr")) 
        a1list<- as_list(a1)
        
        # The following sets up structure for writing out main.xml
        b<- bullet_info
        blist<- as_list(b)
        tmp<- as.relistable(blist)
        
      }
 
  }
  ############################################################################### 

 
  
  if (is.null(general.info)|| k ==1) {
    cat("general info not specified, using template\n")
    general.info <- a1list[[1]]$Record2
  }
  
  if (is.null(feature.info)|| k ==1) {
    cat("feature info not specified, using template\n")
    feature.info <- a1list[[1]]$Record1
    
  }
  if (is.null(matrix.info)|| k ==1) {
    cat("matrix info not specified, using template\n")
    matrix.info <- a1list[[1]]$Record3
    
  }
  
  feature.info$Axes$CX$Increment <- header.info$obs_inc
  feature.info$Axes$CY$Increment <- header.info$profile_inc
  
  matrix.info$MatrixDimension$SizeX <- header.info$num_obs_per_profile
  matrix.info$MatrixDimension$SizeY <- header.info$num_profiles
  matrix.info$MatrixDimension$SizeZ<- as.numeric(matrix.info$MatrixDimension$SizeZ)
  # Storing the Working Dir path
  orig.path<- getwd()
  # Creating Temp directory and bin directory
  # 'File structure'
  dir.create("x3pfolder")
  dir.create("x3pfolder/bindata")
  
  # Change Working Dir 
  setwd(paste0(getwd(),"/x3pfolder"))
  new.wdpath<- getwd()
  # Assigning values to the Record 1 part of the XML
  a1list[[1]]$Record2 <- general.info
  
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
  
  # Updating the Records in list for: main.xml.
  a1list[[1]]$Record3 <- matrix.info
  a1list[[1]]$Record3$DataLink$PointDataLink<- "bindata/data.bin"
  a1list[[1]]$Record1 <- feature.info
  
  
  
  # Writing the Surface Matrix as a Binary file
  writeBin(1e-6* as.vector((surface.matrix)), con = "bindata/data.bin")
  
  # Generating the MD% check sum
  chksum<- digest("bindata/data.bin", algo= "md5", serialize=FALSE, file=TRUE)
  a1list[[1]]$Record3$DataLink$MD5ChecksumPointData<- chksum
  
  
  # Assigning values to Record 4 in main.xml
  a1list[[1]]$Record4$ChecksumFile<- "md5checksum.hex"
  
  # Convert to xml
  final.xml.list<- relist(unlist(a1list), skeleton = tmp) #tmp structure used for writing the xml file
  a1xml<- as_xml_document(list(structure(list(final.xml.list))))
  
  #xml_attrs(a1xml)<- xml_attrs(a1)
  
  # Write the Main xml file
  write_xml(a1xml, "main.xml")
  
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


#' Write (alternate) an x3p file taking Lists: general.info, feature.info, matrix.info and matrix: surface.matrix as inputs
#' 
#' @param x x3pobject
#' @param file where should the file be stored?
#' @param profiley If FALSE, reorient the matrix to ensure a profile is taken is consistent with surface.matrix (input variable). The default value of Profiley is TRUE
#' @param template path to xml file with meta information
#' 
#' @export
#' @import xml2
#' @importFrom digest digest
#' @importFrom utils zip
#' @method write_x3p_alt x3p
#' 
#' @examples
#' \dontrun{
#'  # use all defaults:
#'  write_x3p_alt(surface.matrix=surface.matrix, file="out.x3p", profiley = FALSE)
#'  
#' }
write_x3p_alt.x3p<- function(x, file, profiley= TRUE, template=NULL) {
  write_x3p_alt(x = x$surface.matrix, header.info= x$header.info, general.info=x$general.info, feature.info = x$feature.info,
            matrix.info = x$matrix.info, file=file, profiley=profiley, template=template, bullet_info = x$bullet_info)
}

# Need to specify list complement for the xml_set_text inorder to detect empty or
# Null list values to be 'N/A'
# Later stage can completely get rid of general.info, matrix.info, feature.info
# and can use bullet_info instead. But compatibility with shinyapp has to fixed for that.