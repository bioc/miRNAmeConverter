###############################################################################
#
#  File containing getter methods for MiRNANameConverter class
#

##### dbconn ####
#' @title Get database connectdion
#'
#' @description This function returns an SQLiteConnection object
#'
#' @param this Object of class \code{MiRNAmeConverter}
#' @return An SQLiteConnection object
#' @examples
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' .dbconn(nc);
#' @author Stefan Haunsberger
# @rdname dbconn
# @export
setGeneric(
   name = ".dbconn",
   def = function(this) {
      standardGeneric(".dbconn");
   }
)

# @describeIn dbconn Database connection
setMethod(
   f = ".dbconn",
   signature(this = "MiRNANameConverter"),
   definition = function(this) {
      return(this@.dbconn);
   }
)

##### validVersions #####
#' @title Get valid versions
#'
#' @description  This function returns all valid miRBase versions
#' provided by the package.
#'
#' @param this Object of class \code{MiRNAmeConverter}
#' @return A numeric vector
#' @examples validVersions
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' validVersions(nc);
#' @author Stefan Haunsberger
#' @rdname validVersions
#' @export
setGeneric(
   name = "validVersions",
   def = function(this) {
      standardGeneric("validVersions");
   }
)

#' @describeIn validVersions Retrieve supported miRBase versions
setMethod(
   f = "validVersions",
   signature(this = "MiRNANameConverter"),
   definition = function(this) {
      return(this@.validVersions);
   }
)

##### currentVersion #####
#' @title Get current version
#'
#' @description This function returns the highest miRBase version
#' that is provided by the package.
#'
#' @details
#' The maximum miRBase version of the package is evaluated and set
#' in the object initialization.
#'
#' @param this Object of class \code{MiRNAmeConverter}
#' @return A numeric value
#' @examples
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' currentVersion(nc);
#' @author Stefan Haunsberger
#' @rdname currentVersion
#' @export
setGeneric(
   name = "currentVersion",
   def = function(this) {
      standardGeneric("currentVersion");
   }
)

#' @describeIn currentVersion Retrieve highest supported miRBase version
setMethod(
   f = "currentVersion",
   signature(this = "MiRNANameConverter"),
   definition = function(this) {
      return(this@.currentVersion);
   }
)

##### nOrganisms #####
#' @title Get number of organisms
#'
#' @description This function returns the number of different organisms
#' that are provided by the package.
#'
#' @details
#' The number of different organisms is evaluated and set
#' in the object initialization.
#'
#' @param this Object of class \code{MiRNAmeConverter}
#' @return A numeric value
#' @examples
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' nOrganisms(nc);
#' @author Stefan Haunsberger
#' @rdname nOrganisms
#' @export
setGeneric(
   name = "nOrganisms",
   def = function(this) {
      standardGeneric("nOrganisms");
   }
)

#' @describeIn nOrganisms Retrieve number of organisms
setMethod(
   f = "nOrganisms",
   signature(this = "MiRNANameConverter"),
   definition = function(this) {
      return(this@.nOrganisms);
   }
)

##### nTotalEntries #####
#' @title Get total number database entries
#'
#' @description This function returns the total number of entries contained
#' in the \code{mimat} table. The number is the sum of the entries of all
#' miRBase versions provided by the package.
#'
#' @details
#' The total number is evaluated and set in the object initialization.
#'
#' @param this Object of class \code{MiRNAmeConverter}
#' @return A numeric value
#' @examples
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' nTotalEntries(nc);
#' @author Stefan Haunsberger
#' @rdname nTotalEntries
#' @export
setGeneric(
   name = "nTotalEntries",
   def = function(this) {
      standardGeneric("nTotalEntries");
   }
)

#' @describeIn nTotalEntries Retrieve total number of miRNA entries
setMethod(
   f = "nTotalEntries",
   signature(this = "MiRNANameConverter"),
   definition = function(this) {
      return(this@.nTotalEntries);
   }
)

##### validOrganisms #####
#' @title Get valid organisms
#'
#' @description This function returns all organisms where mature miRNA names
#' are available in any of the provided miRBase versions.
#'
#' @details
#' The valid organisms are evaluated and set in the object initialization.
#'
#' @param this Object of class \code{MiRNAmeConverter}
#' @return A numeric value
#' @examples
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' nOrganisms(nc);
#' @author Stefan Haunsberger
#' @rdname validOrganisms
#' @export
setGeneric(
   name = "validOrganisms",
   def = function(this) {
      standardGeneric("validOrganisms");
   }
)

#' @describeIn validOrganisms Retrieve all supported organisms
setMethod(
   f = "validOrganisms",
   signature(this = "MiRNANameConverter"),
   definition = function(this) {
      return(this@.validOrganisms);
   }
)
