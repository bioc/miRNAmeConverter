###############################################################################
#
#  File containing setter methods for MiRNANameConverter class
#

##### dbconn ####
#' @title Set database connection
#'
#' @description Set an SQLiteConnection object containing the miRBase versions.
#'
#' @param this Object of class \code{MiRNANameConverter}
#' @param value An object SQLiteConnection object
#' @return A \code{MiRNANameConverter} object
#
# @examples
# nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
# .dbconn(nc) =
#' @author Stefan Haunsberger
# @rdname dbconn-set
# @export
setGeneric(
   name = ".dbconn<-",
   def = function(this, value) {
      standardGeneric(".dbconn<-")
   }
)

# @describeIn dbconn-set
setReplaceMethod(
   f = ".dbconn",
   signature = "MiRNANameConverter",
   definition = function(this, value) {
      if (is.null(this@.dbconn)) {
         this@.dbconn = value;
      } else {
         warning("Static value set at object instantiation. Values not set.")
      }
      this;
   }
)

##### validVersions ####
#' @title Set valid versions
#'
#' @description Set version values that are supported by the package.
#'
#' @details The value for the highest versions is a static variable. It is
#' initialized in the initialization method when an instance of a
#' \code{MiRNANameConverter} class is created.
#'
#' @param this Object of class \code{MiRNANameConverter}
#' @param value A vector of numeric values
#' @return A \code{MiRNANameConverter} object
#'
# @examples
# nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
# validVersions(nc) = c(15.0, 16.0, 20.0);
#'
#' @author Stefan Haunsberger
# @rdname validVersions-set
# @export

setGeneric(
   name = "validVersions<-",
   def = function(this, value) {
      standardGeneric("validVersions<-");
   }
)

# @describeIn validVersions-set
setReplaceMethod(
   f = "validVersions",
   signature = "MiRNANameConverter",
   definition = function(this, value) {
      if (is.null(this@.validVersions)) {
         this@.validVersions = value;
      } else {
         warning("Static value set at object instantiation. Values not set.");
      }
      return(this);
   }
)

##### currentVersion ####
#' @title Set current version
#'
#' @description Set the highest version that is supported by the package.
#'
#' @details The value for the highest version is a static variable. It is
#' initialized in the initialization method when an instance of a
#' \code{MiRNANameConverter} class is created.
#'
#' @param this Object of class ''
#' @param value A \code{numeric} value
#' @return Object of class ''
#'
# @examples
# nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
# currentVersion(nc) = 21.0;
#'
#' @author Stefan Haunsberger
# @rdname currentVersion-set
# @export
setGeneric(
   name = "currentVersion<-",
   def = function(this, value) {
      standardGeneric("currentVersion<-");
   }
)

# @describeIn currentVersion-set
setReplaceMethod(
   f = "currentVersion",
   signature = "MiRNANameConverter",
   definition = function(this, value) {
      if (is.null(this@.currentVersion)) {
         this@.currentVersion = value;
      } else {
         warning("Static value set at object instantiation. Value not set.");
      }
      return(this);
   }
)

##### nOrganisms #####
#' @title Set number of organisms
#'
#' @description This function sets the number of different organisms
#' that are provided by the package.
#'
#' @details
#' The number of different organisms is evaluated and set
#' in the object initialization.
#'
#' @param this Object of class \code{MiRNANameConverter}
#' @param value An \code{integer} value
#' @return A \code{MiRNANameConverter} object
#'
# @examples
# nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
# nOrganisms(nc) = 228;
#'
#' @author Stefan Haunsberger
# @rdname nOrganisms-set
# @export
setGeneric(
   name = "nOrganisms<-",
   def = function(this, value) {
      standardGeneric("nOrganisms<-");
   }
)

# @describeIn nOrganisms-set
setReplaceMethod(
   f = "nOrganisms",
   signature(this = "MiRNANameConverter"),
   definition = function(this, value) {
      if (is.null(this@.nOrganisms)) {
         this@.nOrganisms = value;
      } else {
         warning("Static value set at object instantiation. Value not set.");
      }
      return(this);
   }
)

##### nTotalEntries #####
#' @title Set total number database entries
#'
#' @description This function sets the total number of entries contained
#' in the \code{mimat} table. The number is the sum of the entries of all
#' miRBase versions provided by the package.
#'
#' @details
#' The total number is evaluated and set in the object initialization.
#'
#' @param this Object of class \code{MiRNANameConverter}
#' @param value An \code{integer} value
#' @return A \code{MiRNANameConverter} object
#'
# @examples
# nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
# nTotalEntries(nc) = 123456;
#'
#' @author Stefan Haunsberger
# @rdname nTotalEntries-set
# @export
setGeneric(
   name = "nTotalEntries<-",
   def = function(this, value) {
      standardGeneric("nTotalEntries<-");
   }
)

# @describeIn nTotalEntries-set
setReplaceMethod(
   f = "nTotalEntries",
   signature(this = "MiRNANameConverter"),
   definition = function(this, value) {
      if (is.null(this@.nTotalEntries)) {
         this@.nTotalEntries = value;
      } else {
         warning("Static value set at object instantiation. Value not set.");
      }
      return(this);
   }
)

##### validOrganisms #####
#' @title Set valid organisms
#'
#' @description This function sets all organisms where mature miRNA names
#' are available in any of the provided miRBase versions.
#'
#' @details
#' The valid organisms are evaluated and set in the object initialization.
#'
#' @param this Object of class \code{MiRNANameConverter}
#' @param value A \code{character} vector
#' @return A \code{MiRNANameConverter} object
#'
# @examples
# nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
# nOrganisms(nc) = c("hsa", "mmu");
#'
#' @author Stefan Haunsberger
# @rdname validOrganisms-set
# @export
setGeneric(
   name = "validOrganisms<-",
   def = function(this, value) {
      standardGeneric("validOrganisms<-");
   }
)

# @describeIn validOrganisms-set
setReplaceMethod(
   f = "validOrganisms",
   signature(this = "MiRNANameConverter"),
   definition = function(this, value) {
      if (is.null(this@.validOrganisms)) {
         this@.validOrganisms = value;
      } else {
         warning("Static value set at object instantiation. Value not set.");
      }
      return(this);
   }
)

