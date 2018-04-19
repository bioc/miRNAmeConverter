#' @title MiRNANameConverter constructor
#'
#' @description This function returns an instance of a MiRNANAmeConverter class.
#' Handling mature miRNA names from different miRBase versions
#' This package contains algorithms for dealing with mature miRNA names
#' from different miRBase release versions. The functions are provided in form
#' of methods as part of the \code{MiRNANameConverter}-class. The data of all
#' the miRBase release versions is stored in the \code{miRBaseVersions.db}
#' annotation package.
#' The \emph{MiRNAmeConverter} package contains one class that has two
#' categories of functions: getters-functions and algorithms.
#' @seealso \emph{miRBaseVersions.db} for more information about the
#' database holding all major miRBase release versions)
#'
#' @section Classes:
#' The \code{MiRNANameConverter}
# class is an S4 class that provides all the
# functions that are needed for dealing with challenges introduced over
# different miRBase releases.
#' @section Getter functions:
#' The getter functions provide access to the slots of the class.
#' @section Algorithms:
#' There are three algorithms for dealing with miRNA names from different
#' miRBase releases, the \code{\link{assessVersion}},
#' \code{\link{checkMiRNAName}} and \code{\link{translateMiRNAName}}.
#' \describe{
#'   \item{\code{\link{translateMiRNAName}}}{The algorithm coded in this
#'          function can translate given miRNA names to different
#'          miRBase release versions.}
#'   \item{\code{\link{checkMiRNAName}}}{This function is used to check if a
#'          given miRNA name is listed in the current miRBase release.}
#'   \item{\code{\link{assessVersion}}}{The \emph{assessVersion}}-function is
#'          useful when one wants to assess the miRBase version of a given set
#'          of mature miRNA names.
#' }
#'
#' @docType package
#' @name miRNAmeConverter
#'
#' @author Stefan Haunsberger \email{stefanhaunsberger@rcsi.ie}
#' @examples
#' # Translate a mature miRNA name to miRBase version 21.0
#' nc = MiRNANameConverter(); # Object instantiation
#' translateMiRNAName(nc, "hsa-miR-29a", version = 21.0)
#' @import DBI miRBaseVersions.db AnnotationDbi
NULL

# source("R/tools/printFunctionEntryInformation.R");
# source("R/tools/printFunctionExitInformation.R");

# Add package specific environment
# pkg.env <- new.env();

#' @title Instantiate from MiRNANameConverter class
#'
#' @description This function returns back an instance of a
#' \emph{MiRNANAmeConverter} object.
#'
#' @slot .dbconn Database connection
#' @slot .currentVersion Current miRBase version
#' @slot .validVersions Valid/Supported miRBase versions
#' @slot .nOrganisms Number of different organisms supported
#' @slot .nTotalEntries Total number of mature miRNA names among all provided
#' miRBase release versions in the \emph{miRBaseVersions.db} package.
#' @slot .validOrganisms Valid organisms
#' @author Stefan Haunsberger
#'
# @param this Object of class \code{MiRNANameConverter}
#' @param ... any optional arguments
MiRNANameConverter = setClass(
   # Set the name for the class
   Class = "MiRNANameConverter",

   # Define the slots
   slots = representation(
      .dbconn = "SQLiteConnection",
      .currentVersion = "numeric",
      .validVersions = "numeric",
      .nOrganisms = "integer",
      .nTotalEntries = "integer",
      .validOrganisms = "character"
   ),

   # Set the default values for the slots. (optional)
   prototype = list(
      .dbconn = NULL,
      .currentVersion = NULL,
      .validVersions = NULL,
      .nOrganisms = NULL,
      .nTotalEntries = NULL,
      .validOrganisms = NULL
   )
)

##### initialization ####
# Initialization function (constructor)
setMethod(
   f = "initialize",
   signature("MiRNANameConverter"),
   definition = function(.Object) {
      #			writeLines("MiRNANameConverter-constructor");

      # library(miRBaseVersions.db);

      args = as.list(match.call());

      # Store database connection
      .dbconn(.Object) = AnnotationDbi::dbconn(miRBaseVersions.db);
      con = .dbconn(.Object);
      # Recive all version numbers from the db
      versions = (DBI::dbGetQuery(con,
                             "SELECT number AS version FROM version;"))$version;
      validVersions(.Object) = versions;
      currentVersion(.Object) = as.numeric(
                                    AnnotationDbi::dbmeta(con,
                                                          "current_version"));
      nOrganisms(.Object) =
                  DBI::dbGetQuery(con,
                         "SELECT count(DISTINCT organism) as n FROM mimat")$n;
      nTotalEntries(.Object) =
         DBI::dbGetQuery(con,
                    "SELECT count(*) as n FROM mimat")$n;
      validOrganisms(.Object) =
         DBI::dbGetQuery(con,
                    "SELECT organism FROM organism")$organism;

      return(.Object);
   }
)

#'
#'
.check = function(input) {
    # Correct 'mir-' with 'miR-'
    # output = gsub("mir-", "miR-", input, ignore.case = TRUE);
    # Remove quotations (single and double)
    output = gsub("\"|\'|--", "", input);
    return(output);
}

##### MiRNANameConverter - constructor ####
#' @title MiRNANameConverter constructor
#'
#' @description This function returns an instance of a MiRNANAmeConverter class.
#'
#' @return an object of class 'MiRNANameConverter'
#' @examples
#' nc = MiRNANameConverter() # Instance of class 'MiRNANameConverter'
#'
#' @details
#' This function initializes an object of the class MiRNANameConverter. It is a
#' wrapper for \code{new()}.
#'
#' @seealso \code{\link{new}}
#' @author Stefan Haunsberger
#' @export MiRNANameConverter
setMethod(
   f = "MiRNANameConverter",
   signature(),
   definition = function() {
      return(new('MiRNANameConverter'));
   }
)

##### checkMiRNAName ####
#' @title Check miRNA names for validity
#'
#' @description This function checks for a given set of mature 'miRNAs' (names)
#' Check miRNA names for validity
#'
#' This function checks for a given set of mature 'miRNAs' (names)
#' if the names are listed in any miRBase version respectively.
#'
#' @details
#' This function takes the input miRNA names and checks each one of them for
#' validity. The check is done by taking each miRNA and searches for an existing
#' entry in the miRBase database among all versions. miRNAs that are listed in
#' any version will be comprised in the return vector respectively. If no valid
#' miRNA was detected, a \code{character(0)} will be returned.
#'
#' @param this Object of class 'MiRNANameConverter'
#' @param miRNAs A character vector of miRNA names
#' @param verbose A boolean to either show more (TRUE) or less
#' information (FALSE)
#' @return A character vector containing a set of valid miRNA names
#'
#' @examples
#' nc = MiRNANameConverter() # Instance of class 'MiRNANameConverter'
#' # Test with correct inputs
#' checkMiRNAName(nc, miRNAs = c("hsa-miR-29a", "hsa-miR-642"))
#'
#' @importFrom DBI dbGetQuery
#' @author Stefan Haunsberger
#' @rdname checkMiRNAName
#' @export
setGeneric(
   name = "checkMiRNAName",
   def = function(this,
                  miRNAs,
                  verbose = FALSE) {
      standardGeneric("checkMiRNAName");
   }
)

#' @describeIn checkMiRNAName Method for checking for valid miRNA names
setMethod(
   f = "checkMiRNAName",
   signature("MiRNANameConverter"),
   definition = function(this,
                         miRNAs,
                         verbose) {

      if (verbose) {
         printFunctionEntryInformation(fun = as.character((match.call())),
                                       match_call = match.call());
      }

      args = as.list(match.call());

      if (!is.null(args$miRNAs)) {
         if (!(class(miRNAs) == "character")) {
            stop(sprintf("Class of miRNAs ('%s') should be 'character'...
                         .\nPlease try again with a valid class.",
                         class(miRNAs)));
         }
         miRNAs.unique = unique(miRNAs);
         if (length(miRNAs.unique) != length(miRNAs)) {
            message(sprintf("\tInput: %i unique miRNA names",
                            length(miRNAs.unique)));
         }
      }

      # Check which entries are acutally miRNAs listed in the miRBase repository
      miRNAs.valid = (DBI::dbGetQuery(conn = .dbconn(this),
                              sprintf("SELECT name FROM 'vw-mimat-mirna-unique'
                                         WHERE UPPER(name) IN (\"%s\")
                                         ORDER BY name ASC",
                                         paste(toupper(miRNAs.unique),
                                               collapse = "\", \""))))$name;
      n.miRNAs.valid = length(miRNAs.valid);
      if (n.miRNAs.valid != length(miRNAs.unique)) {
         miRNAs.invalid = miRNAs.unique[!(toupper(miRNAs.unique)
                                          %in%
                                             toupper(miRNAs.valid))];
         if (length(miRNAs.invalid) == 1) {
            message("\tThe following miRNA is not listed in the miRBase repository:");
         } else {
            message("\tThe following miRNAs are not listed in the miRBase repository:");
         }
         message(paste0(miRNAs.invalid, collapse = "; "));
      }

      if (verbose) {
         printFunctionExitInformation(fun = as.character((match.call())));
      }

      # if (correct) {
         # return(miRNAs.valid);
#       } else {
         return(miRNAs.unique[(toupper(miRNAs.unique)
                               %in%
                                  toupper(miRNAs.valid))])
#       }
   }
)


##### assessMiRNASwappingMIMAT ####
#' @title Check if given miRNA names can be assigned to unique MIMAT 
#' accessions among all versions
#'
#' @description This function checks if the names from a given set of mature
#' miRNAs have a unique MIMAT ID.
#' Check if given miRNA names can be assigned to unique MIMAT accessions among
#' all versions
#'
#' This function checks if the names from a given set of mature miRNAs
#' have a unique MIMAT ID.
#'
#' @details
#' Although the majority of miRNA names can be assigned to a unique MIMAT ID
#' (accession) some miRNAs changed MIMAT ID in different versions. This function
#' takes the input miRNA names and checks each one of them if they have a unique
#' MIMAT ID over all versions. If a miRNA changes MIMAT ID in a version it will
#' be comprised in the return vector.
#'
#' @param this Object of class 'MiRNANameConverter'
#' @param miRNAs A character vector of miRNA names
#' @param verbose A boolean to either show more (TRUE) or less information
#' (FALSE)
#' @return A character vector containing miRNA names that do not have a unique
#' MIMAT ID
#'
#' @author Stefan Haunsberger
#'
# @export
#' @rdname assessMiRNASwappingMIMAT
setGeneric(
   name = "assessMiRNASwappingMIMAT",
   def = function(this,
                  miRNAs,
                  verbose = FALSE) {
      standardGeneric("assessMiRNASwappingMIMAT");
   }
)

# @describeIn assessMiRNASwappingMIMAT Method for assessing miRNA names that
# swapped MIMAT-accession
setMethod(
   f = "assessMiRNASwappingMIMAT",
   signature("MiRNANameConverter"),
   definition = function(this,
                         miRNAs,
                         verbose) {

      if (verbose) {
         printFunctionEntryInformation(fun = as.character((match.call())),
                                       match_call = match.call());
      }

      args = as.list(match.call());

      if (!is.null(args$miRNAs)) {
         if (!(class(miRNAs) == "character")) {
            stop(sprintf("Class of miRNAs ('%s') should be 'character'.\nPlease try again with a valid class.", class(miRNAs)));
         }
         miRNAs.unique = unique(miRNAs);
         if (length(miRNAs.unique) != length(miRNAs)) {
            message(sprintf("\tInput: %i unique miRNA names",
                            length(miRNAs.unique)));
         }
      }

      # Check which entries are acutally miRNAs listed in the miRBase repository
      miRNAs.swapped = (DBI::dbGetQuery(conn = .dbconn(this),
                                   sprintf("SELECT name FROM 'vw-mimat-mirna-swapped-mimat'
                                            WHERE UPPER(name) IN (\"%s\")
                                            ORDER BY name ASC",
                                           paste(toupper(miRNAs.unique), collapse = "\", \""))))$name;
      n.miRNAs.swapped = length(miRNAs.swapped);
      if (n.miRNAs.swapped == 1) {
         message("\tFollowing miRNA does not have a unique MIMAT accession among the versions:");
         message(paste0(miRNAs.swapped, collapse = "; "));
      } else if (n.miRNAs.swapped > 1){
         message("\tFollowing miRNAs do not have a unique MIMAT accession among the versions:");
         message(paste0(miRNAs.swapped, collapse = "; "));
      }

      if (verbose) {
         printFunctionExitInformation(fun = as.character((match.call())));
      }

      return(miRNAs.swapped);
   }
)


##### assessVersion ####
#' @title Assess miRBase version
#'
#' @description This function detects the most likely miRBase version of a
#' given miRNA set.
#' Assess miRBase version
#'
#' This function detects the most likely miRBase version of a given miRNA set.
#'
#' @param this Object of class 'MiRNANameConverter'
#' @param miRNAs A character vector of miRNA names
#' @param verbose A boolean to either show more (TRUE) or less information (FALSE)
#' @return A data frame with two columns: version and frequency (decreasing
#' order by frequency, version)
#' 				+ version: miRBase version
#' 				+ frequency: the number of valid miRNAs that could be assigned
#' 				to the version respectively
#'
#' @examples
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' assessVersion(nc, miRNAs = c("hsa-miR-140", "hsa-miR-125a"))
#'
#' @details
#' This function takes a set of miRNA names and detects the most likely miRBase
#' version of this given set of 'miRNAs'. First all miRNAs will be checked for
#' validity (if they are actual miRNA names \code{checkMiRNAName} and the set
#' that passes the check will be further processed.
#'
#' @author Stefan Haunsberger
#' @rdname assessVersion
#' @export
setGeneric(
   name = "assessVersion",
   def = function(this,
                  miRNAs,
                  verbose = FALSE) {
      standardGeneric("assessVersion");
   }
)

#' @describeIn assessVersion Method for assessing the most likely miRBase
#' version that a given set of miRNA names is from.
setMethod(
   f = "assessVersion",
   signature("MiRNANameConverter"),
   definition = function(this,
                         miRNAs,
                         verbose) {

      if (verbose) {
         printFunctionEntryInformation(fun = as.character((match.call())),
                                       match_call = match.call());
      }

      args = as.list(match.call());

      if (!is.null(args$miRNAs)) {
         if (!(class(miRNAs) == "character")) {
            stop(sprintf("Class of miRNAs ('%s') should be 'character'.\nPlease try again with a valid class.", class(miRNAs)));
         }
         miRNAs.unique = unique(miRNAs);
         if (length(miRNAs.unique) != length(miRNAs)) {
            message(sprintf("\tInput: %i unique miRNA names",
                            length(miRNAs.unique)));
         }
      }

      # Check which entries are acutally miRNAs listed in the miRBase repository
      miRNAs.valid = checkMiRNAName(this, miRNAs.unique);
      n.miRNAs.valid = length(miRNAs.valid);

      # Receive miRBase versions with number of assigned miRNAs
      versions = DBI::dbGetQuery(conn = .dbconn(this),
                            sprintf("SELECT version, count(*) as frequency FROM mimat
                                     WHERE UPPER(name) IN (\"%s\")
                                     GROUP BY version
                                     ORDER BY frequency DESC, version DESC;",
                                    paste(toupper(miRNAs.valid), collapse = "\", \"")));
      version = versions$version[1];

      # Check which miRNAs could not be assigned to the identified version
      n.miRNAs.not.version = 0;
      if (n.miRNAs.valid != versions$frequency[1]) {
         miRNAs.version = (DBI::dbGetQuery(conn = .dbconn(this),
                                      sprintf("SELECT name FROM 'vw-mimat-%2.1f'
                                               WHERE UPPER(name) IN (\"%s\")",
                                              version, paste(toupper(miRNAs.valid),
                                                             collapse = "\", \""))))$name;
         miRNAs.not.version = miRNAs.valid[!(toupper(miRNAs.valid)
                                             %in%
                                                toupper(miRNAs.version))];

         n.miRNAs.not.version = length(miRNAs.not.version);
         if (n.miRNAs.not.version == 1) {
            message(sprintf("\tmiRNA that could not be found in identified max version %2.1f:", version));
         } else {
            message(sprintf("\tmiRNAs that could not be found in identified max version %2.1f:", version));
         }
         message(paste0(miRNAs.not.version, collapse = "; "));
      }

      message(sprintf("-> %i out of %i valid miRNAs assigned to max version %2.1f.", versions$frequency[1], n.miRNAs.valid, version));

      if (verbose) {
         printFunctionExitInformation(fun = as.character((match.call())));
      }

      return(versions);
   }
)

##### translateMiRNAName ####
#' @title Translate miRNA name
#'
#' @description This function translates input miRNA names to different 
#' miRBase versions.
#' Translate miRNA name
#'
#' This function translates input miRNA names to different miRBase versions.
#'
#' @param this Object of class 'MiRNANameConverter'
#' @param miRNAs A character vector of miRNA names
#' @param versions An integer or numerical vector containing the target versions
#' @param sequenceFormat Integer value indicating the return format for the
#' data frame containing sequence information
#' {1=only sequences, 2=miRNA name and sequence}
#' @param verbose A boolean to either show more (TRUE) or less information
#' (FALSE)
#' @return A (n x m) data frame where n is the number of valid miRNAs and m the
#' number of
#' columns (minimum 3 columns, MIMAT-ID (accession), input miRNA name, current
#' version)
#' In addition an attribute 'description' is added to the data frame where to
#' each miRNA some notes are added (for example why a certain miRNA is not 
#' in the output).
#' Sequence information is attached as the attribute 'sequence'.
#'
#' @examples
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' res = translateMiRNAName(nc, miRNAs = c("hsa-miR-140", "hsa-miR-125a"),
#'                    versions = c(15, 16, 20, 21))
#' res
#' attributes(res)
#' @seealso \code{\link{attr}} for attributes
#'
#' @details
#' The translation and sequence retrieval are done in 5 main steps:
#'		1) Only take miRNA names that do not swap MIMAT IDs among versions
#'		(\code{\link{assessMiRNASwappingMIMAT}})
#'		2) Check, if the miRNA names are valid names (\code{\link{checkMiRNAName}})
#'		3) Receive unique MIMAT IDs for each valid miRNA
#'			- If there are miRNAs that have basically the same name,
#'			  only use miRNA names from the highest version
#'		4) Check if the found MIMAT IDs are still listed in the 
#'		    current miRBase version
#'			- If not, neglect it because then it is not considered to be a 
#'			    miRNA anymore
#'		5) Receive names from desired versions
#'
#' @author Stefan Haunsberger
#' @rdname translateMiRNAName
#' @export
setGeneric(
   name = "translateMiRNAName",
   def = function(this,
                  miRNAs,
                  versions,
                  sequenceFormat = 1,                  
                  verbose = FALSE) {
      standardGeneric("translateMiRNAName");
   }
)

#' @describeIn translateMiRNAName Method for translating miRNA name(s) to
#' different miRBase versions
setMethod(
    f = "translateMiRNAName",
    signature(this = "MiRNANameConverter", miRNAs = "character"),
    definition = function(this,
                        miRNAs,
                        versions,
                        sequenceFormat,
                        verbose) {

    if (verbose) {
        printFunctionEntryInformation(fun = as.character((match.call())),
                                       match_call = match.call());
    }

    args = as.list(match.call());

    if (!is.null(args$miRNAs)) {
        if (!(class(miRNAs) == "character")) {
            stop(sprintf("Class of miRNAs ('%s') should be 'character'.\nPlease try again with a valid class.", class(miRNAs)));
        }
        # Substitute characters/clean
        miRNAs = .check(miRNAs);
        miRNAs.unique = unique(miRNAs);
        if (length(miRNAs.unique) != length(miRNAs)) {
            message(sprintf("\tInput: %i unique miRNA names",
                                                    length(miRNAs.unique)));
        }
    }

    if (is.null(args$versions)) {
        versions = this@.currentVersion;
    } else {
        if (!all(versions %in% this@.validVersions)) {
            message(sprintf("Argument 'versions' is not valid."));
            message("Valid versions are:");
            message(sprintf("%2.1f", this@.validVersions));
            stop("Please try again with valid 'versions' argument.");
        } else {
            versions = unique(versions);
        }
    }

    # Initialize data frame with description to each miRNA
    description = data.frame(input.miRNA = miRNAs.unique,
                            information = "OK",
                            stringsAsFactors = FALSE,
                            row.names = miRNAs.unique);


    ###### ### ###
    # 1) Check for MIMAT swapping miRNAs
    
    ## Only use miRNAs that are not swapping MIMATs as input
    miRNAs.swapping = suppressMessages(
                        assessMiRNASwappingMIMAT(
                            this,
                            miRNAs.unique, verbose = verbose
                            )
                        );
    miRNAs.not.swapping = miRNAs.unique[!(miRNAs.unique %in% miRNAs.swapping)];
    
    if (length(miRNAs.swapping) > 0) {
        if (length(miRNAs.swapping) == 1) {
            message("\tFollowing miRNA will be neglected due to non unique MIMAT accession:");
        } else {
            message("\tFollowing miRNAs will be neglected due to non unique MIMAT accession:");
        }
        message(paste0(miRNAs.swapping, collapse = "; "));
    }

    description[miRNAs.swapping, "information"] = "This miRNA does not have a unique MIMAT ID.";
    
    if (length(miRNAs.not.swapping) == 0) {
        message("No further miRNAs to process, return NULL.");
        return(NULL);
    }
    ###### ### ###
    # 2) Check which entries are acutally miRNAs listed in the miRBase repository
    
    ## Only process miRNAs that are valid miRNA names
    miRNAs.valid = suppressMessages(checkMiRNAName(this,
                                                 miRNAs.not.swapping,
                                                 verbose = verbose));
    miRNAs.not.valid = miRNAs.not.swapping[!(toupper(miRNAs.not.swapping) 
                                                %in% toupper(miRNAs.valid))];
    n.miRNAs.valid = length(miRNAs.valid);
    n.miRNAs.dif = length(miRNAs.not.swapping) - n.miRNAs.valid;
    
    # print(miRNAs.valid)
    miRNAs.valid = suppressMessages(
                        checkMiRNAName(
                            this
                            ,miRNAs.not.swapping
                            ,verbose = verbose
                        )
                    );
    miRNAs.not.valid = miRNAs.not.swapping[!(toupper(miRNAs.not.swapping) 
                                                %in% toupper(miRNAs.valid))];
    n.miRNAs.valid = length(miRNAs.valid);
    n.miRNAs.dif = length(miRNAs.not.swapping) - n.miRNAs.valid;
    
    if (length(miRNAs.valid) == 0) {
        message("No valid miRNAs found.");
        return(NULL);
    }
    
    if (n.miRNAs.dif != 0) {
        if (n.miRNAs.dif == 1) {
            message("\tFollowing miRNA will be neglected (not listed in miRBase):");
        } else {
            message("\tFollowing miRNAs will be neglected (not listed in miRBase):");
        }
        message(paste0(miRNAs.not.valid, collapse = "; "));
    }
    
    description[miRNAs.not.valid, "information"] =
                          "This name is not listed in any miRBase version.";

    ###### ### ###
    # 3) Check if there are miRNAs that have the same name -> same MIMAT
    ##								name		accession	version
    ##		e.g.:	2  hsa-let-7a*		MIMAT0004481	17
    ##				3 	hsa-let-7a-3p 	MIMAT0004481	21		--> would be chosen
    ## They are basically the same but input names are from different versions
    
    ## Receive unique MIMAT accessions for each miRNA name with the
    ## maximum version number
    mimats.versions = DBI::dbGetQuery(
                            conn = .dbconn(this)
                            ,sprintf("SELECT DISTINCT accession, name, 
                                        MAX(version) as version FROM mimat
                                        WHERE UPPER(name) IN (\"%s\")
                                        GROUP BY accession, name
                                        ORDER BY accession",
                                        paste(toupper(miRNAs.valid), 
                                                        collapse = "\", \"")
                                     )
                            );
    if (length(miRNAs.valid) == 1) {
        mimats = mimats.versions$accession;
        names = mimats.versions$name;
    } else {
        mimats.versions.split = split(mimats.versions,
                                    mimats.versions$accession);
        mimats = character(length(unique(mimats.versions$accession)));
        names = character(length(unique(mimats.versions$accession)));
         for (i in 1:length(mimats.versions.split)) {
            idx = 1;
            if (nrow(mimats.versions.split[[i]]) > 1) {
                # Take the name from the highest version
                idx = which.max((mimats.versions.split[[i]])$version);
                name = mimats.versions.split[[i]]$name[idx];
                name.other = mimats.versions.split[[i]]$name[-idx];
                writeLines(sprintf("\tmiRNA '%s' is the same as '%s'.",
                                  name, paste(name.other, collapse = ", ")));
                writeLines(sprintf("\t->%s is the most recent one and will be used. The other ones will be neglected.", name));
                # Add description to miRNAs that will be neglected
                description[name.other, "information"] =
                    sprintf("This miRNA is the same as input miRNA name '%s' from version %2.1f.",
                            name, (mimats.versions.split[[i]])$version[idx]);
            }
            mimats[i] = mimats.versions.split[[i]]$accession[idx];
            names[i] = mimats.versions.split[[i]]$name[idx];
        }
    }
    
    ###### ### ###
    # 4) Check which MIMATs are listed in the current miRBase release
    
    ## Only take miRNAs that are listed in the current version
    ### Receive mimats that are listed in the current version
    mimats.current = (DBI::dbGetQuery(conn = .dbconn(this),
                             sprintf("SELECT accession FROM 'vw-mimat-%2.1f'
                                      WHERE accession IN (\"%s\")",
                                this@.currentVersion, paste(toupper(mimats),
                                         collapse = "\", \""))))$accession;
    mimats.invalid = mimats[!(mimats %in% mimats.current)];
    names.invalid = names[!(mimats %in% mimats.current)];
    if (length(mimats.invalid) > 0) {
        if (length(mimats.invalid) == 1) {
            message(sprintf("\tFollowing miRNA is not listed in the current miRBase version %2.1f.",
                        this@.currentVersion));
        } else {
            message(sprintf("\tFollowing miRNAs are not listed in the current miRBase version %2.1f.",
                        this@.currentVersion));
        }
        message(paste0(names.invalid, collapse = "; "));
    }
    ### Exclude MIMATs that are not listed in the current version
    mimats.valid = mimats[mimats %in% mimats.current];
    names.valid = names[mimats %in% mimats.current];
    
    description[names.invalid, "information"] =
        sprintf("This miRNA is not listed in the current miRBase version %2.1f.",
                                                      this@.currentVersion);

    ###### ### ###
    # 5) Translate miRNA names to desired versions
    
    ## Initialize return data frame for miRNA names and sequences
    ### miRNA names
    ## Initialize return data frame
    n.col = 2 + length(versions);
    headers = c("mimat", "input",
        paste0("v", sprintf("%2.1f",versions)));
    df = as.data.frame(matrix(data = NA_character_, nrow =
                               length(mimats.valid), ncol = n.col));
    colnames(df) = headers;
    ### Sequences
    if (sequenceFormat == 1) {
        dfSeq = as.data.frame(matrix(data = NA_character_,
                                     nrow = length(mimats.valid),
                                                    ncol = n.col),
                            stringsAsFactors = FALSE);
        colnames(dfSeq) = c("mimat", "input", 
                            paste0("v", sprintf("%2.1f",versions)));
    
    } else {
        dfSeq = as.data.frame(matrix(data = NA_character_,
                                   nrow = length(mimats.valid),
                                   ncol = 2 + 2 * length(versions)),
                                stringsAsFactors = FALSE);
        
        nam = unlist(
                lapply(paste0("v", sprintf("%2.1f",versions)), 
                    function(y1) {
                        lapply(c("miRNA", "Sequence"),
                                function(y2) {
                                    paste(y1, y2, sep="-")
                        })}));
        colnames(dfSeq) = c("mimat", "input", nam);
        rm(nam);
    }
    # Convert factor columns to character columns
    fac.cols = sapply(df, is.factor);
    df[, fac.cols] = sapply(df[, fac.cols], as.character);
    # colnames(df) = headers;
    fac.cols = sapply(dfSeq, is.factor);
    dfSeq[, fac.cols] = sapply(dfSeq[, fac.cols], as.character);
    # colnames(dfSeq) = headers;
    
    df$mimat = mimats.valid;
    df$input = names.valid;
    dfSeq$mimat = mimats.valid;
    dfSeq$input = names.valid;
    rownames(df) = mimats.valid;
    rownames(dfSeq) = mimats.valid;
    
    ## Iterate over desired versions and
    for (version in versions) {
    
        miRNAs.version =
        DBI::dbGetQuery(conn = .dbconn(this),
                     sprintf("SELECT accession, name FROM 'vw-mimat-%2.1f'
                              WHERE accession IN (\"%s\")",
                             version, paste(toupper(df$mimat),
                                            collapse = "\", \"")));
        df[miRNAs.version$accession, 
                          sprintf("v%2.1f",version)] = miRNAs.version$name;
        
        # sequences
        if (sequenceFormat == 1) {
            # Only accession and sequence
            seq.version = DBI::dbGetQuery(conn = .dbconn(this),
                             sprintf("SELECT accession, sequence
                                FROM 'vw-mimat-%2.1f'
                                WHERE accession IN (\"%s\")",
                                     version, paste(toupper(df$mimat),
                                                    collapse = "\", \"")));
            dfSeq[seq.version$accession, 
                sprintf("v%2.1f",version)] = seq.version$sequence;
        } else {
            # Accession, miRNA name and sequence
            seq.version = DBI::dbGetQuery(conn = .dbconn(this),
                             sprintf("SELECT accession, name, sequence
                                FROM 'vw-mimat-%2.1f'
                                WHERE accession IN (\"%s\")",
                                     version, paste(toupper(df$mimat),
                                                    collapse = "\", \"")));
            dfSeq[seq.version$accession,
                sprintf("v%2.1f-Sequence",version)] = seq.version$sequence;
            dfSeq[miRNAs.version$accession, 
                    sprintf("v%2.1f-miRNA",version)] = miRNAs.version$name;
        }

    }

    # Reorder data frames
    ##	df by MIMAT ID
    ## description by input miRNA names
    df = df[order(df$mimat),];
    dfSeq = dfSeq[order(dfSeq$mimat),];
    description = description[order(description$input.miRNA),];
    
    # Add description as an attribute
    attr(df, "description") = description;
    attr(df, "sequence") = dfSeq;
    
    if (verbose) {
        printFunctionExitInformation(fun = as.character((match.call())));
    }
    
    return(df);
})

##### saveResults ####
#' @title Save miRNA translation results
#'
#' @description This function saves the data frame returned from
#' translateMiRNAName inclusive the attribute 'description'.
#' Save miRNA translation results
#'
#' This function saves the data frame returned from translateMiRNAName
#' inclusive the attribute 'description'.
#'
#' @param this Object of class 'MiRNANameConverter'
#' @param df A \code{data.frame} with translated results
#' @param outputFilename A filename for the output file, such as 'filename.txt'
#' @param outputPath A file path (character string) to the target directory
#' @param sep Separator
#' @param quote If all data values shall be surrounded by ('"')
#' @param verbose Boolean to either show more (TRUE) or less information (FALSE)
#' @param ... Arguments that can be passed on to \code{write.table}
#'
#' @examples
#' nc = MiRNANameConverter(); # Instance of class 'MiRNANameConverter'
#' res = translateMiRNAName(nc, miRNAs = c("hsa-miR-140", "hsa-miR-125a"),
#'                            versions = c(15, 16, 20, 21))
#' # Save translation results
#' saveResults(nc, res)
#'
#' @details
#' This function saves a data frame that has been returned by
#' \code{translateMiRNAName}.
#' The attribute 'description' of the data frame will be stored as well.
#'
#' @seealso \code{\link{write.table}} for additional parameter values for
#' the '...' argument, \code{\link{attr}} for how to retrieve attributes
#' @author Stefan Haunsberger
#' @rdname saveResults
#' @export
setGeneric(
   name = "saveResults",
   def = function(this,
                  df,
                  outputFilename,
                  outputPath,
                  sep = "\t",
                  quote = FALSE,
                  verbose = FALSE,
                  ...) {
      standardGeneric("saveResults");
   }
)

#' @describeIn saveResults Method for saving translation results
setMethod(
    f = "saveResults",
    signature(this = "MiRNANameConverter", df = "data.frame"),
    definition = function(this,
                            df,
                            outputFilename,
                            outputPath,
                            sep,
                            quote,
                            verbose,
                            ...) {

    if (verbose) {
        printFunctionEntryInformation(fun = as.character((match.call())),
                           match_call = match.call());
    }
    
    args = as.list(match.call());
    
    if (is.null(args$outputFilename)) {
        time = format(Sys.time(), "%H%M%S");
        date = format(Sys.Date(), "%d%m%Y");
        outputFilename = sprintf("miRNA-name-translation-%s-%s.txt",
                               date, time);
    }
    
    if (is.null(args$outputPath)) {
        outputPath = ".";
    } else {
        if (!file.exists(outputPath)) {
            message(sprintf("Given outputPath '%s' does not exist.",
                            outputPath));
            stop("Please try again with valid output path.");
        }
    }
    
    # Construct 'file' (path incl. filename, such as <path>/filename.txt)
    output.file = file.path(outputPath, outputFilename);
    if (file.exists(output.file)) {
        prefix = substr(outputFilename, 1,
                     sapply(gregexpr("\\.", outputFilename), tail, 1) - 1);
        suffix = substr(outputFilename,
                     sapply(gregexpr("\\.", outputFilename), tail, 1),
                             nchar(outputFilename));
        time = gsub(pattern = ":", replacement = "", x = (format(Sys.time(), "%X")));
        date = format(Sys.Date(), "%d-%m-%Y");
        outputFilename = sprintf("%s-%s-%s%s", prefix, date, time, suffix);
        message(sprintf("Output file already exists -> Save file under name '%s'", outputFilename));
        output.file = file.path(outputPath, outputFilename);
    }
    
    writeLines(sprintf("\tSave file to '%s'...", output.file));
    utils::write.table(df, file = output.file, row.names = FALSE, quote = FALSE, sep = sep);
    
    # description
    if (!is.null(attributes(df)$description)) {
        prefix = substr(outputFilename, 1,
                     sapply(gregexpr("\\.", outputFilename), tail, 1) - 1);
        suffix = substr(outputFilename,
                     sapply(gregexpr("\\.", outputFilename), tail, 1),
                             nchar(outputFilename));
        outputFilenameDesc = sprintf("%s-description%s", prefix, suffix);
        output.file = file.path(outputPath, outputFilenameDesc);
        writeLines(sprintf("\tSave file to '%s'...", output.file));
        utils::write.table(attributes(df)$description, file = output.file,
                 row.names = FALSE, quote = FALSE, sep = sep);
    }
    
    if (!is.null(attributes(df)$sequence)) {
        prefix = substr(outputFilename, 1,
                      sapply(gregexpr("\\.", outputFilename), tail, 1) - 1);
        suffix = substr(outputFilename,
                      sapply(gregexpr("\\.", outputFilename), tail, 1),
                      nchar(outputFilename));
        outputFilenameSeq = sprintf("%s-sequence%s", prefix, suffix);
        output.file = file.path(outputPath, outputFilenameSeq);
        writeLines(sprintf("\tSave file to '%s'...", output.file));
        utils::write.table(attributes(df)$sequence, file = output.file,
                         row.names = FALSE, quote = FALSE, sep = sep);
    }
    
    if (verbose) {
        printFunctionExitInformation(fun = as.character((match.call())));
    }
    
    invisible(return);
})


##### show ####
#' @title Show-method
#'
#' @description This function prints object specific information
#' Show-method
#'
#' This function prints object specific information
#'
#' @details
#' This function prints some information to the console.
#' @param object Object of class \code{MiRNANameConverter}
#' @seealso \code{\link{show}}
#'
#' @author Stefan Haunsberger
#' @export
setMethod(
   f = "show",
   signature("MiRNANameConverter"),
   definition = function(object) {

      cat("An object of class: MiRNANameConverter\n");
      cat("- Most recent miRBase version provided in the package:");
      print(currentVersion(object));
      cat("- Valid miRBase versions: ");
      print(validVersions(object));

      cat("- Number of species: ");
      print(nOrganisms(object));
      cat("- Number of database entries among all versions: ");
      print(nTotalEntries(object));

   }
)

##### getMirbaseVersionsXandY ####
#' @title Retrieve two complete miRBase versions in a dataframe
#'
#' @description Return a dataframe with miRNA names from two miRBase
#' release versions in two columns (e.g. useful for joining with other 
#' existing dataframe).
#'
#' @param this Object of class 'MiRNANameConverter'
#' @param version Numeric vector for first miRBase version (default: max version)
#' @param type Character with either \code{'mi'} or \code{'mimat'} for 
#' precursor miRNA or mature miRNA names respectively (default: 'mimat')
#' @param species Character or character vector, such as \code{'hsa'} or \code{c('hsa', 'mmu')} (default: \code{'hsa'})
#' information (FALSE)
#' @return A dataframe
#'
#' @examples
#' nc = MiRNANameConverter() # Instance of class 'MiRNANameConverter'
#' getMirbaseVersionsXandY(nc, version = c(18, 22))
#'
#' @importFrom DBI dbGetQuery
#' @author Stefan J. Haunsberger
#' @rdname getMirbaseVersionsXandY
#' @export
setGeneric(
  name = "getMirbaseVersionsXandY",
  def = function(this,
                 version,
                 type = "mimat",
                 species = "hsa",
                 addLatestVersion = TRUE) {
    standardGeneric("getMirbaseVersionsXandY");
  }
)

#' @describeIn checkMiRNAName Method for checking for valid miRNA names
setMethod(
  f = "getMirbaseVersionsXandY",
  signature("MiRNANameConverter"),
  definition = function(this,
                        version,
                        type,
                        species,
                        addLatestVersion) {

    args = as.list(match.call());
    
    if (is.null(args$version)) {
      version = this@.currentVersion;
    }
    
    if (addLatestVersion) {
      version = c(version, this@.currentVersion);
    }
    
    if (!(type %in% c("mi", "mimat"))) {
      stop(paste("type", type, "not supported (supported are 'mi' and 'mimat')"));
    }
    
    # Send SQL request
    mirDf =
      DBI::dbGetQuery(conn = this@.dbconn,
      sprintf("SELECT accession, name, version, organism
              FROM %s
              WHERE version IN (%s) AND organism IN (\"%s\")
              ORDER BY version ASC",
              type,
              paste(version, collapse = ", "), paste(species, collapse = "\", \"")));
    mirDf$version = paste0("v", mirDf$version);
    
    miRNAs.swapping = suppressMessages(assessMiRNASwappingMIMAT(this,mirDf$name));
    miRNAs.not.swapping = mirDf$name[!(mirDf$name %in% miRNAs.swapping)];
    
    if (length(miRNAs.swapping) > 0) {
      if (length(miRNAs.swapping) == 1) {
        message("\tFollowing miRNA will be neglected due to non unique MIMAT accession:");
      } else {
        message("\tFollowing miRNAs will be neglected due to non unique MIMAT accession:");
      }
      message(paste0(miRNAs.swapping, collapse = "; "));
    }
    mirDf = mirDf[!(mirDf$name %in% miRNAs.swapping),];
    
    mirDfW = reshape2::dcast(mirDf, accession + organism ~ version, value.var = "name");
    
    return(mirDfW);
})

