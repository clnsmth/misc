#' Download EML metadata files from EDI
#' 
#' @description Iterates through all data package identifiers within a 
#' specified scope and downloads if not in the local path or if a newer 
#' revision is available and not yet in the local path. Also, removes old 
#' revisions. Use the \code{ids} parameter to supply a list of data package 
#' identifiers, which is useful users not belonging to an LTER site (i.e. 
#' scope).
#'
#' @param path Path to directory where EML files will be written. May also be 
#' a directory already containing EML files.
#' @param scope An EDI data package scope for which all the associated EML 
#' files will be downloaded, unless the \code{ids} parameter is used, in which 
#' case all \code{ids} belonging to \code{scope} will be downloaded.
#' @param ids A vector of EDI data package identifiers (i.e. the "identifier" 
#' part of "scope.identifier.revision"). Should be an integer value.
#'
#' @return Writes EML files to \code{path}
#' @export
#'
#' @examples
#' \dontrun {
#' # Download all EML files in the EDI scope
#' download_edi_eml("/me/documents/eml", "edi")
#' 
#' # Download all EML files in the CCE LTER scope
#' download_edi_eml("/me/documents/eml", "knb-lter-cce")
#' 
#' # Download a specific set of EML files from the EDI scope
#' download_edi_eml("/me/documents/eml", "edi", ids = c(24, 115, 1221))
#' }
download_edi_eml <- function(path, scope = "edi", ids = NULL) {
  # List data package identifiers of interest
  if (is.null(identifier)) {
    ids <- EDIutils::list_data_package_identifiers(scope)
  } else {
    ids <- as.numeric (ids)
  }
  for (id in ids) {
    rev <- EDIutils::list_data_package_revisions(scope, id, filter = "newest")
    packageId <- paste(scope, id, rev, sep = ".")
    message(packageId)
    files_in_path <- dir(path)
    i <- grep(paste(scope, id, sep = "."), files_in_path)
    if (length(i) > 0) { # id is in path ... is there a new revision?
      rev_in_path <- strsplit(files_in_path[i], ".", fixed = TRUE)[[1]][3]
      new_rev_available <- rev > as.numeric(rev_in_path)
      if (new_rev_available) { # new revision is available
        eml <- try(EDIutils::read_metadata(packageId), silent = TRUE)
        if (!"try-error" %in% attr(eml, "class")) {
          xml2::write_xml(eml, paste0(path, "/", packageId, ".xml"))
          file.remove(paste0(path, "/", packageId, ".xml"))
        }
      }
    } else { # id is not in path ... get it!
      eml <- try(EDIutils::read_metadata(packageId), silent = TRUE)
      if (!"try-error" %in% attr(eml, "class")) {
        xml2::write_xml(eml, paste0(path, "/", packageId, ".xml"))
      }
    }
  }
}

# TODO Remove ids in path that are no longer in EDI (data packages can be 
# deleted)