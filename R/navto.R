#' A function to help navigate through folders
#' will create folders requested if they don't exist already
#'
#'
#' @param folder.names A vector of folder through which to navigate
#' @keywords folders
#' @export

navto <- function( folder.names ) {
  for( folder.name in folder.names ) {
    if( !file.exists( folder.name ) ) {
      dir.create( folder.name )
    }
    setwd( folder.name )
  }
}
