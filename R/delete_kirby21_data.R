#' @title Delete Kirby21 Imaging Data
#' @description This function allows users to remove specific
#' modalities for Kirby21 data sets.  This allows this package to be 
#' on CRAN
#'
#' @param modality modality of images that are to be downloaded.  You
#' must have the package downloaded for that modality.
#'
#' @return Nothing is returned
#' @export
delete_kirby21_data =  function(
  modality = kirby21.base::all_modalities()
  ){
  
  modality = match.arg(modality)
  fnames = get_image_filenames(
    modalities = modality,
    ids = get_ids(),
    FALSE)
  if (!is.null(fnames)) {
    file.remove(fnames) 
  }
  return(invisible(NULL))
}

