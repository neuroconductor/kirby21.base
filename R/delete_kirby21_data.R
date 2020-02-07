#' @title Delete Kirby21 Imaging Data
#' @description This function allows users to remove specific
#' modalities for Kirby21 data sets.  This allows this package to be 
#' on CRAN
#'
#' @param modality modality of images that are to be downloaded.  You
#' must have the package downloaded for that modality.
#' @param outdir output directory for files to download.  It will
#' default to the directory of the corresponding package for the data.
#'
#' @return Nothing is returned
#' @export
#' @examples 
#' on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
#' on_ci <- nzchar(Sys.getenv("CI"))
#' local_run = grepl("musch", tolower(Sys.info()[["user"]]))
#' run_example = !on_cran || on_ci || local_run
#' if (run_example) {
#' outdir = tempdir()
#' res = download_kirby21_data("SURVEY", outdir = outdir, force = TRUE)
#' delete_kirby21_data("SURVEY", outdir = outdir)
#' }
delete_kirby21_data =  function(
  modality = kirby21.base::all_modalities(),
  outdir = NULL
  ){
  
  modality = match.arg(modality)
  fnames = get_image_filenames(
    modalities = modality,
    ids = get_ids(),
    warn = FALSE,
    outdir = outdir)
  if (!is.null(fnames)) {
    file.remove(fnames) 
  }
  return(invisible(NULL))
}

