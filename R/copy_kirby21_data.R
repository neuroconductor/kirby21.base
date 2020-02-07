#' @title Copy Kirby21 Data to an output directory
#'
#' @description Copies files from Kirby21 Package to an output directory
#' @param copydir Output directory for data
#' @param ... Arguments to pass to \code{\link{get_image_filenames}}
#' @return Logical if files are copied
#' @export
#' @examples 
#' on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
#' on_ci <- nzchar(Sys.getenv("CI"))
#' local_run = grepl("musch", tolower(Sys.info()[["user"]]))
#' run_example = !on_cran || on_ci || local_run
#' if (run_example) {
#' tdir = tempfile()
#' dir.create(tdir)
#' outdir = tempdir()
#' surv_installed = "kirby21.survey" %in% installed.packages()
#' if (!surv_installed) {
#' testthat::expect_error(
#' download_kirby21_data("SURVEY", 
#'     force = FALSE))
#' } else {
#' download_kirby21_data("SURVEY", 
#'     force = FALSE)
#' }
#' res = download_kirby21_data("SURVEY", outdir = outdir, force = TRUE)
#' if (!surv_installed) {
#' try({remove.packages("kirby21.survey")})
#' }
#' copy_kirby21_data(copydir = tdir, outdir = outdir)
#' }
copy_kirby21_data = function(copydir, ...){
  niis = get_image_filenames(...)
  stopifnot(length(niis) > 0)
  ### get just the filenames
  
  stubs = file.path(
    basename(dirname(dirname(niis))),
    basename(dirname(niis)),
    basename(niis))
  # stubs = gsub(k21_file, "", niis, fixed = TRUE)
  stubs = gsub("^/", "", stubs)
  
  #################################################
  # Make output directories
  #################################################  
  alldirs = file.path(copydir, dirname(stubs))
  dirs = unique(alldirs)
  direxists = file.exists(dirs)
  mkdirs = dirs[!direxists]
  if (length(mkdirs) > 0) {
    sapply(mkdirs, dir.create, recursive = TRUE)
  }

  for (ifile in seq_along(niis)) {
    nii = niis[ifile]
    file.copy(nii, 
              to = alldirs[ifile], 
              recursive = TRUE)
  }
  return(TRUE)
}