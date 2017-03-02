#' @title Download Kirby21 Imaging Data
#' @description This function allows users to download specific
#' modalities for Kirby21 data sets.  This allows this package to be 
#' on CRAN
#'
#' @param modality modality of images that are to be downloaded.  You
#' must have the package downloaded for that modality.
#' @param progress Should verbose messages be printed when downloading 
#' the data
#'
#' @return A logical indicating the data is there.
#' @export
#' @importFrom git2r clone
download_kirby21_data =  function(
  modality = kirby21.base::all_modalities(),
  progress = TRUE){
  
  modality = match.arg(modality)
  fnames = get_image_filenames(modalities = modality,
                               ids = get_ids())
  
  mod_df = kirby21.base::modality_df()
  pkg = mod_df[ mod_df$modality %in% modality, "package"]  
  ##########################################
  # Get installed packages
  packs = installed.packages()
  packs = packs[, "Package"]
  ##########################################
  
  not_installed = !(pkg %in% packs)
  if (not_installed) {
    stop(paste0(pkg, " Package not installed, must install package", 
                " first to download the data"))
  }
  if (is.null(fnames)) {
    fnames = ""
  }
  if (!all(file.exists(fnames))) {
    
    #### clone repo (which has the data because not in .gitignore,
    # but in .Rbuildignore
    tfile = tempfile()
    dir.create(tfile)
    tdir = file.path(tfile, pkg)
    git2r::clone(
      paste0("https://github.com/muschellij2/", pkg),
      local_path = tdir,
      progress = progress
    )
    ################################
    # Grab the imaging data, leave the CITATION alone
    ################################
    datadir = file.path(tdir, "inst")
    files = list.files(path = datadir, recursive = TRUE)
    files = files[ !(files %in% "CITATION")]
    if (length(files) > 0) {
      pkg_dir = system.file(package = pkg)
      infiles = file.path(datadir, files)
      outfiles = file.path(pkg_dir, files)
      dn = dirname(outfiles)
      sapply(dn, dir.create, showWarnings = FALSE,
             recursive = TRUE)
      file.copy(infiles, outfiles, overwrite = TRUE)
    } else {
      stop("There are no files in this repo - please email maintainer")
    }
  }
  good = tryCatch({
    fnames = get_image_filenames(modalities = modality,
                                 ids = get_ids())
    TRUE
  }, warning = function(w) FALSE)
  return(good)
}

