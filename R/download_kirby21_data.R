#' @title Download Kirby21 Imaging Data
#' @description This function allows users to download specific
#' modalities for Kirby21 data sets.  This allows this package to be 
#' on CRAN
#'
#' @param modality modality of images that are to be downloaded.  You
#' must have the package downloaded for that modality.
#' @param progress Should verbose messages be printed when downloading 
#' the data
#' @param force If the package of that modality is not installed stop.  If
#' \code{force = FALSE}, then this will download the data but not
#' really install the package.
#' @param outdir output directory for files to download.  It will
#' default to the directory of the corresponding package for the data.
#'
#' @return A logical indicating the data is there.
#' @export
#' @importFrom git2r clone
download_kirby21_data =  function(
  modality = kirby21.base::all_modalities(),
  progress = TRUE,
  force = FALSE,
  outdir = NULL){
  
  modality = match.arg(modality)
  fnames = get_image_filenames(
    modalities = modality,
    ids = get_ids(),
    warn = !force,
    outdir = outdir)
  
  mod_df = kirby21.base::modality_df()
  pkg = mod_df[ mod_df$modality %in% modality, "package"]  
  ##########################################
  # Get installed packages
  packs = installed.packages()
  packs = packs[, "Package"]
  ##########################################
  
  not_installed = !(pkg %in% packs)
  no_pack = pkg[not_installed]
  if (any(not_installed)) {
    if (!force) {
      pkg = paste(pkg, collapse = ", ")
      stop(paste0(pkg, 
                  " Package not installed, must install package", 
                  " first to download the data"))
    } else {
      #########################
      # Hack for CRAN
      # Will still download data, but not really 
      # install the package
      #########################      
      pack_dirs = file.path(.Library, no_pack)
      dir.create(pack_dirs, showWarnings = FALSE)
      sapply(no_pack, function(p){
        pp = c(
          paste0("Package: ", p),
          "Version: 0.0")
        desc_file = file.path(.Library, p, "DESCRIPTION")
        writeLines(text = pp, con = desc_file)
      })
      # desc_files = file.path(pack_dirs, "DESCRIPTION")
      # on.exit({
      #   file.remove(desc_files)
      # })
    }
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
      if (is.null(outdir)) {
        pkg_dir = system.file(package = pkg)
      } else {
        pkg_dir = outdir
      }
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
                                 ids = get_ids(),
                                 warn = !force,
                                 outdir = outdir)
    TRUE
  }, warning = function(w) FALSE)
  return(good)
}

