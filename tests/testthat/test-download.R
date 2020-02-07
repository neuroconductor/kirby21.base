testthat::context("Trying a Downloading")
on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
on_ci <- nzchar(Sys.getenv("CI"))
local_run = grepl("musch", tolower(Sys.info()[["user"]]))
run_example = !on_cran || on_ci || local_run

testthat::test_that("Download Data", {
  if (run_example) {
    tdir = tempfile()
    dir.create(tdir)
    outdir = tempdir()
    surv_installed = "kirby21.survey" %in% installed.packages()
    if (!surv_installed) {
      testthat::expect_error(
        download_kirby21_data("SURVEY", 
                              force = FALSE))
    }
    res = download_kirby21_data("SURVEY", outdir = outdir, force = TRUE)
    if (!surv_installed) {
      try({remove.packages("kirby21.survey")}, silent = TRUE)
    }    
    if (surv_installed) {
      testthat::expect_true({
        res = download_kirby21_data("SURVEY", outdir = outdir)
      })
    }
  }
})
