### How to build an R package from scratch
# Enable universe(s) by kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('kwb.pkgbuild')


usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


author <- list(name = "Michael Rustler",
               orcid = "0000-0003-0647-7726",
               url = "https://mrustl.de")


pkg <- list(name = "geosalz.phreeqc",
            title = "R Package for Preparing Lab Samples as PHREEQC input for project GeoSalz",
            desc  = paste("R Package for Preparing Lab Samples as PHREEQC input for project GeoSalz."))


kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("data-preparation")

### R functions
if(FALSE) {
  ## add your dependencies (-> updates: DESCRIPTION)
  pkg_dependencies <- c("dplyr", "kwb.base", "tidyr", "stringr", "rlang")

  sapply(pkg_dependencies, usethis::use_package)

  desc::desc_add_remotes("kwb-r/kwb.base",normalize = TRUE)

  usethis::use_pipe()
}

kwb.pkgbuild::use_ghactions()

kwb.pkgbuild::create_empty_branch_ghpages("geosalz.phreeqc")
