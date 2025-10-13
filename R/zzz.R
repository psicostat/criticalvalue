.onAttach <- function(libname, pkgname) {
  packageStartupMessage("criticalESvalue v", utils::packageVersion(pkgname), "!")
}