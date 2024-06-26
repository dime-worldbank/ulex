if(F){
  setwd("~/Documents/Github/ulex")

  roxygen2::roxygenise("~/Documents/Github/ulex")

  pkgdown::clean_site()
  pkgdown::deploy_to_branch()
  usethis::use_pkgdown_github_pages() #####
  usethis::use_github_action_check_standard()

  ## Comand line code for building and checking package
  #R CMD build --as-cran "~/Documents/Github/googletraffic"
  #R CMD check --as-cran "~/Documents/Github/googletraffic/googletraffic_0.0.0.9000.tar.gz"

  pkgdown::build_favicons(pkg = "~/Documents/Github/ulex",
                          overwrite = FALSE)

  devtools::check("~/Documents/Github/ulex")

  devtools::check_win_devel("~/Documents/Github/ulex")
  devtools::check_win_release("~/Documents/Github/ulex")
  devtools::check_win_oldrelease("~/Documents/Github/ulex")

  devtools::build("~/Documents/Github/ulex")
}
