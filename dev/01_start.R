#
#
#
golem::fill_desc(
  pkg_name = "tellinkiappi",
  pkg_title = "Kaupunkipyöräjärjestelmän tiedot verkossa",
  pkg_description = "Verkkosovellus Helsingin kaupunkin kaupunkipyöräjärjestelmän tietojen esittämiseen",
  author_first_name = "Markus",
  author_last_name = "Kainu",
  author_email = "markus.kainu@kapsi.fi",
  repo_url = NULL
)
golem::set_golem_options()
usethis::use_mit_license("Markus Kainu")
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
usethis::use_git()
golem::use_recommended_tests()
golem::use_favicon()
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)
rstudioapi::navigateToFile("dev/02_dev.R")
