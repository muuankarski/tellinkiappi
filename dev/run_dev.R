# Sass code compilation
# sass::sass(input = sass::sass_file("inst/app/www/custom.sass"), output = "inst/app/www/custom.css", cache = NULL)

options(golem.app.prod = FALSE)
options(shiny.port = httpuv::randomPort())
options(shiny.useragg = TRUE)
golem::detach_all_attached()
golem::document_and_reload()
run_app(enableBookmarking = "url")

