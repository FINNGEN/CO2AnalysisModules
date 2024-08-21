########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################


## Fill the DESCRIPTION ----
usethis::use_description(
  fields = list(
    Title = "CO2AnalysisModules",
    Description = "Analysis modules to be added to CO2",
    `Authors@R` = 'person("Javier", "Gracia-Tabuenca", email = "javier.graciatabuenca@tuni.fi",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "0000-0002-2455-0598"))',
    Language =  "en"
  )
)
usethis::use_mit_license()


## Create Common Files ----
## See ?usethis for more information
usethis::use_readme_rmd( open = FALSE )
#usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()
usethis::use_github(private = F)

## Use local renv
renv::init()
renv::install('usethis')
usethis::use_git_ignore(c("renv/*", "renv.lock", ".Rprofile"))




