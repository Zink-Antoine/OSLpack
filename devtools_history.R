usethis::use_build_ignore("devtools_history.R")
devtools::use_package('rv')
devtools::use_package('R2WinBUGS')
devtools::use_package('Luminescence')
usethis::use_vignette("OSLpack")
devtools::use_package('tcltk')
usethis::use_data(Anatolian2)
usethis::edit_r_profile('project')
codemetar::write_codemeta()
