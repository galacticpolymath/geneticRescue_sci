# Script for assembling a Galactic Polymath interdisciplinary lesson plan
# ver 0.2.0

require(remotes)
install_github("galacticpolymath/galacticPubs")
library(galacticPubs)
install_github("galacticpolymath/galacticEdTools")
library(galacticEdTools)

googledrive::drive_auth() #need to authorize your account first time you use this

editor()



