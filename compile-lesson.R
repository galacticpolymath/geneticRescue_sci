# Script for assembling a Galactic Polymath interdisciplinary lesson plan
# ver 0.2.0

require(remotes)
install_github("galacticpolymath/galacticPubs")
install_github("galacticpolymath/galacticEdTools")
library(galacticPubs)
library(galacticEdTools)

googledrive::drive_auth() #need to authorize your account first time you use this

editor()



