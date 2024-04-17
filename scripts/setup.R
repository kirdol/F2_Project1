# load the required packages and install them if they are not.
packages_loaded <- c(
)

# Function that install the packages if not already installed on your computer
for (pkg in packages_loaded) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg)}}

# load the packages
for (pkg in packages_loaded) {
  library(pkg, character.only = TRUE)}

# cleaning of the environment
rm(pkg)

