if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(version = "3.10")
BiocManager::install("curatedBreastData")  # and dependencies

# BiocManager::install("Fletcher2013a")
# BiocManager::install("Fletcher2013b")

install.packages("devtools")
library(devtools)
install_github('ramhiser/datamicroarray')

