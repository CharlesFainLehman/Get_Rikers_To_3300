library(rsconnect)
rsconnect::setAccountInfo(name='manhattan-institute', token='0AFD0942A9173E026C2B630AD17DE92F', secret=commandArgs(trailingOnly=TRUE)[1])
rsconnect::deployApp()