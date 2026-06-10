library(rsconnect)
args <- commandArgs(trailingOnly=TRUE)
rsconnect::setAccountInfo(name='manhattan-institute', token=args[1], secret=args[2])
rsconnect::deployApp(forceUpdate = TRUE)
