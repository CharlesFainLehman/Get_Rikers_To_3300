library(rsconnect)
rsconnect::setAccountInfo(name='charlesfainlehman', token='6EA6B48E0C14C5CC47C55277A75548C3', secret=commandArgs(trailingOnly=TRUE)[1])
rsconnect::deployApp()