library(rsconnect)
rsconnect::setAccountInfo(name='charlesfainlehman', token='CC92133ADD7968271212E7CA8DE1FD8A', secret=commandArgs(trailingOnly=TRUE)[1])
rsconnect::deployApp()