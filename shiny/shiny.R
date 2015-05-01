library(devtools)
library(shinyapps)
library(shiny)

### Testing
runApp()

### Run on shiny.io
shinyapps::setAccountInfo(name='bitecology', token='6AA1D337343DD81FC75261BCB0CB252A', secret='wSQQh2qMiHPYWTNvMBLjW5UDdQ6a3uJ6UzR7h4xt')
shinyapps::deployApp()
shinyapps::terminateApp('ppSim')
