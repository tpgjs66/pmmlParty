

pmmlHeader<- function (description, copyright, app.name) 
{
  if (is.null(copyright)) 
    copyright <- "Copyright (c) 2018 pmmlParty KimSeheon"
  header <- xmlNode("Header", attrs = c(copyright = copyright, 
                                        description = description))
  header <- append.XMLNode(header, xmlNode("Extension", attrs = c(name = "user", 
                                                                  value = sprintf("%s", Sys.info()["user"]), extender = app.name)))
  VERSION <- "0.1"
  header <- append.XMLNode(header, xmlNode("Application", 
                                           attrs = c(name = app.name, version = VERSION)))
  header <- append.XMLNode(header, xmlNode("Timestamp", sprintf("%s", 
                                                                Sys.time())))
  return(header)
}
