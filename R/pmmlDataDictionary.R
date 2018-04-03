
pmmlDataDictionary <- function(field, dataset=NULL, weights=NULL, transformed=NULL)
{
  # 090806 Generate and return a DataDictionary element that includes
  # each supplied field.
  #
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$name
  # field$levels is indexed by fields$name
  #
  # 091003 If the dataset is supplied then also include an Interval
  # element within the DataField for each numeric variable.

  number.of.fields <- length(field$name)


  begin <- 1

  namelist <- list()
  dnamelist <- list()
  optypelist <- list()
  datypelist <- NULL
  fname <- NULL
  data.fields <- list()

  for(i in begin:number.of.fields)
  {
    fname <- field$name[i]
    if(length(grep("as\\.factor\\(",field$name[i])) == 1)
    {
      fname <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[i], perl=TRUE)
    }
    # Determine the operation type

    optype <- "UNKNOWN"
    datype <- "UNKNOWN"
    values <- NULL

    if (field$class[[field$name[i]]] == "numeric")
    {
      optypelist[[fname]] <- "continuous"
      datypelist[[fname]] <- "double"
    }
    else if (field$class[[field$name[i]]] == "logical")
    {
      optypelist[[fname]] <- "categorical"
      datypelist[[fname]] <- "boolean"
    }
    else if (field$class[[field$name[i]]] == "factor")
    {
      optypelist[[fname]] <- "categorical"
      datypelist[[fname]] <- "string"
    }
    else if (field$class[[field$name[i]]] == "ordered")
    {
      optypelist[[fname]] <- "ordinal"
      datypelist[[fname]] <- "string"
    }
    else #catch any other class, including character
    {
      disallowed_class <- field$class[[field$name[i]]]
      stop(paste(disallowed_class, "class is not supported for features. Supported classes: numeric, logical, factor."))
    }
  }

  for (i in begin:number.of.fields)
  {
    # DataDictionary -> DataField
    if(!is.null(transformed) && i!=1)
    {
      if(transformed$fieldData[field$name[i],"type"] == "original")
      {
        if(!(pmml:::.removeAsFactor(field$name[i]) %in% namelist))
        {
          namelist <- c(namelist,pmml:::.removeAsFactor(field$name[i]))
        }

      } else
      {
        ofnames <- strsplit(transformed$fieldData[field$name[i],"origFieldName"][[1]],",")[[1]]
        for(j in 1:length(ofnames))
        {
          ofname <- gsub("^\\s+|\\s+$","",ofnames[j])
          hname <- transformed$fieldData[ofname,"origFieldName"]
          ancestorField <- ofname
          while(!is.na(hname))
          {
            ancestorField <- hname
            hname <- transformed$fieldData[hname,"origFieldName"]
          }
          fname <- pmml:::.removeAsFactor(ancestorField)
          if((!(fname %in% namelist)) && (!(fname %in% dnamelist)))
          {
            namelist <- c(namelist,fname)
            if(!(pmml:::.removeAsFactor(field$name[i]) %in% dnamelist))
              dnamelist <- c(dnamelist, pmml:::.removeAsFactor(field$name[i]))
          }
        }
      }
    } else
    {
      fName <- field$name[i]
      if(!is.na(field$class[fName]) && field$class[fName] == "factor")
        optypelist[[fName]] <- "categorical"

      if(length(grep("as\\.factor\\(",field$name[i])) == 1)
        fName <- gsub("as.factor\\((\\w*)\\)","\\1", field$name[i], perl=TRUE)

      if(!is.na(field$class[fName]) && field$class[fName] == "ordered")
        optypelist[[fName]] <- "ordinal"

      if(!(fName %in% namelist) && fName != "ZementisClusterIDPlaceHolder")
        namelist <- c(namelist,fName)
    }
  }

  # DataDictionary
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numberOfFields=length(namelist)))

  # if (! is.null(weights) && length(weights))
  #   data.dictionary <-append.XMLNode(data.dictionary, xmlNode("Extension",
  #                                                             attrs=c(name="Weights",
  #                                                                     value=weights, extender="Rattle")))

  nmbr <- 1
  for(ndf2 in 1:length(namelist))
  {
    optype <- optypelist[[namelist[ndf2][[1]]]]
    datype <- datypelist[[namelist[ndf2][[1]]]]
    data.fields[[nmbr]] <- xmlNode("DataField", attrs=c(name=namelist[ndf2],
                                                        optype=optype, dataType=datype))

    # DataDictionary -> DataField -> Interval
    fname <- namelist[ndf2][[1]]
    if (optypelist[[fname]] == "continuous" && !is.null(dataset) && fname != "survival")
    {
      dataval <- NULL
      for(j in 1:length(dataset[[fname]]))
      {
        dataval<-c(dataval,as.numeric(dataset[[fname]][j]))
      }

      interval <-  xmlNode("Interval",
                           attrs=c(closure="closedClosed",
                                   leftMargin=min(dataval, na.rm=TRUE), # 091025 Handle missing values
                                   rightMargin=max(dataval, na.rm=TRUE))) # 091025 Handle missing values
      data.fields[[nmbr]] <- append.XMLNode(data.fields[[nmbr]], interval)
    }

    # DataDictionary -> DataField -> Value
    name <- namelist[nmbr][[1]]
    if (optypelist[[name]] == "categorical" || optypelist[[name]] == "ordinal")
    {
      if(is.null(field$levels[[name]]) && !is.null(transformed))
      {
        lev <- levels(as.list(unique(transformed$data[name]))[[1]])
        for (j in seq_along(lev))
        {
          data.fields[[nmbr]][[j]] <- xmlNode("Value",
                                              attrs=c(value=pmml:::.markupSpecials(lev[j])))
        }
      } else
      {
        for (j in seq_along(field$levels[[namelist[nmbr][[1]]]]))
        {
          data.fields[[nmbr]][[j]] <- xmlNode("Value",
                                              attrs=c(value=field$levels[[namelist[nmbr][[1]]]][j]))
          # attrs=c(value=.markupSpecials(field$levels[[namelist[nmbr][[1]]]][j])))
        }
      }
    }

    data.dictionary <- append.XMLNode(data.dictionary, data.fields[[nmbr]])
    nmbr <- nmbr + 1
  }

  return(data.dictionary)
}
