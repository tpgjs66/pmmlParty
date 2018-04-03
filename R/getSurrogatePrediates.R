

##############################################################################
# function: .getSurrogatePredicates
#
# goal: add the surrogate predicates to take care of the missing value cases
#
# author: Zementis, Inc.
#
# date: June, 2008
##############################################################################
getSurrogatePredicates <- function(predicate, model,i,position)
{



  for (i in 1:length(nodeids(model))) {
    is.leaf<-partykit:::is.terminal(model[[i]]$node)
    if (!is.leaf) {

    }
  }
  op <- "isIn"

  # index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + 1*(!is.leaf)))
  #
  # # j: indices of the surrogate predicates in the splits (list) for the current node
  # j <- seq(1 +index[i] + ff$ncompete[i], length.out=ff$nsurrogate[i])
  #
  # predicateNameList <- dimnames(model$splits)[[1]]
  # predicateSignList <- model$splits[,2]
  # predicateValueList <- model$splits[,4]
  #
  # # n: number of surrogate predicates in the current node
  # n<- length(predicateNameList[j])
  # currentNodePredicateNameList <- predicateNameList[j]
  # currentNodeSignList <- predicateSignList[j]
  # currentNodeValueList <- predicateValueList[j]
  #
  # # for simple set predicate
  # digits=getOption("digits")
  # temp <- model$splits[,2]
  # cuts <- vector(mode='character', length=nrow(model$splits))
  # for (k in 1:length(cuts))
  # {
  #   if (temp[k] == -1)
  #     cuts[k] <-paste("<", format(signif(model$splits[k,4], digits=digits)))
  #   else if (temp[k] ==1)
  #     cuts[k] <-paste("<", format(signif(model$splits[k,4], digits=digits)))
  #   else cuts[k]<- paste(c("L", "-", "R")[model$csplit[model$splits[k,4], 1:temp[k]]], collapse='', sep='')
  # }
  # currentNodeCutsList <- cuts[j]
  # field <- NULL
  # field$name <- as.character(attr(model$terms, "variables"))[-1]
  # number.of.fields <- length(field$name)
  # field$class <- attr(model$terms, "dataClasses")
  # target <- field$name[1]
  #
  # for (i in 1:number.of.fields)
  # {
  #   if (field$class[[field$name[i]]] == "factor")
  #   {
  #     if (field$name[i] == target)
  #     {
  #       field$levels[[field$name[i]]] <- attr(model, "ylevels")
  #     } else
  #     {
  #       field$levels[[field$name[i]]] <- attr(model,"xlevels")[[field$name[i]]]
  #     }
  #   }
  # }

  # # generate simple set predicate
  # for (k in 1:n)
  # {
  #   if(position == "left")
  #   {
  #     if(currentNodeSignList[[k]]==1)
  #     {
  #       op <- "greaterOrEqual"
  #     } else if (currentNodeSignList[[k]]== -1)
  #     {
  #       op <- "lessThan"
  #     } else {
  #       op <- "isIn"
  #     }
  #   } else if(position == "right")
  #   {
  #     if(currentNodeSignList[[k]]==1)
  #     {
  #       op <- "lessThan"
  #     } else if (currentNodeSignList[[k]]==-1)
  #     {
  #       op <- "greaterOrEqual"
  #     } else
  #     {
  #       op <- "isIn"
  #     }
  #   }
  #
  #   if (op == "isIn" && position == "left")
  #   {
  #     # simple set predicate for a left node
  #     value1 <- strsplit(currentNodeCutsList[k],"")
  #     value <- NULL
  #     for(s in 1:length(value1[[1]]))
  #     {
  #       if(value1[[1]][s] == "L")
  #       {
  #         value <-  paste(value, field$levels[[currentNodePredicateNameList[k]]][s],sep="")
  #         value <- paste(value,",",sep="")
  #       }
  #     }
  #     predicate <-  append.XMLNode(predicate,.getSimpleSetPredicate(currentNodePredicateNameList[k],op,value))
  #   } else if( op =="isIn" && position == "right")
  #   {
  #     # simple set predicate for a right node
  #     value1 <- strsplit(currentNodeCutsList[k],"")
  #     value <- NULL
  #     for(s in 1:length(value1[[1]])) {
  #       if(value1[[1]][s] == "R") {
  #         value <-  paste(value, field$levels[[currentNodePredicateNameList[k]]][s],sep="")
  #         value <- paste(value,",",sep="")
  #       }
  #     }
  #     predicate <-  append.XMLNode(predicate,.getSimpleSetPredicate(currentNodePredicateNameList[k],op,value))
  #   } else
  #   {
  #     predicate<- append.XMLNode(predicate,xmlNode("SimplePredicate",attrs=c(field=currentNodePredicateNameList[k], operator=op,
  #                                                                            value=currentNodeValueList[[k]])))
  #   }
  # }
  return(predicate)
}

