
#############################################################################
# FUNCTION: .getPrimaryPredicates
#
# Goal: add the primary predicate for the node

getPrimaryPredicates <- function(field,op,value)
{
  # if (op %in% c("greaterOrEqual", "lessThan", "equal"))
  # {

  predicate <- xmlNode("CompoundPredicate",
                       attrs=c(booleanOperator = "or"))

  for (i in 1:length(value)) {
  op <- "equal"
  predicate <- append.XMLNode(predicate,xmlNode("SimplePredicate",
                                                attrs=c(field=field,
                                                        operator=op,
                                                        value=value[[i]])))
  }
  # }
  # else if (op == "isIn")
  # {
  #   predicate <- getSimpleSetPredicate(field,op,value)
  # }
  #
  return(predicate)
}
