
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
  if (value[[i]] != "NA") {
  predicate <- append.XMLNode(predicate,xmlNode("SimplePredicate",
                                                attrs=c(field=field,
                                                        operator=op,
                                                        value=value[[i]])))
  }
  }
  value.level <-length(value[!value %in% "NA"])
  if (value.level == 1) {
    predicate <- append.XMLNode(predicate,xmlNode("False"))
  }
  return(predicate)
}
