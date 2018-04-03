#######################################################################
# function: .getScoreDistributions
#
# goal: extract the probability for each category (only for classification case)
#
# author: Zementis, Inc.
#
# date: June, 2008
#######################################################################
getScoreDistributions <- function(node, ylevel, model, ii)
{
  # ii: the sequential oreder of the current node

  # n: number of classification categories
  n = length(ylevel)
  # recordCountMap <- ff$yval2[ii,2:(1+n) ,drop=TRUE]
  # confidenceMap <- ff$yval2[ii,(n+2):(2*n+1) ,drop=TRUE]
  recordCountMap <- as.matrix(table(partykit:::data_party(model,ii+1)$'(response)'))

  for(i in 1:n)
  {
    recordCount <- ifelse(is.na(recordCountMap[i]), 0, recordCountMap[ylevel[i],])
    prob <- recordCount/sum(recordCountMap)
    # confidence <- ifelse(is.na(confidenceMap[i]), 0, confidenceMap[i])

    node <- append.XMLNode(node,xmlNode("ScoreDistribution",
                                        attrs = c(value=ylevel[i],
                                                  recordCount = recordCount)))
    node <- append.XMLNode(node,xmlNode("Extension",attrs = c(extender="SeheonKim",
                                                              names = "probability", value = prob)))
  }
  return(node)
}
