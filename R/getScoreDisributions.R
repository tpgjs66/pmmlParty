#######################################################################
# function: .getScoreDistributions
#
# goal: extract the probability for each category (only for classification case)
#
# author: Zementis, Inc.
#
# date: June, 2008
#######################################################################
getScoreDistributions <- function(node, ylevel, model, ii, function.name)
{
  
  x <- partykit:::data_party(model,ii)$'(response)'
  
  if (function.name == "classification") {
  # ii: the sequential order of the current node

  # n: number of classification categories
  n = length(ylevel)
  # recordCountMap <- ff$yval2[ii,2:(1+n) ,drop=TRUE]
  # confidenceMap <- ff$yval2[ii,(n+2):(2*n+1) ,drop=TRUE]

  recordCountMap <- as.matrix(table(x))

  for(i in 1:n)
  {
    recordCount <- ifelse(is.na(recordCountMap[i]), 0, recordCountMap[ylevel[i],])
    prob <- recordCount/sum(recordCountMap)
    # confidence <- ifelse(is.na(confidenceMap[i]), 0, confidenceMap[i])
    scoreDist <-xmlNode("ScoreDistribution",
                        attrs = c(value=ylevel[i],
                                  recordCount = recordCount))
    scoreDist <- append.XMLNode(scoreDist,xmlNode("Extension",
                                                  attrs = c(extender="pmmlParty",
                                                            names = "probability",
                                                            value = prob)))
    node <- append.XMLNode(node,scoreDist)

  }
  } else { ## for regression tree
    
    breaks <- seq(min(x),max(x), by = (max(x)-min(x))/10)
    recordCount <- hist(x,breaks=breaks,plot=F)$counts
  
    for (i in 0:9) {
      scoreDist <-xmlNode("ScoreDistribution",
                          attrs = c(value=i,
                                    recordCount = recordCount[i+1]))
      node <- append.XMLNode(node,scoreDist)
    }
  }
  return(node)
}




