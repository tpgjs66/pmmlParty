
############################################################################
# FUNCTION: genPartyNodes
#
# Goal: create nodes for the tree (a recursive function)

genPartyNodes <- function(depths, ids, counts, scores, fieldLabels,
                          ops, values, model, data=data, parent_ii, rows, position)
{
  depth <- depths[1]
  count <- counts[1]
  score <- scores[1]
  fieldLabel <- fieldLabels[1]
  op <- ops[1]
  value <- values[[1]]

  # Added by zementis

  # ff <- model$frame
  id <- ids[1]
  ii <- rows[1]

  # Assign the default child for non-leaf nodes.

  if(length(ids) > 1) { # Non-leaf node
    # sons <- 2*id + c(0,1)
    sons <- nodeids(model, from = id)
    # sons.n <- ff$n[match(sons, ids)]
    sons.n <- counts[match(sons,ids)]

    # childId = sons[2]
    # if(sons.n[1] >= sons.n[2]) childId = sons[1]

    node <- xmlNode("Node", attrs=c(id=id, score=score, recordCount=count))
  } else  {# Leaf node
    node <- xmlNode("Node", attrs=c(id=id,score=score, recordCount=count))
  }

  # Create the predicate for the node

  if (fieldLabel == "root")   {
    predicate <- xmlNode("True")
  } else { # When the node does not have surrogate predicates

    predicate <-  xmlNode("CompoundPredicate",attrs=c(booleanOperator = "surrogate"))

    # Add the primary predicate
    predicate <- append.XMLNode(predicate,getPrimaryPredicates(fieldLabel,op,value))

    # Add the surrogate predicates
    # predicate <- getSurrogatePredicates(predicate, model, parent_ii, position)

    # predicate <- getPrimaryPredicates(fieldLabel, op, value)
  }
  node <- append.XMLNode(node, predicate)

  method <- "classification"
  # if (! is.factor(data[,as.character(formula[[2]])])) method <- "regression"

  # Add score distribution for classification case.

  if(method == "classification") {
    #ylevel <- attr(model,'ylevels')
    # ylevel <- as.character(unique(partykit:::data_party(chaidUS)[,as.character(formula[[2]])]))
    ylevel <- as.character(unique(partykit:::data_party(model)[,"(response)"]))
    #node <- .getScoreDistributions(node, ylevel, ff, ii)
    node <- getScoreDistributions(node, ylevel, model, ii)
  }

  # The recursive function to create child nodes.

  if (length(depths) == 1) {
    child <- NULL
  } else {
    # split.point <- which(depths[c(-1,-2)] == depths[2]) + 1 # Binary tree
    # lb <- 2:split.point
    # rb <- (split.point + 1):length(depths)
    # left <- genPartyNodes(depths[lb], ids[lb], counts[lb], scores[lb], fieldLabels[lb],
    #                             ops[lb], values[lb], model, ii, rows[lb], "left")
    # right <- genPartyNodes(depths[rb], ids[rb],counts[rb], scores[rb], fieldLabels[rb],
    #                              ops[rb], values[rb], model, ii, rows[rb], "right")

    # nsplits <- length(unique(as.list(model$node)[[ii]]$split$index))

    split.points <- which(depths[c(-1,-2)] == depths[2]) + 1 # Binary tree
    # split.points <- as.list(model$node)[[ii]]$kids

    child <- c()
    nb <- list()
    if (length(split.points) > 0){
      for (i in 1:length(split.points)) {
        # if (length(split.points) == 0) {
        #
        # }
        lb <- 2:split.points[i]
        rb <- (split.points[i]+1):length(depths)


        nb <- list(lb,rb)
      }
      for (i in 1:length(nb)) {

        child <- genPartyNodes(depths[nb[[i]]],ids[nb[[i]]],counts[nb[[i]]],
                               scores[nb[[i]]],fieldLabels[nb[[i]]],ops[nb[[i]]],
                               values[nb[[i]]],model,data,ii,rows[nb[[i]]],i)
        node <- append.XMLNode(node,child)
      }
    }
  }
  # if (!is.null(child)) {
  #   node <- append.XMLNode(node,child)
  # }

  # if (!is.null(left)) {
  #   node <- append.XMLNode(node, left)
  #   node <- append.XMLNode(node, right)
  # }

  return(node)
}
