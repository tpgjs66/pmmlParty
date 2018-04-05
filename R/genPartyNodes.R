
############################################################################
# FUNCTION: genPartyNodes
#
# Goal: create nodes for the tree (a recursive function)

genPartyNodes <- function(function.name, depths, ids, counts, scores, fieldLabels,
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
    sons <- partykit:::nodeids(model, from = id)
    # sons.n <- ff$n[match(sons, ids)]
    sons.n <- counts[match(sons,ids)]

    # childId = sons[2]
    # if(sons.n[1] >= sons.n[2]) childId = sons[1]

    node <- xmlNode("Node", attrs=c(id=id, score=score, recordCount=count))
  } else  {# Leaf node
    node <- xmlNode("Node", attrs=c(id=id,score=score, recordCount=count))
  }

  if (function.name == "classification") {

  } else { ## for regression
    ## Data value in the node
    x <- partykit:::data_party(model,ii)$'(response)'

    extension <- xmlNode("Extension")

    xnode <- xmlNode("X-Node")
    xnode <- append.XMLNode(xnode, xmlNode("X-RegInfo", attrs= c(stdDev = sd(x), mean = mean(x))))
    xnode <- append.XMLNode(xnode, xmlNode("X-NodeStats", attrs= c(df2 = "0",
                                                                   df1 = "0",
                                                                   adjPValue = "0",
                                                                   fStats = "0")))
    extension <- append.XMLNode(extension,xnode)

    ## for non-parametric density estimation
    if (partykit:::is.terminal(model[ii]$node)) {
      extension <- append.XMLNode(extension, xmlNode("DataValues", paste(x,collapse=" ")))

      # Compute a (Gaussian) kernel density estimate.
      d <- density(x, kernel = "gaussian", bw = "nrd0", from = min(x), to = max(x))
      # Sample from the KDE.
      width <- d$bw
      #xnode <- append.XMLNode(xnode, xmlNode("KernelDensity", as.data.frame(x)))

      extension <- append.XMLNode(extension, xmlNode("BandWidth", width))
    }

    node <- append.XMLNode(node,extension)
  }

  # Create the predicate for the node

  if (fieldLabel == "root")   {
    predicate <- xmlNode("True")
  } else { # When the node does not have surrogate predicates

    predicate <-  xmlNode("CompoundPredicate",attrs=c(booleanOperator = "surrogate"))

    # Add the primary predicate
    predicate <- append.XMLNode(predicate,getPrimaryPredicates(fieldLabel,op,value))

    predicate <- append.XMLNode(predicate,xmlNode("True"))

    # Add the surrogate predicates
    # predicate <- getSurrogatePredicates(predicate, model, parent_ii, position)

    # predicate <- getPrimaryPredicates(fieldLabel, op, value)
  }
  node <- append.XMLNode(node, predicate)

  # Add score distribution for classification case.

  if(function.name == "classification") {
    #ylevel <- attr(model,'ylevels')
    # ylevel <- as.character(unique(partykit:::data_party(chaidUS)[,as.character(formula[[2]])]))
    ylevel <- as.character(unique(partykit:::data_party(model)[,"(response)"]))
    #node <- .getScoreDistributions(node, ylevel, ff, ii)
    node <- getScoreDistributions(node, ylevel, model, ii ,function.name)
  } else { ## for regression
    node <- getScoreDistributions(node, ylevel, model, ii ,function.name)
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

    split.points <- which(depths==depths[1]+1)
    # split.points <- as.list(model$node)[[ii]]$kids

    child <- c()
    nb <- c()
    if (length(split.points) > 0){
      for (i in 1:length(split.points)) {
        if (i == 1) {
          nb[[i]] <- as.list(2:as.numeric(split.points[i+1]-1))
        } else if (i == length(split.points)) {
          nb[[i]] <- as.list(as.numeric(split.points[i]):as.numeric(length(depths)))
        } else {
          nb[[i]] <- as.list(as.numeric(split.points[i]):as.numeric(split.points[i+1]-1))
        }
      }
      for (i in 1:length(nb)) {
        nnb <- unlist(nb[[i]])
        child <- genPartyNodes(function.name,depths[nnb],ids[nnb],counts[nnb],
                               scores[nnb],fieldLabels[nnb],ops[nnb],
                               values[nnb],model,data,ii,rows[nnb],ii)
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

