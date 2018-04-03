
############################################################################
# .buildPartyNode
buildPartyNode <- function(model,function.name)
{
  numnode <- partykit:::nodeids(model)
  counts <- c()
  for (i in 1:length(numnode)) {
    # treedepth[i] <- partykit:::depth.party(model[i])
    # depths[i] <- abs(treedepth[i] - max(treedepth))
    counts[i] <- as.numeric(partykit:::.nobs_party(model,id=i))
  }
  depths<-table(unlist(sapply(numnode, function(x) intersect(numnode, nodeids(model, from = x))))) - 1

  label <- labels(model, minlength=0, digits=7)
  fieldLabels <- "root"
  ops <- ""
  values<-list()
  values[[1]]<- "" #list("")

  # Added by Zementis: Information to create nodes.
  ids <- nodeids(model) -1 # node id starts from 0
  rows <- nodeids(model) -1 # node id starts from 0
  parent_ii <- 0  # node id starts from 0

  # Check the function.name.
  scores <-c()
  for (i in 1:length(numnode)) {
    scores[i] <- as.character(predict_party(model,id=i, newdata=data_party(model)))
  }

  # Get the information for the primary predicates

  if (length(numnode) > 1) {
    for (i in 2:length(numnode)) {
      noderule <- partykit:::.list.rules.party(model,i)
      noderule <- gsub(" ", "", noderule)

      noderule <- unlist(strsplit(noderule, '&'))
      n <- length(noderule)
      noderule <- noderule[n]

      var <-  strsplit(noderule, '%in%')[[1]][1]
      fieldLabels <- c(fieldLabels, var)

      ops <- c(ops, "isIn")

      noderule <- strsplit(noderule, '%in%')[[1]][2]

      noderule <- substring(noderule,4,nchar(noderule)-2)
      noderule <- unlist(strsplit(noderule,"[,]|[^[:print:]]",fixed=F))

      tt <- gsub("\"","",noderule)
      values[[i]] <- tt
    }
    nodes <- genPartyNodes(depths, ids, counts, scores, fieldLabels, ops, values,
                           model, data, parent_ii, rows,"right")
  } else {
    nodes <- genPartyNodes(depths, ids, counts, scores, fieldLabels, ops, values,
                           model, data, parent_ii, rows,"right")
  }
  # return(node)
}
