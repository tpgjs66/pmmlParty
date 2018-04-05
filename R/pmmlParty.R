
#' @title Generate PMML for party objects
#' @description Generate the PMML representation for a party object from package party and partykit
#' @param model an object of class party
#' @param formula an object of class formula
#' @param data dataset used for training the model
#' @details PMML is an XML based language which provides a way for applications to define statistical and data mining models and to share models between PMML compliant applications. More information about PMML and the Data Mining Group can be found at http://www.dmg.org.
#' This package generates PMML for party objects, and is not a part of the PMML package for R.
#' This package is developed for own use (i.e., CHAID).
#' @import XML
#' @import pmml
#' @return An object of class pmml
#' @export
#' @seealso \code{\link[pmml]{pmml.rpart}} \code{\link[pmml]{pmml.randomForest}}
#' @examples
#' library("CHAID")
#' ### fit tree to subsample
#' set.seed(290875)
#' USvoteS <- USvote[sample(1:nrow(USvote), 1000),]
#' ctrl <- chaid_control(minsplit = 200, minprob = 0.1)
#' formula <- vote3 ~.
#' chaidUS <- chaid(formula = formula, data = USvoteS, control = ctrl)
#'
#' library("pmmlParty")
#' pmml <- pmmlparty(chaidUS, formula, USvoteS)
#' pmml

pmmlparty <- function(model,
                       formula,
                       data = data,
                       model.name="CHAID_Model",
                       app.name="PMML",
                       description="CHAID Decision Tree Model",
                       copyright=NULL,
                       transforms=NULL,
                       unknownValue=NULL,
                       dataset=NULL,
                       ...)
{
  if (! inherits(model, "party")) stop("Not a legitimate party object")
  requireNamespace("party", quietly=TRUE)

  function.name <- "classification"
  if (! is.factor(data[,as.character(formula[[2]])])) function.name <- "regression"


  # Collect the required information.

  # We list all variables, irrespective of whether they appear in the
  # final model. This seems to be the standard thing to do with
  # PMML. It also adds extra information - i.e., the model did not
  # need these extra variables!

  field <- NULL
  field$name <- as.character(attr(model$terms, "variables"))[-1]
  field$name[1] <- as.character(formula[[2]])
  for (i in 1:length(field$name)) {
    field$class[i] <- class(data[,field$name[i]])[1]
  }
  # field$class <- attr(model$terms, "dataClasses")


  for (i in 1:length(field$name)) {
    field$class[i] <- "numeric"
    if (is.factor(data[,field$name[i]])) field$class[i] <- "factor"
    if (is.ordered(data[,field$name[i]])) field$class[i] <- "ordered"
    names(field$class)[i] <-c(field$name[i])
  }

  # 100829 Record the variable used for a weight, if there is one.

  #weights <- model$call$weights
  #if (! is.null(weights))
  #  weights <- gsub("^\\(|\\)$", "",
  #                  gsub("crs\\$dataset\\$|\\[.*\\]", "",
  #                       capture.output(print(weights))))

  # 081229 Our names and types get out of sync for multiple transforms
  # on the one variable. TODO How to fix? For now, we will assume a
  # single transform on each variable.

  ofield <- field

  # 090617 Ensure that the list of fields includes those necessary for
  # the transforms. By this stage the transforms should have removed
  # from it any that are not needed in the model.

  number.of.fields <- length(field$name)

  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    # 090829 Zementis Move the test for factor to inside the test for
    # a target.  This will guarantee that for a non-string target in a
    # classification tree model, the probability can still be output
    # for each category.

    # if (field$class[[field$name[i]]] == "factor")

    if (field$name[i] == target)
      field$levels[[field$name[i]]] <- as.character(unique(data[,as.character(formula[[2]])]))
    else
      if (field$class[[field$name[i]]] == "factor" || field$class[[field$name[i]]] == "ordered")
        field$levels[[field$name[i]]] <- as.character(levels(data[,field$name[i]]))
  }

  pmml <- pmml:::.pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field, dataset, weights=weights,transformed=transforms))

  # PMML -> TreeModel

  the.model <- xmlNode("TreeModel", attrs=c(modelName=model.name,
                                            functionName=function.name,
                                            algorithmName="CHAID",
                                            splitCharacteristic="multiSplit",
                                            missingValueStrategy="",
                                            noTrueChildStrategy="returnLastPrediction"))

  # PMML -> TreeModel -> MiningSchema

  the.model <- append.XMLNode(the.model, pmml:::.pmmlMiningSchema(field, target, transformed=transforms,unknownValue=unknownValue))

  # PMML -> TreeModel -> Output
  if (function.name == "classification"){
  the.model <- append.XMLNode(the.model, pmml:::.pmmlOutput(field, target, switch(function.name,
                                                                           classification="categorical", regression="continuous")))
  }
  # PMML -> TreeModel -> LocalTransformations -> DerivedField -> NormContiuous

  # test of Zementis xform functions
  if(!is.null(transforms))
  {
    the.model <- append.XMLNode(the.model, pmml:::.pmmlLocalTransformations(field, transforms, NULL))
  }

  # PMML -> TreeModel -> Node

  # Collect information to create nodes.

  xmlTreeNode <- buildPartyNode(model,function.name)
  the.model <- append.XMLNode(the.model, xmlTreeNode)

  # Add to the top level structure.

  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}


