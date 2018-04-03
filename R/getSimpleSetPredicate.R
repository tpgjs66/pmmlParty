########################################################################
# Function: .getSimpleSetPredicate
#
# Goal: refactor the original code, which creates the simple set predicate
#
# Original by: Togaware
# Refactored by: Zementis, Inc.
# Refactored date: June, 2008

getSimpleSetPredicate <- function(field, op, value)
{
  predicate <- xmlNode("SimpleSetPredicate", attrs=c(field=field,
                                                     booleanOperator=op))
  # 090112 gjw We need to account for embedded commans in the
  # values. This comes up when we have R generated bins which will
  # have a list of valeus like "[402,602],(602,697],(697,763]" or
  # "(101,165],(229,292]". This is getting split incorrectly when
  # splitting on commas. I should get the values directly, using the
  # variable levels, but for now, look out for this special case and
  # deal with it. Another solution would be to change how the values
  # are represented in the binning, replacing the embedded "," with a
  # "-"

  # value <- value[[1]]

  if (length(grep("^[[(][[:digit:]\\.]*", value))>0)
    value <- sub(']]', ']', paste(strsplit(value, "],")[[1]], "]", sep=""))
  else
    value <- strsplit(value, ",")[[1]]

  # 081019 gjw We need quotes around the values since there may be
  # embedded spaces. So we ensure they have quotes. In the PMML this
  # comes out as "&quot;", which when read back into R comes as a '"',
  # so perhaps that is okay? The SPSS generated PMML has actual
  # quotes. We may need to change this to ensure we get actual quotes
  # rather than the XML code for a quote.

  vals <- paste('"', value, '"', collapse=" ", sep="")

  predicate <- append.XMLNode(predicate, xmlNode("Array", vals,
                                                 attrs=c(n=length(value),
                                                         type="string")))

  return(predicate)
}
