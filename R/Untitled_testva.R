# x <- c(23, 56, 74, 58) ## ETDRS letters
# va(x)
#
# va(x, to = "snellen") ## ... or convert to snellen
# va(x, to = "snellen", type = "m") ## ... or convert to snellen
#
# #
# ## A mix of notations
# x <- c("NLP", "0.8", "34", "3/60", "2/200", "20/50", "  ", ".", "-", "NULL")
# va(x)
#
# ## "plus/minus" entries are converted to the most probable threshold (any spaces allowed)
# x <- c("20/200", "20/200 - 1", "6/6", "6/6-2", "20/50 + 3", "20/50 -2")
# va(x)
#
# ## or evaluating them as logmar values
# va(x, logmarstep = TRUE)
#
