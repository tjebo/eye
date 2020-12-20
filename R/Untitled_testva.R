# # library(eye)
# x <- c("NLP", "NPL", "0.8", "34", "3/60", "2/200", "20/50", "  ", ".", "-", "NULL")
# va(x, to = "logmar")
# va(x, to = "snellen", from = "etdrs")
# va(x, to = "snellen", from = "snellendec")
# # va(c("20/50"), to = "logmar")
# # va(1:100, to = "logmar")
# # va(NA, to = "logmar")
# # va("a", to = "logmar")
# # va(c(0.1, "nlp" , "a", NA), to = "logmar")
# # va(c(0.1, "3" , "a", NA), to = "logmar", from = "snellen")
# # va(c(1, "5" , "a", NA), to = "logmar")
# # va(x, to = "logmar", from = "snellen")
# # va(x, to = "logmar", from = "quali")
# # va(NA, to = "logmar", from = "snellen")
# # va(c(23, 56, 74, 58), to = "logmar")
# # va(c(23, 56, 74, NA), to = "logmar")
# # va(c("20/32", 56, 74, NA), to = "logmar")
# #
# # x <- c("NLP", "0.8", "34", "3/60", "2/200", "20/50", "  ", ".", "-", "NULL")
# # va(x, to = "logmar")
# #
# # which_va(x)
# # #
# # ## A mix of notations
# # x <- c("NLP", "0.8", "34", "3/60", "2/200", "20/50", "  ", ".", "-", "NULL")
# # va(x)
# #
# # ## "plus/minus" entries are converted to the most probable threshold (any spaces allowed)
# # x <- c("20/200", "20/200 - 1", "6/6", "6/6-2", "20/50 + 3", "20/50 -2")
# # va(x)
# #
# #
# #
# #
# #
# # # x <- c("NLP", "NPL", "0.8", "34", "3/60", "2/200", "20/50", "  ", ".", "-", "NULL")
# # va(x, to = "logmar")
# # # va(x, to = "snellen", type = "m")
# # # va(x, to = "etdrs")
# # va(x, from = "logmar", to = "snellen")
# # va(x, from = "logmar", to = "etdrs")
# # va(x, to = "etdrs")
# #
# # ## or evaluating them as logmar values
# # va(x, logmarstep = TRUE)
# # va(x, logmarstep = FALSE)
# #
# #
# #
# #
# #
# #
# # va(c("nlp", NA, "npl"), to = "logmar")
# # va(x, to = "etdrs")
# # va(x, to = "logmar")
# # va(x, to = "snellendec")
#
#
#
#
# va(x, to = "logmar")
# va(x, to = "snellen", from = "etdrs")
# xx <- va(x, to = "snellen", from = "snellendec")
# xx <- xx[[2]]
# convertVA(xx, to = 'snellen', type = "ft", FALSE)
