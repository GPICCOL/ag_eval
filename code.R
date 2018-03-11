library(abind)
a <- read.csv("data/anna.csv", stringsAsFactors = FALSE)
g <- read.csv("data/grace.csv", stringsAsFactors = FALSE)
r <- read.csv("data/rachel.csv", stringsAsFactors = FALSE)
ag <- read.csv("data/autog.csv", stringsAsFactors = FALSE)

colnames(a) <- tolower(colnames(a))
colnames(g) <- tolower(colnames(g))
colnames(r) <- tolower(colnames(r))
colnames(ag) <- tolower(colnames(ag))

identical(colnames(a), colnames(g))
identical(ag[1], g[1])

col_retain <- intersect(colnames(a), colnames(ag))
a <- a[3:52, col_retain]
g <- g[3:52, col_retain]
r <- r[3:52, col_retain]

identical(colnames(a), colnames(ag))
colnames(ag)
ag <- ag[3:52, col_retain]
identical(colnames(a), colnames(ag))
colnames(ag)

aa <- abind(as.matrix(a[, c(4:29)]), as.matrix(g[, c(4:29)]), as.matrix(r[, c(4:29)]), as.matrix(ag[, c(4:29)]), 
            along = 3, new.names = list(unname(unlist(a[1])), NULL, c("anna", "grace", "rachel", "autograder")))

totals <- colSums(aa, dims = 1)
avg <- round(rowMeans(aa, dims = 2), 2)
stdv <- round(apply(aa, c(1,2), sd), 2)

rowSums(aa, dims = 2)

aa[c("9.a"), 1, ]
aa[c("7","9.a"), c("cmcgre7","smanto3"), ]

write.csv(totals, file = "totals.csv")
write.csv(avg, file = "avg.csv")
write.csv(stdv, file = "stdv.csv")

