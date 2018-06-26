## ----include=FALSE-------------------------------------------------------
library(abind)
library(tidyverse)
library(knitr)

tm <- read.csv("data/time.csv", stringsAsFactors = TRUE, na.strings = 0)
a <- read.csv("data/anna_exam1.csv", stringsAsFactors = FALSE)#exam1
g <- read.csv("data/grace_exam1.csv", stringsAsFactors = FALSE)
r <- read.csv("data/rachel_exam1.csv", stringsAsFactors = FALSE)
ag <- read.csv("data/autog_exam1.csv", stringsAsFactors = FALSE)
t <- read.csv("data/truth_exam1.csv", stringsAsFactors= FALSE)#truth exam 1 
names(t)[1] <- "task"
t_scores <- read.csv("data/truth_exam1.csv", stringsAsFactors = FALSE)
names(t_scores)[1] <- "task"


# df <- read.csv("data/chunks.csv", stringsAsFactors = TRUE)
# roster <- read.csv("data/roster.csv", stringsAsFactors = FALSE)
# ct <- read.csv("data/chunk_tasks.csv", stringsAsFactors = FALSE)


## ----reliability_validity, include=FALSE---------------------------------
colnames(a) <- tolower(colnames(a))
colnames(g) <- tolower(colnames(g))
colnames(r) <- tolower(colnames(r))
colnames(ag) <- tolower(colnames(ag))
colnames(t) <- tolower(colnames(t))
colnames(t_scores) <- tolower(colnames(t_scores))


identical(colnames(a), colnames(g))
identical(ag[1], g[1])


col_retain <- intersect(colnames(a), colnames(ag))
a <- a[2:28, col_retain]
g <- g[2:28, col_retain]
r <- r[2:28, col_retain]

identical(colnames(a), colnames(ag))
colnames(ag)
ag <- ag[2:28, col_retain]
identical(colnames(a), colnames(ag))
colnames(ag)

identical(colnames(a), colnames(t))
identical(ag[1], t[1])
t <- t[2:28, col_retain]
identical(colnames(a), colnames(t))

identical(colnames(a), colnames(t_scores))
identical(ag[1], t_scores[1])
t_scores <- t_scores[2:28, col_retain]
identical(colnames(a), colnames(t_scores))

aa <- abind(as.matrix(a[, c(3:9)]), as.matrix(g[, c(3:9)]), as.matrix(r[, c(3:9)]),
            as.matrix(ag[, c(3:9)]), as.matrix(t[, c(3:9)]), abs(as.matrix(t[, c(3:9)])),
            as.matrix(t_scores[, c(3:9)]),
            along = 3, new.names = list(unname(unlist(a[1])), NULL, c("anna", "grace", "rachel", "autograder",
                                                                      "truth", "positive_truth", "true_scores")))

perc_error <- NULL
perc_error["autograder"] <- round(sum(aa[ , , 6])/(dim(aa[ , , 6])[1]*dim(aa[ , , 6])[2])*100, 2)

#perfect <- round(apply(aa[1:50, 1:26, c(4,6)], c(1,2), prod), 2)
#write.csv(perfect, file = "exact_scores.csv")

raters_err <- abind(apply(aa[, , c(1,7)], c(1,2), diff), apply(aa[, , c(2,7)], c(1,2), diff), 
                    apply(aa[, , c(3,7)], c(1,2), diff), apply(aa[, , c(4,7)], c(1,2), diff),
                    along = 3, new.names = list(unname(unlist(a[1])), NULL, c("anna", "grace", "rachel", "autograder")))



points_error <- NULL
perc_error["anna"] <- round(sum(raters_err[ , , c("anna")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["anna"] <- sum(abs(raters_err[ , , c("anna")]))
perc_error["grace"] <- round(sum(raters_err[ , , c("grace")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["grace"] <- sum(abs(raters_err[ , , c("grace")]))
perc_error["rachel"] <- round(sum(raters_err[ , , c("rachel")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["rachel"] <- sum(abs(raters_err[ , , c("rachel")]))
perc_error["autograder"] <- round(sum(raters_err[ , , c("autograder")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["autograder"] <- sum(abs(raters_err[ , , c("autograder")]))

tot_perc_error <- NULL
tot_points_error <- NULL

tot_perc_error["anna"] <- sum(raters_err[ , , c("anna")] == 0)
tot_points_error["anna"] <- sum(abs(raters_err[ , , c("anna")]))
tot_perc_error["grace"] <- sum(raters_err[ , , c("grace")] == 0)
tot_points_error["grace"] <- sum(abs(raters_err[ , , c("grace")]))
tot_perc_error["rachel"] <- sum(raters_err[ , , c("rachel")] == 0)
tot_points_error["rachel"] <- sum(abs(raters_err[ , , c("rachel")]))
tot_perc_error["autograder"] <- sum(raters_err[ , , c("autograder")] == 0)
tot_points_error["autograder"] <- sum(abs(raters_err[ , , c("autograder")]))

tot_dim <- dim(raters_err)[1]*dim(raters_err)[2]

## ------------------------------------------------------------------------
# str(ct)
# ct$exercise <- substring(ct$Practice.Assignemnt, 1, 2)
# mean(ct$Number.of.tasks)
# range(ct$Number.of.tasks)
# with(ct, mean(table(exercise)))
# tot_chunks <- with(ct, tapply(Number.of.tasks, exercise, length))

## ----time----------------------------------------------------------------
str(tm)
sapply(tm[ , 2:4], mean, na.rm= TRUE)
mn <- apply(tm[ , 2:4], 1, mean, na.rm = TRUE)
names(mn) <- tm[, 1]
mn
mean(mn, na.rm = TRUE)

## ----percentage error----------------------------------------------------
kable(perc_error, caption = "Percentage correct")

## ----points error--------------------------------------------------------
kable(points_error, caption = "Total score error")

kable(points_error/(sum(ag$points)*dim(raters_err)[2])*100, caption = "Percentage score error")


## ----include=FALSE-------------------------------------------------------

a <- read.csv("data/anna_exam2.csv", stringsAsFactors = FALSE)#exam1
g <- read.csv("data/grace_exam2.csv", stringsAsFactors = FALSE)
r <- read.csv("data/rachel_exam2.csv", stringsAsFactors = FALSE)
ag <- read.csv("data/autog_exam2.csv", stringsAsFactors = FALSE)
t <- read.csv("data/truth_exam2.csv", stringsAsFactors= FALSE)#truth exam 1 
names(t)[1] <- "task"
t_scores <- read.csv("data/truth_exam2.csv", stringsAsFactors = FALSE)
names(t_scores)[1] <- "task"


## ----reliability_validity 2, include=FALSE-------------------------------
colnames(a) <- tolower(colnames(a))
colnames(g) <- tolower(colnames(g))
colnames(r) <- tolower(colnames(r))
colnames(ag) <- tolower(colnames(ag))
colnames(t) <- tolower(colnames(t))
colnames(t_scores) <- tolower(colnames(t_scores))


identical(colnames(a), colnames(g))
identical(ag[1], g[1])


col_retain <- intersect(colnames(a), colnames(ag))
a <- a[2:27, col_retain]
g <- g[2:27, col_retain]
r <- r[2:27, col_retain]

identical(colnames(a), colnames(ag))
colnames(ag)
ag <- ag[2:27, col_retain]
identical(colnames(a), colnames(ag))
colnames(ag)

identical(colnames(a), colnames(t))
identical(ag[1], t[1])
t <- t[2:27, col_retain]
identical(colnames(a), colnames(t))

identical(colnames(a), colnames(t_scores))
identical(ag[1], t_scores[1])
t_scores <- t_scores[2:27, col_retain]
identical(colnames(a), colnames(t_scores))

aa <- abind(as.matrix(a[, c(2:8)]), as.matrix(g[, c(2:8)]), as.matrix(r[, c(2:8)]),
            as.matrix(ag[, c(2:8)]), as.matrix(t[, c(2:8)]), abs(as.matrix(t[, c(2:8)])),
            as.matrix(t_scores[, c(2:8)]),
            along = 3, new.names = list(unname(unlist(a[1])), NULL, c("anna", "grace", "rachel", "autograder",
                                                                      "truth", "positive_truth", "true_scores")))

perc_error <- NULL
perc_error["autograder"] <- round(sum(aa[ , , 6])/(dim(aa[ , , 6])[1]*dim(aa[ , , 6])[2])*100, 2)

#perfect <- round(apply(aa[1:50, 1:26, c(4,6)], c(1,2), prod), 2)
#write.csv(perfect, file = "exact_scores.csv")

raters_err <- abind(apply(aa[, , c(1,7)], c(1,2), diff), apply(aa[, , c(2,7)], c(1,2), diff), 
                    apply(aa[, , c(3,7)], c(1,2), diff), apply(aa[, , c(4,7)], c(1,2), diff),
                    along = 3, new.names = list(unname(unlist(a[1])), NULL, c("anna", "grace", "rachel", "autograder")))



points_error <- NULL
perc_error["anna"] <- round(sum(raters_err[ , , c("anna")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["anna"] <- sum(abs(raters_err[ , , c("anna")]))
perc_error["grace"] <- round(sum(raters_err[ , , c("grace")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["grace"] <- sum(abs(raters_err[ , , c("grace")]))
perc_error["rachel"] <- round(sum(raters_err[ , , c("rachel")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["rachel"] <- sum(abs(raters_err[ , , c("rachel")]))
perc_error["autograder"] <- round(sum(raters_err[ , , c("autograder")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["autograder"] <- sum(abs(raters_err[ , , c("autograder")]))

tot_perc_error["anna"] <- sum(tot_perc_error["anna"], raters_err[ , , c("anna")] == 0)
tot_points_error["anna"] <- sum(tot_points_error["anna"], abs(raters_err[ , , c("anna")]))
tot_perc_error["grace"] <- sum(tot_perc_error["grace"], raters_err[ , , c("grace")] == 0)
tot_points_error["grace"] <- sum(tot_points_error["grace"], abs(raters_err[ , , c("grace")]))
tot_perc_error["rachel"] <- sum(tot_perc_error["rachel"], raters_err[ , , c("rachel")] == 0)
tot_points_error["rachel"] <- sum(tot_points_error["rachel"], abs(raters_err[ , , c("rachel")]))
tot_perc_error["autograder"] <- sum(tot_perc_error["autograder"], raters_err[ , , c("autograder")] == 0)
tot_points_error["autograder"] <- sum(tot_points_error["autograder"], abs(raters_err[ , , c("autograder")]))

tot_dim <- sum(tot_dim, dim(raters_err)[1]*dim(raters_err)[2])

## ----percentage error 2--------------------------------------------------
kable(perc_error, caption = "Percentage correct")

## ----points error 2------------------------------------------------------
kable(points_error, caption = "Total score error")

kable(points_error/(sum(ag$points)*dim(raters_err)[2])*100, caption = "Percentage score error")


## ----include=FALSE-------------------------------------------------------

a <- read.csv("data/anna_exam3.csv", stringsAsFactors = FALSE)#exam1
g <- read.csv("data/grace_exam3.csv", stringsAsFactors = FALSE)
r <- read.csv("data/rachel_exam3.csv", stringsAsFactors = FALSE)
ag <- read.csv("data/autog_exam3.csv", stringsAsFactors = FALSE)
t <- read.csv("data/truth_exam3.csv", stringsAsFactors= FALSE)#truth exam 1 
names(t)[1] <- "task"
t_scores <- read.csv("data/truth_exam3.csv", stringsAsFactors = FALSE)
names(t_scores)[1] <- "task"


## ----reliability_validity 3, include=FALSE-------------------------------
colnames(a) <- tolower(colnames(a))
colnames(g) <- tolower(colnames(g))
colnames(r) <- tolower(colnames(r))
colnames(ag) <- tolower(colnames(ag))
colnames(t) <- tolower(colnames(t))
colnames(t_scores) <- tolower(colnames(t_scores))


identical(colnames(a), colnames(g))
identical(ag[1], g[1])


col_retain <- intersect(colnames(a), colnames(ag))
a <- a[2:23, col_retain]
g <- g[2:23, col_retain]
r <- r[2:23, col_retain]

identical(colnames(a), colnames(ag))
colnames(ag)
ag <- ag[2:23, col_retain]
identical(colnames(a), colnames(ag))
colnames(ag)

identical(colnames(a), colnames(t))
identical(ag[1], t[1])
t <- t[2:23, col_retain]
identical(colnames(a), colnames(t))

identical(colnames(a), colnames(t_scores))
identical(ag[1], t_scores[1])
t_scores <- t_scores[2:23, col_retain]
identical(colnames(a), colnames(t_scores))

aa <- abind(as.matrix(a[, c(3:10)]), as.matrix(g[, c(3:10)]), as.matrix(r[, c(3:10)]),
            as.matrix(ag[, c(3:10)]), as.matrix(t[, c(3:10)]), abs(as.matrix(t[, c(3:10)])),
            as.matrix(t_scores[, c(3:10)]),
            along = 3, new.names = list(unname(unlist(a[1])), NULL, c("anna", "grace", "rachel", "autograder",
                                                                      "truth", "positive_truth", "true_scores")))

perc_error <- NULL
perc_error["autograder"] <- round(sum(aa[ , , 6])/(dim(aa[ , , 6])[1]*dim(aa[ , , 6])[2])*100, 2)

#perfect <- round(apply(aa[1:50, 1:26, c(4,6)], c(1,2), prod), 2)
#write.csv(perfect, file = "exact_scores.csv")

raters_err <- abind(apply(aa[, , c(1,7)], c(1,2), diff), apply(aa[, , c(2,7)], c(1,2), diff), 
                    apply(aa[, , c(3,7)], c(1,2), diff), apply(aa[, , c(4,7)], c(1,2), diff),
                    along = 3, new.names = list(unname(unlist(a[1])), NULL, c("anna", "grace", "rachel", "autograder")))



points_error <- NULL
perc_error["anna"] <- round(sum(raters_err[ , , c("anna")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["anna"] <- sum(abs(raters_err[ , , c("anna")]))
perc_error["grace"] <- round(sum(raters_err[ , , c("grace")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["grace"] <- sum(abs(raters_err[ , , c("grace")]))
perc_error["rachel"] <- round(sum(raters_err[ , , c("rachel")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["rachel"] <- sum(abs(raters_err[ , , c("rachel")]))
perc_error["autograder"] <- round(sum(raters_err[ , , c("autograder")] == 0)/(dim(raters_err)[1]*dim(raters_err)[2])*100, 2)
points_error["autograder"] <- sum(abs(raters_err[ , , c("autograder")]))

tot_perc_error["anna"] <- sum(tot_perc_error["anna"], raters_err[ , , c("anna")] == 0)
tot_points_error["anna"] <- sum(tot_points_error["anna"], abs(raters_err[ , , c("anna")]))
tot_perc_error["grace"] <- sum(tot_perc_error["grace"], raters_err[ , , c("grace")] == 0)
tot_points_error["grace"] <- sum(tot_points_error["grace"], abs(raters_err[ , , c("grace")]))
tot_perc_error["rachel"] <- sum(tot_perc_error["rachel"], raters_err[ , , c("rachel")] == 0)
tot_points_error["rachel"] <- sum(tot_points_error["rachel"], abs(raters_err[ , , c("rachel")]))
tot_perc_error["autograder"] <- sum(tot_perc_error["autograder"], raters_err[ , , c("autograder")] == 0)
tot_points_error["autograder"] <- sum(tot_points_error["autograder"], abs(raters_err[ , , c("autograder")]))

tot_dim <- sum(tot_dim, dim(raters_err)[1]*dim(raters_err)[2])


## ----percentage error 3--------------------------------------------------
kable(perc_error, caption = "Percentage correct")

## ----points error 3------------------------------------------------------
kable(points_error, caption = "Total score error")

kable(points_error/(sum(ag$points)*dim(raters_err)[2])*100, caption = "Percentage score error")

perc_error <- NULL
points_error <- NULL
perc_error["anna"] <- round(tot_perc_error["anna"]/tot_dim*100, 2)
points_error["anna"] <- tot_points_error["anna"]
perc_error["grace"] <- round(tot_perc_error["grace"]/tot_dim*100, 2)
points_error["grace"] <- tot_points_error["grace"]
perc_error["rachel"] <- round(tot_perc_error["rachel"]/tot_dim*100, 2)
points_error["rachel"] <- tot_points_error["rachel"]
perc_error["autograder"] <- round(tot_perc_error["autograder"]/tot_dim*100, 2)
points_error["autograder"] <- tot_points_error["autograder"]

## ----percentage error tot--------------------------------------------------
kable(perc_error, caption = "Percentage correct")

## ----points error tot------------------------------------------------------
kable(points_error, caption = "Total score error")

#kable(points_error/(sum(ag$points)*dim(raters_err)[2])*100, caption = "Percentage score error")
