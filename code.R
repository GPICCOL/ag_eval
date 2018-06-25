library(abind)
a1 <- read.csv("data/anna_exam1.csv", stringsAsFactors = FALSE)#exam1
g1 <- read.csv("data/grace_exam1.csv", stringsAsFactors = FALSE)
r1 <- read.csv("data/rachel_exam1.csv", stringsAsFactors = FALSE)
ag1 <- read.csv("data/autog_exam1.csv", stringsAsFactors = FALSE)
a2 <- read.csv("data/anna_exam2.csv", stringsAsFactors = FALSE)#exam2
g2 <- read.csv("data/grace_exam2.csv", stringsAsFactors = FALSE)
r2 <- read.csv("data/rachel_exam2.csv", stringsAsFactors = FALSE)
ag2 <- read.csv("data/autog_exam2.csv", stringsAsFactors = FALSE)
a3 <- read.csv("data/anna_exam3.csv", stringsAsFactors = FALSE)#exam3
g3 <- read.csv("data/grace_exam3.csv", stringsAsFactors = FALSE)
r3 <- read.csv("data/rachel_exam3.csv", stringsAsFactors = FALSE)
ag3 <- read.csv("data/autog_exam3.csv", stringsAsFactors = FALSE)
t1 <- read.csv("data/truth_exam1.csv", stringsAsFactors= FALSE)#truth exam 1 
t2 <- read.csv("data/truth_exam2.csv", stringsAsFactors= FALSE)#truth exam 2 
t3 <- read.csv("data/truth_exam3.csv", stringsAsFactors= FALSE)#truth exam 3 



colnames(a1) <- tolower(colnames(a1))
colnames(g1) <- tolower(colnames(g1))
colnames(r1) <- tolower(colnames(r1))
colnames(ag1) <- tolower(colnames(ag1))
colnames(t1) <- tolower(colnames(t1))

identical(colnames(a3), colnames(ag3))
identical(ag1[1], g1[1])
############Exam 1######################
col_retain <- intersect(colnames(a1), colnames(ag1))
a1 <- a1[2:28, col_retain]
g1 <- g1[2:28, col_retain]
r1 <- r1[2:28, col_retain]

identical(colnames(a1), colnames(ag1))
colnames(ag1)
ag1 <- ag1[2:28, col_retain]
identical(colnames(a1), colnames(ag1))
colnames(ag1)
identical(colnames(a1), colnames(t1))
colnames(t1)
t1 <- t1[2:28, col_retain]
identical(colnames(g1), colnames(t1))
colnames(t1)


aa1 <- abind(as.matrix(a1[, c(3:9)]), as.matrix(g1[, c(3:9)]), as.matrix(r1[, c(3:9)]), as.matrix(ag1[, c(3:9)]), 
             along = 3, new.names = list(unname(unlist(a1[1])), NULL, c("anna", "grace", "rachel", "autograder")))#####for rater agreement

totals1 <- colSums(aa1, dims = 1)
avg1 <- round(rowMeans(aa1, dims = 2), 2)
stdv1 <- round(apply(aa1, c(1,2), sd), 2)

perc_correct1 <- sum(stdv1==0)/(dim(stdv1)[1]*dim(stdv1)[2])

rowSums(aa1, dims = 2)
# write.csv(totals1, file = "totals1.csv")
# write.csv(avg1, file = "avg1.csv")
# write.csv(stdv1, file = "stdv1.csv")

########automatic grading vs truth
agt1 <- abind(as.matrix(ag1[, c(3:9)]), as.matrix(t1[, c(3:9)]), as.matrix(t1[, c(3:9)]),
              along = 3, new.names = list(unname(unlist(ag1[1])), NULL, c("autograder", "truth", "t1")))

agt1sub<- asub(agt1, list(1:27,1:7, 1:2))
totalsAGT1 <- colSums(agt1sub, dims = 1)
avgAGT1 <- round(rowMeans(agt1sub, dims = 2), 2)
stdvAGT1 <- round(apply(agt1sub, c(1,2), sd), 2)

perc_correctT1ag <- sum(stdvAGT1==0)/(dim(stdvAGT1)[1]*dim(stdvAGT1)[2])

rowSums(agt1sub, dims = 2)
######## humans vs truth
aht1 <- abind(as.matrix(a1[, c(3:9)]), as.matrix(g1[, c(3:9)]), as.matrix(r1[, c(3:9)]), as.matrix(t1[, c(3:9)]), 
             along = 3, new.names = list(unname(unlist(a1[1])), NULL, c("anna", "grace", "rachel", "truth")))

totalsT1 <- colSums(aht1, dims = 1)
avgT1 <- round(rowMeans(aht1, dims = 2), 2)
stdvT1 <- round(apply(aht1, c(1,2), sd), 2)

perc_correctT1hum <- sum(stdvT1==0)/(dim(stdvT1)[1]*dim(stdvT1)[2])

rowSums(aht1, dims = 2)








############Exam 2######################
col_retain <- intersect(colnames(a2), colnames(ag2))
a2 <- a2[2:27, col_retain]
g2 <- g2[2:27, col_retain]
r2 <- r2[2:27, col_retain]

identical(colnames(a2), colnames(ag2))
colnames(ag2)
ag2 <- ag2[2:27, col_retain]
identical(colnames(a2), colnames(ag2))
colnames(ag2)
t2 <- t2[2:27, col_retain]
identical(colnames(g2), colnames(t2))
colnames(t2)

aa2 <- abind(as.matrix(a2[, c(2:8)]), as.matrix(g2[, c(2:8)]), as.matrix(r2[, c(2:8)]), as.matrix(ag2[, c(2:8)]), 
            along = 3, new.names = list(unname(unlist(a2[1])), NULL, c("anna", "grace", "rachel", "autograder")))

totals2 <- colSums(aa2, dims = 1)
avg2 <- round(rowMeans(aa2, dims = 2), 2)
stdv2 <- round(apply(aa2, c(1,2), sd), 2)

perc_correct2 <- sum(stdv2==0)/(dim(stdv2)[1]*dim(stdv2)[2])

rowSums(aa2, dims = 2)
# write.csv(totals2, file = "totals2.csv")
# write.csv(avg2, file = "avg2.csv")
# write.csv(stdv2, file = "stdv2.csv")


########automatic grading vs truth
agt2 <- abind(as.matrix(ag2[, c(2:8)]), as.matrix(t2[, c(2:8)]), as.matrix(t2[, c(2:8)]),
              along = 3, new.names = list(unname(unlist(ag2[1])), NULL, c("autograder", "truth", "t2")))

agt2sub<- asub(agt2, list(1:26,1:7, 1:2))
totalsAGT2 <- colSums(agt2sub, dims = 1)
avgAGT2 <- round(rowMeans(agt2sub, dims = 2), 2)
stdvAGT2 <- round(apply(agt2sub, c(1,2), sd), 2)

perc_correctT2ag <- sum(stdvAGT2==0)/(dim(stdvAGT2)[1]*dim(stdvAGT2)[2])

rowSums(agt2sub, dims = 2)
######## humans vs truth
aht2 <- abind(as.matrix(a2[, c(2:8)]), as.matrix(g2[, c(2:8)]), as.matrix(r2[, c(2:8)]), as.matrix(t2[, c(2:8)]), 
              along = 3, new.names = list(unname(unlist(a2[1])), NULL, c("anna", "grace", "rachel", "truth")))

totalsT2 <- colSums(aht2, dims = 1)
avgT2 <- round(rowMeans(aht2, dims = 2), 2)
stdvT2 <- round(apply(aht2, c(1,2), sd), 2)

perc_correctT2hum <- sum(stdvT2==0)/(dim(stdvT2)[1]*dim(stdvT2)[2])

rowSums(aht2, dims = 2)








############Exam 3######################
col_retain <- intersect(colnames(a3), colnames(ag3))
a3 <- a3[2:23, col_retain]
g3 <- g3[2:23, col_retain]
r3 <- r3[2:23, col_retain]

identical(colnames(a3), colnames(ag3))
colnames(ag3)
ag3 <- ag3[2:23, col_retain]
identical(colnames(a3), colnames(ag3))
colnames(ag3)
t3 <- t3[2:23, col_retain]
identical(colnames(g3), colnames(t3))
colnames(t3)
aa3 <- abind(as.matrix(a3[, c(3:10)]), as.matrix(g3[, c(3:10)]), as.matrix(r3[, c(3:10)]), as.matrix(ag3[, c(3:10)]), 
             along = 3, new.names = list(unname(unlist(a3[1])), NULL, c("anna", "grace", "rachel", "autograder")))

totals3 <- colSums(aa3, dims = 1)
avg3 <- round(rowMeans(aa3, dims = 2), 2)
stdv3 <- round(apply(aa3, c(1,2), sd), 2)

perc_correct3 <- sum(stdv3==0)/(dim(stdv3)[1]*dim(stdv3)[2])

rowSums(aa3, dims = 2)
# write.csv(totals3, file = "totals3.csv")
# write.csv(avg3, file = "avg3.csv")
# write.csv(stdv3, file = "stdv3.csv")


########automatic grading vs truth 
agt3 <- abind(as.matrix(ag3[, c(3:10)]), as.matrix(t3[, c(3:10)]), as.matrix(t3[, c(3:10)]),
              along = 3, new.names = list(unname(unlist(ag3[1])), NULL, c("autograder", "truth", "t3")))

agt3sub<- asub(agt3, list(1:22,1:7, 1:2))
totalsAGT3 <- colSums(agt3sub, dims = 1)
avgAGT3 <- round(rowMeans(agt3sub, dims = 2), 2)
stdvAGT3 <- round(apply(agt3sub, c(1,2), sd), 2)

perc_correctT3ag <- sum(stdvAGT3==0)/(dim(stdvAGT3)[1]*dim(stdvAGT3)[2])

rowSums(agt3sub, dims = 2)
######## humans vs truth
aht3 <- abind(as.matrix(a3[, c(3:10)]), as.matrix(g3[, c(3:10)]), as.matrix(r3[, c(3:10)]), as.matrix(t3[, c(3:10)]), 
              along = 3, new.names = list(unname(unlist(a3[1])), NULL, c("anna", "grace", "rachel", "truth")))

totalsT3 <- colSums(aht3, dims = 1)
avgT3 <- round(rowMeans(aht3, dims = 2), 2)
stdvT3 <- round(apply(aht3, c(1,2), sd), 2)

perc_correctT3hum <- sum(stdvT3==0)/(dim(stdvT3)[1]*dim(stdvT3)[2])

rowSums(aht3, dims = 2)










