##HW Assignment 2.1 - 2.3 Template

##Required Libraries
library(ggplot2)
library(ggmosaic)

##Required Functions
n2prop.test<- function(p1, p2, alternative, alpha, power) {

  ztwo<- qnorm(1 - alpha/2)
  zone<- qnorm(1 - alpha)
  zpower<- qnorm(power)

  p0<- (p1 + p2)/2
  R<- sqrt((2*p0*(1-p0))/((p1*(1-p1) + p2*(1-p2))))

  switch(alternative, two.sided = ceiling((zpower + R*ztwo)^2  *(p1*(1-p1)+p2*(1-p2))/(p1-p2)^2), greater = ceiling((zpower + R*zone)^2*(p1*(1-p1)+p2*(1-p2))/(p1-p2)^2), less = ceiling((zpower + R*zone)^2*(p1*(1-p1)+p2*(1-p2))/(p1-p2)^2))
}

n2prop.ci<- function(p1, p2, m, conf.level) {
  alpha<- 1 - conf.level
  z<- qnorm(1 - alpha/2)
  ceiling((z/m)^2*(p1*(1-p1)+p2*(1-p2)))
}

##Required Functions
rr.ci<- function(y, n, conf.level){
  y1<- y[1]
  y2<- y[2]
  n1<- n[1]
  n2<- n[2]
  alpha<- 1 - conf.level
  z<- qnorm(1 - alpha/2)

  phat1<- y1/n1
  phat2<- y2/n2
  rr<- phat1/phat2

  selogrr<- sqrt((1-phat1)/(n1*phat1) + (1-phat2)/(n2*phat2))

  logrr.lower<- log(rr) - z*selogrr
  logrr.upper<- log(rr) + z*selogrr

  rr.lower<- exp(logrr.lower)
  rr.upper<- exp(logrr.upper)

  cat("Estimated Relative Risk = ", rr, "\n")
  cat("Confidence Interval for Population Relative Risk = ", rr.lower, rr.upper, "\n")
}

or.ci<- function(y, n, conf.level){
  y1<- y[1]
  y2<- y[2]
  n1<- n[1]
  n2<- n[2]
  alpha<- 1 - conf.level
  z<- qnorm(1 - alpha/2)

  phat1<- y1/n1
  phat2<- y2/n2

  or<- (phat1/(1-phat1))/(phat2/(1-phat2))

  selogor<- sqrt(1/(n1*phat1) + 1/(n1*(1-phat1)) + 1/(n2*phat2) + 1/(n2*(1-phat2)))

  logor.lower<- log(or) - z*selogor
  logor.upper<- log(or) + z*selogor

  or.lower<- exp(logor.lower)
  or.upper<- exp(logor.upper)

  cat("Estimated Odds Ratio = ", or, "\n")
  cat("Confidence Interval for Population Odds Ratio = ", or.lower, or.upper, "\n")
}
################################################################################
##Enter the R code used to obtain your answers for this HW assignment.
##Upload this R file along with the Answer Sheet to Canvas.
################################################################################

##Problem 1 - Women's Health Study

  ##Part a
whi <- read.csv("WHI.csv")
cont_table <- table(whi$Group, whi$Cancer)
prop_table <- prop.table(cont_table, margin = 1)
p_hormone <- prop_table["Hormone", "Yes"]
p_placebo <- prop_table["Placebo", "Yes"]

  ##Part b
prop_test <- prop.test(cont_table, alternative = "two.sided", correct = FALSE)
  ##Part c
diff_ci <- prop.test(cont_table, conf.level = 0.90, correct = FALSE)$conf.int
  ##Part d
y <- c(cont_table["Hormone", "Yes"], cont_table["Placebo", "Yes"])
n <- c(sum(cont_table["Hormone", ]), sum(cont_table["Placebo", ]))
rr.ci(y, n, conf.level = 0.90)
  ##Part e
or.ci(y, n, conf.level = 0.90)
##Problem 2 - Titanic

  ##Part a
titanic <- read.csv("titanic.csv")
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Ticket), fill = Status)) +
  labs(x = "Ticket Class", title = "Mosaic Plot of Survival by Class")

  ##Part b
titanic_table <- table(titanic$Ticket, titanic$Status)
chisq_test <- chisq.test(titanic_table)

  ##Part c
pairwise.prop.test(titanic_table, p.adjust.method = "bonferroni")
##Problem 3 - Religious Affiliation and Premarital Sex

  ##Part a
gss <- read.csv("GSS.csv")
catholic <- subset(gss, Religion == "Catholic")
cath_dist <- prop.table(table(catholic$Wrong))

  ##Part b
protestant <- subset(gss, Religion == "Protestant")
prot_dist <- prop.table(table(protestant$Wrong))

  ##Part c
ggplot(data = gss) +
  geom_mosaic(aes(x = product(Religion), fill = Wrong)) +
  labs(title = "Attitudes by Religon")

  ##Part d
religion_table <- table(gss$Religion, gss$Wrong)
chisq_gss <- chisq.test(religion_table)

  ##Part e
expected <- chisq_gss$expected
which(expected < 5, arr.ind = TRUE)

