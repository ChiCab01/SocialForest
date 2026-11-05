################ Simulated dataset ################
set.seed(123)
n <- 400

# Variables definition
municipalities <- data.frame(
  Municipality = c("ARDENVALE", "BRAMHOLT", "CALDERIS", "DUNMERE", "ELMSWORTH", "FENNOCK"),
  weight = c(55, 6, 5, 4, 4, 3)
)
k <- nrow(municipalities)
municipalities$Province.Code <- ifelse(municipalities$Municipality == "FENNOCK", "Ostrevia", "Valdoria")
municipalities$Density <- round(c(7700, runif(k - 1, 1500, 7000)))
municipalities$Income_percapita <- round(c(33000, runif(k - 1, 19000, 27000)))
municipalities$Services <- round(c(95, runif(k - 1, 30, 80)), 1)
municipalities$Employment <- round(c(70, runif(k - 1, 60, 72)), 1)
municipalities$Culture <- round(c(90, runif(k - 1, 10, 60)), 1)
municipalities$Perc.online.purchases <- round(c(45, runif(k - 1, 25, 42)), 1)
municipalities$Download <- round(c(300, runif(k - 1, 80, 250)))
municipalities$GreenAreas_perc <- round(c(8, runif(k - 1, 10, 45)), 1)

idx_c <- sample(seq_len(k), n, replace = TRUE, prob = municipalities$weight)
d <- municipalities[idx_c, setdiff(names(municipalities), "weight")]
rownames(d) <- NULL

d$ID <- seq_len(n)
d$Service <- sample(c("SVC1", "SVC2", "SVC3"), n, replace = TRUE)
d$Sex <- sample(c("M", "F"), n, replace = TRUE)
d$Age <- round(runif(n, 18, 75))
d$young <- as.integer(d$Age < 30)
d$Nationality <- sample(c("Velorian", "Ostrian", "Kaldorian", "Tessarine"), n, replace = TRUE, prob = c(.85, .05, .05, .05))
d$D_aggregate <- sample(c("Schizophrenia", "Bipolar", "Depression", "Anxiety", "Generalized epilepsy",
                          "Focal epilepsy", "Eating disorders", NA),
                        n, replace = TRUE, prob = c(.22, .18, .2, .14, .1, .1, .03, .03))
d$psychiatric <- as.integer(!grepl("Epilepsy", d$D_aggregate, ignore.case = TRUE))
d$IllnessDuration <- pmax(0, round(rnorm(n, 10 + 0.15 * (d$Age - 40), 5)))
d$CivilDisability <- sample(c(0, 33, 46, 67, 74, 100), n, replace = TRUE, prob = c(.4, .1, .1, .2, .1, .1))
d$Education <- sample(c("elementary", "middle", "high", "degree"), n, replace = TRUE, prob = c(.1, .35, .4, .15))
d$Profession <- sample(c("unemployed", "blue-collar", "white-collar", "student", "retired"), n, replace = TRUE)
d$MaritalStatus <- sample(c("single", "married", "separated", "widowed"), n, replace = TRUE, prob = c(.55, .3, .12, .03))
d$Children <- rpois(n, 0.7)
d$Siblings <- rpois(n, 1.3)
d$WorkIncapacity <- rbinom(n, 1, .3)
d$AlcoholCurrent <- rbinom(n, 1, .2)
d$AlcoholAbuseCurrent <- rbinom(n, 1, .1)
d$AlcoholPast <- rbinom(n, 1, .25)
d$Cannabis <- sample(0:2, n, replace = TRUE, prob = c(.75, .15, .1))
d$Substances <- sample(c("none", "occasional", "habitual"), n, replace = TRUE, prob = c(.75, .15, .1))
d$PathAcuteSub <- rbinom(n, 1, .1)
d$PathChronic <- rbinom(n, 1, .25)

sev <- c(Schizophrenia = 1, Bipolar = .5, Depression = .3, Anxiety = 0)
cgi_lin <- 3.5 + ifelse(is.na(sev[d$D_aggregate]), 0, sev[d$D_aggregate]) + rnorm(n, 0, 1)
d$CGI <- pmin(7, pmax(1, round(cgi_lin)))

items <- c("HAB_Dwelling", "HAB_Stability", "HAB_Preference", "HAB_Surroundings",
           "NET_Peers", "NET_Gatherings", "NET_Withdrawal", "NET_Prejudice",
           "RES_Budget", "RES_Leisure", "RES_Occasions", "RES_Buffer",
           "ACT_Engagement", "ACT_Conditions", "ACT_Unfairness", "ACT_HealthImpact")
latent <- rnorm(n) + 0.25 * (d$CGI - 4) - 0.00006 * (d$Income_percapita - 25000) +
  0.4 * d$WorkIncapacity + 0.3 * (d$Profession == "unemployed")
for (it in items) d[[it]] <- rbinom(n, 1, plogis(0.5 + 1.3 * latent + rnorm(n, 0, .3)))



# Missing values
miss <- function(x, p) { x[runif(length(x)) < p] <- NA; x }
for (it in items) d[[it]] <- miss(d[[it]], .01)
d$CGI <- ifelse(grepl("Epilepsy", d$D_aggregate, ignore.case = TRUE) & runif(n) < .7, NA, d$CGI)
d$CGI <- miss(d$CGI, .04)
for (v in c("IllnessDuration", "CivilDisability", "Education", "AlcoholCurrent", "AlcoholAbuseCurrent",
            "Cannabis", "AlcoholPast", "Substances")) d[[v]] <- miss(d[[v]], .06)
for (v in c("Profession", "Children", "Siblings", "MaritalStatus")) d[[v]] <- miss(d[[v]], .02)
d$PathAcuteSub <- miss(d$PathAcuteSub, .5)
d$PathChronic <- miss(d$PathChronic, .5)

# Total (target varible) definition
blk <- function(prefix) rowSums(d[, grep(paste0("^", prefix, "_"), names(d))])
d$HAB <- blk("HAB")
d$NET <- blk("NET")
d$RES <- blk("RES")
d$ACT <- blk("ACT")
d$Total <- d$HAB + d$NET + d$RES + d$ACT

write.csv(d, "Project_data.xls", row.names = FALSE, fileEncoding = "UTF-8", na = "")

cat("Rows:", nrow(d), "\n")
print(table(cut(d$Total, c(-Inf, 5, 12, Inf), 
                labels = c("low", "medium", "high")), 
                useNA = "ifany"))