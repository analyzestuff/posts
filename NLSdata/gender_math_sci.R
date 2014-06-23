library(NLSdata)

codebook <- system.file("Investigator", "Occupations.cdb", package = "NLSdata")
csv <- system.file("Investigator", "Occupations.csv", package = "NLSdata")

nls <- CreateNLSdata(codebook, csv)

summary(nls)
KeywordSearch("programming", nls)

nls$metadata[["YSCH_23104~000003.1998"]]

static.df <- nls$data[, c("PUBID.1997",
                          "KEY!SEX.1997",
                          "CV_HH_POV_RATIO.1997",
                          "YSCH_33000_000003.1997",
                          paste0("YSCH_36", c("4", "5"), "00.1997"),
                          "FP_ADENRCHI.1997",
                          paste0("YSCH_23104~000003.", c("1998", "2001")),
                          "YEXP_4750.2000",
                          paste0("YSAQ_282", c("J", "K", "L", "M", "N", "0",
                                               "P", "Q"), ".2002"))]

names(static.df) <- c("PUBID.1997",
                      "gender",
                      "poverty.ratio",
                      "hs.prog.1997",
                      "teachers.good",
                      "teachers.interested",
                      "enriching.environment",
                      "prog.course.since.dli.1998",
                      "prog.course.since.dli.2001",
                      "pr.five.yr.marriage",
                      "organized",
                      "conscientious",
                      "dependable",
                      "thorough",
                      "agreeable",
                      "cooperative",
                      "flexible",
                      "trustful")

static.df <- EncodeFactorAsNumeric("teachers.good", c(4, 3, 2, 1), static.df)
static.df <- EncodeFactorAsNumeric("teachers.interested", c(4, 3, 2, 1),
                                   static.df)
static.df <- EncodeFactorAsNumeric("hs.prog.1997", c(1, 0), static.df)
static.df <- EncodeFactorAsNumeric("prog.course.since.dli.1998", c(1, 0),
                                   static.df)
static.df <- EncodeFactorAsNumeric("prog.course.since.dli.2001", c(1, 0),
                                   static.df)

KeywordSearch("occupation", nls)

occ.df <- RosterToLongDf(nls$data, "YEMP_OCCODE_2002")
occ.df$math.cs <- (occ.df$YEMP_OCCODE_2002 >= 1000 &
                   occ.df$YEMP_OCCODE_2002 <= 1240)

agg.math.cs <- aggregate(math.cs ~ PUBID.1997, data = occ.df, FUN = max)
XS.df <- merge(agg.math.cs, static.df, all.x = TRUE, by = "PUBID.1997") 

library(mice)
mids <- mice(XS.df[, 2:ncol(XS.df)], m = 10, maxit = 15, seed = 143252)
mira.main.effects <- with(mids, glm(math.cs ~ gender + 
                                    poverty.ratio + 
                                    hs.prog.1997 + 
                                    teachers.good +
                                    teachers.interested +
                                    enriching.environment + 
                                    prog.course.since.dli.1998 +
                                    prog.course.since.dli.2001 +
                                    pr.five.yr.marriage +
                                    organized +
                                    conscientious +
                                    dependable +
                                    thorough +
                                    agreeable +
                                    cooperative +
                                    flexible +
                                    trustful, family = binomial))
main.effects.summary <- summary(pool(mira.main.effects))

main.plot.df <- as.data.frame(main.effects.summary)[-1, ]
main.plot.df$predictor <- factor(rownames(main.plot.df),
                               levels = unique(rownames(main.plot.df)))
main.plot.df$fdr.p.value <- p.adjust(main.plot.df[, "Pr(>|t|)"], method = "fdr")

library(ggplot2)
ggplot(main.plot.df,
       aes(x = predictor, y = t, fill = fdr.p.value)) +
       geom_bar(stat = 'identity') + coord_flip() + 
       xlab("Predictor of Future Math/Science Occupation") +
       ylab("Predictor t-Statistic") +
       scale_y_continuous(breaks = -10:10) +
       ggtitle("Evidence of Ability to Predict Math/Science Occupations") +
       scale_fill_gradient2(low = muted("red"), mid = "white",
                            high = muted("blue"), midpoint = .5,
                            space = "rgb") + 
       theme(axis.text = element_text(size = 12))

predictors <- c("hs.prog.1997", "prog.course.since.dli.1998",
                "prog.course.since.dli.2001", "teachers.good",
                "enriching.environment", "organized", "conscientious",
                "dependable", "thorough", "cooperative", "flexible", "trustful")
     
formula.str <- paste("math.cs ~ poverty.ratio + gender",
                     paste(predictors, collapse = "+"), 
                     paste0("gender:", predictors, collapse = "+"), sep = "+")

mira.interactions <- with(mids, glm(formula(formula.str), family = binomial))
X.summary <- summary(pool(mira.interactions))
X.terms <- grep(":",rownames(X.summary))
X.plot.df <- as.data.frame(X.summary)[X.terms, ]

X.plot.df$predictor <- factor(rownames(X.plot.df),
                              levels = unique(rownames(X.plot.df)))

X.plot.df$p.value <- X.plot.df[, "Pr(>|t|)"]
X.plot.df$fdr.p.value <- p.adjust(X.plot.df[, "Pr(>|t|)"], method = "fdr")

ggplot(X.plot.df,
       aes(x = predictor, y = t, fill = p.value)) +
       geom_bar(stat = 'identity') + coord_flip() + 
       xlab("Interaction Term") +
       ylab("Interaction t-Statistic") +
       scale_y_continuous(limits = c(-5, 5)) +

       ggtitle("Evidence of Gender-based Interactions") +
       scale_fill_gradient2(low = muted("red"), mid = "white",
                            high = muted("blue"), midpoint = 0, space = "rgb") + 
       theme(axis.text = element_text(size = 12))
