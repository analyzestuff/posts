# Load raw data, stored at textuploader.com
speech.raw <- paste(scan(url("http://textuploader.com/1k0g/raw"), 
                         what="character"), collapse=" ")

library(wordcloud)
wordcloud(speech.raw) # Also takes other arguments like color

library(qdap)
library(data.table)

# Split into sentences
# qdap's sentSplit is modeled after dialogue data, so person field is needed
speech.df <- data.table(speech=speech.raw, person="MLK")
sentences <- data.table(sentSplit(speech.df, "speech"))
# Add a sentence counter and remove unnecessary variables
sentences[, sentence.num := seq(nrow(sentences))]
sentences[, person := NULL]
sentences[, tot := NULL]
setcolorder(sentences, c("sentence.num", "speech"))

# Syllables per sentence
sentences[, syllables := syllable.sum(speech)]
# Add cumulative syllable count and percent complete as proxy for progression
sentences[, syllables.cumsum := cumsum(syllables)]
sentences[, pct.complete := syllables.cumsum / sum(sentences$syllables)]
sentences[, pct.complete.100 := pct.complete * 100]

# Sentiment and word count
pol.df <- polarity(sentences$speech)$all
sentences[, words := pol.df$wc]
sentences[, pol := pol.df$polarity]

# Basic plot of sentiment over time
with(sentences, plot(pct.complete, pol))

# Better plots with ggplot2
library(ggplot2)
library(scales)

my.theme <- 
  theme(plot.background = element_blank(), # Remove background
        panel.grid.major = element_blank(), # Remove gridlines
        panel.grid.minor = element_blank(), # Remove more gridlines
        panel.border = element_blank(), # Remove border
        panel.background = element_blank(), # Remove more background
        axis.ticks = element_blank(), # Remove axis ticks
        axis.text=element_text(size=14), # Enlarge axis text font
        axis.title=element_text(size=16), # Enlarge axis title font
        plot.title=element_text(size=24, hjust=0)) # Enlarge, left-align title

CustomScatterPlot <- function(gg)
  return(gg + geom_point(color="grey60") + # Lighten dots
           stat_smooth(color="royalblue", fill="lightgray", size=1.4) + 
           xlab("Percent complete (by syllable count)") + 
           scale_x_continuous(labels = percent) + my.theme)

CustomScatterPlot(ggplot(sentences, aes(pct.complete, pol)) +
                    ylab("Sentiment (sentence-level polarity)") + 
                    ggtitle("Sentiment of I Have a Dream speech"))

# Readability
sentences[, readability := automated_readability_index(speech, sentence.num)
          $Automated_Readability_Index]

CustomScatterPlot(ggplot(sentences, aes(pct.complete, readability)) +
                    ylab("Automated Readability Index") +
                    ggtitle("Readability of I Have a Dream speech"))

# Adapted from theBioBucket at http://goo.gl/TXvTxP
GoogleHits <- function(query){
  require(XML)
  require(RCurl)
  
  url <- paste0("https://www.google.com/search?q=", gsub(" ", "+", query))
  
  CAINFO = paste0(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt")
  script <- getURL(url, followlocation=T, cainfo=CAINFO)
  doc <- htmlParse(script)
  # Results look like this:
  # <div class="sd" id="resultStats">About 10,300,000 results</div>
  res <- xpathSApply(doc, '//*/div[@id="resultStats"]', xmlValue)
  return(as.numeric(gsub("[^0-9]", "", res)))
}

# Calculate Google search results for each sentence
# Group full sentence using brackets and then add "mlk" 
sentences[, google.hits := GoogleHits(paste0("[", gsub("[,;!.]", "", speech), 
                                             "] mlk"))]

# Graph Google hits throughout speech
ggplot(sentences, aes(pct.complete, google.hits / 1e6)) +
  geom_line(color="grey40") + # Lighten dots
  xlab("Percent complete (by syllable count)") + 
  scale_x_continuous(labels = percent) + my.theme +
  ylim(0, max(sentences$google.hits) / 1e6) +
  ylab("Sentence memorability (millions of Google hits)") +
  ggtitle("Memorability of I Have a Dream speech")

# Examine top 7 sentences
head(sentences[order(-google.hits)]$speech, 7)

# Plot log of hits to reduce outliers
sentences[, log.google.hits := log(google.hits)]

CustomScatterPlot(ggplot(sentences, aes(pct.complete, log.google.hits)) +
                    ylab("Memorability (log of sentence's Google hits)") +
                    ggtitle("Memorability of I Have a Dream speech"))

# Linear model
library(MASS)
google.lm <- stepAIC(lm(log(google.hits) ~ poly(readability, 3) + pol +
                          pct.complete.100, data=sentences))

# Check out the coefficients and R-sq
summary(google.lm)

# Exponentiate coefficients and print as percentage since outcome was logged
exp(google.lm$coefficients["pct.complete.100"])

new.data <- data.frame(readability=seq(min(sentences$readability), 
                                       max(sentences$readability), by=0.1),
                       pct.complete.100=mean(sentences$pct.complete.100))

new.data$pred.hits <- predict(google.lm, newdata=new.data)

ggplot(new.data, aes(readability, pred.hits)) + 
  geom_line(color="royalblue", size=1.4) + 
  xlab("Automated Readability Index") +
  ylab("Predicted memorability (log Google hits)") +
  ggtitle("Predicted memorability ~ readability") +
  my.theme


exp.coef <- sprintf("%1.1f%%", (exp(google.lm$coefficients) - 1) * 100)
names(exp.coef) <- names(google.lm$coefficients)
exp.coef







# APPENDIX

# Formality scores are a bit unreliable, e.g. a couple sentences randomly got 0
formality <- 
  data.table(formality(sentences$speech, sentences$sentence.num)$formality)
sentences[, formality := formality[order(sentence.num)]$formality]
sentences[, formality.smooth := loess(formality ~ syllables.cumsum)$fitted]

# Smooth the trends via loess
sentences[, syllables.smooth := loess(syllables ~ syllables.cumsum)$fitted]
sentences[, pol.smooth := loess(pol ~ syllables.cumsum)$fitted]
sentences[, words.smooth := loess(words ~ syllables.cumsum)$fitted]
sentences[, readability.smooth := loess(readability ~ syllables.cumsum)$fitted]
