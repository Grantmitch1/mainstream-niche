## Read in data ----
d <- read.csv("data/Reduced_CMP_Mainstream_Niche.csv")

# clean up some column names to make them easier to type and understand
colnames(d)[6] <- "Vote.Pct"
colnames(d)[8] <- "Dimension.Emphasis"
head(d)


## Brief EDA and cleaning ----
# check missings
sapply(d, function(col) sum(is.na(col))) # Total.Per.Codes has 6 missing data points
d[is.na(d$Total.Per.Codes), ] # this is fine, not used in main calcs

# check types
sapply(d, class)

# could change date to be actual date type, but won't:
# d$Date <- lubridate::dmy(d$Date)
# just check range
range(lubridate::dmy(d$Date)) # 1991-03-17 to 2017-10-15

# check levels
length(levels(d$Country.Name)) # 13 different countries
levels(d$Country.Name)
length(levels(d$Date)) # 89 different dates
length(unique(d$Party.Name)) # 204 different parties


## Nicheness Scores ----
# function just needs a vector of vote percents and dimension emphasis and it does the leave one out calculations
# it is up to the user to apply it within only 1 election
nicheness_scores <- function(vote_pct, dim_emph) {
  stopifnot(length(vote_pct) == length(dim_emph))
  is <- seq_along(vote_pct) # i's to iterate along and leave one out in each iteration
  
  raw <- sapply(is, function(i) { # calculate raw nicheness score
    dim_by_vote <- (dim_emph * vote_pct)[-i]
    total_vote <- sum(vote_pct[-i])
    sqrt((dim_emph[i] - sum(dim_by_vote) / total_vote) ^ 2)
  })
  
  mean <- sapply(is, function(i) { # calculate mean nicheness score
    raw_by_vote <- (raw * vote_pct)[-i]
    total_vote <- sum(vote_pct[-i])
    sum(raw_by_vote) / total_vote
  })
  
  standardised <- raw - mean # calculate standardised nicheness score
  
  # return list
  list(
    Raw.Nicheness.Score = raw,
    Mean.Nicheness.Score = mean,
    Standardised.Nicheness.Score = standardised
  )
}

# test with data from examples in "data/Party Nicheness Score Calculation.docx"
doc_ex <- data.frame(
  party = c("Lab", "LD", "Con", "UKIP"),
  vote_pct = c(39.99, 7.37, 42.35, 1.85),
  dim_emph = c(0.527, 0.354, 0.601, 1.667) 
)

# everything seems to match (or close enough, the document might have errors?)
cbind(doc_ex, data.frame(nicheness_scores(doc_ex$vote_pct, doc_ex$dim_emph)))

# test with 1 election period
d$Date[1]
d1994 <- d[d$Date == "18/09/1994", ]
cbind(d1994, data.frame(nicheness_scores(d1994$Vote.Pct, d1994$Dimension.Emphasis)))

# apply across all election periods
# this step assumes "CMP.Row", "Date", "Vote.Pct", and "Dimension.Emphasis" are on the data.frame, d
ns <- do.call("rbind", c(by(d, d$Date, function(d_date) {
  cbind(
    CMP.Row = d_date$CMP.Row,
    data.frame(nicheness_scores(d_date$Vote.Pct, d_date$Dimension.Emphasis))
  )
}), make.row.names = FALSE)) # don't have rbind add row names

# by sorts the data by the levels of the factor column Date, resort using unique row id
res <- merge(d, ns, by = "CMP.Row")

# note: CMP.Row is unique so this is fine
length(unique(d$CMP.Row)) == nrow(d)

head(res)
View(res)


## Output results ----
# write to a csv
write.csv(res, "data/Reduced_CMP_Mainstream_Niche_Result.csv", row.names = FALSE)
