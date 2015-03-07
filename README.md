# Regression Project
## Summary
This project attempts to uncover several predictors of sales for several companies.  Our goal is to determine the "important" predictors of sales modifications for companies selling different products to different customer segments, not necessarily to determine any causal relationships or specificly accurate coefficients.  More concretely, we do not seek to answer the question "If it rains 3 inches in zipcode 23042 in the 22nd week of the year, how much will sales increase or decrease?"  Rather, we are interested in the relative impact of more rain versus hotter weather for gas sales compared to hotel room sales.  Or further, how much does rain in the northeast impact sales versus rain in the west?  And why should some companies care?

## Data
Originally, the data reflected individual transactions.  We have collapsed this into weekly totals to "smooth" errors in the post date versus transaction data, since we are only provided with post dates.  One pitfall of this method is that we probably have bled sales for week *t* into week *t-1*, since we used post dates that trail transaction dates by some matter of days.  We don't expect this will severly mutate our findings since most of our explanitory variables, such as temperature or precipitation, are also likely to slowly transition over time.  Though imperfect, we will at least get a sense of trends, if not a specific causal coefficient.

## What's in the error term?
Some thoughts on what unobservables are in the epsilon.  A Porter's Five Forces analysis seems 
 - personal preferences
   - how many folks happened to need some gas this week?
 - product quality
   - How attractive is the product itself?  Did a hotel remodel?
 - entrance/exit of compliments or substitutes
   - Did a Tesla store open up in town? How did that affect demand for gas?
 
