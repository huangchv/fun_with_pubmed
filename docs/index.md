---
layout: default
---

# Introduction

If you are a scientist or a researcher, you may have published your research in a peer-reviewed journal. Publishing papers can serve many purposes, one of which may include disseminating your findings to the community so others can build upon it. However, we are only human, and we take pride in our work. We want to be successful.

So how does one go about evaluating whether their paper is successful? Easy, citations. 
The more times your paper is cited, the more _successful_. But is it possible to predict the number of citations you can get based on your abstract? 

Are there certain keywords you can use in your abstract that will immediately:
- Get you tons of citations? 
- The respect of your colleagues?
- The attention of the cute post doc in the neighbouring lab
- The Nobel prize? 

Let's find out if we can answer that first question. I don't think I can help you with the others.

Here, I will present a tongue-in-cheek analysis of citations based on text mining data from PubMed. 

# Methods
## Dataset 
Data was pulled from from the PubMed API using the RISmed R package. Query to API was '"journal article"[Publication Type] AND "english"[Language] AND hasabstract[text]' as it will ideally yield English research papers with an abstract. We extracted 5,000 abstracts for each year from 2010 to 2015. To avoid pulling papers in order, which will produce swathes of papers from the same journal, the function pulls queries for 10x the number of requested abstracts, and then randomly selects 'n' from the returned PMIDs. From each abstract object, we extracted title, abstract, author's list, number of citations, month submitted, and month published. Abstracts without text in the actual abstract were dropped.

## Parsing 
The syuzhet R package was used to perform NRC sentiment analysis on the text of each abstract. Sentiment values were retained as features for downstream analyses.

Journal impact factors and Eigenfactors from 2017 were obtained from Thomas Reuters and placed into a lookup table. Journal names were cleaned up and parsed to match the names in the impact factor lookup data as well as possible before being matched with an impact factor and Eigenfactor. Remaining journal names that defied parsing and matching to lookup table were assigned an impact factor value from a Gaussian distribution centered around 2.5 and an SD of 0.83, while the an Eigenfactor was assigned a value from a distribution centered around 0.002120 and an SD of 0.0007066667.

The tm R package was used to clean up the title and abstract text, remove stopwords, extract stem words, before converting word frequencies to 'term frequency-inverse document frequency' (TF-IDF). Sparse terms that occured in less than 5% of abstracts or less than 1% of titles were dropped. Title and abstract TF-IDF were extracted separately, and are used separately as features. 

## Model fitting


# Results
## Summary stats

