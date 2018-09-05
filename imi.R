text = "We describe how the genome-wide transcriptional profiling can be used in network-based 
systems toxicology, an approach leveraging biological networks for assessing
the health risks of exposure to chemical compounds. Driven by the technological
advances changing the ways in which data are generated, systems toxicology has
allowed traditional toxicity endpoints to be enhanced with far deeper levels of analy sis. 
In combination, new experimental and computational methods have offered the
potential for more effective, efficient, and reliable toxicological testing strategies. We
illustrate these advances by the 'network perturbation amplitude' methodology that
quantifies the effects of exposure treatments on biological mechanisms represented
by causal networks. We also describe recent developments in the assembly of high-quality 
causal biological networks using crowdsourcing and text-mining approaches.
We further show how network-based approaches can be integrated into the multi-scale 
modeling framework of response to toxicological exposure. Finally, we combine
biological knowledge assembly and multiscale modeling to report on the promising
developments of the 'quantitative adverse outcome pathway' concept, which spans
multiple levels of biological organization, from molecules to population, and has direct
relevance in the context of the 'Toxicity Testing in the 21st century' vision of the US
National Research Council."
#library(tokenizers)
#words = tokenize_words(sentence)
#library(tm)
#dtm = VectorSource(text) %>% VCorpus() %>% DocumentTermMatrix(control = list(removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE))
#inspect(dtm)
#plot(dtm, terms=findFreqTerms(dtm, lowfreq=15)[1:10], corThreshold=0.1)
library(textrank)
library(tidytext)
library(dplyr)
library(wordcloud)
library(tidyr)
library(reshape2)
a = unlist(strsplit(text, "[.]")) #"\n"))
d = data.frame(id=1:length(a), sentence=as.vector(a), stringsAsFactors=F)
d$sentence = gsub("\n", " ", d$sentence)
d.tidy = d %>% unnest_tokens(word, sentence)
d.clean = d.tidy %>% anti_join(get_stopwords())
d.mod = d.clean
tr.keywords = textrank_keywords(d.mod$word)
tr = textrank_sentences(d, d.mod)
summary(tr, n = 2) # keep.sentence.order = T)
d.words = d.clean %>% count(word, sort = TRUE) 
d.words %>% with(wordcloud(word, n, min.freq=2, max.words = 100)) # , scale=c(2,3)
#nrcjoy = get_sentiments("nrc") %>% filter(sentiment == "joy")
#d.tidy %>% semi_join(nrcjoy) %>% count(word, sort = TRUE)
bing = get_sentiments("bing")
d.sentiment = d.tidy %>% inner_join(bing) %>% count(word, sentiment, sort = TRUE) 
d.sentiment %>% filter(n >= 1) %>% mutate(n = ifelse(sentiment == "negative", -n, n)) %>% 
    mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) +
    geom_col() + coord_flip() + labs(y = "Contribution to sentiment")
d.sentiment$sentiment = factor(d.sentiment$sentiment, levels=c("positive", "negative"))
d.sentiment %>% acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#00BFC4", "#F8766D"), max.words = 100, scale=c(3,3), title.size=1) # scale=c(4,5)
a = d.sentiment %>% acast(word ~ sentiment, value.var = "n", fill = 0)
a = as.data.frame(a)
a$na = 0
b = d.words %>% filter(n > 1)
b = data.frame(word=b %>% select(word), positive=0, negative=0, na=b$n)
rownames(b) = b$word
b = b[,-1]
for(i in rownames(b)) {
    if(paste0(i, "s") %in% rownames(b)) {
	b[i, "na"] = b[i, "na"] + b[paste0(i, "s"), "na"]
	b = b[-which(rownames(b) == paste0(i, "s")),]
    }
    if(paste0(i, "es") %in% rownames(b)) {
	b[i, "na"] = b[i, "na"] + b[paste0(i, "es"), "na"]
	b = b[-which(rownames(b) == paste0(i, "es")),]
    }
}
a = rbind(a, b)
a %>% comparison.cloud(colors = c("#00BFC4", "#F8766D", "black"), max.words = 100, scale=c(1,2), title.size=2) # scale=c(4,5)

