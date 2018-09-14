library(rvest)
library(Rcrawler)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(httr)
library(rvest)
library(tidytext)
library(dplyr)
library(treemap)
library(ggplot2)

source("retrieve.data.R")

base.dir = "../data/imi/"
color.palette = c("darkred", "grey20") #"blue", "orange", "green", "red", "grey20", "yellow") 

main<-function() {
    #get.data()
    plot.data()
}

plot.data<-function() {

    #out.file = paste0(base.dir, "keywords.dat")
    #d = read.table(out.file)
    #keywords = d$x
    #out.file = paste0(base.dir, "keyword.svg")
    #get.treemap(keywords, out.file)

    d = read.table(paste0(base.dir, "projects.dat"))
    #d$bin = cut(d$budget, breaks=2000000*seq(1,11), labels=paste0(20*seq(1,10), "M")) #breaks=10)
    d = na.omit(d)
    d$bin = cut(d$budget, breaks=c(1E6, 5E6, 10E6, 20E6, 50E6, 100E6, 200E6, 500E6), labels=paste0(c(5, 10, 20, 50, 100, 200, 500), "M"))
    d$transqst = factor(ifelse(d$name == "TRANSQST", 1, 0))
    p = ggplot(d, aes(bin)) + geom_bar(stat="count") #, aes(fill=transqst))
    p = p + theme_minimal() + labs(x="Budget (€)", y="Number of projects")
    p = p + scale_fill_manual(values=color.palette, guide=guide_legend("Budget (€)"))
    out.file = paste0(base.dir, "budget.svg")
    svg(out.file)
    print(p)
    dev.off()

    d = read.table(paste0(base.dir, "institutions.dat"))

    a = table(d$country)
    a = a[order(-a)]
    d$country = factor(d$country, levels=names(a))
    p = ggplot(d, aes(country)) + geom_bar(stat="count") 
    p = p + theme_minimal() + labs(x="Country", y="Number of participants") # consider counting one country per project
    p = p + coord_flip()
    p = p + scale_fill_manual(values=color.palette)
    out.file = paste0(base.dir, "country_participant.svg")
    svg(out.file)
    print(p)
    dev.off()

    for(suffix in c("efpia", "research", "sme", "patient", "third")) {
	#suffix = "efpia"
	#suffix = "research"
	e = d[d$category == suffix,]
	a = table(e$name)
	a = a[order(-a)][1:20]
	e = as.data.frame(a)
	colnames(e) = c("name", "n")
	p = ggplot(e, aes(name, n)) + geom_col() 
	p = p + theme_minimal() + labs(x="Participant", y="Number of projects") 
	p = p + scale_fill_manual(values=color.palette)
	p = p + coord_flip()
	out.file = paste0(base.dir, "participant_", suffix, ".svg")
	svg(out.file)
	print(p)
	dev.off()
    }
}

get.data<-function() {
    #stores the webpage url in factsheets variable
    factsheets<-readLines("http://www.imi.europa.eu/projects-results/project-factsheets")
    pattern_line="\t\t\t<a href=\"/projects-results/project-factsheets/"

    #gets the line where is the project names
    projects = grep(pattern_line,factsheets,value=TRUE)

    #formats and builds the url for each project, storing it in the project_url variable 
    projects<-gsub("\"|\\s|class.*","",projects)
    pattern_sub="<ahref=/"
    project_urls<-gsub(pattern_sub,"http://www.imi.europa.eu/", projects)

    number_of_projects=length(project_urls)
    paste("The number of projects registered so far is:", number_of_projects)

    #get the project names
    pattern_sub='http://www.imi.europa.eu/projects-results/project-factsheets/'
    all_project_names<-gsub(pattern_sub,"",project_urls)

    #build a data.frame to be used for wordcloud, this case the value for all is 1.
    cloud_words<-data.frame(matrix(0, ncol = 2, nrow = number_of_projects))
    colnames(cloud_words)<-c("name","value")
    cloud_words[1]<-all_project_names
    cloud_words[2]<-c(1)
    cloud_words[cloud_words$name == "transqst","value"]<-3

    svg(paste0(base.dir, "name.svg"))
    wordcloud(words = cloud_words$name, freq = cloud_words$value, min.freq = 1, scale=c(1.5,.5), max.words=90, random.order=FALSE, rot.per=0.35)
    dev.off()

    #d = retrieve.data('http://www.imi.europa.eu/projects-results/project-factsheets/direct')
    #all_data<-list()
    sentences = c()
    keywords = c()
    projects = data.frame(matrix(NA, ncol = 5, nrow = number_of_projects))
    colnames(projects)<-c("name", "category", "budget", "start", "end") #, n.efpia, n.research, n.sme, n.patient, n.third)
    institutions = data.frame(matrix(NA, ncol = 4, nrow = 2000))
    colnames(institutions)<-c("name", "category", "country", "project")
    k = 1
    for(i in 1:number_of_projects) {
	url = as.character(project_urls[i])
	#all_data[i]<-list(retrieve.data(url))
	d = retrieve.data(url)
	a = unlist(strsplit(d$summary, "[.]")) 
	sentences = c(sentences, a)
	keywords = c(keywords, d$keyword)
	projects[i,"name"] = d$title
	print(c(i, length(d)))
	if(length(d) == 3) {
	    next() 
	}
	projects[i,"category"] = d$call
	projects[i,"budget"] = d$contribution
	projects[i,"start"] = d$start
	projects[i,"end"] = d$end
	for(type in c("efpia", "research", "sme", "patient", "third")) {
	    b = d[[type]]
	    if(length(b) == 1 && is.na(b)) {
		next()
	    }
	    for(j in 1:length(b)) {
		institutions[k, "name"] = b[j]
		institutions[k, "category"] = type
		institutions[k, "country"] = d[[paste0(type, ".country")]][j]
		institutions[k, "project"] = d$title
		k = k + 1
	    }
	}
    }

    print(k)

    out.file = paste0(base.dir, "keywords.dat")
    write.table(keywords, out.file)
    out.file = paste0(base.dir, "keyword.svg")
    #out.file = paste0(gsub("\\.dat$", "", out.file), ".svg") 
    get.treemap(keywords, out.file)

    out.file = paste0(base.dir, "sentences.dat")
    write.table(sentences, out.file)
    out.file = paste0(base.dir, "summary.svg")
    get.treemap(sentences, out.file)

    out.file = paste0(base.dir, "projects.dat")
    write.table(projects, out.file)

    out.file = paste0(base.dir, "institutions.dat")
    write.table(institutions, out.file)
}


get.treemap<-function(a, out.file) {
    d = data.frame(id=1:length(a), sentence=as.vector(a), stringsAsFactors=F)
    d$sentence = gsub("\n", " ", d$sentence)
    d.tidy = d %>% unnest_tokens(word, sentence)
    d.clean = d.tidy %>% anti_join(get_stopwords())
    d.words = d.clean %>% count(word, sort = TRUE) 
    d.words = d.words %>% join.plurals()
    #d.words %>% with(wordcloud(word, n, min.freq=quantile(d.words$n)[4], max.words = 100)) # , scale=c(2,3)

    group = d.words$word
    value = d.words$n
    data = data.frame(group, value)
    print(quantile(data$value))
    data.filtered = subset(data, data$value > quantile(data$value)[4])
    if(nrow(data.filtered) == 0) {
	data.filtered = subset(data, data$value > quantile(data$value)[3]) # median
	if(nrow(data.filtered) == 0) {
	    #data.filtered = NULL
	    data.filtered = data
	}
    }
    if(!is.null(data.filtered)) {
	svg(out.file)
	treemap(data.filtered, index="group", vSize="value", vColor="value", type="index", palette= brewer.pal(n=8, "RdYlGn"))
	dev.off()
    }
}


join.plurals<-function(b) {
    b = data.frame(word=b %>% select(word), n=b$n)
    rownames(b) = b$word
    #b = b[,-1]
    for(i in rownames(b)) {
	if(paste0(i, "s") %in% rownames(b)) {
	    b[i, "n"] = b[i, "n"] + b[paste0(i, "s"), "n"]
	    b = b[-which(rownames(b) == paste0(i, "s")),]
	}
	if(paste0(i, "es") %in% rownames(b)) {
	    b[i, "n"] = b[i, "n"] + b[paste0(i, "es"), "n"]
	    b = b[-which(rownames(b) == paste0(i, "es")),]
	}
    }
    return(b)
}

main()

