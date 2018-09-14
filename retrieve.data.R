retrieve.data <- function(url)
{
    #webpage = readLines('http://www.imi.europa.eu/projects-results/project-factsheets/abirisk')
    webpage<-readLines(url) #read the webpage html
    #html <- httr::POST(url = url) %>% httr::content() 
    #webpage <- rvest::html_text(html)
    
    mypattern_title = '<meta name=\"title\" content' 
    mypattern_summary = '<meta name=\"description\" content' 
    mypattern_abstract = '<meta name=\"abstract\" content'
    #--------------------Title summary two word abstract-------------------------------------------------
    #Title
    title = grep(mypattern_title, webpage, value=TRUE)
    mypattern_remove_title = '<meta name="title" content="'
    title1<-gsub(pattern = mypattern_remove_title, replacement = "", title)
    #title<-gsub('IMI - Innovative Medicines Initiative" />| \\|| " "', replacement = "", title1)
    title<-gsub(' IMI Innovative Medicines Initiative" />| \\|| " "', replacement = "", title1)

    #Summary
    summary = grep(mypattern_summary,webpage,value=TRUE)
    summary<-gsub('<meta name="description" content="|" />', replacement="", summary)

    #Two word abstract
    abstract = grep(mypattern_abstract,webpage,value=TRUE)
    abstract<-gsub('<meta name="abstract" content="|" />', replacement="", abstract)
    abstract<-gsub("\t|\n|:|\\bfrom\\b |\\b-\\b |\\bfor\\b |\\ba\\b |\\band\\b | \\bto\\b |\\bor\\b| \\bthe\\b | \\bbut\\b |\\bAnd\\b| \\bThe\\b| \\bOr\\b| \\bBut\\b | \\bin\\b | \\bIn\\b | \\bon\\b| \\bOn\\b | \\bof\\b |\\bOf\\b"," ",abstract)
    abstract<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", abstract, perl=TRUE)
    abstract<-list(unlist(strsplit(abstract, " ", fixed=TRUE)))

    #----------------------start date end date class--------------------------------------------------
    mypattern_start_date = '<div class=\"field field--name-field-start-date field--type-datetime field--label-hidden field--item\">'
    mypattern_end_date = '<div class=\"field field--name-field-end-date field--type-datetime field--label-hidden field--item\">' 
    mypattern_class = '<div class=\"field field--name-field-project-call field--type-entity-reference field--label-hidden field--item\">'

    #Start_date
    start_date = grep(mypattern_start_date, webpage, value=TRUE)
    start_date<-gsub('<div class=\"field field--name-field-start-date field--type-datetime field--label-hidden field--item\"><time datetime=.*\"',"",start_date)
    start_date<-gsub("</time>","",start_date, fixed=TRUE)
    start_date<-gsub(">","",start_date, fixed=TRUE)
    start_date<-gsub(" ","",start_date, fixed=TRUE)

    #End_date
    end_date = grep(mypattern_end_date,webpage, value=TRUE)
    end_date<-gsub('<div class=\"field field--name-field-end-date field--type-datetime field--label-hidden field--item\"><time datetime=.*\"',"",end_date)
    end_date<-gsub("</time>","",end_date, fixed=TRUE)
    end_date<-gsub(">","",end_date, fixed=TRUE)
    end_date<-gsub(" ","",end_date, fixed=TRUE)

    #Class
    class= grep(mypattern_class,webpage, value=TRUE)
    class<-gsub('<div class=\"field field--name-field-project-call field--type-entity-reference field--label-hidden field--item\">',"",class)
    class<-gsub(pattern="Call","Call.",class)
    class<-gsub(pattern="</div|\\s|>","",class)
    class<-gsub(pattern="Call.","Call ",class)

    #----------------------participants, city, country-----------------------------------------------
    mypattern_project_participants='<div class="project-participants info-box light-grey-bg"><div class="project-participants-category"><h5><strong>'
    project_participants = grep(mypattern_project_participants, webpage, value=TRUE)

    empty_m<-matrix(NA, ncol = 3)
    efpia_df<-empty_m
    universities_df<-empty_m
    smes_df<-empty_m
    patient_df<-empty_m
    third_parties_df<-empty_m
	
    separate_participants<-function(x, b, i) {
	c=x
	c<-unlist(b[[i]][-1])
	for(j in 1:length(c)) {
	     all<-(strsplit(c[j], ","))
	     all<-unlist(all)
	     company_name[[j]]<-all[1]
	     city[[j]]<-trimws(all[(length(all)-1)])
	     country[[j]]<-trimws(all[length(all)])
	}
	z<-cbind(company_name, city, country)
	return(z)
    }

    if(length(project_participants) != 0) {
	projects_participants_classification<-strsplit(project_participants, '<h5><strong>')
	mypattern_separator = '<li class=\"text-capitalize\">'
	a<-strsplit(projects_participants_classification[[1]][], mypattern_separator)
	a<-a[-1]
	b<-lapply(a, gsub, pattern = "</li>|\"|>|</strong></h5><ul>|</ul>.*", replacement = "")

	company_name<-list()
	city<-list()
	country<-list()
	
	for(i in 1:length(b)) {
	       if(b[[i]][1]=="EFPIA companies")
	       {
		   x=b[[i]][-1]
		 #  print("pase")
		   z<-separate_participants(x, b, i)
		   efpia_df<-as.data.frame(z)
		   colnames(efpia_df)<-c("NAME_efpia","CITY_efpia","COUNTRY_efpia")
		#   print(efpia_df)
	       }
	    
	       if(b[[i]][1]=="Universities, research organisations, public bodies, non-profit groups")
	       {
		   x=b[[i]][-1]
		 #  print("pase2")
		   z<-separate_participants(x, b, i)   
		   universities_df<-as.data.frame(z)
		   colnames(universities_df)<-c("NAME_university","CITY_university","COUNTRY_university")
		 #  print(universities_df)
	       }
	    
	      if(b[[i]][1] == 'Small and medium-sized enterprises (SMEs)')
		{  
		   x=b[[i]][-1]
		  # print("pase3")
		   z<-separate_participants(x, b, i) 
		   smes_df<-as.data.frame(z)
		   colnames(smes_df)<-c("NAME_sme","CITY_sme","COUNTRY_sme")
		   #print(smes_df)
	       }
	    
	      if(b[[i]][1] == 'Patient organisations')
		{  
		   x=b[[i]][-1]
		 #  print("pase4")
		   z<-separate_participants(x, b, i)   
		   patient_df<-as.data.frame(z)
		   colnames(patient_df)<-c("NAME_patient","CITY_patient","COUNTRY_patient")
		 #  print(patient_df)
	       }
	    
	       if(b[[i]][1]=="Third parties")
	       {
		   x=b[[i]][-1]
		#   print("pase5")
		   z<-separate_participants(x, b, i)  
		   third_parties_df<-as.data.frame(z)
		   colnames(third_parties_df)<-c("NAME_third","CITY_third","COUNTRY_third")
		#  print(third_parties_df)
	       }
	}
	
    }

    i = 1
    if(length(project_participants) == 0) {
	full_data<-setNames(c(title, summary, abstract), c("title", "summary", "keyword"))
	return(full_data)
	company_name<-list()
	city<-list()
	country<-list()
    
	company_name[[i]]<-c(0)
	city[[i]]<-c(0)
	country[[i]]<-c(0)
    }
    #---------------------------------------------------------------------------------------------
    #Contribution
    html <- httr::POST(url = url) %>% httr::content() 
    contribution <- html %>% rvest::html_nodes(xpath="//table") %>% rvest::html_text() %>% .[2] %>% trimws()
    contribution <- gsub(" ", "", gsub("Total Cost", "", rev(strsplit(contribution, "\n")[[1]])[1])) #%>% trimws()

    #full_data<-c(title, summary, abstract, start_date, end_date, class, efpia_df, universities_df, smes_df, patient_df, third_parties_df)
    #print(c(title, summary, abstract, start_date, end_date, class, contribution, efpia_df, universities_df, smes_df, patient_df, third_parties_df))
    full_data<-setNames(c(title, summary, abstract, start_date, end_date, class, contribution, efpia_df, universities_df, smes_df, patient_df, third_parties_df), c("title", "summary", "keyword", "start", "end", "call", "contribution", "efpia", "efpia.city", "efpia.country", "research", "research.city", "research.country", "sme", "sme.city", "sme.country", "patient", "patient.city", "patient.country", "third", "third.city", "third.country"))
    return(full_data)
} 

