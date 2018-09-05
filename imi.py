
import urllib2, json, urllib
from bs4 import BeautifulSoup
import ssl

FIELD_PROJECT = "http://www.imi.europa.eu/projects-results/project-factsheets"

def main():
    project = "abirisk" 
    print get_project_info(project)
    return


def get_data(command, parameter):
    if command == "project": 
        txt = parameter.rstrip("+")
    else:
        raise ValueError("Unknown command: " + command)
    url = FIELD_PROJECT + '/%s' % (txt)
    #print url
    req = urllib2.Request(url)
    try:
	response = urllib2.urlopen(req) 
    except urllib2.HTTPError:
	print "Problem with response (probably no info):", url 
        return None
    return response


def get_project_info(project):
    try:
        response = get_data("project", project)
    except urllib2.HTTPError:
        print "No info for", project 
        return []
    values = []
    print response
    soup = BeautifulSoup(html_doc, "lxml")
    valid_tags = set(["div", "li", "dd"]) # li class="active" div class="field field--name-field-project-title" "Contributions"
    #!
    for tag in soup.find_all(['h1']):
        #print tag, tag.get("class")
        cls = tag.get("class")
        tag = tag.find_next()
        cls_inner = tag.get("class")  
        #print tag, cls
        if cls is not None and cls[0] == "Highlights":
            if tag.string is None:
                continue
            header = tag.string.encode().strip().lower()
            #print header
            flag = None
            if header.find("contraindication") != -1:
                flag = "contraindication"
    return values


if __name__ == "__main__":
    main()

