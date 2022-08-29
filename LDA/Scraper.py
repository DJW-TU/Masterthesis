from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import (TimeoutException,ElementNotSelectableException)
from tqdm import tqdm

import urllib.parse as parse
import pandas as pd
import time
import random

# import links 

link_list = pd.read_csv("C:/Users/dominik/Desktop/Uni/Masterthesis/Data/Outputs/Scrape/220704_project_website_links_nos.csv")
link_list['links_no_utm'] = 'https://' + link_list['links_no_utm'].astype(str)
link_list = link_list["links_no_utm"].to_list()


#define wait function
ignore_list = [ElementNotSelectableException,TimeoutException]
wait = WebDriverWait(driver, timeout=20, poll_frequency=1, ignored_exceptions=ignore_list)

# page_links = links of scraped page, link_list1 = link short list, that adds links of current page, link_list2 = link_long_list of all pages of current page, start_link = first link where scrape started
def get_links(page_links,link_list1,link_list2,start_link):
    # checks the number of links on the passed links of page
    if len(page_links) <= 50:
        # iterates over the passed page links
        for i in page_links:
            #checks if the link contains the initial starting page == equal to base and not linkedin/youtube or smth else
            if i.get_attribute("href")[0:13] in start_link:
                # adds link to list, if it is not present yet
                link_list1.append(i.get_attribute("href")) if i.get_attribute("href") not in link_list2 else None
            else:
                # does nothing
                pass
    else:
        print("More than 50 links. There are " + str(len(page_links)) + " links on the page")
    return link_list1

# get all links of initial page and save to a list - only append if not in list yet, start = initial link, from dappRadar
def get_link_tree(start):
    #creates empty list
    links = []
    base = None
    try:
        driver.get(start) # goes to start page
        base = driver.current_url # saves the current url as the start page, as the urls which are used as inputs could sometimes redirect the driver to another page
        time.sleep(1) # waits to load site content
        page_links = driver.find_elements(by=By.XPATH, value="//a[@href]") # extracts all links with href reference from website
        links = get_links(page_links,links,links,base) # iterates through links and saves them in a list if they are not already in the link list. Link list 1 and 2 of the get_links function are the same as no links are recorded in the first iteration 
        links = list(filter(None, links)) # elimate empty entries from list
    except:
        pass
    return links, base if base is not None else links # hands over link list 

#iterates through link tree of first page and appends page if not already in list - base is the start url of initial page
def link_tree_iterate(link_tree,start_link):
    for x in link_tree:
        links2 = [] 
        try:
            driver.get(x)
            time.sleep(1)
            page_links = driver.find_elements(by=By.XPATH, value="//a[@href]")
            links2 = get_links(page_links,links2,link_tree,start_link)
            #after 1st iteratiom, link_tree needs to change to updated list of links
            links2 = list(filter(None, links2))
            link_tree = links2 + link_tree
        except:
            pass
    return link_tree

# main function which calls all other functions
def get_page_links(start_link):
    link_tree,base = get_link_tree(start_link)  
    if len(link_tree)>0:
        links = link_tree_iterate(link_tree,base)
    else:
        links = [None]
    return links

#initilize driver
driver = webdriver.Chrome('C:/Users/dominik/Desktop/Python Projects/chromedriver')

# get sublinks of websites
link_tree_list = []
for x in tqdm(link_list):
    links = get_page_links(x)
    links = list(set(links))
    link_tree_list += links

# import extracted links which have been imported before
extracted_links = pd.read_csv("C:/Users/dominik/Desktop/Uni/Masterthesis/Data/Outputs/Scrape/220704_sublinks_NFT_projects_raw.csv")

# aggregate link list and initial links if not already imported above
extracted_links = pd.Series(link_tree_list + link_list)
extracted_links = extracted_links.to_frame()
extracted_links.to_csv("C:/Users/dominik/Desktop/Uni/Masterthesis/Data/Outputs/Scrape/220704_sublinks_NFT_projects_raw.csv")
extracted_links.columns = ["index","websites"]

# postprocess generated links
#extracted_links["linkedin2"] = extracted_links[extracted_links['websites'].str.contains('linkedin')==False]
extracted_links = extracted_links[~extracted_links["websites"].str.contains("linkedin", na = False)]
extracted_links = extracted_links[~extracted_links["websites"].str.contains("cloudflare", na = False)]
extracted_links = extracted_links.dropna()
extracted_links = extracted_links["websites"].drop_duplicates()

# create list of links and sublinks
el_list = extracted_links.to_list()

# save links to csv
el_list.to_csv("C:/Users/dominik/Desktop/Uni/Masterthesis/Data/Outputs/Scrape/220704_sublinks_NFT_projects.csv")

# extract html conent of links and sublinks
html_content = []
links = []

for x in tqdm(el_list):
    try:
        driver.get(x)
        time.sleep(random.uniform(1.5,2.5))
        page_text = driver.find_element(by=By.XPATH, value="/html/body").text
        html_content.append(page_text)
        links.append(x)
    except:
        pass

# create dictionary of links and html content
d = {'links':links,'html_content':html_content}

# creates dataframe out of scraped html content
html_df = pd.DataFrame(d)
html_df.to_csv("C:/Users/dominik/Desktop/Uni/Masterthesis/Data/Outputs/Scrape/220705_html_content.csv",sep ='\t')

#html_data = pd.read_csv('C:/Users/dominik/Desktop/Uni/Masterthesis/Data/Outputs/Scrape/220705_html_content.csv', sep='\t')



