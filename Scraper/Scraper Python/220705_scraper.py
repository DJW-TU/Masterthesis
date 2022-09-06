#!/usr/bin/env python
# coding: utf-8

# ### Document specifications
# Author: Dominik Wulf <br>
# Matriculation Number: 364 100 <br>
# Creation Date: 05. July 2022

# In[1]:


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


# ### ! Insert File Path to Chromedriver here !

# In[5]:


# Insert file path of chromedriver here
driver = webdriver.Chrome('Path to chromedriver')


# ### ! BEFORE START SET FILEPATH AND IMPORT DATA HERE ! 

# In[46]:


# import data, please insert file path for '220704_project_website_links_nos.csv' here 
link_list = pd.read_csv("C:/Users/dominik/Documents/GitHub/Masterthesis/Scraper/Data/220704_project_website_links_nos.csv")


# In[47]:


link_list = link_list["links_no_utm"].to_list()
link_list[:5]


# In[8]:


#define wait function
ignore_list = [ElementNotSelectableException,TimeoutException]
wait = WebDriverWait(driver, timeout=20, poll_frequency=1, ignored_exceptions=ignore_list)


# In[9]:


# page_links = links of scraped page, link_list1 = link short list, that adds links of current page, link_list2 = link_long_list of all pages of current page, start_link = first link where scrape started
def get_links(page_links,link_list1,link_list2,start_link):
    # checks the number of links on the passed links of page
    if len(page_links) <= 50:
        # iterates over the passed page links
        for i in page_links:
            #checks if the link contains the initial starting page == equal to bage and not linkedin/youtube or smth else
            if i.get_attribute("href")[0:13] in start_link:
                # adds link to list, if it is not present yet
                link_list1.append(i.get_attribute("href")) if i.get_attribute("href") not in link_list2 else None
            else:
                # does nothing
                pass
    else:
        print("More than 50 links. There are " + str(len(page_links)) + " links on the page")
    return link_list1


# In[10]:


def get_page_links(start_link):
    link_tree,base = get_link_tree(start_link)  
    if len(link_tree)>0:
        links = link_tree_iterate(link_tree,base)
    else:
        links = [None]
    return links


# In[11]:


# get all links and save to a list - only append if not in list yet, start = initial link, from dappRadar
def get_link_tree(start):
    #creates empty list
    links = []
    base = None
    try:
        driver.get(start)
        base = driver.current_url
        time.sleep(1)
        page_links = driver.find_elements(by=By.XPATH, value="//a[@href]")
        links = get_links(page_links,links,links,base)
        links = list(filter(None, links))
    except:
        pass
    return links, base if base is not None else links 


# In[12]:


def link_tree_iterate(link_tree,start_link):
    links2= []
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


# In[13]:


# get sublinks of NFT projects through iteration over link list
link_tree_list = []
for x in tqdm(link_list):
    links = get_page_links(x)
    links = list(set(links))
    link_tree_list += links


# In[25]:


# combine starting links and merge starting and sublinks list together
extracted_links = pd.Series(link_tree_list + link_list)
extracted_links = extracted_links.to_frame()
extracted_links.to_csv("C:/Users/dominik/Desktop/Uni/Masterthesis/Data/Outputs/Scrape/220906_sublinks_NFT_projects_raw.csv")


# In[26]:


# rename columns
extracted_links.columns = ["websites"]


# In[27]:


# remove websites that are no part of NFT projects
extracted_links = extracted_links[~extracted_links["websites"].str.contains("linkedin", na = False)]
extracted_links = extracted_links[~extracted_links["websites"].str.contains("cloudflare", na = False)]


# In[34]:


# drop all duplicates and NaN values from link list
extracted_links = extracted_links.dropna()
extracted_links = extracted_links.drop_duplicates(subset=["websites"],keep = "first")


# In[35]:


# get number of websites in link list
len(extracted_links)


# In[36]:


extracted_links


# In[37]:


# convert dataframe to list
el_list = extracted_links["websites"].to_list()


# In[45]:


el_list[:5]


# ### Retrieve HTML content

# In[39]:


# insert location of chromedriver
driver = webdriver.Chrome('C:/Users/dominik/Desktop/Python Projects/chromedriver')


# In[40]:


# visit every website in link list and get the HTML content
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


# In[41]:


# save html content as dictionary to convert to dataframe
d = {'links':links,'html_content':html_content}


# In[42]:


# save html content as dataframe
html_df = pd.DataFrame(d)


# In[43]:


# save html content to csv
html_df.to_csv("C:/Users/dominik/Desktop/Uni/Masterthesis/Data/Outputs/Scrape/220906_html_content.csv",sep ='\t')

