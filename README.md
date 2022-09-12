### Document specifications
Author: Dominik Wulf <br>
Matriculation Number: 364 100 <br>
Creation Date: 06. September 2022

# Intro

The data is structured as follows. The folders are separated into five different sections: Scraper, LDA, Bubble Tests, Latex, Screenshots Websites. In order to review the code and use the right dataset, this file explains the usage of the different scripts. The numbers before the datasets or the filenames describe the dates the scrape or the data was collected as YYYYMMDD. For readability reasons it is advised to use the jupyter notebooks to review the code.

## Scraper

The Scraper contains the sections:<br>
* Data
* Output
* Scraper Jupyter
* Scraper Python <br>

The Data contains the input data for the scraper. The file is named **'220704_project_website_links_nos'** and contains all individual links from [dappradar.com](https://dappradar.com/) from NFT projects. The list doesn't contain any links of the website [opensea.io](https://opensea.io/) which were also referenced for some projects on the website dappradar.com. The Scraper Jupyter, respectively Python contains two python scripts. 
<br> The first one is called **220705_scraper** which is the main script for the scraper. The links from the above named file are iteratively visited and the HTML content is collected as described in chapter 3.2.1. The output dataset from the script is called **'220705_html_content'**
<br> The second script is called **Scrape Postprocessing** and uses the output dataset from the first script. The script removes duplicates and NaN values from the data and removes sublinks from the link list as described in chapter 3.2.1. The output dataset is called **'220705_html_content_normalized'** and is the final dataset used in the LDA model.
<br> As a prerequesite to run the scrape, the chromedriver https://chromedriver.chromium.org/downloads has to be installed together with the chrome browser.
<br> In order for the scraper to work, the following files need to be downloaded and the respective links need to be inserted to import the data at the beginning of the script where indicated: <br>

* **'220704_project_website_links_nos.csv'**,
* **'220705_html_content.csv'**
* **'220705_html_content_normalized.csv'**

<br> In the displayed scriped the results represent a trial with 15 links from the link list. A reinitilization of the script will result in a full run of the script. This was done as the script takes a longer time to run and it was reviewed with a shorter link list before.

## LDA

The LDA folder contains four parts:<br>
* LDA Models Jupyter
* LDA Models Python
* Data
* HTML output <br>

The Data contains one file called **'220705_html_content_normalized'**. This file is the result of the scraper and has been prepared as described in chapter 3.2.1. Within the LDA files, only the websites with more than 10,000 words are removed. The ipynb files are the jupyter notebooks used to create the LDA models and produce the HTML output. Respectively the python files present a copy of the jupyter notebooks. The data The HTML output contains two files depicting the pyLDAvis inter topic distance map described in chapter 3 in the thesis. 

In order for the script to work please download the above named file and insert the filepath where indicated in the script.

## Bubble Tests

The Bubble Test folder contains four parts:<br>
* Data
* Scripts
* Figs
* Bubble Tests<br>

The Data folder contains all the time series for the NFT projects and their aggregated series retrieved from [nonfungible.com](https://nonfungible.com/market-tracker). **Bubble Tests** is the R project created for conducting the tests. The Scripts folder contains the aggregated tests, the test for the individual sections and the cryptocurrencies. The .md files can be viewed in github. The Figs folder contains the figures of the BSADF test on the time series.
<br> As a prerequesite to run the r script the package [exuber](https://github.com/kvasilopoulos/exuber) among other common packages needs to be installed and additionally [exuberdata](https://github.com/kvasilopoulos/exuberdata) dataset needs to be retrieved. This is due as some datasets contain more than 600 observations and the exuberdata dataset contains critical values and critical value sequences for the GSADF and BSADF test.

## Latex

The Latex folder contains the raw form of the Latex file, which also contains the bibliography in bibtex format.

## Screenshots Websites

The folder contains all accessed and cited websites. The websites were visited on the 07.09.2022.
