#!/usr/bin/env python
# coding: utf-8

# ### Document specifications
# Author: Dominik Wulf <br>
# Matriculation Number: 364 100 <br>
# Creation Date: 05. July 2022

# ### Intro
# 
# For the code various websites were used as references and get information about the implementation of the LDA models. The references used in the code below are the following: <br>
# 
# * https://radimrehurek.com/gensim/auto_examples/tutorials/run_lda.html
# * https://towardsdatascience.com/evaluate-topic-model-in-python-latent-dirichlet-allocation-lda-7d57484bb5d0
# * https://towardsdatascience.com/topic-modeling-and-latent-dirichlet-allocation-in-python-9bf156893c24
# * https://towardsdatascience.com/6-tips-to-optimize-an-nlp-topic-model-for-interpretability-20742f3047e2
# * https://towardsdatascience.com/end-to-end-topic-modeling-in-python-latent-dirichlet-allocation-lda-35ce4ed6b3e0
# * https://www.machinelearningplus.com/nlp/topic-modeling-gensim-python/
# * https://nicharuc.github.io/topic_modeling/

# In[1]:


#import needed libraries
import warnings
warnings.filterwarnings('ignore')
import numpy as np
import pandas as pd
import gensim
import nltk
import re
import spacy
import en_core_web_sm
import pyLDAvis
import pyLDAvis.gensim_models
import matplotlib.pyplot as plt
import unicodedata
import regex as rx
import qgrid

from tqdm import tqdm
from pprint import pprint
from gensim.utils import simple_preprocess
from gensim.parsing.preprocessing import STOPWORDS
from gensim.models import CoherenceModel
import gensim.corpora as corpora
from nltk.stem import WordNetLemmatizer, SnowballStemmer


# In[2]:


#import stop word list
from nltk.corpus import stopwords
nltk.download('stopwords')


# ### ! BEFORE START SET FILEPATH AND IMPORT DATA HERE ! 

# In[41]:


# import data, please insert file link to '220705_html_content_normalized.csv' file
data = pd.read_csv('C:/Users/dominik/Documents/GitHub/Masterthesis/LDA/Data/220705_html_content_normalized.csv', sep=',') 
# randomize data set
data = data.sample(frac=1, random_state = 1)


# In[4]:


# view data
data


# In[5]:


# create corpus
documents = data['html_content']
documents = pd.DataFrame(documents)


# In[6]:


# define functions to remove html content and long and short words

CLEANR = re.compile('<.*?>|&([a-z0-9]+|#[0-9]{1,6}|#x[0-9a-f]{1,6});')

def cleanhtml(raw_html):
  cleantext = re.sub(CLEANR, '', raw_html)
  return cleantext

def remove_sandl_words(s,s_count,l_count):
    return ' '.join([w for w in s.split() if len(w)>s_count and len(w)<l_count])


# In[7]:


def preprocess(df):

    # remove html tags from data
    df['text_preprocessed'] = df.iloc[:, 0].str.replace(r"->","")
    df['text_preprocessed'] = df.iloc[:, 1].str.replace(r"<-","")
    df['text_preprocessed'] = df.iloc[:, 1].str.replace(r"[","<")
    df['text_preprocessed'] = df.iloc[:, 1].str.replace(r"]",">")
    df['text_preprocessed'] = df.iloc[:, 1].apply(cleanhtml)

    #remove control characters
    df['text_preprocessed'] = df.iloc[:, 1].map(lambda x: rx.sub(r'\p{C}', ' ',x))
    
    #remove emails
    df['text_preprocessed'] = df.iloc[:, 1].map(lambda x: rx.sub(r'\S*@\S*\s?', '',x))
    
    # remove punctuation
    df['text_preprocessed'] = df.iloc[:, 1].map(lambda x: re.sub(r'[,\.!?]', '', x))

    # remove words shorter than 3
    df['text_preprocessed'] = df.iloc[:, 1].map(lambda x: remove_sandl_words(x,2,20))
    
    # change to lowercase letters
    df['text_preprocessed'] = df.iloc[:, 1].map(lambda x: x.lower())
    
    return df


# In[8]:


# take first preprocessing step and clean html content
documents = preprocess(documents)


# In[9]:


# count words and append as column
documents["word_count"] = documents['text_preprocessed'].str.split().str.len()


# In[10]:


# check dataframe if preprocessing was successful
documents.head()


# In[11]:


# check single doc if preprocessing was successful
documents.iloc[4][1]


# In[12]:


# remove website with more than 10.000 words 
documents = documents[documents["word_count"]<10000]


# In[13]:


# count number of words in corpus
num_words = documents["word_count"].sum()
num_words


# In[14]:


# create bag of words for 
def sent_to_words(sentences):
    for sentence in sentences:
        yield(gensim.utils.simple_preprocess(str(sentence), deacc=False))  # deacc=True removes punctuations


# In[15]:


data = documents.text_preprocessed.values.tolist()
documents_words = list(sent_to_words(data))


# In[16]:


# Build the bigram models
bigram = gensim.models.Phrases(documents_words, min_count=5, threshold=100) # higher threshold fewer phrases.
# Faster way to get a sentence clubbed as a bigram
bigram_mod = gensim.models.phrases.Phraser(bigram)


# In[17]:


stop_words = stopwords.words('english')

stop_words.extend(stopwords.words('german'))

stop_words.extend(['blockchain','bitcoin','ethereum','technology','nft','nfts','mint','javascript','terms','condition','websites','terms','service'
                   ,'website','cookie','impressum','imprint','etc','cookies'])

# Define functions for stopwords, bigrams, trigrams and lemmatization
def remove_stopwords(texts):
    return [[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]

def make_bigrams(texts):
    return [bigram_mod[doc] for doc in texts]

def lemmatization(texts, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV']):
    """https://spacy.io/api/annotation"""
    texts_out = []
    for sent in texts:
        doc = nlp(" ".join(sent)) 
        texts_out.append([token.lemma_ for token in doc if token.pos_ in allowed_postags])
    return texts_out


# In[18]:


# Remove Stop Words
data_words_nostops = remove_stopwords(documents_words)

# Form Bigrams
data_words_bigrams = make_bigrams(data_words_nostops)

# Initialize spacy 'en' model, keeping only tagger component (for efficiency)
nlp = spacy.load("en_core_web_sm", disable=['parser', 'ner'])
nlp.max_length = 1500000

# Do lemmatization keeping only noun, adj, vb, adv
data_lemmatized = lemmatization(data_words_bigrams, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV'])
print(data_lemmatized[:1])


# In[19]:


# Create Dictionary
id2word = corpora.Dictionary(data_lemmatized)
# Create Corpus
texts = data_lemmatized
# Term Document Frequency
corpus = [id2word.doc2bow(text) for text in texts]
# View
print(corpus[:1])


# In[20]:


# count words after preprocessing
words_total = 0
for t in texts:
    x = len(t)
    words_total = words_total + x
words_total


# In[21]:


#initilize first LDA Model
from multiprocessing import Process, freeze_support
if __name__ == "__main__":
    freeze_support()
    # Build LDA model
    lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,
                                               id2word=id2word,
                                               num_topics=7, 
                                               random_state=100,
                                               update_every=1,
                                               chunksize=100,
                                               passes=10,
                                               alpha='auto',
                                               per_word_topics=True)
    # Print the Keyword in the 8 topics
    pprint(lda_model.print_topics())
    doc_lda = lda_model[corpus]


# In[22]:


#calculate coherence score of above model
coherence_model_lda = CoherenceModel(model=lda_model, texts=data_lemmatized, 
                                                  dictionary=id2word, 
                                                      coherence='c_v')
coherence_lda = coherence_model_lda.get_coherence()
print('\nCoherence Score: ', coherence_lda)


# In[23]:


# function to calculate coherence score for multiple models and save them to a list
def compute_coherence_values(dictionary, corpus, texts, limit, start=2, step=1):

    coherence_values = []
    model_list = []

    for n_topics in tqdm(range(start, limit+1, step)):
        model = gensim.models.ldamodel.LdaModel(corpus=corpus, id2word=id2word, num_topics= n_topics, random_state=100, update_every=1, chunksize=100, passes=10, alpha='auto', per_word_topics=True)        
        model_list.append(model)
        coherencemodel = CoherenceModel(model=model, texts=texts, dictionary=dictionary, coherence='c_v')
        coherence_values.append(coherencemodel.get_coherence())

    return model_list, coherence_values


# In[24]:


# define number of topics to 
total_num_topics = 20
# compute coherence for 20 topics and save them to values
models, cvalues = compute_coherence_values(id2word,corpus,texts,total_num_topics)


# In[26]:


# Show graph over number of topics
limit=total_num_topics; start=2; step=1;
x = range(start, limit+1, step)
y = cvalues

# predefine the graph results 
fig, ax = plt.subplots()
plt.rcParams["figure.figsize"] = [12.00, 6.00]
#plt.rcParams["figure.autolayout"] = True
ticksize = 12
labelsize = 14
markersize = 7
markercolor = 'blue'

#plot the graph
plt.plot(x, y, color='black', linestyle='-', linewidth = 2,
         marker='o', markerfacecolor='white', markeredgecolor=markercolor, markersize=markersize, label ="coherence values")

# define range on axes
plt.xticks(np.arange(min(x), max(x)+1, 1.0), fontsize = ticksize)
plt.yticks(np.arange(round(min(y),2), max(y)+0.02, 0.01), fontsize = ticksize)

# name axes
plt.xlabel("Number of Topics", fontsize = labelsize)
plt.ylabel("Coherence score", fontsize = labelsize)

# title & legend
plt.suptitle("Coherence Score over number of topics", fontsize = 14, y = 0.97)
plt.legend(loc='upper left')

#remove borders
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['left'].set_visible(True)
ax.spines['bottom'].set_visible(True)

#grid
plt.grid(color = 'black',which = 'both', linestyle = '--', linewidth = 0.25)

# print plot
plt.show()


# In[28]:


def compute_coherence_values_a(corpus, dictionary, k, a, b):
    
    lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,
                                           id2word=dictionary,
                                           num_topics=k, 
                                           random_state=100,
                                           chunksize=100,
                                           passes=10,
                                           alpha=a,
                                           eta=b)
    
    coherence_model_lda = CoherenceModel(model=lda_model, texts=data_lemmatized, dictionary=id2word, coherence='c_v')
    
    return coherence_model_lda.get_coherence()


# In[29]:


grid = {}
grid['Validation_Set'] = {}

# Alpha parameter
alpha = list(np.arange(0.01, 1, 0.11))
alpha.append('symmetric')
alpha.append('asymmetric')
# Beta parameter
beta = list(np.arange(0.01, 1, 0.11))
beta.append('symmetric')

# Validation sets
num_of_docs = len(corpus)

model_results = {'Topics': [],
                 'Alpha': [],
                 'Beta': [],
                 'Coherence': []
                }
# Can take a long time to run
if 1 == 1:
    pbar = tqdm(total=len(alpha)*len(beta))
    
    # iterate through validation corpuses
    for a in alpha:
        # iterare through beta values
        for b in beta:
            # get the coherence score for the given parameters
            cv = compute_coherence_values_a(corpus=corpus, dictionary=id2word, 
                                          k=7, a=a, b=b)
            # Save the model results
            model_results['Topics'].append(7)
            model_results['Alpha'].append(a)
            model_results['Beta'].append(b)
            model_results['Coherence'].append(cv)

            pbar.update(1)
    pd.DataFrame(model_results).to_csv('lda_tuning_results.csv', index=False)
    pbar.close()


# In[30]:


model_res = pd.DataFrame(model_results)
model_res.sort_values(by = ['Coherence'],ascending = False)


# In[31]:


best_alpha = 0.67
best_beta = 0.23
best_num_top = 7

final_LDA_a = gensim.models.ldamodel.LdaModel(corpus=corpus,
                                           id2word=id2word,
                                           num_topics=best_num_top, 
                                           random_state=100,
                                           chunksize=100,
                                           passes=10,
                                           alpha=best_alpha,
                                           eta=best_beta)
pprint(final_LDA_a.print_topics())

coherence_LDA_a = compute_coherence_values_a(corpus=corpus, dictionary=id2word, 
                                          k=best_num_top, a=best_alpha, b=best_beta)


# In[32]:


import pyLDAvis.gensim_models
pyLDAvis.enable_notebook()

topic_data2 = pyLDAvis.gensim_models.prepare(final_LDA_a, corpus, id2word) 
topic_data2


# In[33]:


#save inter topic distance map
pyLDAvis.save_html(topic_data2, '220711_pyLDA_NFT_LDA_a.html')


# In[34]:


all_topics = {} # Create empty dictionary
num_terms = 20 # Adjust number of words to represent each topic
lambd = 0.5 # Set the lambda parameter after viewing Intertopic Distance Map
for i in range(1,best_num_top+1): #Adjust this to reflect number of topics chosen for final LDA model
    topic = topic_data2.topic_info[topic_data2.topic_info.Category == 'Topic'+str(i)].copy()
    topic['relevance'] = topic['loglift']*(1-lambd)+topic['logprob']*lambd
    all_topics['Topic '+str(i)] = topic.sort_values(by='relevance', ascending=False).Term[:num_terms].values


# In[35]:


# show topic dataframe 
topic_words = pd.DataFrame(all_topics).T
topic_words


# In[37]:


# tabelize data to display in thesis
table_data = {
    "Model":["LDA_a"],
    "Words initial":[num_words],
    "Words after preprocess":[words_total],
    "Number of Topics":[best_num_top],
    "LDA Alpha":[best_alpha],
    "LDA Beta":[best_beta],
    "LDA coherrence":[coherence_LDA_a],
    "LDA Lambda":[lambd]
    }

table_inf = pd.DataFrame(table_data)


# In[38]:


# display info table
table_inf

