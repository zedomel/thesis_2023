import re
import nltk
import spacy
import gensim
import pyLDAvis
import nlp
import pandas as pd
import numpy as np
import os
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel
import pyLDAvis.gensim
import matplotlib.pyplot as plt
import tqdm
import pickle

#%matplotlib inline

def sent_to_words(sentences):
    for sentence in sentences:
        yield(gensim.utils.simple_preprocess(str(sentence), deacc=True, min_len= 3))


def remove_stopwords(texts, stop_words):
    return [[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]

def lemmatization(texts, allowed_postags=['NOUN']):
    texts_out = []
    for sent in texts:
        doc = nlp(" ".join(sent))
        texts_out.append([token.lemma_ for token in doc if token.pos_ in allowed_postags])
    return texts_out

def fit_lda(corpus, id2word, texts, k, a, b, corpus_test, texts_test):
    lda_model = gensim.models.ldamulticore.LdaMulticore(corpus=corpus,
                                                id2word=id2word,
                                                num_topics=k,
                                                random_state=1234,
                                                iterations=100,
                                                chunksize=10000,
                                                passes=1,
                                                alpha=a,
                                                eta=b,                                                        
                                                per_word_topics=True)

    #Save the model for later    
    #lda_model.save(f"./data/models/lda_model_{k}_{a}_{b}")
    
    c_v = CoherenceModel(model=lda_model, texts=texts_test,corpus=corpus_test, dictionary=id2word, coherence='c_v')    
    u_mass  = CoherenceModel(model=lda_model, texts=texts_test,corpus=corpus_test, dictionary=id2word, coherence='u_mass')    
    perp = lda_model.log_perplexity(corpus_test)
    return (c_v.get_coherence(),u_mass.get_coherence(), perp)

# nltk.download('stopwords')

# df = pd.read_csv("../R/bib-analysis/data/datasets/publications-uniq.tsv", sep='\t')
# df["text"] = df["TI"] + ". " + df["AB"]
# data = df.text.values.tolist()

# # Remove new line characters
# data = [re.sub(r'\s+', ' ', str(i)) for i in data]

# data = [re.sub("'", "", i) for i in data]

# data_words = list(sent_to_words(data))

# with open('stopwords.txt', 'r') as f:
#     custom_stop_words = f.read().split("\n")

# stop_words = nltk.corpus.stopwords.words("english")
# nlp = spacy.load("en_core_web_trf", disable=['parser', 'ner'])

# data_words_nostops = remove_stopwords(data_words, stop_words)
# data_words_nostops = remove_stopwords(data_words_nostops, custom_stop_words)

# data_lemmatized = lemmatization(data_words_nostops, allowed_postags=["NOUN"])

# # LDA
# id2word = corpora.Dictionary(data_lemmatized)
# texts  = data_lemmatized

# id2word.filter_extremes(no_below=0.01*len(texts),no_above=0.50, keep_n=None)
# corpus = [id2word.doc2bow(text) for text in texts]

# print(f'Number of unique tokens: {len(id2word)}')
# print(f'Number of documents: {len(corpus)}')


# Topics range
topics_range = range(2,20,2)

# Alpha parameter
alpha = [0.05,0.1,0.5,1.5,10]
alpha.append("symmetric")

# Beta parameter
beta = [0.05,0.1,0.5,1.5,10]
beta.append("symmetric")

# Validation set
models_results = {
    'Topics': [],
    'Alpha': [],
    'Beta': [],
    'CV': [],
    'UMASS': [],
    'PERP': []
}

os.environ["TOKENIZERS_PARALLELISM"] = "false"

# Save dictionary
# id2word.save('./data/id2word')
# with open("./data/texts.pkl","wb") as f:
#     pickle.dump(texts,f)
#Load
with open('./data/xl/id2word-extremes','rb') as f:
    id2word =pickle.load(f)

with open('./data/xl/texts.pkl','rb') as f:
    texts =pickle.load(f)

corpus = [id2word.doc2bow(text) for text in texts]

# Documents
df = pd.read_csv("data/dimensions/publications-ecology-filtered-big.csv")

# Train documents
train_docs=df.groupby("GROUP").sample(frac=0.75,random_state=200)
idx = train_docs.index.to_numpy()
train_texts = [texts[i] for i in idx]
train_corpus = [id2word.doc2bow(text) for text in train_texts]

# Test documents
test_docs=df.drop(train_docs.index)
test_texts = [texts[i] for i in range(0,len(texts)) if i not in idx]
test_corpus = [id2word.doc2bow(text) for text in test_texts]

print(id2word)
print(f"Train size: {len(train_corpus)}")
print(f"Test size: {len(test_corpus)}")

pbar = tqdm.tqdm(total=len(topics_range)*len(alpha)*len(beta))

for k in topics_range:
    for a in alpha:
        for b in beta:
            (cv,umass, perp) = fit_lda(corpus=train_corpus, texts=train_texts, 
                                       id2word=id2word,k=k,a=a,b=b, 
                                       corpus_test=test_corpus,texts_test=test_texts)
            models_results['Topics'].append(k)
            models_results['Alpha'].append(a)
            models_results['Beta'].append(b)
            models_results['CV'].append(cv)
            models_results['UMASS'].append(umass)
            models_results['PERP'].append(perp)

            pbar.update(1)



pd.DataFrame(models_results).to_csv('data/xl/lda_tuning_results_extremes.csv', index=False)
pbar.close()
