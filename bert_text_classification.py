#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from datetime import datetime
import warnings
import ktrain
from ktrain import text
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.naive_bayes import MultinomialNB
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.pipeline import Pipeline
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import KFold
from sklearn.metrics import make_scorer, accuracy_score


# ### Read in and clean data

# In[2]:


all_data = pd.read_csv('./data/Team_6.csv')
selected = pd.read_csv('https://raw.githubusercontent.com/vinnyricciardi/ceres2030_team6/master/data/review_59500_select_csv_20190925102202.csv')
irrelevant = pd.read_csv('https://raw.githubusercontent.com/vinnyricciardi/ceres2030_team6/master/data/review_59500_irrelevant_csv_20190925102123.csv')


# In[3]:


# selected['text'] = selected['Title'] + ' ' + selected['Abstract']
selected['text'] = selected['Abstract'].str.replace('Abstract: ', '')
selected['text'] = selected['text'].str.split('#',  n = 1, expand = True)
selected['sentiment'] = 1

# irrelevant['text'] = irrelevant['Title'] + ' ' + irrelevant['Abstract']
irrelevant['text'] = irrelevant['Abstract'].str.replace('Abstract: ', '')
irrelevant['text'] = irrelevant['text'].str.split('#',  n = 1, expand = True)
irrelevant['sentiment'] = 0

df = pd.concat([
    selected.loc[:, ['text', 'sentiment']],
    irrelevant.loc[:, ['text', 'sentiment']],
])


# Clean
# Remove header and tail from each abstract.
# Note these features were added to our abstracts
# by an initial attempt to use unsupervised pre-processing
# but we did not use them in our final models. Citation:
# Porciello, et al. Forthcoming. Using machine-learning models 
# to accelerate donor-driven evidence for the Sustainable
# Development Goals. Submitted to Nature Machine Intelligence.
#

df.text = df.text.str.replace('Abstract: ', '')
df.text = df.text.str.split('#').str[0]

# Remove empty/sparse abstracts
df = df.loc[df.text.str.len() > 200,:]

# Lower case
df.text = df.text.str.lower()


# ### Train-test-split

# In[4]:


train, test = train_test_split(df)
train = train.sample(len(train))
train = train.reset_index(drop=True)
test = test.reset_index(drop=True)


# ### Define k-folds CV

# In[23]:


def run_kfold(clf=None, X_all=df.text, y_all=df.sentiment, mod_type='scikit-learn'):
    kf = KFold(n_splits=10)
    accuracy = []
    precision = []
    recall = []
    f1 = []
    fold = 0
    for train_index, test_index in kf.split(X_all):
        fold += 1

        if mod_type == 'scikit-learn':
            
            X_train, X_test = X_all.values[train_index], X_all.values[test_index]˜
            y_train, y_test = y_all.values[train_index], y_all.values[test_index]

            clf.fit(X_train, y_train)
            predictions = clf.predict(X_test)
        
        elif mod_type == 'bert':

            X_train, y_train = df.iloc[train_index, 0], df.iloc[train_index, 1]
            X_test, y_test = df.iloc[train_index, 0], df.iloc[train_index, 1]

            MODEL_NAME = 'bert-base-multilingual-uncased'     # main model 1; check out https://towardsdatascience.com/text-classification-with-hugging-face-transformers-in-tensorflow-2-without-tears-ee50e4f3e7ed
            t = text.Transformer(MODEL_NAME, maxlen=500, classes=[0,1])
            trn = t.preprocess_train(X_train, y_train)
            val = t.preprocess_test(X_test, y_test)
            model = t.get_classifier()
            learner = ktrain.get_learner(model, train_data=trn, val_data=val, batch_size=6)
            learner.lr_find(show_plot=False, max_epochs=2)
            learner.fit_onecycle(5e-5, 4)  # replace var1 with optimal learning rate from above (i.e., apex of valley)
            predictor = ktrain.get_predictor(learner.model, preproc=t)
            predictions = X_test.apply(lambda x: predictor.predict(x))

        
        accuracy.append(accuracy_score(y_test, predictions))
        precision.append(classification_report(
            y_test, predictions, output_dict=True)['weighted avg']['precision'])           
        recall.append(classification_report(
            y_test, predictions, output_dict=True)['weighted avg']['recall'])
        f1.append(classification_report(
            y_test, predictions, output_dict=True)['weighted avg']['f1-score'])  
        
    mean_accuracy = np.mean(accuracy)
    mean_precision = np.mean(precision)
    mean_recall = np.mean(recall)
    mean_f1 = np.mean(f1)
    std_accuracy = np.std(accuracy)
    std_precision = np.std(precision)
    std_recall = np.std(recall)
    std_f1 = np.std(f1)

    return(mean_accuracy, mean_precision, mean_recall, mean_f1,
           std_accuracy, std_precision, std_recall, std_f1) 


# In[48]:


# pipeline1 = Pipeline((
#     ('vect', CountVectorizer()),
#     ('tfidf', TfidfTransformer()),
#     ('clf', RandomForestClassifier())    
#     ))

# pipeline2 = Pipeline((
#     ('vect', CountVectorizer()),
#     ('tfidf', TfidfTransformer()),
#     ('clf', KNeighborsClassifier()),
#     ))

pipeline3 = Pipeline((
    ('vect', CountVectorizer()),
    ('tfidf', TfidfTransformer()),
    ('clf', SVC()),
    ))

pipeline4 = Pipeline((
    ('vect', CountVectorizer()),
    ('tfidf', TfidfTransformer()),
    ('clf', MultinomialNB()),
    ))

parameters1 = {
    'clf__n_estimators': [10, 20, 30],
    'clf__criterion': ['gini', 'entropy'],
    'clf__max_features': [5, 10, 15],
    'clf__max_depth': ['auto', 'log2', 'sqrt', None]
    }

parameters2 = {
    'clf__n_neighbors': [3, 7, 10],
    'clf__weights': ['uniform', 'distance']
    }

parameters3 = {
    'clf__C': [0.01, 0.1, 1.0],
    'clf__kernel': ['rbf', 'poly'],
    'clf__gamma': [0.01, 0.1, 1.0],

    }
parameters4 = {
    'clf__alpha': [0.01, 0.1, 1.0]
    }

pars = [parameters3, parameters4]
pips = [pipeline3, pipeline4]
mods = ['svm', 'nb']
scores = []

print("starting Gridsearch")
for i in range(len(pars)):
    gs = GridSearchCV(pips[i], pars[i], n_jobs=-1)
    gs.fit(train.text, train.sentiment)
    gs = gs.best_estimator_
    gs.fit(train.text, train.sentiment)

    mean_accuracy, mean_precision, mean_recall, mean_f1, std_accuracy, std_precision, std_recall, std_f1 = run_kfold(clf=gs)
    print("finished Gridsearch")
    print('model = {}'.format(mods[i]))
    print(mean_accuracy, mean_precision, mean_recall, mean_f1)

    scores.append(list([
        mods[i], 
        mean_accuracy, 
        mean_precision, 
        mean_recall, 
        mean_f1, 
        std_accuracy, 
        std_precision, 
        std_recall, 
        std_f1
        ]))



# BERT

mean_accuracy, mean_precision, mean_recall, mean_f1, std_accuracy, std_precision, std_recall, std_f1 = run_kfold(mod_type='bert')
print(mean_accuracy, mean_precision, mean_recall, mean_f1)

scores.append(list([
    'bert', 
    mean_accuracy, 
    mean_precision, 
    mean_recall, 
    mean_f1, 
    std_accuracy, 
    std_precision, 
    std_recall, 
    std_f1
    ]))

# In[55]:


out = pd.DataFrame(scores)
print(out)
out.to_csv('~/Downloads/out.csv')

