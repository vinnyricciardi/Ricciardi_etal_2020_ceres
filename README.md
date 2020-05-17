This folder contains analysis for the Ceres2030 Team 6, who conducted a semi-automatic systematic review of over 18,000 articles. The goal was to identify solutions for small farms to adapt to climate change - we focused on water scarce environments. This analysis is under review at the academic journal Nature Sustainability. A link to the article will be provided once published.

Our analysis was completed in 3 stages.

    We use manually coded abstracts to build a natural language processing (NLP) model to include or exclude abstracts to our study. See the `abstract_nlp` folder for  analysis and results. A deep learning model was developed using Bidirectional Encoder Representations from Transformers (BERT) and a cross entropy function to classify abstracts. This model performed better than other classifiers (e.g., optimized support vector machine (SVM) and naive bayes) across accuracy, percision, and recall metrics.

    We use look up tables to extract key information from each article's full-text. See the `fulltext_analysis` folder for  analysis and results. This step tested a customized ontology of look-up terms that were created using several NLP topic models from a seperate project (paper forthcoming in Nature Machine Intelligence).

    We conducted a geospatial analysis to compare the articles with water scarce regions where small-farmers live. This was a to step process. First, we downscaled national farm size distributions using a crowd sourced dataset of farmers' field sizes (n=500k) and cropland masks at 5-arc minute (~10km2) resolution. A weighted k-nearest neighbor routine was used to interpolate crowd sourced data, then a custom built non-parametric method was developed to downscale results. This first step is in press at Nature Sustainability and code will be released with the article. The second step comprised of several spatial overlays to understand how many small farms have irrigation and are located in water scarce areas. This analysis is in the `irrigation gap` folder.


