This folder contains analysis for our [Ceres2030 Team 6](https://ceres2030.org/water-scarce-solutions/), who conducted a semi-automatic systematic review of over 18,000 articles. 

The goal was to identify solutions for small farms to adapt to climate change - we focused on water scarce environments. This analysis is published in the academic journal Nature Sustainability:

Ricciardi, V., Wane, A., Sidhu, B.S. et al. A scoping review of research funding for small-scale farmers in water scarce regions. Nat Sustain 3, 836–844 (2020). https://doi.org/10.1038/s41893-020-00623-0


Our analysis was completed in 3 stages.

1. We use manually coded abstracts to build a natural language processing (NLP) model to include or exclude abstracts to our study. A deep learning model was developed using Bidirectional Encoder Representations from Transformers (BERT) and a cross entropy function to classify abstracts. This model performed better than other classifiers (e.g., optimized support vector machine (SVM) and naive bayes) across accuracy, precision, and recall metrics.<br><br>
1. We conducted a geospatial analysis to compare the articles with water scarce regions where small-farmers live. This was a to step process. First, we downscaled national farm size distributions using a crowd sourced dataset of farmers' field sizes (n=500k) and cropland masks at 5-arc minute (~10km2) resolution. A weighted k-nearest neighbor routine was used to interpolate crowd sourced data, then a custom built non-parametric method was developed to downscale results. This first step is in press at Nature Sustainability and code will be released with the article. The second step comprised of several spatial overlays to understand how many small farms have irrigation and are located in water scarce areas.<br><br>
1. We used results from steps 1 and 2 to perform a graphic analysis on the data and to visualize results.
<br><br>
All data can be found in the supplemental information of our published manuscript.
