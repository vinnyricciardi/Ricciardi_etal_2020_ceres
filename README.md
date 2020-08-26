This folder contains analysis for the Ceres2030 Team 6, who conducted a semi-automatic systematic review of over 18,000 articles. The goal was to identify solutions for small farms to adapt to climate change - we focused on water scarce environments. This analysis is under review at the academic journal Nature Sustainability. A link to the article will be provided once published. The manuscript's current citation is:

Ricciardi, V., Wane, A., Singh Sidhu, B., Goode, C., Solomon, D., McCullough, E., Diekmann, F., Porciello, J., Jain, M., Randall, N., Mehrabi, Z.
Evidence synthesis: Funding research for small-scale farmers in water scarce regions to achieve SDG 2.3. In Review. Nature Sustainability.


Our analysis was completed in 3 stages.

1. We use manually coded abstracts to build a natural language processing (NLP) model to include or exclude abstracts to our study. A deep learning model was developed using Bidirectional Encoder Representations from Transformers (BERT) and a cross entropy function to classify abstracts. This model performed better than other classifiers (e.g., optimized support vector machine (SVM) and naive bayes) across accuracy, precision, and recall metrics.<br><br>
1. We conducted a geospatial analysis to compare the articles with water scarce regions where small-farmers live. This was a to step process. First, we downscaled national farm size distributions using a crowd sourced dataset of farmers' field sizes (n=500k) and cropland masks at 5-arc minute (~10km2) resolution. A weighted k-nearest neighbor routine was used to interpolate crowd sourced data, then a custom built non-parametric method was developed to downscale results. This first step is in press at Nature Sustainability and code will be released with the article. The second step comprised of several spatial overlays to understand how many small farms have irrigation and are located in water scarce areas.
1. We used results from steps 1 and 2 to perform a graphic analysis on the data and to visualize results.

All data can be found in the supplemental information of our published manuscript.
