# <img width="30" height="30" alt="Diapositiva2" src="https://github.com/user-attachments/assets/c40773a1-7e93-4fd0-8c38-2b42137ff078" /> Social inclusion and chronic illness: is it a matter of regression or classification?


We study how perceived social inclusion in patients from the Milan area relates to their clinical condition, socio-demographic profile and municipality of residence.
The outcome is a 0–16 questionnaire score (housing, social life, finances, work/study). We model it in two ways, as a continuous score and as three classes (low / medium / high),
to show how much information is lost when a score is turned into categories.

In particular:
- Train/test split (80/20) before any preprocessing
- Missing values imputed with MICE (training data only)
- SMOTE-NC to balance the classes (training data only)
- Models: decision trees, bagging, random forest, H2O AutoML as benchmark
- Interpretation: variable importance, partial dependence plots, SHAP

The real data are not public for privacy reasons. A script to simulate an appropriate dataset was loaded instead.
