import pandas as pd
import streamlit as st
import numpy as np

# web interface
st.title('Mixed and functional analysis models for studying Heart Failure (re)-hospitalizations')

st.markdown("""
Original repository: https://github.com/marcolucchini/Applied-Statistics-project

Here you can find extra material about our project including predictions example and more graphs.
At the end you will find the bibliography.
For more indormation about the models see `Applied_project.R` in the `Script` folder of this repository. 
""")

# main page
st.subheader("Research goals")
st.image("research_goals.png")

st.subheader("Multiple Correspondence Analysis")
st.write("""In statistics, multiple correspondence analysis (MCA) is a data analysis technique for 
categorical data, used to detect and represent underlying structures in a data set. 
It does this by representing data as points in a low-dimensional Euclidean space. 
The procedure thus appears to be the counterpart of principal component analysis for categorical data.
We tried to apply a MCA on comorbidities but the results were not so relevant. 
Indeed to reach an acceptable percentage of explained variability (80%) we should have 
kept al least 14 factors out of 20 comorbidities.
""")
st.image("MCA.png")

st.subheader("Poisson Regression")
st.write("""In statistics, Poisson regression is a generalized linear model form of regression analysis used to model count data. 
It assumes the response variable Y has a Poisson distribution, and that the logarithm of its expected value can be 
modeled by a linear combination of unknown parameters.
If 𝑥∈𝑅^(𝑟+1) is a vector of is a vector of independent variables, then the model takes the form: 
log⁡(𝐸[𝑦│𝑥])=𝛽𝑥  , where  𝛽∈𝑅^(𝑟+1)""")
st.image("Fit poisson ric1.png")


st.subheader("Cluster Interpretation")
st.image("Cluster.png")

st.subheader("Example of prediction")
st.write("""In conclusion, we want to show a case example with one imaginary patient to present 
the information we could provide to help doctors in making their decisions. 
To do so, we make predictions for the number of hospitalizations and the probability 
of death within a one-year horizon, and we also studied how our prediction changes depending 
on the hospital where the patient is admitted and on his initial comorbidities.
""")
st.image("logistic ranef.png")



st.subheader("Bibliography")
st.write("""* Mohammad MA, Koul S, Rylance R, et al. Association of Weather With Day-to-Day Incidence of Myocardial Infarction: A SWEDEHEART Nationwide Observational Study. JAMA Cardiol. 2018
* Mazzali, C., Paganoni, A.M., Ieva, F. et al. Methodological issues on the use of administrative data in healthcare research: the case of heart failure hospitalizations in Lombardy region, 2000 to 2012
* Trevor Hastie,Robert Tibshirani,Jerome Friedman, et. al. The Elements of Statistical Learning Data Mining, Inference, and Prediction. Second edition, Springer, 2008""", )

# # estimated needs
# if (man.accumulation and man.income):
#     st.write(f"The estimate need are: Income and Accumulation")
# elif (man.accumulation):
#     st.write(f"The estimate need is: Accumulation")
# elif (man.income):
#     st.write(f"The estimate need is: Income")
# else:
#     st.write("The client seem not to need products, ask him more informations")
# # estimated risk
# st.write(f"The estimated risk is: {man.risk[0]:.2f}")
# # suggested products
# st.subheader(f"Suggested products sorted by risk are:")
# st.dataframe(man.suggested)


# # shap explanation
# explainer_acc = shap.Explainer(xgb_acc)
# shap_values_acc = explainer_acc(man.Xsmall)

# explainer_inc = shap.Explainer(xgb_inc)
# shap_values_inc = explainer_inc(man.Xsmall)

# # plots of explanation
# st.subheader("Explanation of the models")

# st.write("Explanation of **Income** prediction (with rescaled parameters):")
# fig_inc = plt.figure()
# shap.plots.waterfall(shap_values_inc[0],) 
# st.pyplot(fig_inc)

# st.write("Explanation of **Accumulation** prediction (with rescaled parameters):")
# fig_acc = plt.figure()
# shap.plots.waterfall(shap_values_acc[0],)
# st.pyplot(fig_acc)
