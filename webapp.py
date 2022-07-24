import pandas as pd
import streamlit as st
import numpy as np

# web interface
st.title('Mixed and functional analysis models for studying Heart Failure (re)-hospitalizations')

st.markdown("""
Original repository: https://github.com/marcolucchini/Applied-Statistics-project

Here you can find extra material about our project including prediction examples and more graphs.
In the end, you will find the bibliography.
For more information about the models see `Applied_project.R` in the `Script` folder of this repository. 
""")

# main page
st.subheader("Research goals")
st.image("research_goals.png", )

st.subheader("Multiple Correspondence Analysis")
st.write("""In statistics, multiple correspondence analysis (MCA) is a data analysis technique for 
categorical data, used to detect and represent underlying structures in a data set. 
It does this by representing data as points in a low-dimensional Euclidean space. 
The procedure thus appears to be the counterpart of principal component analysis for categorical data.
We tried to apply an MCA on comorbidities but the results were not so relevant. 
Indeed to reach an acceptable percentage of explained variability (80%) we should have 
kept at least 14 factors out of 20 comorbidities.
""")
st.image("MCA.png")

st.subheader("Poisson Regression")
st.write("""In statistics, Poisson regression is a generalized linear model form of regression analysis used to model count data. 
It assumes the response variable Y has a Poisson distribution, and that the logarithm of its expected value can be 
modelled by a linear combination of unknown parameters.
If 𝑥∈𝑅^(𝑟+1) is a vector of independent variables, then the model takes the form: 
log⁡(𝐸[𝑦│𝑥])=𝛽𝑥 , where 𝛽∈𝑅^(𝑟+1)""")
st.image("Fit poisson ric1.png")


st.subheader("Cluster Interpretation")
st.image("Cluster.png")

st.subheader("Example one year life expectancy prediction")
st.write("""In conclusion, we want to show a case example with one imaginary patient to present 
the information we could provide to help doctors in making their decisions. 
To do so, we make predictions for the probability 
of death within a one-year horizon. We also studied how our prediction changes depending 
on the hospital, the best and the worst, where the patient is admitted and on his initial comorbidities.
""")
st.image("logistic ranef.png")



st.subheader("Bibliography")
st.write("""* Mohammad MA, Koul S, Rylance R, et al. Association of Weather With Day-to-Day Incidence of Myocardial Infarction: A SWEDEHEART Nationwide Observational Study. JAMA Cardiol. 2018
* Mazzali, C., Paganoni, A.M., Ieva, F. et al. Methodological issues on the use of administrative data in healthcare research: the case of heart failure hospitalizations in Lombardy region, 2000 to 2012
* Johnson, R.A. e Wichern, D.W., Applied Multivariate Statistical Analysis , Editore: Prentice Hall, Upper Saddle River, Anno edizione: 2007
* Trevor Hastie,Robert Tibshirani,Jerome Friedman, et. al. The Elements of Statistical Learning Data Mining, Inference, and Prediction. Second edition, Springer, 2008
* Ramsay, J.O. e Silverman, B.W.,, Functional Data Analysis (second edition), Editore: Springer Series in Statistics, Springer, Anno edizione: 2005""")
