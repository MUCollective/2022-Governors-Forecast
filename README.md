# 2022 Governor Forecasts

This repository provides our model forecast and outputs for 2022 U.S. governor elections. 

Our forecast is inspired by previous models on forecasting presidential elections [1](https://hdsr.mitpress.mit.edu/pub/nw1dzd02/release/2),[2](https://votamatic.org/wp-content/uploads/2013/07/Linzer-JASA13.pdf). 
The model assumes each state has day-to-day changes that vary a set of factors: polls, pollsters, polling bias in each state, polling methods, and voter types. 
It starts with the last election outcomes and moves forwards to predict vote share between the two major parties. 
We use polling data from [Fivethirtyeight](https://projects.fivethirtyeight.com/polls/) and convert it to two-party vote share. 

We organize these in the folder `forecasts` as follows:

  `forecast-governors.Rmd`: the main source file  
  `forecast-governors.md`: the github version of the main source file  
  `forecast-governors_files`: the files needed to render the md on Github  
  `model-output` folder: it contains the election day prediction every time we run the model  
  `stan` folder: it holds the stan source file and the compiled stan model  
  `data` folder: it holds two other datasets we use: cook report and Fivethirtyeight's prediction on Sep 1  
 
  
