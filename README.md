# Consumer-behavior-forecasting

This project was undertaken as academic study to understand and apply the concepts of Business Analytics using R. The data set chosen is taken from Kaggle and is a random selection from Expedia and is not representative of the overall statistics.

The Expedia dataset contains logs of customer behavior about the hotel booking data across multiple continents, which was further subdivided at countries, city and region level. These include what customers searched for, how they interacted with search results (click/book), whether or not the search result was a travel package. 

Expedia has provided three datasets that are split based on time :
  1. train.csv - the training set contains info for the year 2013 & 2014. It has logs on all users and their clicks/booking.
  2. test.csv - the test dataset contains only the booking information from the year 2015.
  3. destinations.csv - consists of features extracted from hotel reviews text.
  
Although Kaggle provided training and test dataset separately, we decided to work with training dataset only. The reason for this is the size of the training dataset. With 24 variables, 37 million records and 4GB in size, it is large enough to split this dataset into training and validation sets according to the problem statement and still obtain effective results.
  
