kaggle_accelerometer
====================

STAT 542

1. This is the final project for STAT 542 Course
2. Only Code is uploaded
1. The data was imported into R by ”ff” packages. Preprocessing procedures include: splitting,resampling and smoothing. 
2. Features include signal mean and variance, covariance, dominant frequency, signal time of the day and device sampling frequency
3. We formulated this task as a binary classification problem. For data from one device, randomly selected some pieces as positive samples and pieces from other device as negative samples. The number of negative samples should be equal to that of positive samples to get a balanced data set. Then trained the classifier for this device. 
4. Classification methods include SVM, LDA, Logistic regression, Random forest, Lasso regression and KNN
5. Random Forest has the best performance of 81.3% on test data, or 78.1% without using sampling frequency.