CODE BOOK

DATA

For column headings - see features.txt and README.md. All column headings represent mean and standard deviation data from accelerometer and gyrometers in standard gravitational units. Other data has been removed. For computational ease column names have been reduced to strings of lower case letters, but otherwise are exactly as described in the previously mentioned documents.

alltest -> cleaned data derived from the test data set, the data is separated into the desired variables using the cleanup() function and bound to the subject and activity data

alltrain -> as for alltest, but derived from the train data set

cleandata -> final clean dataset, including both test and train data after cleaning

df -> temporary dataframe used during binding of data sets

features -> a dataframe which is modified to extract desired features from the data sets

subjecttest -> id numbers for subjects in test data set

subjecttrain -> id numbers for subjects in train data set

xt -> temporary data frame for outputting clean data from the cleanup() function

xtest -> raw data from the test subjects, later transformed into clean data using the cleanup() function

xtrain -> as for xtest but for train subjects

VARIABLES

activities <- 6 level factor containing the activities performed during experiment phase

activitylabels <- vector of character strings for activities

columns <- temporary vector for renaming columns

featuresvect <- temporary vector to extract features of interest from features.txt

vector <- temporary vector to extract features of interest from features.txt

FUNCTIONS

cleanup() -> breaks space-separated vectors into individual components, writes them to a vector and combines vectors into a data frame which is then exported from the function