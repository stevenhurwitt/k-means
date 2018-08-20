# k-means in R
code to run k-means from scratch via command line.

#command line
cd to the directory with R file & data to be used with k-means in it.

code can be run as follows:

Rscript k-means-alg.R [input_data.csv x_col_index y_col_index k tolerance output_file_name]

# k-means in Python
Implementation of k means as a class in Python, depends on numpy and pandas.

can be run in python in the following two lines:

from Kmeans import Kmeans

kmeans_clusters = Kmeans(X, k, tolerance).clusters)
