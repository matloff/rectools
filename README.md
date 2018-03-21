# rectools

## Advanced Package for Recommender Systems

A featured-filled package of tools from the recommender systems
("recsys") world, plus some new methods.

## Recommender Systems

In recsys, we have data in which some "users" rate some "items,"
and we wish to predict how users would rate other items.  Example
applications are:

* Moviegoers rate films (rhe "Hello World" of recsys).

* Link prediction in graphs (where will the next new edge be created?).

* Will a certain user have a bad reaction to a certain prescription
  drug?

* How well will sports team A do against team B?

## Package Features

* Incorporate user and item covariate information, including item
  category preferences.

* New methods, and novel variations on common models. 

* Parallel computation.

* Plotting.

* Focus group finder.

## Example: InstEval data

Let's start with a concrete example, the **InstEval** data that is
bundled with the **lmer4** package, which is included by **rectools**.
The data consist of student valuations of instructors at the famous
Swiss university ZTH.  There are 73421 ratings submitted by 2972
students of 1128 instructors.  Ratings are on a scale of 1 to 5.

Not surprisingly, the ratings matrix is sparse; most students did not
rate most instructors.  The goal is to "complete" the matrix, i.e.
predict how well all the students would like all the instructors.

For convenience, **rectools** includes a function that loads this data,
processes it (e.g. creating dummy variables for the school's
departments), and assigning the result to **ivl**:

``` R
> getInstEval()
> ivl[28888,]
         s   d y studage lectage service dpt15 dpt5 dpt10
28888 1178 220 5       3       5       0     0    0     0
      dpt12 dpt6 dpt7 dpt4 dpt8 dpt9 dpt14 dpt1 dpt3 dpt11
28888     1    0    0    0    0    0     0    0    0     0
```

The data frame **ivl** follows the standard input format in the recsys
world: user ID, item ID, rating, covariates.

In that record 28888, for instance, Student 1178 gave a rating of 5 to
Instructor 220.  The class was not a service class (i.e. not for
nonmajors) in Department 12.  The student was in his/her third year
(**studage** = 3), and had taken the course 5 semesters earlier.

### Random effects ANOVA model:

A simple statistical random effects latent factor model, often called a
*baseline* model in the recsys literature, is

E(Y) =  &mu; + &alpha;<sub>i</sub> + &beta;<sub>j</sub>

where Y<sub>ij</sub> is the rating, with &alpha;<sub>i</sub> and
&beta;<sub>j</sub> being specific latent effects for user i and item j,
e.g. moviegoer i and movie j.

Though typically Maximum Likelihood Estimation is used for random
effects models in statistics, this is computationally infeasible on
large data sets.  Instead, we use the Method of Moments, which provides
a closed-form solution, estimating &alpha;<sub>i</sub> by Yi. - Y..,
where the first term is the mean of all observed ratings by user i and
the second is the overall mean of all ratings.  We estimate
&beta;<sub>j</sub> similarly, and estimate &mu; by the overall mean Y..
The predicted value of Y<sub>ij</sub> is then

Y.. + (Yi. - Y..) + (Y.j - Y..) = Yi. Y.j - Y.. 

Let's see how student 1128 would like instructor 99.  For now, let's not
include the covariates.

``` R
> mmout <- trainMM(ivl[,1:3])  # exclude covariates
> predict(mmout,data.frame(s=1128,d=99))
[1] 3.119652
```

We do make MLE available.  Here &alpha;<sub>i</sub> and
&beta;<sub>j</sub> are assumed to have independent normal distributions
with different variances.  (The error term &epsilon;<sub>ij</sub> =
Y<sub>ij</sub> - EY<sub>ij</sub> is assumed independent of
&alpha;<sub>i</sub> and &beta;<sub>j</sub>, with variance constant
across i and j.) We piggyback R's **lme4** package, forming a wrapper
for our application, and adding our function **predict.ydotsMLE()** for
prediction, also an **lme4** wrapper suited for our context.  Since MLE
computation can be voluminous, our package offers a parallel version.

One important advantage of the MLE version is that it lends itself to
the case of binary Y, with the logistic model.

Covariates are allowed for both the MM and MLE versions, as well as for
other methods in **rectools**.  Let's try predicting student 1128 and
instructor 99 again, this time using the service and department
information.  

``` R
> mmout1 <- trainMM(ivl[,-(4:5)],userCovsStartCol=NULL,itemCovsStartCol=4)
```

Start building the test set for the prediction:

``` R
> testset <- ivl[28888,-(3:5)]
> testset
         s   d service dpt15 dpt5 dpt10 dpt12 dpt6 dpt7
28888 1178 220       0     0    0     0     1    0    0
      dpt4 dpt8 dpt9 dpt14 dpt1 dpt3 dpt11
28888    0    0    0     0    0    0     0
```

Now, which department does this instructor teach in?

``` R
> which(ivl$d == 99)
 [1]  3015  3604  6214  9347 13507 18235 22131 23514 25397
[10] 31402 46502 55132 57645 59201 66132 68640 69885 73205
> ivl[3015,]
       s  d y studage lectage service dpt15 dpt5 dpt10
3015 124 99 4       3       4       0     0    0     0
     dpt12 dpt6 dpt7 dpt4 dpt8 dpt9 dpt14 dpt1 dpt3 dpt11
3015     0    0    0    0    0    0     0    1    0     0
```

Ah, department 1.  Now go ahead with the prediction:

``` R
> mmout1 <- trainMM(ivl[,-(4:5)],userCovsStartCol=NULL,itemCovsStartCol=4) 
> testset
         s  d service dpt15 dpt5 dpt10 dpt12 dpt6 dpt7 dpt4
28888 1128 99       0     0    0     0     0    0    0    0
      dpt8 dpt9 dpt14 dpt1 dpt3 dpt11
28888    0    0     0    1    0     0
> predict(mmout1,testset)
[1] 3.350096
```



## Matrix factorization model:

Let A denote the matrix of ratings, with Y<sub>ij</sub> in row i, column
j.  Most of A is unknown, and we wish to predict the unknown values.
Nonnegative Matrix Factorization (NMF) does this as follows:

We find nonnegative matrices W and H, each of rank k, such that A is
approximately equal to the product WH.  Here k is a user-defined tuning
parameter, typically much smaller than the number of rows and columns of
A.  It is kept small to avoid overfitting but large enough to capture
most of the structure of the data.  Default value is k = 10.

Here we piggyback on the R package **recosystem**, adding convenient
wrappers and adding a parallel computation capability.  See the
functions **trainReco()**, **predictReco()** and so on.

## Cosine model:

The basic idea here is as follows.  The predict the rating user i would
give to item j, find some users who are similar to user i and who have
rated item j, and average their ratings of that item.  The CRAN package
**recommderlab** is one implementation of this idea, but our
**rectools** package uses its own implementation, including novel
enhancements.

One such enhancement is to do item category matching, an example of
categories being movie genres.  For each user, our code calculates the
proportion of items in each category rated by this user, and
incorporates this into the calculation of similarity between any pair of
users.  See the functions **cosDist()**, **formUserData()** and
**predictUsrData()**.

## Cross validation:

The MM, MLE, NMF and cosine methods all have wrappers to do
cross-validation, reporting the accuracy measures exact prediction; mean
absolute deviation; and l2.  In our experiments so far, MM seems to give
the best accuracy (and the greatest speed).

## Plotting:

Some plotting capability is provided, currently in the functions
**plot.ydotsMM()** and **plot.xvalb()**.  The former, for instance, can be
used to assess the normality and independence assumptions of the MLE
model, and to identify a possible need to consider separate analyses for
subpopulations.


# REFERENCES

K. Gao and A. Owen, *Efficient Moment Calculations for Variance
Components in Large Unbalanced Crossed Random Effects Models*, 2016.

M. Hahsler, **recommderlab**, CRAN vignette.

Y. Koren et al, Matrix Factorization Techniques for Recommender 
Systems, *IEEE Computer*, 2009.

N. Matloff, [Collaborative Filtering in Recommender Systems: 
a Short Introduction](http://heather.cs.ucdavis.edu/RSTutorial.pdf), 2016. 


P. Perry, *Fast Moment-Based Estimation for Hierarchical Models*, 2015.

