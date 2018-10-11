# rectools

## Advanced Package for Recommender Systems

A featured-filled package of tools from the recommender systems
(RS) world, plus some new methods.

## Recommender Systems

In RS, we have data in which some "users" rate some "items,"
and we wish to predict how users would rate other items.  Example
applications are:

* Moviegoers rate films (the "Hello World" of RS).

* Link prediction in graphs (where will the next new edge be created?).

* Will a certain user have a bad reaction to a certain prescription
  drug?

* How well will sports team A do against team B?

Here we will focus on *collaborative systems,* in which the user and
item data are combined to produce predicted ratings.
 
## Package Features

* Incorporate user and item covariate information, including item
  category preferences.

* New methods, and novel variations on common models. 

* Parallel computation.

* Plotting.

* Focus group finder.

## Example: InstEval data

Let's start with a concrete example, the **InstEval** data that is
bundled with the **lmer4** package, which is included with **rectools**.
The data consist of student valuations of instructors at the famous
Swiss university ZTH.  There are 73421 ratings submitted by 2972
students of 1128 instructors.  Ratings are on a scale of 1 to 5.

Not surprisingly, the ratings matrix is mostly unknown; most students
did not rate most instructors.  The goal is to "complete" the matrix,
i.e.  predict how well all the students would like all the instructors.

For convenience, **rectools** includes a function that loads this data,
processes it (e.g. creating dummy variables for the school's
departments), and assigning the result to **ivl**:

``` R
> getInstEval()
> ivl[28888,]  # look at some example record
         s   d y studage lectage service dpt15 dpt5 dpt10
28888 1178 220 5       3       5       0     0    0     0
      dpt12 dpt6 dpt7 dpt4 dpt8 dpt9 dpt14 dpt1 dpt3 dpt11
28888     1    0    0    0    0    0     0    0    0     0
```

The data frame **ivl** follows the standard input format in the RS
world: user ID, item ID, rating, covariates.

In that record 28888, for instance, Student 1178 gave a rating of 5 to
Instructor 220.  The class was not a service class (i.e. not for
nonmajors) in Department 12 (**dpt15** etc. are dummy variables.) The
student was in his/her third semester (**studage** = 3), and had taken the
course 5 semesters earlier.

(For more information on the data, type '?InstEval')


## Nearest-neighbors/similarity model:

The basic idea here is as follows.  To predict the rating user i would
give to item j, find some users who are similar to user i and who have
rated item j, and average their ratings of that item.  The CRAN package
**recommderlab** is one implementation of this idea, but our
**rectools** package uses its own implementation, including novel
enhancements.

The functions used are **formUserData()**, to initialize the database, and
**predict.usrData()**, that does the actual prediction.  The latter
function calls **cosDist()** to compute distances/similarities.

To keep our example below simple, we won't use the covariates, just the
student and instructor IDs, and the ratings.  Let's use student 28 in
our example, and predict how well the user might like instructor 122.

``` r
> ud <- formUserData(ivl[,1:3]) 
> u28 <- ud[[28]]
> u28$itms
[1]   17   65  269  750 1002 1589 2097
> predict(ud,u28,122,10)  # would User 28 like Item 122?
[1] 4.3
```

Here's what happened above.  First, the R list **ud** will have
one element per user, with components **itms** and
**ratings** showing what items this user has rated and
what ratings he/she gave.  

The call to **predict()** is a little more complicated.  It is what is
called a *generic* function in R.  When R sees this call, it will ask,
"What is the class of **ud**?"  Upon learning that **ud** is an object
of class **usrData**.  R then *dispatched* the call to the method
**predict.usrData()** function then found the 10 closest users in our
dataset who had rated item 122, and averaged their ratings for that
item, yielding 4.3.

## Matrix factorization model:

Let A denote the matrix of ratings, with Y<sub>ij</sub>, the rating user
i has for item j, in row i, column j.  Most of A is unknown, and we wish to 
predict the unknown values.  Nonnegative Matrix Factorization (NMF) 
does this as follows:

We find fully known nonnegative matrices W and H, each of rank k, such
that A is approximately equal to the product WH.  Here k is a
user-defined tuning parameter, typically much smaller than the number of
rows and columns of A.  It is kept small to avoid overfitting but large
enough to capture most of the structure of the data.  Default value is k
= 10.

Here we piggyback on the R package **recosystem**, adding convenient
wrappers and adding a parallel computation capability.  The
functions involved are **trainReco()**, **predictReco()** and so on.

Once again, let's try this on the InstEval data:

``` r

> tr <- trainReco(ivl[,1:3])
> names(tr)
[1] "P" "Q"
> dim(tr$P)  # W
[1] 2972   10
> dim(tr$Q)  # H'
[1] 2160   10
> class(tr)
[1] "RecoS3"
> testSet <- data.frame(s=28,d=122)
> predict(tr,testSet)
[1] 5.310598

```

Here **trainReco()** did the factorization, returning an object of class
**"RecoS3"** whose components are W and the transpose of H.  As our example 
for prediction, we again took student number 28 and instructor number 122.  
Since **ud** is of class **"RecoS3"**, the R interpreter dispatched our 
**predict()** call to **predict.RecoS3**.

Note that before we could do any prediction, we needed to *train* our
model, a machine learning term meaning to fit the model to our dataset.
Here the fitting consisted of finding the matrices W and H, and from now
on, we can use them to make as many predictions as we want.

### Random effects ANOVA model:

A simple statistical random effects latent factor model, often called a
*baseline* model in the RS literature, is

E(Y) =  &mu; + &alpha;<sub>i</sub> + &beta;<sub>j</sub>

where Y<sub>ij</sub> is the rating, with &alpha;<sub>i</sub> and
&beta;<sub>j</sub> being specific latent (i.e. hidden) effects 
for user i and item j,
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

Let's see how student 1178 would like instructor 99.  For now, let's not
include the covariates.

``` R
> mmout <- trainMM(ivl[,1:3])  # exclude covariates
> predict(mmout,data.frame(s=1178,d=99))
[1] 3.676795
```

We fit our model by calling **trainMM()**, then did our prediction.

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
other methods in **rectools**.  Let's try predicting student 1178 and
instructor 99 again, this time using covariates.  

``` R
> mmout1 <- trainMM(ivl)
```

We now will start building the a test set for the prediction.  We'll use
student 1178 and instructor 99 again, adding the conditions that the
course in question is not a service course, and was taken in the
student's 6th semester, which was 2 semesters earlier than now.

Now that we are using covariates, we'll need to determine what they are
for this student and this instructor.  We already saw above that
**studage** is 3 for student 1178.  We also need to find the department
of instructor 99, so let's find a record involving that instructor.

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

Ah, it's department 1.   We'll now create the test set, a one-row data
frame, as in the MM example above, but instead of typing in all those
department variables by hand, we'll take one of the rows and modify it.

``` R
> testset <- ivl[28888,-3]  # no 'y' in our test set
> testset
         s   d studage lectage service dpt15 dpt5 dpt10
28888 1178 220       3       5       0     0    0     0
      dpt12 dpt6 dpt7 dpt4 dpt8 dpt9 dpt14 dpt1 dpt3 dpt11
28888     1    0    0    0    0    0     0    0    0     0
> testset$d <- 99
> testset$dpt12 <- 0
> testset$dpt1 <- 1
> testset$studage <- 6
> testset$lectage <- 2
> testset  # check it
         s  d studage lectage service dpt15 dpt5 dpt10
28888 1178 99       6       2       0     0    0     0
      dpt12 dpt6 dpt7 dpt4 dpt8 dpt9 dpt14 dpt1 dpt3 dpt11
28888     0    0    0    0    0    0     0    1    0     0

```

Now go ahead with the prediction:

``` R
> mmout1 <- trainMM(ivl) 
> predict(mmout1,testset)
[1] 3.727755
```


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


# FURTHER INFORMATION ON RECOMMENDER SYSTEMS

C. Aggarwal, *Recommender Systems: the Textbook*, Springer, 2016.

K. Gao and A. Owen, *Efficient Moment Calculations for Variance
Components in Large Unbalanced Crossed Random Effects Models*, 2016.

M. Hahsler, **recommderlab**, CRAN vignette.

D. Jannach *et al*, *Recommender Systems: an Introduction*, Cambridge
University Press, 2010.

Y. Koren et al, Matrix Factorization Techniques for Recommender 
Systems, *IEEE Computer*, 2009.

N. Matloff, [Collaborative Filtering in Recommender Systems: 
a Short Introduction](http://heather.cs.ucdavis.edu/RSTutorial.pdf), 2016. 

N. Matloff, [A Tour of Recommender Systems](http://heather.cs.ucdavis.edu/~matloff/189G/PLN/recsysCourse/LaTeX/RSbook.pdf), 2018 (in progress). 


P. Perry, *Fast Moment-Based Estimation for Hierarchical Models*, 2015.

