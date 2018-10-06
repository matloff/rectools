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
student was in his/her third year (**studage** = 3), and had taken the
course 5 semesters earlier.

(For more information on the data, type '?InstEval')

### Random effects ANOVA model:

A simple statistical random effects latent factor model, often called a
*baseline* model in the RS literature, is

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
information, which are covariates of the items here.  

``` R
> mmout1 <- trainMM(ivl)
```

Start building the a set for the prediction.  We'll use student 1128 and
instructor 99 again, but now that we are using covariates, we'll need to
determine what they are for this student and this instructor.

We already saw that **studage**, the sole covariate for students, is 3
for student 1128.  Let's find a record involving instructor 99.

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

Ah, it's department 1, a non-service unit, and the instructor has "age"
4.  So, let's assemble an example test set.

``` R
> testset <- ivl[28888,-3]  # pretend we don't know y
> testset
         s   d studage lectage service dpt15 dpt5 dpt10
28888 1178 220       3       5       0     0    0     0
      dpt12 dpt6 dpt7 dpt4 dpt8 dpt9 dpt14 dpt1 dpt3 dpt11
28888     1    0    0    0    0    0     0    0    0     0
> testset$d <- 99
> testset$lectage <- 4
> testset$dpt12 <- 0
> testset$dpt1 <- 1

```

Now go ahead with the prediction:

``` R
> mmout1 <- trainMM(ivl) 
> predict(mmout1,testset)
[1] 3.498892
```


## Nearest-neighbors/similarity model:

The basic idea here is as follows.  To predict the rating user i would
give to item j, find some users who are similar to user i and who have
rated item j, and average their ratings of that item.  The CRAN package
**recommderlab** is one implementation of this idea, but our
**rectools** package uses its own implementation, including novel
enhancements.

One such enhancement is to do item category matching, an example of
categories being movie genres.  For each user, our code calculates the
proportion of items in each category rated by this user, and
incorporates this into the calculation of similarity between any pair of
users.  The functions used are **cosDist()**, **formUserData()** and
**predictUsrData()**.

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

Let A denote the matrix of ratings, with Y<sub>ij</sub> in row i, column
j.  Most of A is unknown, and we wish to predict the unknown values.
Nonnegative Matrix Factorization (NMF) does this as follows:

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
**"RecoS3"** whose components are W and the transpose of H.  The method 
**predict.RecoS3()** was called.


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

