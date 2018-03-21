
# Mixed Models/lme4 Tutorial

One of the classes of methods offered by **rectools** is based on
Maximum Likelihood Estimation (MLE) in *mixed effects* models, using the
**lmer4** package.  What is the former, and how is the latter used?

## Our Basic Model

Let Y<sub>ij</sub> denote the rating of item j by user i.  The model is

E(Y<sub>ij</sub>) =  &mu; + &alpha;<sub>i</sub> + &beta;<sub>j</sub>

Here &alpha;<sub>i</sub> is the "effect" of user i, meaning the degree
of tendency for user i to give ratings that are higher or lower than the
average user.  Similarly, Here &beta;<sub>j</sub> is the degree of
tendency for item j to be rated higher or lower than the average item.
The quantity &mu; is the overall overage rating over all possible users
and all possible items.

There is a subtlety in that term "all possible."  The model treats the
users in our data as a sample from a (conceptual) population of all
possible users, and the same for the items.  (One drawback of this model
is that it cannot accmmodate new users or items.)

So, &alpha;<sub>i</sub> and &beta;<sub>j</sub> are considered random
variables.  We assume them to be independent and normally distributed,
with means 0 and variances &sigma;<sub>a</sub><sup>2</sup> and
&sigma;<sub>b</sub><sup>2</sup>.  The latter two quantities and &mu; are
then estimated via MLE.  The variances are known as *variance
components*.

We may also have a vector X of p covariates.  For simplicity, let's assume
here that these pertain to users, say the age and gender.  If we assume
a linear regression model for these, the above model becomes

E(Y<sub>ij</sub>) =  &mu; + &gamma;'X<sub>i</sub> + &alpha;<sub>i</sub> + &beta;<sub>j</sub>

where &gamma; is the vector of regression coefficients and ' indicates
matrix transpose. Now the MLE process will estimate &gamma; as well.

The above setting is referred to as having *crossed effects*, with
various levels of the &alpha; factor combined with various levels of the
&beta; factor.  Another major setting is that of *nested factors*, with
multiple levels of one factor for each level of the other.  For
instance, in an education study, we may be looking at several schools
within each of a number of school districts.  The nesting could be more
complex, say students within schools within districts within states.


## Use of the Model

In many statistical applications, estimation is the end result.  The
relative sizes of the variance components are the primary aspects of
interest.

In other applications, though, prediction is the main goal, and that is
the case for recommender systems.  Let A denote the matrix whose rows
are our users and columns are our items.  A is typically a very sparse
matrix; most users have not rated most items.  We would like to predict
the missing entries.  This is sometimes called the *matrix completion*
problem.

There is a mathematical theory for all this, e.g. for Best Linear
Unbiased Predictors (BLUP), not presented here.

## Estimation and prediction via lmer4

The **lmer4** package is extremely versatile, though extremely complex.
Fortunately, **rectools** uses only a small fraction of **lmer4**'s
capabilities.  Let's try it on a small artificial example.

This code will generate the data, using the second model above:

``` R
set.seed(99999)
m <- matrix(nrow=1000,ncol=4)
md <- as.data.frame(m)
colnames(md) <- c('x','u','v','y')
md$x <- rnorm(1000,1,1)
u <- 1:50
v <- 1:50
md$u <- sample(u,1000,replace=TRUE)
md$v <- sample(v,1000,replace=TRUE)
alpha <- rnorm(1000)[md$u]
beta <- 2*rnorm(1000)[md$v]
md$y <- 1 + 1.5*md$x + alpha + beta + rnorm(1000)
```

We are simulating 50 users rating 50 items, with 1000 user-item
pairings, thus 1000 ratings.  The A matrix is then 40% full.  Here &mu;
is 1 and &gamma; is 1.5; p = 1.

So, let's fit the model:

``` R
lmout <- lmer(y ~ x + (1|u) + (1|v),data=md)
```

At first this looks like an ordinary **lm()** call, predicting y from x,
u and v.  However, the notation is different, in the '(1|' expressions.

In R regression formulas, '1' means an intercept term, b in the equation

w = a q + b  

The notation here extends that, saying that there is a different
intercept for each value of u.  That gives us our &alpha;<sub>i</sub>.
The (1|v) term similarly gives us &beta;<sub>j</sub>.

The class of the output above, lmout, is of course 'lme4'.  There is a
corresponding method, predict.lme4(), with which we can now do
prediction.  Let's make a little test case.  We will take one of the
existing data points, and predict how this user would rate another item.

``` R
> testset <- z$md[976,]
> > testset$v <- 20
            x  u v        y
976 0.6757279 17 4 5.733739
> testset$v <- 20
> testset
            x  u  v        y
976 0.6757279 17 20 5.733739
> predict(lmout,testset)
     976 
4.783199 

