
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
possible users, and the same for the items.

So, &alpha;<sub>i</sub> and &beta;<sub>j</sub> are considered random
variables.  We assume them to be independent and normally distributed,
with means 0 and variances &sigma;<sub>a</sub><sup>2</sup> and
&sigma;<sub>b</sub><sup>2</sup>.  The latter two quantities and &mu; are
then estimated via MLE.

We may also have a vector X of covariates.  For simplicity, let's assume
here that these pertain to users, say the age and gender.  If we assume
a linear regression model for these, the above model becomes

E(Y<sub>ij</sub>) =  &mu; + &gamma;'X<sub>i</sub> + &alpha;<sub>i</sub> + &beta;<sub>j</sub>

where &gamma; is the vector of regression coefficients and ' indicates
matrix transpose.


## Use of the Model

In many statistical


