# Boston, TTC (Top Trading Cycles) and AR (Applicant-Rejection) algorithms
This project provides functions that generate the matching allocation from the Boston, TTC, AR matching mechanisms.

The code also simulates a lower bound to the expected proportion of colleges that have incentives to reduce their desirability to at least one student in each of these mechanisms. The outputs of these simulations are available in [Manipulation of Attractiveness in Two-Sided Matching Markets](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4523268)



## Boston Mechanism

The function iaa( ) from file "Boston.R" outputs the matching allocation from the Boston mechanism, plus a lower bound to the proportion of colleges that could be made better off by rendering themselves unacceptable to some students.

### The iaa( ) function

#### Inputs

`s.prefs`: A matrix defining student preferences (each row corresponds to the preferences of each student).

`c.prefs`: A matrix defining colleges' preferences (each row corresponds to the preferences from each college).

`nSlots`: A vector of length ncol(c.prefs) which gives the number of vacancies from each college.

`k`: Indicates the maximum number of colleges that each student deems acceptable.

#### Outputs
The function returns the final matching allocation


#### Usage:
The following code
```r
#=======USAGE=====
s.prefs <- matrix(c(1,2,3,
                    1,2,3,
                    1,3,2,
                    2,1,3,
                    3,1,2),
                  byrow = FALSE, ncol = 5, nrow = 3)
c.prefs <- matrix(c(1,4,2,3,5,
                    5,2,3,4,1,
                    1,2,3,4,5),
                  byrow = FALSE, ncol = 3, nrow = 5)
nSlots <- c(2,1,1)
 iaa(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots,k=3)
```
returns some outputs, a few of which are:

``matchings``: the students that each college ends up matched with

``prop_benefits_misreport``: The proportion of colleges who have **unilateral** incentives to deviate from no manipulation by rendering themselves unacceptable to at least one student.

In this example, we get
```r 
$matchings

college student
  
1       1

1       2

2       4

3       5
```
meaning that college 1 is matched with students 1 and 2, college 2 is matched with student 4, and college 3 is matched with student 5 (student 3 stays unmatched).

For this study, the output of interest is the proportion of colleges that could be made better off by rendering themselves unacceptable to at least one student. 

In this example, all else being equal, college 2 has incentives to render itself unacceptable to Student 4: this way, Student 4 would not apply to College 2, which would cause College 2 to end up matched with Student 2. Similarly, assuming no other college reduces its desirability, College 3 would have incentives to render itself unacceptable to Student 5, which would cause this college to be matched with Student 3 (notice that we are only considering unilateral deviations). Therefore, in this example, two of the three colleges have incentives to render themselves unacceptable to at least one student, so the proportion of colleges that have incentives to render themselves unacceptable to at least one student is given by:
```r 
$prop_benefits_misreport

[1] 0.6666667
```

Notice that this algorithm only provides a **lower bound** to the expected number of colleges that would have incentives to render themselves unacceptable to at least one student. <!--Details are available in [Manipulation of Attractiveness in Two-Sided Matching Markets](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4523268).-->



## AR Mechanism

The AR mechanism is a convex combination between the Student Optimal Stable Match (SOSM) and the Boston mechanism. Essentially we implement the SOSM algorithm but using as inputs colleges *ex-post* preferences. These ex-post preferences are lexicographic, where the first classification criterion is how well a college is ranked by a student and, secondly, how highly the student is ranked by the college according to the college's original preferences.

The function iaa( ) from file "AR.R" outputs the matching allocation from one instance of the AR mechanism, plus a lower bound to the proportion of colleges that could be made better off by rendering themselves unacceptable to some students.

The instance from this code corresponds to the case in which colleges ex-post preferences are given by:

1. First admission criterion: each college `c` gives a higher priority to students who chose `c` as one of their top 2 choices

2. Second admission criterion: ties are resolved through the college's original preferences over students

The SOSM algorithm is then implemented using these ex post preferences from colleges as inputs. More details are available in [Manipulation of Attractiveness in Two-Sided Matching Markets](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4523268)

 


## TTC mechanism
The function top_trading_cycles( ) from file "TTC.R" outputs the matching allocation from the TTC mechanism, plus a lower bound to the proportion of colleges that could be made better off by rendering themselves unacceptable to some students.




## Technical details

Because the simulations are time-consuming, some of them rely on parallel computing.

To increase the speed of the simulations, reducing the number of colleges (the parameter `m`) and the sample size (the parameter `sample_size`) are recommended.

## Acknowledgments

The function `iaa()` used to implement the Boston and AR algorithms in this project is adapted from the matchingMarkets library developed by Thilo Klein [matchingMarkets](https://github.com/thiloklein/matchingMarkets). The following modifications and extensions were made:
<!--
1. Outputting the maximum utility that a college could have received from students who proposed to it but were rejected (e.g., due to full capacity) (because all of the school's available vacancies were already
filled in the current or previous rounds).

2. Outputting the minimum utility that a college gets from the students it ends up matched with
-->

1. Compute a lower bound to the proportion of colleges who could end up better off by rendering themselves unacceptable to at least one student. 

2. Introducing the parameter k, which indicates the number of colleges that each student finds acceptable

3. For the AR mechanism, introduced as an input colleges' ex-post preferences that take into account how well students rank the college (students who give a low priority to colleges are not as well ranked as those who give the college a higher priority), thus allowing us to implement the AR mechanism.
