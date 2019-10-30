
## IQR (45 points)

The interquartile range (IQR) is the 75th percentile minus the 25th
percentile and is a measure of spread.

1.  Create a function called `iqr` that takes a single numeric vector,
    `x`, as an argument and returns the interquartile range of `x`. You
    should **not** use the built in `IQR()` function in R (10 points).
2.  Use your function defined above to calculate the IQR for the data
    provided in `iqr.R` (5 points).
3.  Use bootstrap sampling to build a 95% confidence interval around the
    IQR value you calculated in step 2 (15 points).
4.  Plot a histogram of your bootstrap sample IQRs. Add red vertical
    lines that represent the lower and upper bounds of the confidence
    interval (10 points).
5.  If someone said that the IQR of the population from which this data
    was sampled is 0.25, what would you say about this claim (5 points)?
    (Answer in comments if you’re using an R script, or answer in plain
    text if you are using Rmarkdown)

## Alabama (75 points)

Alabama has been a college football dynsty for the past decade. Over
that time frame, the program has built a legacy of strong defense that
has led to multiple national championships. Now, this year, Alabama is
experiencing an offensive resurgence led by sophomore quarterback Tua
Tagovailoa. Tua’s rise to the top of the nation’s premier program hasn’t
been without controversy. At the start of this season Tua replaced Jalen
Hurts as Alabama’s starting quarterback. Jalen has been an excellent
quarterback for the Crimson Tide and owns a 26-2 record as a starter. As
a true freshman Jalen was named the SEC Offensive Player of the year.

As this current season continues, both quarterbacks remain on the
roster, and both have seen meaningful playing time. The question we want
to answer is this: *does starting Tua make Alabama a better team than
starting Jalen?* Now, the notion of what makes a “better” team is
certainly one that can be debated. In this scenario, we’re going to use
Yards Per Point (YPP) Differential, as introduced at the [MIT Sloan
Sports Analytics Conference
in 2013](http://www.sloansportsconference.com/wp-content/uploads/2013/Slides/EOS/Identifying%20an%20accurate%20metric%20for%20football%20efficiency.pdf).

Yards Per Point Differential is calculated based on Yards Per Point
Defense minus Yards Per Point Offense. Yards Per Point Defense is the
total yards allowed by a team divided by the total points scored against
a team. Yards Per Point Offense is the total number of yards gained by a
team divided by the total number of points a team scores. This metric
provides a measure of team efficiency - a high Yards Per Point
Differential indicates that a team was, on average, more efficient than
its opponents.

The `alabama.csv` file contained in your repository contains the
following columns:

  - `date`: date of the game
  - `opponent`: team Alabama played against
  - `starter`: name of Alabama’s starting quarterback for the game
  - `yards`: number of yards gained by Alabama
  - `yards_against`: number of yards gained by the other team
  - `points`: number of points scored by Alabama
  - `points_against`: number of points scored by the other team

Given this dataset, the following can be used to calculate Yards Per
Point Differential:

\[
YPP_{diff} = YPP_{defense} - YPP_{offense}\\
YPP_{defense} = \frac{\sum \text{yards_against}}{\sum \text{points_against}}\\
YPP_{offense} = \frac{\sum \text{yards}}{\sum \text{points}}
\]

With this setup, we have the following hypotheses:

\[
H_0 = \text{Tua }YPP_{diff} = \text{Jalen } YPP_{diff}\\
H_a = \text{Tua }YPP_{diff} \gt \text{Jalen } YPP_{diff}
\]

1.  Read the `alabama.csv` file in your repository into R (10 points).
2.  Calculate the observed test statistic. That is, Tua’s Yards Per
    Point Differential minus Jalen’s Yards Per Point Differential. The
    starter for each game is identified in the `starter` column (15
    points).
3.  Conduct a permutation test of 10,000 permutations to build a
    distribution of this test statistic under the null hypothesis (20
    points).
4.  Create a histogram of your results from step 3. Include a vertical
    red line indicating the original observed test statistic calculated
    in step 1 (10 points).
5.  Using the values derived in step 3, calculate the p-value associated
    with the original observed test statistic calculated in step 1 (10
    points).
6.  Build a 95% confidence interval around this p-value (5 points).
7.  Based on the results of your permutation test, what do you conclude
    (5 points)? (Answer in comments if you’re using an R script, or
    answer in plain text if you are using Rmarkdown)a
