Probability Distributions
================

- <a href="#background" id="toc-background">Background</a>
  - <a href="#discrete-distributions"
    id="toc-discrete-distributions">Discrete Distributions</a>

## Background

Recently, in the stats class that I’m taking, I’ve been exposed to
several foundational distributions. These distributions are of great
relevance to more advanced statistics concepts and so developing a
profound understanding of them seems like time well-spent.

In this notebook, I’ll take a look at the following distributions:

**Discrete**

1.  [Bernoulli](#bernoulli-distribution)
2.  [Binomial](#binomial-distribution)
3.  Hyper-geometric
4.  Geometric
5.  Poisson

**Continuous**

1.  Uniform
2.  Normal
3.  Exponential

------------------------------------------------------------------------

### Discrete Distributions

Discrete distributions are those in which the elements composing the
distribution take on a **countable** number of elements. For all of the
following distributions, I will simply be focusing on the *probability
density function*.

#### Bernoulli Distribution

The Bernoulli distribution is characterized by only having two outcomes.
Assuming the two outcomes are $\{0, 1\}$, we typically assign $1$ a
probability of $p$ and assigning $0$ a probability of $1-p$. An
important note here is that the two events are **independent** of one
another (this will come back up when we generalize the distribution).

Most often, the probability distribution is associated with the tossing
of a coin, where we have two outcomes: heads or tails. Using this
example, we can arrive at the general form of the Bernoulli
distribution. If you want to know the probability that you get one toss
a specific amount of times in a row - let’s say 10 heads in a row, you
multiply the probability for each toss (hence the independence statement
before) and you reach the following:

$$ f(x) = (1-p)^{x-1}p $$ So if we want to know the probability that we
get ten heads in a row for a fair coin (50-50 chance to get either
side), we get the following:

$$f(10) = (1-0.5)^{9}0.5 = 0.000976 \ \text {or} \ \frac{1}{2^{10}}$$
Ok, now that we’ve written out the function, let’s go onto some R-code…

But, let’s first make some imports:

``` r
library("Rlab")
library(tidyverse)
library(ggplot2)
library(tidytuesdayR) # library from which we'll retrieve the data
library(patchwork)
library(stringr)
library(reshape2)
library(scales)
# Let's also set a theme for our plots
theme_set(theme_minimal())
```

Now, let’s go ahead and plot out some coin tosses, which is
representative our Bernoulli distribution:

``` r
# To maintain reproducibility we'll set a seed:
set.seed(42)

# list the number of samples:
num_samples <- 10

# Create a dataframe for the bernoulli:
bernoulli <- data.frame(
  toss = 1:num_samples,
  heads = rbern(n = num_samples, prob = 0.5)
)

# Convert to factor to plot:
factor <- as.factor(bernoulli$outcome)

# Plot a bar graph of the number of heads:
ggplot(mapping = aes(x=bernoulli$heads)) + 
  geom_bar(
    color = "black",
    fill = "white"
  ) + 
  labs(
    title = "Is it a fair coin?",
    x = "Outcome",
    y = "Number of Successes"
  ) + 
  scale_x_discrete(limits=c(0, 1))
```

    ## Warning: Continuous limits supplied to discrete scale.
    ## ℹ Did you mean `limits = factor(...)` or `scale_*_continuous()`?

![](probability_distributions_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

What gives? Shouldn’t we have the same number of head and tails if we
set the probability to 0.5? The reason we don’t is because of the
inherent randomness of the trial (nevertheless, the two outcomes still
technically do arrive at a $p$ and $1-p$ as expected).

Let’s take a quick tangent to quickly describe the *Weak Law of Large
Number*. This law basically states that as we increase our number of
samples, we will get closer to the expected value of the population.

In our case, as we toss more coins, we should expect that we come to a
50-50 split. Let’s demonstrate that using by plotting:

``` r
# To maintain reproducibility we'll set a seed:
set.seed(42)

# Set the number of samples to 500
num_samples <- 500

# Create a new dataframe with a counter and probability
bernoulli_large <- data.frame(
  toss = 1:num_samples,
  heads = rbern(n = num_samples, prob = 0.5)
) %>%
  mutate(
    heads_sum = cumsum(heads),
    heads_prob = heads_sum/toss
  )

# Plot out the results:
ggplot(data = bernoulli_large) + 
  aes(x = toss, y = heads_prob) + 
  geom_line() + 
  geom_hline(yintercept = 0.5, color='red', linetype=2) + 
  annotate("text", x = 350, y = 0.52, color='red', label = "Expected Value: 0.5") + 
  labs(
    title = "Converging to the Expected Value",
    x = "Number of Tosses",
    y = "Cumulative Probability of Heads"
  )
```

![](probability_distributions_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

We can see that as we increase the number of tosses, we get closer and
closer to the expected value.

#### Binomial Distribution

The Binomial distribution is related closely to the Bernoulli
distribution. In fact, the Binomial distribution is really just a number
of independent Bernoulli trials. That said, we can write the
distribution fo the Binomial as follows:

$$ f(x) = \binom{n}{x}(1-p)^{1-x}p^x$$ where $n$ is the number of
trials, $x$ is the number of successes and $p$ is the probability of
success. Furthermore, the $\binom{n}{x}$ is the combination which is
calculated as:

$$ \binom{n}{x} = \frac{n!}{x!(n-x)!}$$