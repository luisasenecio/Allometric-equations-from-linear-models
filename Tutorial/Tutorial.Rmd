---
title: "**Introduction to producing allometric equations from linear models**"
subtitle: "Featuring light data wrangling, log-transformations, and error quantifications"
author: "Luisa-Marie Dickenmann"
date: "November 2022"
output: 
  html_document:
    css: "style.css"
---

# Tutorial aims

-   Apply and expand on the Coding Club tutorial "Transforming and Scaling Data"
-   Use model outputs to produce allometric equations on preferred variables

# Steps

1.  [Subset preferred variables with `tidyr`](#subset-preferred-variables-with-%60tidyr%60)
2.  [Modify the data to prepare it for the model](#investigate-the-distribution-of-the-variables)
3.  [Apply an appropriate model](#building-the-model)
4.  [Extract relevant coefficients from the model to produce allometric equations](#extracting-the-coefficients-for-an-allometric-model)
5.  [Quantify errors associated with the allometric equation](#error-quantification)

If you have ever created a model before, you remember the wealth of coefficients and significance values that it produces. Say your model is on two variables, but one of them was really difficult to measure and you only collected data from a few points of a possible range of values. Some of the coefficients from the model can be used to create create an allometric equation, which takes a variable that are easy to measure as an input to predict the variable that is difficult to measure.

However, similar to having a mean of a distribution that needs a standard deviation to be informative, the result from an allometric equation requires a measure of variation. How accurate is the value that it produces? To achieve that, we will compare our observed data with what the allometric equation would produce and produce a 'root mean square error'.

This tutorial works with a large data set on destructively sampled biomass structure of trees in Eurasia, which can be found here: <https://www.nature.com/articles/sdata201770#Sec3>, and is a continuation of the [Transforming and Scaling Data](https://ourcodingclub.github.io/tutorials/data-scaling/) tutorial.

If you'd like to familiarise yourself with R Studio or linear models first, I highly recommend the Coding Club's tutorials: [Getting Started with R and RStudio](https://ourcodingclub.github.io/tutorials/intro-to-r/) and [Intro to Model Design](https://ourcodingclub.github.io/tutorials/model-design/)

All the materials you need, including the data set, a full example script and the figures we will produce can be found in [this repository](https://github.com/luisasenecio/Allometric-equations-from-linear-models.git). Simply click on the [green]{.green} '\<\> Code' button, then on 'Download ZIP' and finally unzip the downloaded folder.

**Let's get started then!**

## 1. Subset preferred variables with `tidyr`

First, we need to include a couple of libraries that will help us load and manipulate the data.

```{r, echo=FALSE, eval=FALSE}
library(tidyverse)
library(readxl)
library(skimr)
```

If you have created a new R Project for this tutorial and your data sets are in the same folder then you don't need to set your working directory as R already knows where to look for it.

If you simply made a new R Script file, then please set your working directory to where your files are stored with `setwd("file_path")`\`.

We can now have a look at what this data set has to offer.

```{r, echo=FALSE, eval=FALSE}
# Loading the data
  # If you are using an Excel file, specify the exact sheet name in 'sheets'
raw <- read.csv("Data/Biomass_trees.csv")

# Filtering for desired species and variables and saving the new selection in a new dataframe
  # NAs are also removed 
trees <- raw %>% 
  filter(Species=="Pinus sylvestris L.") %>% 
  drop_na(Ptot, DBH) %>% 
  select(Species, DBH, Ptot)

```

In the above code chunk, we are selecting the Scots pine as our species to focus on. This tutorial is looking at the relationship between total live biomass (Ptot), which is quite difficult to measure in the field without digging up roots and cutting up branches, and diameter at breast height (DBH), which is an easily accessible variable. Feel free to chose whatever species and variables, but keep in mind that the range of values for other observations might be different and require modifications that are not part of this tutorial.

# 2. Investigate the distribution of the variables {#investigate-the-distribution-of-the-variables}

Every model has its own requirements about the data. A general linear model for example would like the data to be normally distributed, meaning a bell-shaped distribution where intermediate values are most frequent, and observations trail off towards the extremes of the range. The model also assumes that the relationship between the variables is actually linear. If we think about our variables logically, trees with larger diameters should also have a greater total biomass, so it's not unrealistic to assume a linear relationship between DBH and total biomass.

However, as for the normal distribution we might run into a few problems. There are many small trees around, but not an equal number of medium or very large trees because not every sapling survives to become mature.

```{R, echo=FALSE, eval=FALSE}
# Plot the distribution of the explanatory variable
(distribution_DBH <- ggplot(trees, aes(x=DBH)) +
   geom_histogram(fill="#006400"))
```

```{R, echo=FALSE}
knitr::include_graphics("//Users/mano/Documents/Uni/Third Year/Data Science/Challenge 4/tutorial-luisasenecio/Outputs/DBH distribution.png")
```

We can see that the response variable (DBH) is definitely not normally distributed but skewed to the right (I know, it sounds a bit counter-intuitive, but just imagine a shovel coming from the right and pushing our values the other way).

```{R, echo=FALSE, eval=FALSE}
# Plot the distribution of the response variable
(distribution_Ptot <- ggplot(trees, aes(x=Ptot)) +
   geom_histogram(fill="#006400"))
```

```{R, echo=FALSE}
knitr::include_graphics("/Users/mano/Documents/Uni/Third Year/Data Science/Challenge 4/tutorial-luisasenecio/Outputs/Ptot distribution.png")
```

The distribution of the response variable (total live biomass) is even more right-skewed.

These distributions would not work with a linear model without tweaking them a bit beforehand. By taking the log10 of each of our values our distributions will become normalised.

```{r, echo=FALSE, eval=FALSE}
# Log10 transforming both variables
trees <- trees %>% 
  mutate(logPtot = log10(Ptot)) %>% 
  mutate(logDBH = log10(DBH))
```

**But be careful here since log10 transformations only work with positive data!** Neither DBH or total live biomass can be below 0, so this is no problem for us here.

If you find when you're plotting your distributions that the data is not that extremely right-skewed, you can try applying a weaker square root transformation.

We can now have a look if our transformations have worked by plotting another set of histograms but this time with the transformed data.

```{r, echo=FALSE}
knitr::include_graphics("/Users/mano/Documents/Uni/Third Year/Data Science/Challenge 4/tutorial-luisasenecio/Outputs/log10 DBH distribution.png")
```

```{r, echo=FALSE}
knitr::include_graphics("/Users/mano/Documents/Uni/Third Year/Data Science/Challenge 4/tutorial-luisasenecio/Outputs/log10 Ptot distribution.png")
```

This looks much better than before and we can see how the assumptions for a linear model are now met by visualising the relationship between the log-transformed variables.

```{r, echo=FALSE, eval=FALSE}
# Visualising the relationship
(DBH_biomass <- ggplot(trees, aes(x = logDBH, y = logPtot)) +
    geom_point(size=2, colour="#528B8B")) +
  geom_smooth(method="", colour="#8F8F8F") +
  theme_bw() + 
  xlab("DBH (cm)") +
  ylab("Total aboveground biomass (kg)")  +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        panel.grid=element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.5), units = , "cm"),
        axis.title.x=element_text(margin = margin(t = 10)),
        axis.title.y=element_text(margin = margin(r = 10)),
        axis.title=element_text(size=14)
  ) +
  xlim(0,2) +
  ylim(0,4)
```

Which produces this:

```{r, echo=FALSE}
knitr::include_graphics("/Users/mano/Documents/Uni/Third Year/Data Science/Challenge 4/tutorial-luisasenecio/Outputs/Relationship log DBH and Ptot.png")
```

We can now build a linear model!

# 3. Building the model {#building-the-model}

```{r, echo=FALSE, eval=FALSE}
# Fit model using the log10 transformed data
DBH_biomass_mod_transformed <- lm(logPtot~logDBH, data=trees)
# Extract coefficients 
summary(DBH_biomass_mod_transformed)
```

The coefficients that we need are in the red box in the summary output below.

```{r, echo=FALSE}
knitr::include_graphics("/Users/mano/Documents/Uni/Third Year/Data Science/Challenge 4/tutorial-luisasenecio/Outputs/log10 model output.png")
```

The green box shows the p value and it seems like the relationship between explanatory (DBH) and response (total biomass) variables are highly significant with a p value of less than 0.05 and a small standard error!

Before we proceed, we should make sure that our model actually fulfills the assumptions of normality.

```{r, echo=FALSE, eval=FALSE}
qqnorm(DBH_biomass_mod_transformed$residuals, main = 'Q-Q Plot Transformed Data')
qqline(DBH_biomass_mod_transformed$residuals)
```

```{r, echo=FALSE}
knitr::include_graphics("/Users/mano/Documents/Uni/Third Year/Data Science/Challenge 4/tutorial-luisasenecio/Outputs/Q-Q plot log10 model.png")
```

This is looking good since the data (open circles) largely follows the straight black line.

However, our model outputs are in units of log10 transformed data. To actually be able to predict anything with our model, we need to back-transform our data.

```{r, echo=FALSE, eval=FALSE}
# Back-transform data
trees <- trees %>% 
  mutate(reversePtot = 10^log(Ptot)) %>% 
  mutate(reverseDBH = 10^log(DBH))
```

If you compare the original data with the back-transformed data, they should match relatively closely.

# 4. Extracting the coefficients for an allometric model {#extracting-the-coefficients-for-an-allometric-model}

As mentioned above, we need the coefficients from the red box. The estimate for `intercept`\` is indeed the intercept, which is technically the value for total live biomass when DBH = 0. However, because we have log10 transformed the data, the intercept is now negative and carries little applicable meaning. It is however very useful to build the equation of a straight line.

The second coefficient we need is the estimate for `logDBH`\`.

If we combine them together into an equation for a straight line, we get:

\_Total live biomass = 2.52 \* DBH + (-1.13)\_

Which is the allometric equation to calculate total live biomass from DBH measurements.

# 5. Error quantification {#error-quantification}

The equation on its own is a good start, but we need to be able to give an indication of the accuracy of the resulting total biomass.

To do this, we calculate the root means square error (RMSE) by comparing the actual measured data with what our model would predict the response variable to be.

```{r, echo=FALSE, eval=FALSE}
# Predict total live biomass with allometric equation from model outputs
RMSE_calculations <- trees %>% 
    select(logPtot, logDBH) %>%
    mutate(intercept=-1.13, slope=2.52, predicted_biomass=slope*logDBH+intercept,
           difference=predicted_biomass-logPtot, reverse_difference=10^log(difference))
```

The difference is calculated by subtracting the predicted biomass from the observed, but log10 transformed biomass. This produces a measure of error for each tree, which can be summed together to produce an RMSE for the entire allometric equation.

```{r, echo=FALSE, eval=FALSE}
# RMSE = the square root of the sum of all differences divided by the number of observations
RMSE <- (sqrt(sum(RMSE_calculations$difference))/length(RMSE_calculations$difference))
```

And this gives us 0.00194, which means that when predicting total live biomass with the log10 transformed data and the allometric equation, we should expect the result to be off by 0.00194 (log10 transformed kg).

This is not very helpful, but wen can back-transform the error as well to be able to indicate RMSE in the original units. This gives 5.71\*10^-7^ kg which is indeed a very small error and makes predictions from our allometric equation very accurate.

**Careful!** This equation and its associated RMSE is only accurate for the range of observed values it was created from. But because our DBH measurements ranged from 0.3 to 56 cm and total live biomass ranged from 0.005 to 1'420.5 kg, a lot of different tree sizes are covered by it.
