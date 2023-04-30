# Analysis of Street Fighter V Attacks App

## Purpose

For this project, the purpose was to have some fun breaking down the SFV data for all the moves of every character in the game with a three-phase analysis:

- To first better understand exactly what data we have and hopefully gain some insights by making comparisons across characters and variables, which we can do in the first tab: **Information**.
- Next, to see how the data and relationships within it "look" through the second tab: **Visualization**.
- And ultimately, to put statistical regression models and machine learning techniques to use in the **Prediction** tab. This way, we can make predictions about a portion of our data (20%) our models don’t train on and only see at the last step (testing), see how "on point" these predictions are and play around with different parameter values to see how they affect the performance of the more complex model (elastic net) on the test data, and compare the performances of the two models.

![Information module]("https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/www/img/screenshots/information.png")
![Visualization module]("https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/www/img/screenshots/viz.png")
![Prediction module]("https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/www/img/screenshots/pred.png")

## Abstract

The user should be able to see and freely tinker with the three aforementioned tabs as the "modules" of the app. The full data frame has about 1,800 moves (our observations) and 20 columns --- of these, two are tags so we know which move is what, 16 are features that our models will fit/train on, and two are responses (the Damage and Stun inflicted on opponent). Of the features, six are categorical and particular to each move, and ten are numerical stats for the characters performing the moves. Most of it is self-explanatory once you have a look, but I’d like to point out that plnCmd is just the [motion input](https://www.youtube.com/watch?v=1qEguZXyWjw) the player has to perform for the attack to execute --- many share these, so I was able to numerically encode them with each belonging to one of 545 unique inputs, as you will see in the app.

## Dataset

I would also like to give a massive shoutout to the maintainers and developers of the [FAT (Frame Assistant Tool)](https://github.com/D4RKONION/FAT). This is a fully open source [web](https://fullmeter.com/fatonline/) and [mobile](https://fullmeter.com/fat/) app that can be invaluable for Street Fighter players looking to examine and learn the data of the game --- chiefly the frame data, with the most relevant column frames on block highlighted and color-coded, to help decide on a good strategy. Credit goes to them for [the original dataset](https://github.com/D4RKONION/fatsfvframedatajson) in JSON format, which I spent some time wrangling and cleaning so that I could use it for the purposes of my project. Cheers!

## Try it yourself

If you're looking to run and experience the app yourself, then not to worry: I'll explain exactly how. I would recommend simply [visiting the page](https://erenakgunduz.shinyapps.io/sfv_attacks_analysis/) for it deployed on shinyapps.io --- the link is also provided in the About section of this repository. I’d advise to be patient when it’s starting up, because it has a lot of packages and libraries it needs to load in every time an instance spins up again when a user visits the site. I hope all of you enjoy! To be clear, I'm also absolutely open to all feedback, comments, and improvements!

### Note for R users

While you could certainly also run the app on your local device with this command in the console:

```r
shiny::runGitHub("SFV_Attacks_Analysis", "erenakgunduz")
```

I personally wouldn’t recommend this method, as besides just installing R packages, it will also create a Python virtual environment for you (assuming you have Python installed too) almost exactly as it exists on my computer I used to bring this to the people and on the cloud. This means it will install all the necessary packages such as [scikit-learn](https://scikit-learn.org/stable/), which you may not want installed just to be able to run this app, or that you may already have installed, in which case you may want to take matters in your own hands to not need to re-install what you installed prior. Regardless, running it locally is more involved than simply visiting the site, so that decision is up for you to make :)

## Description

Users have many options, with the ability to choose the character (or all characters) and variable they wish to examine more closely in the first two modules. The first module provides various summary tables and key descriptive statistics to help the user familiarize themselves with the data. For the Information tab, this, as well as getting a sense of how values are distributed in terms of frequency/probability of occurrence for three highly important variables (frames on block, and two responses) using kernel density estimates (KDEs). For any user, you can think of this as just like a histogram, only as one smooth and curvy line instead of split into a certain number of bars based on splitting the values into even intervals called the "bins."

The Visualization tab mainly aims to see if, and to what extent, frames on block seem to be correlated with the two responses in question. We achieve this in three respects:

1. By rendering a **tile map**, where for truly and reliably correlated variables we would consistently expect to see values of a certain (deeper or lighter) color for frames on block concentrated mainly on one side of the plot (left or right) --- the x-axis being response values --- and then gradually tapering off to the other colors binned for lower or higher values.
2. **Pearson correlation test**. Essentially, the sign tells us if it calculated a negative or positive correlation, and the p-value gives us a major tool to help us decide if the result is significant and if it would thus be better to rule out the idea that the test doesn’t _really_ explain anything, not allowing us to make a respectable inference. In our case, when p-values lower than at least 0.05 occur, we get results that can give us a stronger inference that the variables are, in fact, correlated.
3. **Simple linear regression**. When fitting a predictive line to a scatter plot of the data points like this, a line with a high degree of slant/slope looks to illustrate a potential positively or negatively correlated relationship, whereas if it’s close to a horizontal (zero slope) line it’s obviously hard to say that they’re related at all. [ggplot2](https://ggplot2.tidyverse.org) includes the confidence intervals by default, which essentially shows where the point might actually reside if not on the line, within a certain degree of confidence. Really thick confidence intervals seem to any user like there is more unsureness about where the predicted point should actually be.

These two modules are then followed by the Prediction module, with a set of interactive elements like checkboxes for controlling the randomness of shuffling for splits by setting a seed (resulting in consistent shuffles and splits if checked), buttons to perform training/fitting of the model, then to perform five-fold cross-validation to determine exactly how features could potentially be selected more advantageously for fitting thus far unseen test data by looking the plot of errors, and finally to test the performance on the testing set --- which, as mentioned, consists of 20% of the original full set of observations (moves) in the data. Un-centering the response vectors for the elastic net also makes it possible for the user to compare tables for both models side-by-side containing the names of the attacks in the test dataset, the predicted values ($\hat{Y}$), and the actual values (Y, the truth).

## Gallery

![Visualization for stun]("https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/www/img/screenshots/viz_stun.png")
![Scatter plot and Pearson correlation]("https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/www/img/screenshots/sp.png")
![Scores output]("https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/www/img/screenshots/scores.png")
![Sliders and tables in Prediction module]("https://raw.githubusercontent.com/erenakgunduz/SFV_Attacks_Analysis/main/www/img/screenshots/sliders_tables.png")
