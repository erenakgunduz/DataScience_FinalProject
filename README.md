# Data Science Final Project

## Data

[The data](https://github.com/D4RKONION/fatsfvframedatajson) is for the excellent [Frame Assistant Tool](https://fullmeter.com/fatonline/) (FAT) developed for Street Fighter &mdash; these data specifically for SFV attacks for all characters in JSON format.

## Outline

### Modules

1. Visualization of character attack data \[scatterplots of frame advantage on block (x) vs damage and stun (y)\], also have tile plots with all characters (rows) and maximum number of attacks (columns, but sorted) with colormap for plotting both frames oB and damage of attack, include Pearson correlation

![Tile plot example](https://www.royfrancis.com/assets/images/posts/2015/2015-11-01-a-guide-to-elegant-tiled-heatmaps-in-r/measles-mod3.png)
_Here's an example of the tile plot, but sorting by frame advantage instead of year and heatmap the damages_

2. Comparison of prediction accuracy of damage from frame data using simple linear regression with frames oB, LOESS/built-in R curve (don't care if overfit, just for illustration to compare non-parametric model), with plots for the first two, and elastic net regression with more frame data (S, A, R, oB) and also character total health points as features &mdash; let user select their tuning parameters then press Run button to train the model, will plot parameters which can be used to test and compare prediction with real data that user inputs, cross-validation button to test elastic net model with various $\lambda$ and $\alpha$ values, will have plots of validation accuracy & MSE loss and output the optimal parameters based on CV, user can compare with CV of simple LR
3. Comparison of classification accuracy for character archetype using parametric (logistic ridge regression with several features) and non-parametric algorithm (haven't decided yet, maybe KNN or something?), again will have cross-validation and train+test parameters option

### Questions and hypotheses

1. Does the frame advantage of an attack impact its damage output (or vice versa)? In other words, are they correlated? I anticipate a negative correlation, so when the frame advantage is better, the damage of the attack (if successful) will typically be lower.
2. How does elastic net regression compare to simple linear regression for predicting damage of an attack from its frame data? I expect the elastic net on optimal parameters to have a greater validation accuracy than simple LR.
3. How does logistic ridge regression compare to the non-parametric approach for accurately classifying characters to their correct archetype? I expect the logistic regression to be more accurate.
