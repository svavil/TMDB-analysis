# TMDB-analysis
An R-based text analysis of a TMDB 5000 movie dataset

## Dataset
This is an analysis of [this Kaggle dataset](https://www.kaggle.com/tmdb/tmdb-movie-metadata).

## Usage
Run `tmdb_analysis.R`. The `summary_data` variable in the script will contain a list of significant variables. 

## Interpretation
This script produces a linear model that predicts film revenue, based on film budget, genre, production country and words used in the title and tagline. The most significant variable is, of course, the film budget, followed by a collection of lemmas used in taglines and titles.
