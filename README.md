# How often do animals die in popular movies and TV series?

This script starts with the top 5000 movies and TV series from IMDB ([Movie data source](https://www.kaggle.com/datasets/tiagoadrianunes/imdb-top-5000-movies); [TV series data source](https://www.kaggle.com/datasets/tiagoadrianunes/imdb-top-5000-tv-shows)). It then queries the [Does the Dog Die?](https://www.doesthedogdie.com/api) API to check whether a dog, a cat, or other animal dies in each one.

To run this script, you must have an API key for Does the Dog Die?. You must specify this as an environment variable in R. In the script, this variable is named `DDD_API_KEY`.
