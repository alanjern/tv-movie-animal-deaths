# How often do animals die in popular movies and TV series?

This script starts with the top 5000 movies and TV series from IMDB ([Movie data source](https://www.kaggle.com/datasets/tiagoadrianunes/imdb-top-5000-movies); [TV series data source](https://www.kaggle.com/datasets/tiagoadrianunes/imdb-top-5000-tv-shows)). It then queries the [Does the Dog Die?](https://www.doesthedogdie.com/api) API to check whether a dog, a cat, or other animal dies in each one.

## Setup

This project uses [renv](https://rstudio.github.io/renv/) to manage R package dependencies. After cloning the repository, restore the exact package versions recorded in `renv.lock` by running the following in R from the project root:

```r
renv::restore()
```

To run this script, you must have an API key for Does the Dog Die?. You must specify this as an environment variable in R. In the script, this variable is named `DDD_API_KEY`.

In order to run the LLM labeling analysis, you must also have an Anthropic API key. You must specify this as an environment variable in R. In the script, this variable is named `ANTHROPIC_API_KEY`.

Both environment variables should be set in a `.Renviron` file in the project root (this file is gitignored and should never be committed). Add the following lines, then restart R for the changes to take effect:

```
DDD_API_KEY=your-doesthedogdie-api-key
ANTHROPIC_API_KEY=your-anthropic-api-key
```

You can open (or create) this file directly from R with `usethis::edit_r_environ(scope = "project")`.

## Reproducing this analysis

The main analysis script is `analyze_animal_deaths.qmd`. This also generates the figures for the paper.

The analysis of comments is performed by two scripts:
1. `analyze_comments_llm.R`: An LLM-based analysis that requires an Anthropic API key. I abandoned this approach for being too unreliable.
2. `analyze_comments_nrc.R`: A sentiment analysis that relies on the NRC word-emotion assocation lexicon (EmoLex). This is the main one I used in my analysis.

All scripts generate intermediate data files along the way in the [`data/`](./data) folder. See [`data/README.md`](./data/README.md) for a description of each data file.

This analysis was originally run on R version 4.5.2.

## Web app

[`docs/`](./docs) contains a small static web app for exploring the data interactively: filter by type (movie/TV), year, genre, and minimum IMDB votes; view death rates with confidence intervals overall, by genre, and over time; build side-by-side comparisons of different filter combinations; and browse the underlying titles.

It's plain HTML/CSS/JS (using [Chart.js](https://www.chartjs.org/) from a CDN) with no build step, so it can be hosted for free on GitHub Pages by pointing Pages at the `docs/` folder on the `main` branch (Settings → Pages → Source → Deploy from a branch → `main` / `docs`).

The app reads from `docs/data/dataset.json`, a joined/flattened copy of the files in `data/`. After re-running `analyze_animal_deaths.qmd` to refresh the underlying data, regenerate it with:

```
python3 scripts/build_dataset.py
```