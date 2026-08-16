#!/usr/bin/env python3
"""
Builds docs/data/dataset.json for the animal-deaths web app by joining the
DTDD-matched IMDB results with the DTDD vote-detail files produced by
analyze_animal_deaths.qmd.

Usage: python3 scripts/build_dataset.py
Reads from data/
Writes to docs/data/dataset.json

Re-run this after regenerating the data/*.csv files (e.g. after a fresh run
of analyze_animal_deaths.qmd) to refresh the web app's dataset.
"""
import csv
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = ROOT / "data"
OUT_PATH = ROOT / "docs" / "data" / "dataset.json"

csv.field_size_limit(sys.maxsize)


def na(v):
    if v is None:
        return None
    v = v.strip()
    if v == "" or v.upper() == "NA":
        return None
    return v


def to_int(v):
    v = na(v)
    return int(float(v)) if v is not None else None


def to_float(v):
    v = na(v)
    return float(v) if v is not None else None


def load_csv(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def build_details_index(rows):
    idx = {}
    for row in rows:
        idx[row["id"]] = row
    return idx


def death(yes, no):
    yes = yes or 0
    no = no or 0
    return yes > no


def make_record(result_row, detail_row, kind):
    dog_yes = to_int(detail_row["dog_yes_votes"]) or 0
    dog_no = to_int(detail_row["dog_no_votes"]) or 0
    cat_yes = to_int(detail_row["cat_yes_votes"]) or 0
    cat_no = to_int(detail_row["cat_no_votes"]) or 0
    animal_yes = to_int(detail_row["animal_yes_votes"]) or 0
    animal_no = to_int(detail_row["animal_no_votes"]) or 0

    genres_raw = na(result_row.get("genres"))
    genres = [g.strip() for g in genres_raw.split(",")] if genres_raw else []

    tconst = result_row["tconst"]

    return {
        "id": result_row["id"],
        "tconst": tconst,
        "title": result_row["primaryTitle"],
        "type": kind,
        "year": to_int(result_row.get("startYear")),
        "endYear": to_int(result_row.get("endYear")) if kind == "tv" else None,
        "rating": to_float(result_row.get("averageRating")),
        "numVotes": to_int(result_row.get("numVotes")),
        "runtime": to_int(result_row.get("runtimeMinutes")),
        "genres": genres,
        "imdbUrl": f"https://www.imdb.com/title/{tconst}/",
        "dog": {"yes": dog_yes, "no": dog_no, "death": death(dog_yes, dog_no)},
        "cat": {"yes": cat_yes, "no": cat_no, "death": death(cat_yes, cat_no)},
        "animal": {
            "yes": animal_yes,
            "no": animal_no,
            "death": death(animal_yes, animal_no),
        },
    }


def main():
    movie_results = load_csv(DATA_DIR / "imdb_5000_dtdd_results.csv")
    movie_details = build_details_index(
        load_csv(DATA_DIR / "imdb_5000_dtdd_movie_details.csv")
    )
    tv_results = load_csv(DATA_DIR / "imdb_tvseries_5000_dtdd_results.csv")
    tv_details = build_details_index(
        load_csv(DATA_DIR / "imdb_tvseries_5000_dtdd_series_details.csv")
    )

    records = []
    skipped = 0

    for row in movie_results:
        detail = movie_details.get(row["id"])
        if detail is None:
            skipped += 1
            continue
        records.append(make_record(row, detail, "movie"))

    for row in tv_results:
        detail = tv_details.get(row["id"])
        if detail is None:
            skipped += 1
            continue
        records.append(make_record(row, detail, "tv"))

    all_genres = sorted({g for r in records for g in r["genres"] if g})

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(OUT_PATH, "w", encoding="utf-8") as f:
        json.dump(
            {
                "generatedFrom": "https://github.com/alanjern/tv-movie-animal-deaths",
                "count": len(records),
                "genres": all_genres,
                "records": records,
            },
            f,
            separators=(",", ":"),
        )

    n_movies = sum(1 for r in records if r["type"] == "movie")
    n_tv = sum(1 for r in records if r["type"] == "tv")
    print(f"Wrote {len(records)} records ({n_movies} movies, {n_tv} tv) to {OUT_PATH}")
    print(f"Skipped {skipped} rows with no matching detail row")
    print(f"{len(all_genres)} distinct genres")


if __name__ == "__main__":
    main()
