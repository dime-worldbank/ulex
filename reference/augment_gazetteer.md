# Augments Landmark Gazetteer

Augments Landmark Gazetteer

## Usage

``` r
augment_gazetteer(
  landmarks,
  landmarks.name_var = "name",
  landmarks.type_var = "type",
  grams.min_words = 3,
  grams.max_words = 6,
  grams.skip_gram_first_last_word_match = TRUE,
  grams.add_only_if_name_new = FALSE,
  grams.add_only_if_specific = FALSE,
  types_rm = c("route", "road", "toilet", "political", "locality", "neighborhood",
    "area", "section of populated place"),
  types_rm.except_with_type = c("flyover", "round about", "roundabout"),
  types_rm.except_with_name = c("flyover", "round about", "roundabout"),
  parallel.sep_slash = TRUE,
  parallel.rm_begin = c(tm::stopwords("en"), c("near", "at", "the", "towards", "near")),
  parallel.rm_end = c("bar", "shops", "restaurant", "sports bar", "hotel", "bus station"),
  parallel.word_diff = "default",
  parallel.word_diff_iftype = list(list(words = c("stage", "bus stop", "bus station"),
    type = "transit_station")),
  parallel.rm_begin_iftype = NULL,
  parallel.rm_end_iftype = list(list(words = c("stage", "bus stop", "bus station"), type
    = "transit_station")),
  parallel.word_begin_addtype = NULL,
  parallel.word_end_addtype = list(list(words = c("stage", "bus stop", "bus station"),
    type = "stage")),
  parallel.add_only_if_name_new = FALSE,
  parallel.add_only_if_specific = FALSE,
  rm.contains = c("road", "rd"),
  rm.name_begin = c(tm::stopwords("en"), c("near", "at", "the", "towards", "near")),
  rm.name_end = c("highway", "road", "rd", "way", "ave", "avenue", "street", "st"),
  pos_rm.all = c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "INTJ", "NUM", "PRON", "SCONJ",
    "VERB", "X"),
  pos_rm.except_type = list(pos = c("NOUN", "PROPN"), type = c("bus", "restaurant",
    "bank"), name = ""),
  close_thresh_km = 1,
  quiet = TRUE
)
```

## Arguments

- landmarks:

  `sf` spatial points data.frame of landmarks.

- landmarks.name_var:

  Name of variable indicating name of landmark. (Default: `"name"`).

- landmarks.type_var:

  Name of variable indicating type of landmark. (Default: `"type"`).

- grams.min_words:

  Minimum number of words in name to make n/skip-grams out of name.
  (Default: `3`).

- grams.max_words:

  Maximum number of words in name to make n/skip-grams out of name.
  Setting a cap helps to reduce spurious landmarks that may come out of
  really long names. (Default: `6`).

- grams.skip_gram_first_last_word_match:

  For skip-grams, should first and last word be the same as the original
  word? (Default: `TRUE`).

- grams.add_only_if_name_new:

  When creating new landmarks based on n- and skip-grams, only add an
  additional landmark if the name of the landmark is new; i.e., the name
  doesn't already exist in the gazetteer. (Default: `FALSE`).

- grams.add_only_if_specific:

  When creating new landmarks based on n- and skip-grams, only add an
  additional landmark if the name of the landmark represents a specific
  location. A specific location is a location where most landmark
  entries with the same name are close together (within
  `close_thresh_km` kilometers). (Default: `FALSE`).

- types_rm:

  If landmark has one of these types, remove - unless
  `types_rm.except_with_type` or `types_rm.except_with_name` prevents
  removing. (Default:
  `c("route", "road", "toilet", "political", "locality", "neighborhood", "area", "section of populated place")`).

- types_rm.except_with_type:

  Landmark types to always keep. This parameter only becomes relevant in
  cases where a landmark has more than one type. If a landmark has both
  a "types_rm" and a "types_always_keep" landmark, this landmark will be
  kept. (Default: `c("flyover", "round about", "roundabout")`).

- types_rm.except_with_name:

  Landmark names to always keep. This parameter only becomes relevant in
  cases where a landmark is one of "types_rm" Here, we keep the landmark
  if "names_always_keep" is somewhere in the name. For example, if the
  landmark is a road but has flyover in the name, we may want to keep
  the landmark as flyovers are small spatial areas. (Default:
  `c("flyover", "round about", "roundabout")`).

- parallel.sep_slash:

  If a landmark contains a slash, create new landmarks before and after
  the slash. (Default: `TRUE`).

- parallel.rm_begin:

  If a landmark name begins with one of these words, add a landmark that
  excludes the word. (Default:
  `c(tm::stopwords("en"), c("near","at","the", "towards", "near"))`).

- parallel.rm_end:

  If a landmark name ends with one of these words, add a landmark that
  excludes the word. (Default:
  `c("bar", "shops", "restaurant","sports bar","hotel", "bus station")`).

- parallel.word_diff:

  If the landmark includes one of these words, add a landmark that swaps
  the word for the other word (e.g., "center" with "centre"). By
  default, uses a set collection of words. Users can also manually
  specify different word versions. Input should be a `data.frame` with
  the following variables: `version_1` (for one spelling of the word)
  and `version_2` (for a second spelling of the word).

- parallel.word_diff_iftype:

  If the landmark includes one of these words, add a landmark that swaps
  the word for the other word (e.g., "bus stop" with "bus station").
  Enter a named list of words, with `words = c()` and `type = c()`.
  (Default:
  `list(list(words = c("stage", "bus stop", "bus station"), type = "transit_station"))`).

- parallel.rm_begin_iftype:

  If a landmark name begins with one of these words, add a landmark that
  excludes the word if the landmark is a certain type. (Default:
  `NULL`).

- parallel.rm_end_iftype:

  If a landmark name ends with one of these words, add a landmark that
  excludes the word if the landmark is a certain type. (Default:
  `list(list(words = c("stage", "bus stop", "bus station"), type = "transit_station"))`).

- parallel.word_begin_addtype:

  If the landmark begins with one of these words, add the type. For
  example, if landmark is "restaurant", this indicates the landmark is a
  restaurant. Adding the "restaurant" to landmark ensures that the type
  is reflected. (Default: `NULL`).

- parallel.word_end_addtype:

  If the landmark ends with one of these words, add the type. For
  example, if landmark is "X stage", this indicates the landmark is a
  bus stage. Adding the "stage" to landmark ensures that the type is
  reflected. (Default:
  `list(list(words = c("stage", "bus stop", "bus station"), type = "stage"))`).

- parallel.add_only_if_name_new:

  When creating parallel landmarks using the above parameters, only add
  an additional landmark if the name of the landmark is new; i.e., the
  name doesn't already exist in the gazetteer. (Default: `FALSE`).

- parallel.add_only_if_specific:

  When creating parallel landmarks using the above parameters, only add
  an additional landmark if the name of the landmark represents a
  specific location. A specific location is a location where most
  landmark entries with the same name are close together (within
  `close_thresh_km` kilometers). (Default: `FALSE`).

- rm.contains:

  Remove the landmark if it contains one of these words. Implemented
  after N/skip-grams and parallel landmarks are added. (Default:
  `c("road", "rd")`).

- rm.name_begin:

  Remove the landmark if it begins with one of these words. Implemented
  after N/skip-grams and parallel landmarks are added. (Default:
  `c(tm::stopwords("en"), c("near","at","the", "towards", "near"))`).

- rm.name_end:

  Remove the landmark if it ends with one of these words. Implemented
  after N/skip-grams and parallel landmarks are added. (Default:
  `c("highway", "road", "rd", "way", "ave", "avenue", "street", "st")`).

- pos_rm.all:

  Part-of-speech categories to remove. Part-of-speech determined by
  Spacy. (Default:
  `c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "INTJ", "NUM", "PRON", "SCONJ", "VERB", "X")`).

- pos_rm.except_type:

  When specify part-of-speech categories to remove in `pos_rm.all`, when
  to override `pos_rm.all` and keep the word. Names list with: (1) `pos`
  (if the word is also another type of part-of-speech); (2) `type` (if
  the word is also a certain type of place); and (3) `name` (if the word
  includes certain text). Example:
  `list(pos = c("NOUN", "PROPN"), type = c("bus", "restaurant", "bank"), name = c("parliament"))`.
  (Default:
  `list(pos = c("NOUN", "PROPN"), type = c("bus", "restaurant", "bank"), name = "")`).

- close_thresh_km:

  When to consider locations close together. Used when determining if a
  landmark name with multiple locations are specific (close together) or
  general (far apart). (Default: `1`).

- quiet:

  Print progress of function. (Default: `TRUE`).

## Value

`sf` spatial point data.frame of landmarks.

## Examples

``` r
# \donttest{
library(ulex)
library(spacyr)
spacy_install()
#> Warning: Skipping installation. Use `force` to force installation or update. Or use `spacy_download_langmodel()` if you just want to install a model.

lm_sf <- data.frame(name = c("white house",
                             "the world bank group",
                             "the george washington university"),
                    lat = c(38.897778,
                            38.89935,
                            38.9007),
                    lon = c(-77.036389,
                            -77.04275,
                            -77.0508),
                    type = c("building", "building", "building")) |>
sf::st_as_sf(coords = c("lon", "lat"),
         crs = 4326)

lm_aug_sf <- augment_gazetteer(lm_sf)
#> successfully initialized (spaCy Version: 3.7.2, language model: en_core_web_sm)
# }
```
