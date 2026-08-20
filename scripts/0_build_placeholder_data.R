# ============================================================================ #
#  0 · SIMULATE PLACEHOLDER DATA — spatial-frames referential game
#  --------------------------------------------------------------------------- #
#  Builds a fake annotation file with the SAME columns/format as the real coder
#  sheet, so 2_descriptive / 3_modeling / 4_results can be written and knitted
#  before the empirical data arrive. Fits nothing, tests nothing: it only makes
#  data of the correct SHAPE (right columns, right factor structure, plausible
#  outcomes with mild hypothesis-shaped signal). Swap in the real file later and
#  every downstream script runs unchanged.
# ============================================================================ #

#################### packages ####################
library(tidyverse)   # wrangling (harmless if already loaded in setup)
library(writexl)     # write_xlsx(); install.packages("writexl") if needed

set.seed(2026)       # reproducible placeholder data

#################### design constants ####################
n_pairs     <- 20    # registered first batch (hard ceiling 40); change freely
trials_pair <- 16    # 16 unique stimuli, one presentation each per dyad

#################### paths ####################
# Anchor to THIS script's folder (…/scripts), write one level up into …/data.
# Requires running interactively in RStudio (as in your other scripts).
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # -> …/scripts
parentfolder <- dirname(getwd())                             # -> project root
data         <- paste0(parentfolder, "/data/")               # path object; reuse downstream
if (!dir.exists(data)) dir.create(data, recursive = TRUE)
out_file     <- paste0(data, "annotation_spatialFrames_simulated.xlsx")

#################### canonical stimulus set (fixed 2 × 2 × 4 = 16) ####################
# story_id = [size][distance][item]:  e/c = element/complete, p/d = proximate/displaced.
# trial_id is the stable 1–16 stimulus code; target is the referent label.
stimuli <- tribble(
  ~trial_id, ~story_id, ~size,      ~distance,   ~item, ~target,
  1L, "cd1", "complete", "displaced", 1L, "house",
  2L, "cd2", "complete", "displaced", 2L, "attic",
  3L, "cd3", "complete", "displaced", 3L, "mountains",
  4L, "cd4", "complete", "displaced", 4L, "countryside",
  5L, "cp1", "complete", "proximate", 1L, "room",
  6L, "cp2", "complete", "proximate", 2L, "basement",
  7L, "cp3", "complete", "proximate", 3L, "laboratory",
  8L, "cp4", "complete", "proximate", 4L, "city",
  9L, "ed1", "element",  "displaced", 1L, "stairs",
  10L, "ed2", "element",  "displaced", 2L, "sofa",
  11L, "ed3", "element",  "displaced", 3L, "tree",
  12L, "ed4", "element",  "displaced", 4L, "carpet",
  13L, "ep1", "element",  "proximate", 1L, "window",
  14L, "ep2", "element",  "proximate", 2L, "chair",
  15L, "ep3", "element",  "proximate", 3L, "coathanger",
  16L, "ep4", "element",  "proximate", 4L, "painting"
)
stopifnot(nrow(stimuli) == trials_pair)        # keep stimuli and trials aligned

#################### trial skeleton (one row per production) ####################
# Partners alternate the producer role (slot 1 on odd trials, slot 2 on even);
# the 16 stimuli appear once each in a random order per dyad.
skeleton <- expand_grid(pair = seq_len(n_pairs), trial = seq_len(trials_pair)) %>%
  group_by(pair) %>%
  mutate(
    trial_id    = sample(stimuli$trial_id, n()),      # random stimulus order
    participant = if_else(trial %% 2 == 1L, 1L, 2L)   # producer slot WITHIN pair
  ) %>%
  ungroup() %>%
  left_join(stimuli, by = "trial_id") %>%
  # unique person key (slot repeats across pairs, so cross with pair); guesser = partner
  mutate(
    producer_uid = paste(pair, participant, sep = "_"),
    guesser_uid  = paste(pair, if_else(participant == 1L, 2L, 1L), sep = "_")
  )

#################### random deviates (so RE terms are estimable) ####################
subj_ids <- sort(unique(c(skeleton$producer_uid, skeleton$guesser_uid)))
subj_re  <- tibble(
  uid            = subj_ids,
  prod_dev_succ  = rnorm(length(subj_ids), 0, 0.5),   # producer skill → success
  guess_dev_succ = rnorm(length(subj_ids), 0, 0.5),   # guesser skill → success
  prod_dev_icon  = rnorm(length(subj_ids), 0, 0.4)    # producer style → icon use
)
item_re <- stimuli %>%
  transmute(trial_id,
            item_dev_succ = rnorm(n(), 0, 0.4),        # item difficulty → success
            item_dev_icon = rnorm(n(), 0, 0.3))        # item pull → icon use

#################### outcomes (data-generating model; sum-coded ±0.5) ####################
sim <- skeleton %>%
  left_join(subj_re %>% select(uid, prod_dev_succ, prod_dev_icon),
            by = c("producer_uid" = "uid")) %>%
  left_join(subj_re %>% select(uid, guess_dev_succ),
            by = c("guesser_uid"  = "uid")) %>%
  left_join(item_re, by = "trial_id") %>%
  mutate(
    size_c = if_else(size == "complete",      0.5, -0.5),
    dist_c = if_else(distance == "displaced", 0.5, -0.5),
    
    ## communicative success — H1: complete < element ; distance ≈ null (H2)
    eta_succ = 0.85 - 0.60 * size_c + 0.00 * dist_c +
      prod_dev_succ + guess_dev_succ + item_dev_succ,
    correct_target = rbinom(n(), 1, plogis(eta_succ)),
    
    ## strategies — three NON-exclusive flags; distance pushes icon (displaced)
    ## vs index (proximate); prop is a rare, unregistered extra strategy
    p_icon  = plogis( 0.20 + 0.90 * dist_c + prod_dev_icon + item_dev_icon),
    p_index = plogis(-0.30 - 0.90 * dist_c),
    p_prop  = plogis(-1.60 + rnorm(n(), 0, 0.3)),
    icon  = rbinom(n(), 1, p_icon),
    index = rbinom(n(), 1, p_index),
    prop  = rbinom(n(), 1, p_prop)
  ) %>%
  ## guard: avoid degenerate all-zero productions (switch on the likeliest). Cosmetic.
  mutate(
    none_used = (icon + index + prop) == 0L,
    top_p     = pmax(p_icon, p_index, p_prop),
    icon  = if_else(none_used & p_icon  == top_p, 1L, as.integer(icon)),
    index = if_else(none_used & p_index == top_p, 1L, as.integer(index)),
    prop  = if_else(none_used & p_prop  == top_p, 1L, as.integer(prop))
  )

#################### final columns — EXACT annotation-sheet layout ####################
sim_out <- sim %>%
  transmute(
    pair, participant, trial, trial_id, story_id, target,
    correct_target = as.integer(correct_target),
    icon  = as.integer(icon),
    index = as.integer(index),
    prop  = as.integer(prop)
  ) %>%
  arrange(pair, trial)

#################### write it out (mirrors the real coder sheet) ####################
write_xlsx(list(coder_sim = sim_out), out_file)
message("Wrote ", nrow(sim_out), " rows (", n_pairs, " dyads) to: ", out_file)

sim_out    # peek