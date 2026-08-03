# baxter-king_etal_2025/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then fit every model, then the tables, the figures, the in-text quantities and
# the ground truth table. Every script is self-contained and can also be run on
# its own.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from OSF on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Models ----
source(here::here("maintained", "analysis_e1_endorse_wave3.R"))
source(here::here("maintained", "analysis_e2_endorse_wave5.R"))
source(here::here("maintained", "analysis_guidance_information.R"))

# Tables ----
source(here::here("maintained", "table_1_experiment_directory.R"))
source(here::here("maintained", "table_2_guidance_experiments.R"))
source(here::here("maintained", "table_3_information_experiments.R"))
source(here::here("maintained", "table_a1_endorsement_1.R"))
source(here::here("maintained", "table_a2_endorsement_2.R"))
source(here::here("maintained", "table_a3_mandate_vignettes.R"))
source(here::here("maintained", "table_a4_mandate_pooled.R"))

# Figures ----
source(here::here("maintained", "figure_1_trump_endorsement.R"))
source(here::here("maintained", "figure_2_all_endorsements.R"))
source(here::here("maintained", "figure_3_cdc_mask_guidance.R"))
source(here::here("maintained", "figure_4_mandate_vignettes.R"))
source(here::here("maintained", "figure_5_adult_booster.R"))
source(here::here("maintained", "figure_a1_a22_experiment_panels.R"))

# In-text quantities ----
source(here::here("maintained", "text_endorsement_claims.R"))
source(here::here("maintained", "text_guidance_information_claims.R"))
source(here::here("maintained", "text_equivalence_claims.R"))

# Ground truth ----
# The deposited code is run first, in a throwaway copy, so the archive column of
# the ground truth is read from a fresh run rather than from a committed file
# that may have drifted. The build then reads everything above back out of
# maintained/output/ and joins it to the values printed in the article.
source(here::here("ground_truth", "extract_archive_values.R"))
source(here::here("ground_truth", "build_ground_truth.R"))

# In-text claims ----
# Every number the article prints, paired with the pipeline output behind it, by
# a route independent of the ground truth build above.
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on checksum, byte size and membership.
source(here::here("download_original.R"))
