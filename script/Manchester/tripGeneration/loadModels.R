########## LOAD TRIP GENERATION MODELS ##########

models <- readRDS("results/tripGenModels.rds")

# Three model groups:
#   models$yn     – binary logit (HBW, HBE):        does person make any trips?
#   models$polr   – ordered logit (HBW, HBE):       how many trips, given >0?
#   models$hurdle – hurdle neg-binomial (HBS, HBR, HBO, HBA, NHBW, NHBO, RRT)

m.tripGenYn     <- models$yn
m.tripGenPolr   <- models$polr
m.tripGenHurdle <- models$hurdle

# Quick sanity check
cat("Binary logit models:  ", paste(names(m.tripGenYn),     collapse = ", "), "\n")
cat("Ordered logit models: ", paste(names(m.tripGenPolr),   collapse = ", "), "\n")
cat("Hurdle models:        ", paste(names(m.tripGenHurdle), collapse = ", "), "\n")
