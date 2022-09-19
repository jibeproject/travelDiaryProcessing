# Write to CSV.R

TRADS <- readRDS("data/Manchester/processed/TRADS_safe_routed.rds")

saveRDS(TRADS,"data/Manchester/processed/TRADS_safe_routed_v2.rds", version = 2)
saveRDS(TRADS,"data/Manchester/processed/TRADS_safe_routed_v3.rds", version = 3)

for(df in names(TRADS)) {
  write.csv(TRADS[[df]],paste0("data/Manchester/processed/TRADS_safe_routed/",df,".csv"))
}