\dontrun{
## Get NGRIP records
data("ngrip2010", package = "folio")
ngrip2010 <- subset(ngrip2010, !is.na(MCE))
ngrip2010 <- ngrip2010[nrow(ngrip2010):1, ] # Sort in chronological order

## Replicate fig. 3d from Boers et al. (2017)
## /!\ This may take a while... /!\
ngrip_record <- proxy_ensemble(
  depth = ngrip2010$depth,
  proxy = ngrip2010$delta,
  proxy_error = 0.01,
  step = 0.001,
  time = ngrip2010$age,
  time_error = ngrip2010$MCE,
  calendar = b2k(), # /!\
  by = 20,
  n = 30
)

plot(ngrip_record)
}
