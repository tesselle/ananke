\dontrun{
## Get NGRIP records
data("ngrip2010", package = "folio")
ngrip2010 <- subset(ngrip2010, !is.na(MCE))
ngrip2010 <- ngrip2010[nrow(ngrip2010): 1, ] # Sort in chronological order

## Replicate fig. 3d from Boers et al. (2017)
ngrip_record <- proxy_record(
  depth = ngrip2010$depth,
  proxy = ngrip2010$delta,
  proxy_error = 0.01,
  time = ngrip2010$age,
  time_error = ngrip2010$MCE,
  by = 20,
  n = 30
)

plot(ngrip_record)
}
