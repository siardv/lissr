# income_cleaning_smoke.R
# seeded synthetic verification for the income-cleaning framework.
# plants known error types at known rows in a 400-household panel, runs
# liss_clean_income(), and reports per-type recovery, the false-positive
# rate on clean cells, correction accuracy for scale errors, mode
# agreement, and the ledger invariants. artifacts go to
# LISSR_SMOKE_DIR (default: a tempdir).
#
# usage: Rscript inst/scripts/verification/income_cleaning_smoke.R

library(lissr)

set.seed(2026)

lo <- c(0, 8000, 16000, 24000, 36000, 48000, 60000)
hi <- c(8000, 16000, 24000, 36000, 48000, 60000, 120000)

n_hh <- 400
rows <- list()
pid <- 0
for (h in seq_len(n_hh)) {
  k <- sample(3:8, 1)
  waves <- sort(sample(1:10, k))
  base <- min(max(rlnorm(1, log(30000), 0.35), 9000), 130000)
  vals <- round(base * exp(rnorm(k, 0, 0.06)) / 100) * 100
  pid <- pid + 1
  adults <- sample(1:3, 1)
  kids <- sample(0:2, 1)
  netto <- round(vals * runif(1, 0.35, 0.6) / adults / 100) * 100
  code <- if (runif(1) < 0.6) {
    pmin(findInterval(vals, c(lo, Inf)), 7)
  } else {
    rep(NA_real_, k)
  }
  rows[[h]] <- data.frame(
    nomem_encr = pid, nohouse_encr = h, wavenr = waves, nethh = vals,
    nethh_min = code, nettoink = netto, brutoink = round(netto * 1.35),
    aantalhh = adults + kids, positiehh = 1,
    belbezig = sample(c(1, 2, 3), 1), leeftijd = sample(25:70, 1),
    oplmet = sample(1:6, 1), gebjaar = NA_real_
  )
}
panel <- do.call(rbind, rows)
orig <- panel$nethh
n <- nrow(panel)
cat("panel:", n, "rows,", n_hh, "households\n")

# plant errors on distinct rows; keep the truth table
avail <- seq_len(n)
plant <- function(n_pick, type, fn) {
  pick <- sample(avail, n_pick)
  avail <<- setdiff(avail, pick)
  panel$nethh[pick] <<- fn(panel$nethh[pick], pick)
  data.frame(row = pick, type = type)
}
truth <- rbind(
  plant(round(0.030 * n), "decimal_shift", function(v, i) round(v / 10)),
  plant(round(0.015 * n), "extra_zero", function(v, i) v * 10),
  plant(round(0.004 * n), "cap_blowout", function(v, i) v * 50),
  plant(round(0.010 * n), "personal_echo", function(v, i) {
    pmin(panel$nettoink[i], 9500) + sample(-40:40, length(i), replace = TRUE)
  }),
  plant(round(0.005 * n), "tiny_junk", function(v, i) {
    sample(0:9, length(i), replace = TRUE)
  }),
  plant(round(0.005 * n), "sign_flip", function(v, i) -v),
  plant(round(0.003 * n), "sentinel", function(v, i) 9999999999)
)
cat("planted:", nrow(truth), "errors across", length(unique(truth$type)),
    "types\n\n")

t0 <- Sys.time()
res <- liss_clean_income(panel, verbose = FALSE)
cat("correct mode:",
    round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), "s\n")

st <- res$data$nethh_clean_status
mutated <- which(!is.na(st))

cat("\nrecovery (planted row mutated by any rule):\n")
for (tp in unique(truth$type)) {
  r <- truth$row[truth$type == tp]
  hitn <- sum(r %in% mutated)
  cat(sprintf("  %-14s %3d/%3d  (%.0f%%)\n", tp, hitn, length(r),
              100 * hitn / length(r)))
}

fp <- setdiff(mutated, truth$row)
clean_rows <- setdiff(seq_len(n), truth$row)
cat(sprintf("\nfalse positives: %d of %d clean cells (%.2f%%)\n",
            length(fp), length(clean_rows),
            100 * length(fp) / length(clean_rows)))
if (length(fp) > 0) print(table(st[fp]))

scale_rows <- truth$row[truth$type %in% c("decimal_shift", "extra_zero")]
sr <- intersect(scale_rows, mutated)
sr <- sr[!is.na(res$data$nethh[sr])]
rel_err <- abs(res$data$nethh[sr] - orig[sr]) / orig[sr]
cat(sprintf("\nscale-error corrections within 10%% of truth: %.0f%% (median rel. err %.1f%%)\n",
            100 * mean(rel_err <= 0.10), 100 * stats::median(rel_err)))

res_f <- liss_clean_income(panel, mode = "flag", verbose = FALSE)
stopifnot(identical(res_f$data$nethh_proposed, res$data$nethh))
stopifnot(identical(res_f$data$nethh, as.numeric(panel$nethh)))
cat("\nflag mode: proposals identical to correct-mode output; data untouched\n")

before <- res$data$nethh_observed
after <- res$data$nethh
diff_rows <- which(xor(is.na(before), is.na(after)) |
                     (!is.na(before) & !is.na(after) & before != after))
mut_led <- res$decisions[res$decisions$applied &
                           res$decisions$variable == "nethh" &
                           res$decisions$action %in%
                             c("correct", "set_na", "rectify_sign", "cap_na"), ]
stopifnot(setequal(diff_rows, unique(mut_led$row)))
stopifnot(identical(before, as.numeric(panel$nethh)))
cat("ledger invariants hold:", nrow(res$decisions), "decisions cover",
    length(diff_rows), "changed cells; input preserved in *_observed\n")

out_dir <- Sys.getenv("LISSR_SMOKE_DIR", file.path(tempdir(), "lissr-smoke"))
paths <- liss_cleaning_report(res, out_dir, verbose = FALSE)
cat("\nartifacts:\n")
for (p in unlist(paths)) cat(" ", p, "\n")

print(res)
