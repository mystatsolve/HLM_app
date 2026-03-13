pkgs <- c("plumber","lme4","lmerTest")
res <- sapply(pkgs, requireNamespace, quietly=TRUE)
cat(paste(pkgs, res, collapse="\n"), "\n")
