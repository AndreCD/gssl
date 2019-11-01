## select rslocal samples to MAP
names.rslocal <- noquote(rownames(rslocal))
gssl.test$ID  <- rownames(gssl.test)
rslocal$ID    <- rownames(rslocal)
test <- dplyr::left_join(rslocal, gssl.test, by = "ID")
rslocal2 <- test[,2153:length(test)]
write.csv(rslocal2, "C:/Users/280240B/Downloads/spectra/rslocal.csv")
rslocal$ID <- NULL
gssl.test$ID <- NULL
