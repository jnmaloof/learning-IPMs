# Simple direct way to test the remove_correlated_snps function
source("remove_correlated_snps.R")

# Create a test genotype matrix with known correlations
set.seed(123)
n_ind <- 100

# Create independent and correlated SNPs
snp1 <- sample(0:2, n_ind, replace = TRUE)
snp2 <- sample(0:2, n_ind, replace = TRUE)
snp3 <- snp1 + rnorm(n_ind, sd = 0.1)  # highly correlated with snp1
snp4 <- sample(0:2, n_ind, replace = TRUE)
snp5 <- snp2 + rnorm(n_ind, sd = 0.1)  # highly correlated with snp2
snp6 <- sample(0:2, n_ind, replace = TRUE)
snp7 <- sample(0:2, n_ind, replace = TRUE)
snp8 <- snp7 + rnorm(n_ind, sd = 0.1)  # highly correlated with snp7
snp9 <- sample(0:2, n_ind, replace = TRUE)
snp10 <- sample(0:2, n_ind, replace = TRUE)

# Combine into matrix
test_geno <- cbind(snp1, snp2, snp3, snp4, snp5, snp6, snp7, snp8, snp9, snp10)

# Print correlation matrix
cat("Correlation matrix of test data:\n")
print(round(cor(test_geno), 2))

# Run the function
cat("\nRunning remove_correlated_snps function...\n")
kept <- remove_correlated_snps(test_geno, cor_threshold = 0.7, 
                             window_size = 5, chunk_size = 5, verbose = TRUE)

# Show results
cat("\nExpected: The function should keep about 7 SNPs (10 - 3 correlated ones)\n")
cat("Actual: The function kept", length(kept), "SNPs\n")
cat("Kept SNPs:", kept, "\n")

# Check which correlations were removed
if(length(kept) > 1) {
  cat("\nCorrelation matrix of kept SNPs:\n")
  print(round(cor(test_geno[, kept]), 2))
}
