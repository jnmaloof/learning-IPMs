#' Remove correlated SNPs from a genotype matrix
#'
#' @param geno A matrix of genotype data with individuals in rows and SNPs in columns
#' @param cor_threshold The correlation threshold above which to consider SNPs as highly correlated
#' @param window_size The number of neighboring SNPs to check for correlation
#' @param chunk_size Number of SNPs to process at once (helps with memory management)
#' @param verbose Logical; if TRUE, print progress updates
#' @return A vector of column indices to keep
#'
#' @details This function processes SNPs in chunks to efficiently handle large matrices.
#' For each SNP, it checks correlation only with nearby SNPs within the specified window.
#'
remove_correlated_snps <- function(geno, cor_threshold = 0.8, window_size = 100, 
                                  chunk_size = 5000, verbose = FALSE) {
  
  n_snps <- ncol(geno)
  if(verbose) cat("Processing", n_snps, "SNPs\n")
  
  # Initialize vector to track which SNPs to keep (1) or remove (0)
  keep_snps <- rep(1, n_snps)
  
  # Process SNPs in chunks to manage memory
  n_chunks <- ceiling(n_snps / chunk_size)
  
  for(chunk in 1:n_chunks) {
    if(verbose) cat("Processing chunk", chunk, "of", n_chunks, "\n")
    
    # Define start and end indices for this chunk
    start_idx <- (chunk - 1) * chunk_size + 1
    end_idx <- min(chunk * chunk_size, n_snps)
    chunk_indices <- start_idx:end_idx
    
    # Only process SNPs that haven't been marked for removal
    active_indices <- chunk_indices[keep_snps[chunk_indices] == 1]
    if(length(active_indices) == 0) next
    
    for(i in active_indices) {
      # Define window of SNPs to check (respect matrix boundaries)
      window_end <- min(n_snps, i + window_size)
      
      # Only check SNPs that haven't been removed and are ahead of current SNP
      # (we've already checked those behind)
      # Make sure we don't go beyond the matrix dimensions
      if(i+1 > window_end) next
      
      ahead_indices <- (i+1):window_end
      to_check <- ahead_indices[keep_snps[ahead_indices] == 1]
      if(length(to_check) == 0) next
      
      # Instead of calculating correlations one by one, calculate them in a batch
      # Calculate correlations between current SNP and the ones to check
      cors <- abs(cor(geno[, i], geno[, to_check, drop=FALSE]))
      
      # Mark highly correlated SNPs for removal
      high_cor <- which(cors > cor_threshold)
      if(length(high_cor) > 0) {
        keep_snps[to_check[high_cor]] <- 0
      }
    }
  }
  
  # Return indices of SNPs to keep
  kept_indices <- which(keep_snps == 1)
  if(verbose) cat("Kept", length(kept_indices), "of", n_snps, "SNPs\n")
  
  return(kept_indices)
}
