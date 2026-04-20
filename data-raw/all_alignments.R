## code to prepare `all_alignments` dataset goes here

# Import the paf files
all_alignments <- import_paf_from_folder("inst/extdata/paf/")

# Rename the chromosomes
all_alignments$tname <- stringr::str_split_fixed(all_alignments$tname, "\\.", 2)[, 1]
all_alignments$qname <- stringr::str_split_fixed(all_alignments$qname, "\\.", 2)[, 1]
all_alignments <- all_alignments %>%
  dplyr::select(qname, qstart, qend, tname, tstart, tend, alen, query, target)

# Export the data
usethis::use_data(all_alignments, overwrite = TRUE)
