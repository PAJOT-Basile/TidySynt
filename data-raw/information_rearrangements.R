## code to prepare `information_rearrangements` dataset goes here
data("all_alignments")
data("names_of_chromosomes")

information_rearrangements <- all_alignments %>%
  dplyr::group_by(query, target, qname, tname) %>%
  dplyr::summarize(total_length = sum(alen), .groups = "drop_last") %>%
  dplyr::arrange(dplyr::desc(total_length)) %>%
  dplyr::mutate(sp_pairs = paste(query, target, sep = "_")) %>%
  dplyr::slice_head(n = 2) %>%
  dplyr::mutate(name = dplyr::row_number()) %>%
  tidyr::pivot_wider(names_from = "name", values_from = c("tname", "total_length"))

usethis::use_data(information_rearrangements, overwrite = TRUE)
