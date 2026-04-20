data("all_alignments")
data("correspondences_chromosomes")

alignments_ready_to_plot <- all_alignments %>%
  reverse_all_chromosomes(correspondences_chromosomes)

usethis::use_data(alignments_ready_to_plot, overwrite = TRUE)
