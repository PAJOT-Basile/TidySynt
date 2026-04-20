data("all_alignments")
data("names_of_chromosomes")
data("correspondences_chromosomes")
data("reference_chromosomes")

chromosome_names <- get_chromosome_names_all_species(names_of_chromosomes, correspondences_chromosomes, all_alignments, reference_chromosomes) %>%
  group_by(Species) %>%
  arrange(as.numeric(gsub("\\D*(\\d+).*", "\\1", Chromosome_name))) %>%
  mutate(Cumul_position_start_chromosome = cumsum(lag(Length, default = 0))) %>%
  ungroup()
usethis::use_data(chromosome_names, overwrite = TRUE)
