
load("data/names_of_chromosomes.rda")
load("data/all_alignments.rda")

correspondences_chromosomes <- get_corresponding_chromosomes_species(all_alignments, names_of_chromosomes)

load("data/reference_chromosomes.rda")

# Get the name of the reference species given as input or appointed
reference_species <- reference_chromosomes %>%
  dplyr::pull(Species) %>%
  unique()

# Get all the pair of species that can be considered
pairs_species <- correspondences_chromosomes %>%
  dplyr::mutate(pair_species = paste(query, target, sep = "_")) %>%
  dplyr::pull(pair_species) %>%
  unique()

# Initialize variables in loop
already_done_species <- c()
example_rename_chromosomes <- data.frame()
while (!is.null(pairs_species)){

  list_outputs_iteration <- iterate_over_pairs_of_species_to_get_name_of_chromosomes(
    pair_of_species_to_do = pairs_species,
    already_done_pairs_of_species = already_done_species,
    named_chromosomes_all_sps = example_rename_chromosomes,
    correspondences_chromosomes = correspondences_chromosomes,
    reference_chromosomes = reference_chromosomes,
    reference_species = reference_species
  )

  pairs_species <- list_outputs_iteration$to_do_later
  already_done_species <- list_outputs_iteration$already_done
  example_rename_chromosomes <- list_outputs_iteration$named_chromosomes_all_sps

  if ((!is.null(reference_species)) && (!any_string_in_vector_contains_pattern(reference_species, pairs_species))){
    reference_species <- NULL
    reference_chromosomes <- NULL
  }
}


usethis::use_data(example_rename_chromosomes, overwrite = TRUE)
