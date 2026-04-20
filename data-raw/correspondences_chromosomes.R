## code to prepare `correspondences_chromosomes` dataset goes here

# Import the alignments
load("data/all_alignments.rda")

# Import the lengths and assembly names of chromosomes
load("data/names_of_chromosomes.rda")

# Get the correspondences between chromosomes of different species
correspondences_chromosomes <- get_corresponding_chromosomes_species(all_alignments, names_of_chromosomes)

usethis::use_data(correspondences_chromosomes, overwrite = TRUE)
