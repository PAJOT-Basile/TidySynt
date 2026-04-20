## code to prepare `names_of_chromosomes` dataset goes here

# Import the names and lengths of chromosomes
names_of_chromosomes <- import_chromosome_lengths_for_all_files_of_folder("inst/extdata/refgenome/")
names_of_chromosomes$Species <- stringr::str_split_fixed(names_of_chromosomes$Species, "_", 3)[, 2]

# Export the dataset
usethis::use_data(names_of_chromosomes, overwrite = TRUE)
