## code to prepare `reference_chromosomes` dataset goes here

load("data/names_of_chromosomes.rda")

reference_chromosomes <- names_of_chromosomes %>%
  filter(Species == "praehirsuta") %>%
  define_reference_for_chromosome_naming(prefix = "LG")

usethis::use_data(reference_chromosomes, overwrite = TRUE)
