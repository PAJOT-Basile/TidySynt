paths_to_genomes <- data.frame(
  "Species" = c("praehirsuta", "albifrons"),
  "paths" = c("TidySynt/extdata/refgenome/Jaera_praehirsuta_chromosomes.fasta", "TidySynt/extdata/refgenome/Jaera_albifrons_chromosomes.fasta")
)

usethis::use_data(paths_to_genomes, overwrite = TRUE)
