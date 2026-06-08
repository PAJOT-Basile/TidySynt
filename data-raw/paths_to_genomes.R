paths_to_genomes <- data.frame(
  "Species" = c("praehirsuta", "albifrons"),
  "paths" = c("inst/extdata/refgenome/Jaera_praehirsuta_chromosomes.fasta.fai", "inst/extdata/refgenome/Jaera_albifrons_chromosomes.fasta.fai")
)

usethis::use_data(paths_to_genomes, overwrite = TRUE)
