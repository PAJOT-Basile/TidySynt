#' All alignments
#'
#' An example of a paf file imported as a data frame that can be used in tidyverse.
#'
#' @format ## `all_alignments`
#' A data frame with 27,749 rows and 9 columns:
#' \describe{
#'    \item{qname}{Name of the assembled chromosome for the query species.}
#'    \item{qstart, qend}{Start/End coordinates of the mapping sequence on the
#'    chromosome of the query species.}
#'    \item{tname}{Name of the assembled chromosome for the target species.}
#'    \item{tstart, tend}{Start/End coordinates of the mapping sequence on the
#'    chromosome of the target species.}
#'    \item{alen}{Length of the mapping sequence.}
#'    \item{query, target}{Names of the query/target species.}
#' }
#'
#' @source Run of `minimap2` on chromosome-scale reference genomes published by
#' the Darwin Tree of Life project.
#'
"all_alignments"

#' Alignments ready to plot
#'
#' An example of a dataset that is ready to plot without having to prepare it
#' in the examples. We simply return chromosomes that were assembled differently
#' for the studied species.
#'
#' @format ## `alignments_ready_to_plot`
#' A data frame with 27,749 rows and 9 columns:
#' \describe{
#'    \item{qname}{Name of the assembled chromosome for the query species.}
#'    \item{qstart, qend}{Start/End coordinates of the mapping sequence on the
#'    chromosome of the query sepcies.}
#'    \item{tname}{Name of the assembled chromosome for the target species.}
#'    \item{tstart, tend}{Start/End coordinates of the mapping sequence on the
#'    chromosome of the target species.}
#'    \item{alen}{Length of the mapping sequence.}
#'    \item{query, target}{Names of the query/target species.}
#' }
#' @source Run of `minimap2` on chromosome-scale reference genomes published by
#' the Darwin Tree of Life project on which we reordered chromosomes that were
#' not assembled in the same direction.
"alignments_ready_to_plot"

#' Chromosome names
#'
#' Assembly and alignment names of the chromosomes as well as their lengths and
#' cumulative positions for each species.
#'
#' @format ## `chromosome_names`
#' A data frame with 15 rows and 5 columns
#' \describe{
#'    \item{Chromosome}{Assembly name of the chromosome.}
#'    \item{Chromosome_name}{Alignment name of the chromosome.}
#'    \item{Species}{Species name.}
#'    \item{Length}{Length of the chromosome in base pairs.}
#'    \item{Cumul_position_start_chromosome}{Cumulative position at which the
#'    chromosome starts if they are arranged from chromosome 1 to chromosome n.
#'    This cumulative position is independent for the different species and is
#'    counted in base pairs.}
#' }
#'
"chromosome_names"

#' Correspondences between chromosomes
#'
#' Correspondences between chromosomes of the different studied species.
#'
#' @format ## `correspondences_chromosomes`
#' A data frame with 13 rows and 4 columns.
#' \describe{
#'    \item{query, target}{Names of the query/target species.}
#'    \item{qname, tname}{Names of the assembled chromosomes of the query/target
#'    species.}
#' }
"correspondences_chromosomes"

#' Data to plot
#'
#' Example data to plot in the "make_synteny_plot" function of the package.
#'
#' @format ## `data_to_plot`
#' A data frame with 4 rows and 7 columns.
#' \describe{
#'    \item{Chromosome_name_query, Chromosome_name_target}{Alignment names of the
#'    chromosomes of the query/target species.}
#'    \item{Position}{Group number to trace the polygon in the "make_synteny_plot"
#'    function.}
#'    \item{name}{Column resulting from the use of a pivot_longer function that
#'    is used to be able to have all the coordinates of the polygon to trace in
#'    the synteny plot in one unique column.}
#'    \item{delims_polygon}{Coordinates of the mapping sequence along the
#'    chromosomes of the query/target species.}
#'    \item{direction}{Indication for the "arrange_for_polygons" function to order
#'    the data in the correct order for polygons to be traced.}
#'    \item{Species}{Target/query species.}
#' }
"data_to_plot"

#' Example of chromosomes to rename
#'
#' Example of data frame in which we have some chromosomes to rename in the
#' package.
#'
#' @format ## `example_rename_chromosomes`
#' A data frame with 25 rows and 3 columns.
#' \describe{
#'    \item{Chromosome}{Assembly name of the chromosome.}
#'    \item{Chromosome_name}{Alignment name of the chromosome.}
#'    \item{Species}{Species name.}
#' }
#'
"example_rename_chromosomes"

#' Top two correspondences for each chromosome of the query species and the total
#' mapping sequence length of these two top correspondences.
#'
#' @format ## `information_rearrangements`
#' A data frame with 12 rows and 8 columns.
#' \describe{
#'    \item{query, target}{Name of the query/target species.}
#'    \item{qname}{Assembly name of the chromosomes for the query species.}
#'    \item{sp_pairs}{Pair of query-target species separated by an underscore.}
#'    \item{tname_1, tname_2}{Assembly names of the chromosomes that have the
#'    largest/second largest cumulative mapping sequence with the chromosome of
#'    the query species.}
#'    \item{total_length_1, total_length_2}{Total cumulative mapping sequence
#'    of the chromosome_1/_2 on the chromosome of the query species.}
#' }
"information_rearrangements"

#' Assembly names of the chromosomes.
#'
#' Assembly names of the chromosomes, their length and species.
#'
#' @format ## `names_of_chromosomes`
#' A data frame with 24 rows and 3 columns.
#' \describe{
#'    \item{Chromosome}{Assembly name of the chromosome.}
#'    \item{Length}{Length of the chromosome.}
#'    \item{Species}{Species name.}
#' }
"names_of_chromosomes"

#' Names of reference chromosomes
#'
#' Correspondences between the assembly and alignment names for the reference
#' species.
#'
#' @format ## `reference_chromosomes`
#' A data frame with 12 rows and 3 columns.
#' \describe{
#'    \item{Chromosome}{Assembly name of the chromosome.}
#'    \item{Chromosome_name}{Alignment name of the chromosome for the reference
#'    species.}
#'    \item{Species}{Species name.}
#' }
"reference_chromosomes"
