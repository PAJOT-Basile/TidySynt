## code to prepare `data_to_plot` dataset goes here
data("all_alignments")
data("chromosome_names")

data_to_plot <- prepare_data_for_plotting(all_alignments, chromosome_names, NULL, 1e4)$data_to_plot %>%
  dplyr::select(tidyselect::contains("Chromosome"), qstart, qend, tstart, tend, query, target) %>%
  # Here, we prepare the data so it is plotable using polygons
  dplyr::mutate(Position = dplyr::row_number(),
                Chromosome_name_query = stringr::str_split_fixed(Chromosome_name_query, "\\.", 2)[, 1]) %>%
  tidyr::pivot_longer(cols = c(qstart, qend, tstart, tend), names_to = "name", values_to = "delims_polygon") %>%
  tidyr::pivot_longer(cols = c(query, target), names_to = "direction", values_to = "Species") %>%
  dplyr::filter((grepl("^q", name) & grepl("^q", direction)) | (grepl("^t", name) & grepl("^t", direction))) %>%
  unique() %>%
  dplyr::filter(Position == 1) %>%
  dplyr::group_by(Position)

usethis::use_data(data_to_plot, overwrite = TRUE)
