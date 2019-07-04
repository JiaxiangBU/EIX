#' @param interactions interactions
#' @export
tidy_interaction <- function(interactions) {
  x_name_id_pairs <-
    data.frame(x_name =
                 c(interactions$Parent, interactions$Child) %>%
                 unique(), stringsAsFactors = FALSE) %>%
    mutate(x_id = x_name %>% as.factor %>% as.integer())
  interactions_pairs <-
    interactions %>%
    as_tibble() %>%
    left_join(x_name_id_pairs, by = c("Parent" = "x_name")) %>%
    rename(parent_id = x_id) %>%
    left_join(x_name_id_pairs, by = c("Child" = "x_name")) %>%
    rename(child_id = x_id) %>%
    mutate(pairs = case_when(
      parent_id > child_id ~ paste0(Parent, ',', Child),
      parent_id <= child_id ~ paste0(Child, ',', Parent)
    ))
  interactions_pairs %>%
    group_by(pairs) %>%
    summarise(sumGain = sum(sumGain)) %>%
    separate(pairs, into = c("Parent", "Child"), sep = ",") %>%
    arrange(desc(sumGain))
}
