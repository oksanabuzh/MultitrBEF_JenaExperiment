# Purpose: prepare traits and community matrix and calculate functional diversity

library(tidyverse)
library(picante)

# Read data
traits <- read_csv("Data/traits_st_PlasFons.van.der_NatEcol_Evol_2020.csv")
# read all columns as character
community_raw <- read_csv("Data/Plant_Commun_Compos.csv", col_types = cols(.default = "c"))

# Prepare community --------------------------------------------------------
community <- community_raw |>
  filter(Year == "2010") |>
  pivot_longer(-c(Month, Year, Plot), names_to = "sp", values_to = "cover") |>
  mutate(cover = as.numeric(cover)) |>
  drop_na() |>
  summarize(
    cover = mean(cover),
    .by = c("Plot", "sp")
  )

# Order species alphabetically
community <- community |>
  arrange(sp)

# Create community matrix for functional diversity
community <- community |>
  pivot_wider(names_from = sp, values_from = cover, values_fill = 0)

# Convert to community matrix
community_matrix <- as.matrix(community |> select(-Plot))

rownames(community_matrix) <- community$Plot

# Prepare traits -----------------------------------------------------------
# use same species names in traits as in the communit
traits <- traits |>
  mutate(sp = str_replace(sp, "\\.", "_")) |>
  arrange(sp)

# convert into a matrix with species as rownames
trait_matrix <- as.matrix(traits |> select(-sp))
rownames(trait_matrix) <- traits$sp

# Calculate functional diversity -------------------------------------------
# Are there species in the community_matrix (colnames) that are not in the
# trait matrix (rownames)?
species_not_in_traits <- setdiff(colnames(community_matrix), rownames(trait_matrix))
# Remove this species (Primula vera) from the community matrix
community_matrix <- community_matrix[, -which(colnames(community_matrix) %in% species_not_in_traits)]

# Are there species that are not part of any community (cover is 0 in all plots)?
species_not_in_comm <- colSums(community_matrix)[colSums(community_matrix) == 0] |> names()
# remove them from traits and community
trait_matrix <- trait_matrix[-which(rownames(trait_matrix) %in% species_not_in_comm), ]
community_matrix <- community_matrix[, -which(colnames(community_matrix) %in% species_not_in_comm)]

# Calculate functional diversity
fun_div <- FD::dbFD(trait_matrix, community_matrix,
  calc.CWM = FALSE
)
fun_div <- as_tibble(fun_div)

# Calculate sum of branchlengths of dendrogram -----------------------------
# Calculated equivalent to the SumBL function from the cati r package

# Example using all traits and species
tree.dis <- FD::gowdis(trait_matrix)
picante::mpd(community_matrix, tree.dist, abundance.weighted = TRUE)
tree.dist <- na.omit(tree.dist)
tree <- hclust(tree.dist, method = "average")
res <- treeheight(tree)

picante::pd(tree)

library(cati)
data(finch.ind)
SumBL(traits.finch)

# Define a function
community <- community_matrix[1, ] |> as.vetor()
sum_branch_lengths <- function(trait_matrix, community) {
  # Calculate trait-trait distance matrix using cover as weights
  tree_dist_weighted <- FD::gowdis(as.data.frame(trait_matrix), community) # to do: Add weights based on community
}

# Check correlation between indices -------------------------------------------------
fun_div |>
  dplyr::select(-qual.FRic) |>
  view()
cor() |>
  ggcorrplot::ggcorrplot(
    lab = TRUE
  )
