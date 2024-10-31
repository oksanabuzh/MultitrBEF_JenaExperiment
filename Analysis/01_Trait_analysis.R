# Purpose: prepare traits and community matrix and calculate functional diversity

library(tidyverse)


# Read data
traits <- read_csv("Data/traits_st_PlasFons.van.der_NatEcol_Evol_2020.csv")
# read all columns as character
community_raw <- read_csv("Data/Plant_Commun_Compos.csv", col_types = cols(.default = "c"))

# Prepare community --------------------------------------------------------
community <- community_raw |>
  filter(Year == "2003") |>
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
fun_div <- FD::dbFD(trait_matrix, community_matrix, calc.CWM = FALSE, 
                    w.abun = T
)
fun_div_results <- as_tibble(fun_div)
fun_div_results$plot <- names(fun_div$FDis)

# Calculate FD based on presence/absence
fun_div_pa <- as_tibble(FD::dbFD(trait_matrix,  
                                 as.matrix((community_matrix>0)+0)), calc.CWM = FALSE,
                        w.abun = F)
fun_div_pa$plot <- names(fun_div_pa$FDis)    


fun_div_results <- fun_div_pa %>% 
  select(plot, FDis) %>% 
  rename("FDis_pa"="FDis") %>% 
  left_join(fun_div_results%>% 
              select(plot, nbsp, FDis), by="plot")

fun_div_results
# Calculate sum of branchlengths of dendrogram -----------------------------
# Calculated equivalent to the SumBL function from the cati r package

# Extract the species present in the community for each plot separately
community_p_a <- community_matrix |>
  as_tibble(rownames = "Plot") |>
  pivot_longer(cols = -Plot, names_to = "sp", values_to = "cover") |>
  filter(cover > 0) |>
  # split into list by plot id and add plot as a name in the list
  group_by(Plot) |>
  group_split() |>
  map(~ .x |> pivot_wider(names_from = sp, values_from = cover))

# Give the list elements names according to the plot id
names(community_p_a) <- community_p_a |>
  map_chr(~ .x$Plot[1])


# create a trait matrix separately for each plot
create_trait_matrix <- function(plot_community, all_traits) {
  # extract species names (all column names excepte the first which is Plot)
  species <- names(plot_community)[-1]
  trait_subset <- all_traits[species, ]
  return(trait_subset)
}

# Run function over the community list to get a list of trait matrices
trait_matrix_list <- community_p_a |>
  map(create_trait_matrix, all_traits = trait_matrix)

# Function to calculate the sum of branch lengths
trait_matrix <- trait_matrix_list[[5]]
sum_bl <- function(trait_matrix) {
  if (!is.matrix(trait_matrix)) {
    # if we only have one species, trait_matrix will be a vectors and we return 0
    return(0)
  } else {
    tryCatch(
      {
        # If there are traits that have the exact same value for all species
        # remove this trait. This will not affect the results but otherwise
        # we would get NaN for the distance because we divide by 0
        # find all columns where all values are exactly the same
        same_values <- apply(trait_matrix, 2, function(x) length(unique(x)) == 1) |> which()
        # add a very small random value to these traits to make them unique
        if (length(same_values) > 0) {
          print(paste("Removed columns:", same_values))
          values_to_add <- rnorm(nrow(trait_matrix) * length(same_values), 
                                 mean = 0, sd = 0.0001)
          trait_matrix[, same_values] <- trait_matrix[, same_values] + values_to_add
        }

        dist <- FD::gowdis(trait_matrix)
        dist <- na.omit(dist)
        tree <- hclust(dist, method = "average")
        res <- vegan::treeheight(tree)
        return(res)
      },
      error = function(e) {
        print(e$message)
        print(trait_matrix)
        return(NA)
      }
    )
  }
}


bl_results <- trait_matrix_list |>
  # apply function sum_bl to each element and save result as a tibble
  map_dbl(sum_bl)
# convert to tibble with vector names in a column

# Convert into a table
bl_results <- tibble(
  FDbranch = bl_results,
  plot = names(bl_results)
)

# Combine all fd results and make correlation ------------------------------
all_fun_div <- left_join(fun_div_results, bl_results, by = "plot") |>
 # select(-qual.FRic) |>
  select(plot, FDis, FDis_pa, FDbranch)

# Save the results
write_csv(all_fun_div, "Results/functional_diversity.csv")

# Check correlations

Index <- readr::read_csv("Data/net_ind_fluxes.csv")
str(Index)

# Read functional diversity indices
fun_div <- left_join(fun_div_results, bl_results, by = "plot") |>
 # select(-qual.FRic) |>
  select(plot, nbsp, FDis, FDis_pa, FDbranch)

# join both tables by Plot ID
Index <- Index %>% 
  left_join(fun_div, by = c("plotcode" = "plot")
  ) |> 
  relocate(
    all_of(c("nbsp", "FDis", "FDis_pa", "FDbranch")),
    .before = sowndiv 
  ) %>% 
  rename("SR_2010"="nbsp")


Index |>
  select(sowndiv, SR_2010, numfg, FDbranch, FDis, FDis_pa) |>
  cor() |>
  ggcorrplot::ggcorrplot(
    lab = TRUE, type = "lower",
    outline.color = "white",
    colors = c("red", "white", "blue")
  )
