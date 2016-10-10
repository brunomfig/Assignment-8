# ---------- Problem 2 ----------
# The objective of this analysis is to try to correlate house elfs magic power to their ear size and the precentage of 'g'and 'c' base pair in their DNA.

# ---------- Problem 3 ----------
# 3 Import data into R
elf_1 <- read.csv("./data/houseelf_earlength_dna_data_1.csv")

# ---------- Problem 6 ----------
# 1 Function to calculate 'g' and 'c' base pairs content in a given DNA sequence
gc_content <- function(dnaseq) {
  l <- str_to_lower(dnaseq)
  t <- str_length(l)
  g <- str_count(l, "g")
  c <- str_count(l, "c")
  gc <- round((g + c) / t * 100, 2)
  return(gc)}

gc_content(elf_1$dnaseq)

# ---------- Problem 7 ----------
# 1 Pull the new function from github
get_size_class <- function(ear_length){
  # Calculate the size class for one or more earth lengths
  ear_lengths <- ifelse(ear_length > 10, "large", "small")
  return(ear_lengths)
}

# 4 Creat a data frame wih IDs, ear class and gc content
