### Organize Images into Surface/Rep folder structure ###
# This script copies and renames images from the source folder
# into an organized structure for ImageJ processing
#
# PREREQUISITE: Run 00-GlobalCode.R first

# === IMAGE MAPPING ===
# Images are in sequence: A_blank, B_blank, C_blank, A_before, B_before, C_before, A_after, B_after, C_after
# This pattern repeats for each replication

# Get list of TIF images
image_files <- list.files(path = SourceImages.dir, pattern = "\\.TIF$", full.names = FALSE)
image_paths <- list.files(path = SourceImages.dir, pattern = "\\.TIF$", full.names = TRUE)

# Sort by filename to ensure correct order
image_files <- image_files[order(image_files)]
image_paths <- image_paths[order(image_files)]

# Check we have expected number of images (should be 18 = 2 reps × 9 images)
num_images <- length(image_files)
cat(sprintf("Found %d images\n", num_images))

# Define surface and state mappings
# Each group of 9 images: positions 1-3=Blank, 4-6=Before, 7-9=After
# Surface order within each group: 1, 2, 3
states <- c("Blank", "Blank", "Blank", "Before", "Before", "Before", "After", "After", "After")
surfaces <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)

# Calculate number of replications
images_per_rep <- 9
num_reps <- num_images %/% images_per_rep

# Create mapping data frame
mapping <- data.frame(
  OriginalFile = character(),
  OriginalPath = character(),
  NewFolder = character(),
  NewFilename = character(),
  Surface = integer(),
  State = character(),
  Rep = integer(),
  Pattern = character(),
  stringsAsFactors = FALSE
)

# Process each image
for (i in 1:num_images) {
  rep_num <- ((i - 1) %/% images_per_rep) + 1
  pos_in_rep <- ((i - 1) %% images_per_rep) + 1
  
  surface_num <- surfaces[pos_in_rep]
  state <- states[pos_in_rep]
  
  # Define folder name
  new_folder <- sprintf("Surface%d_Rep%d", surface_num, rep_num)
  
  # Define swabbing pattern based on surface and rep
  if (surface_num == 1) {
    pattern <- "Snake"
  } else if (surface_num == 2) {
    pattern <- "BaF"
  } else if (surface_num == 3) {
    if (rep_num == 1) {
      pattern <- "BaF"
    } else {
      pattern <- "Ratchet"
    }
  } else {
    pattern <- "Unknown"
  }
  
  # Add to mapping
  mapping <- rbind(mapping, data.frame(
    OriginalFile = image_files[i],
    OriginalPath = image_paths[i],
    NewFolder = new_folder,
    NewFilename = paste0(state, ".tif"),
    Surface = surface_num,
    State = state,
    Rep = rep_num,
    Pattern = pattern,
    stringsAsFactors = FALSE
  ))
}

# Display mapping
cat("\n=== IMAGE MAPPING ===\n")
print(mapping[, c("OriginalFile", "NewFolder", "NewFilename", "Pattern")])

# Save mapping to CSV for reference
write.csv(
  mapping,
  file = paste0(OrganizedImages.dir, "ImageMapping.csv"),
  row.names = FALSE
)
cat(sprintf("\nMapping saved to: %s\n", paste0(OrganizedImages.dir, "ImageMapping.csv")))

# === COPY AND RENAME IMAGES ===

cat("\n=== COPYING IMAGES ===\n")

for (i in 1:nrow(mapping)) {
  # Create folder if it doesn't exist
  dest_folder <- file.path(OrganizedImages.dir, mapping$NewFolder[i])
  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Define destination path
  dest_path <- file.path(dest_folder, mapping$NewFilename[i])
  
  # Copy file
  file.copy(from = mapping$OriginalPath[i], to = dest_path, overwrite = TRUE)
  
  cat(sprintf("Copied: %s -> %s/%s\n", 
              mapping$OriginalFile[i], 
              mapping$NewFolder[i], 
              mapping$NewFilename[i]))
}

cat("\n=== COMPLETE ===\n")
cat(sprintf("Organized images saved to: %s\n", OrganizedImages.dir))
cat(sprintf("Total images processed: %d\n", nrow(mapping)))
cat(sprintf("Total folders created: %d\n", length(unique(mapping$NewFolder))))

# List created folders
cat("\nCreated folder structure:\n")
created_folders <- list.dirs(OrganizedImages.dir, recursive = FALSE, full.names = FALSE)
for (folder in sort(created_folders)) {
  cat(sprintf("  %s/\n", folder))
  files_in_folder <- list.files(file.path(OrganizedImages.dir, folder))
  for (f in files_in_folder) {
    cat(sprintf("    - %s\n", f))
  }
}

