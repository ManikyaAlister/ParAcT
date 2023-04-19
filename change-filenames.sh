# Define the directory containing the R scripts to modify
R_SCRIPT_DIR="/Recovery/a-exp-mispecification/Fits_recovery"

# Loop over each R script in the directory and rename the file
for script_file in "$R_SCRIPT_DIR"/*simple*.R; do
  new_script_file=$(echo "$script_file" | sed "s/a-exp-mispecification/a-exp-mispecification/")
  mv "$script_file" "$new_script_file"
done