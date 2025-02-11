#!/bin/bash
# sometimes (i'm not sure why) when I pull from github after running new model fits, some output files are saved with [conflicted] in the name, 
# even though there is no other file with that name. This script can remove the [conflicted] from the filename. 
# Set the target directory (adjust each time)
target_dir="Recovery/v-exp/Fits_recovery/"

# Loop through all .Rdata files that contain " [conflicted].Rdata"
for file in "$target_dir"*" [conflicted].Rdata"; do
    # Check if file exists (handles case where glob doesn't match anything)
    [ -e "$file" ] || continue
    
    # Construct new filename by removing " [conflicted]" using sed
    new_file=$(echo "$file" | sed 's/ \[conflicted\]//')
    
    # Rename the file
    mv "$file" "$new_file"
    echo "Renamed: $file -> $new_file"
done
