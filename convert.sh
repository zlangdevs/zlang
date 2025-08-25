#!/bin/bash

BLACKLIST_DIRS="zig-out,.git,.zig-cache"

convert_extensions() {
    local zig_count=0
    local txt_count=0

    local find_cmd="find . -type f \( -name \"*.zig\" -o -name \"*.txt\" \)"
    
    IFS=',' read -ra DIRS <<< "$BLACKLIST_DIRS"
    for dir in "${DIRS[@]}"; do
        find_cmd+=" -not -path \"*/${dir}/*\""
    done

    while IFS= read -r -d '' file; do
        if [[ "$file" == *.zig ]]; then
            new_name="${file%.zig}.txt"
            if [[ -e "$new_name" ]]; then
                echo -e "\033[33m[WARNING]\033[0m File '$new_name' already exists, skipping '$file'"
            else
                mv -- "$file" "$new_name"
                ((zig_count++))
                echo -e "\033[32m[SUCCESS]\033[0m Renamed: $file -> $new_name"
            fi
        elif [[ "$file" == *.txt ]]; then
            new_name="${file%.txt}.zig"
            if [[ -e "$new_name" ]]; then
                echo -e "\033[33m[WARNING]\033[0m File '$new_name' already exists, skipping '$file'"
            else
                mv -- "$file" "$new_name"
                ((txt_count++))
                echo -e "\033[32m[SUCCESS]\033[0m Renamed: $file -> $new_name"
            fi
        fi
    done < <(eval "$find_cmd -print0")

    echo "$zig_count $txt_count"
}

echo "Processing files..."
result=$(convert_extensions)
zig_count=$(echo $result | cut -d' ' -f1)
txt_count=$(echo $result | cut -d' ' -f2)

echo "--------------------------------"
echo -e "\033[36m[SUMMARY]\033[0m"
echo "Converted .zig -> .txt: $zig_count files"
echo "Converted .txt -> .zig: $txt_count files"
echo -e "\033[32mOperation completed successfully!\033[0m"