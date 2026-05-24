#!/bin/bash
# dicom-convert.sh thumb <src> <dst> <size>
# dicom-convert.sh image <src> <dst>
# dicom-convert.sh video <src> <dst> <fps>

set -euo pipefail

[[ $# -eq 3 && "$1" == "image" || $# -eq 4 ]] ||
    (echo "Not enough arguments" && exit 1)

cmd=$1
src=$2
dst=$3
tmp="${dst}.tmp"
conv="${dst}.conv"

cleanup() {
    rm -f -- "$tmp" "$conv"
}

trap cleanup EXIT INT TERM HUP

case "$cmd" in
    thumb)
        size=$4
        dcm2img --write-png --scale-y-size "$size" "$src" "$tmp" ||
            magick "$src"[0] -resize x"$size" "png:$tmp" ||
            magick "$src"[-1] -resize x"$size" "png:$tmp" ||
            (gdcmconv --raw "$src" "$conv" &&
                 dcm2img --write-png --scale-y-size "$size" "$conv" "$tmp") || exit 1
        ;;
    image)
        dcm2img --write-png "$src" "$tmp" ||
            magick "$src"[0] "png:$tmp" ||
            magick "$src"[-1] "png:$tmp" ||
            (gdcmconv --raw "$src" "$conv" &&
                 dcm2img --write-png "$conv" "$tmp") || exit 1
        ;;
    video)
        fps=$4
        (
            dcm2img --all-frames --write-bmp "$src" ||
                magick "$src" bmp:- ||
                (gdcmconv --raw "$src" "$conv" && dcm2img --all-frames --write-bmp "$conv") || exit 1
        ) | ffmpeg -y -framerate "$fps" -i - -f mp4 "$tmp" || exit 1
        ;;
    *)
        echo "Invalid command $cmd"
        exit 1
        ;;
esac

mv -f -- "$tmp" "$dst"
