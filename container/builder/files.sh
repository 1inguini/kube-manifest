#!/bin/sh

# needs moreutls (sponge)

unalias -a
set -eu
set -x

# check number of args
: "$1"

files="$(mktemp)"

# cleanup
trap "
rm -rf $files
" EXIT

# $PACMAN -Qql filesystem | sed -e '\:/usr/.:d' >> "$files"

$PACMAN -S --noconfirm "$@"

$PACMAN -Qql "$@" |
  sed -e '\:^/usr/share:d' |
  tee -a "$files" |
  xargs ldd 2>/dev/null |
  grep -Po ' /[^ ]*' |
  sed -e 's/^ //' >> "$files"

# 必要じゃないかも
printf '%s\n' '/usr/lib/locale/locale-archive' >> "$files"
printf '%s\n' '/etc/ld.so.cache' >> "$files"

sort <"$files" | uniq | sponge "$files"
# xargs readlink -f <"$files" | sponge -a "$files"

sed -i "$files" -e 's:^/:./:'

mkdir /tmp/files
sudo tar --dereference --ignore-failed-read --no-recursion -C / --verbatim-files-from -T "$files" -cpf - |
  sudo tar -C /tmp/files/ -xpf -