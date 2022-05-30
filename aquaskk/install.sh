#! /usr/bin/env bash
set -e -x

SCRIPT_DIR=$(cd $(dirname $0); pwd)
REAL_KEYMAP_FILE_PATH=${SCRIPT_DIR}/keymap.conf
LINK_KEYMAP_FILE_PATH="${HOME}/Library/Application Support/AquaSKK/keymap.conf"
ln -sf ${REAL_KEYMAP_FILE_PATH} "${LINK_KEYMAP_FILE_PATH}"
echo "Installed AquaSKK Keymap file. Let's reload configuration on AquaSKK Menu."
