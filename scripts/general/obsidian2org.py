#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os

# This script converts an Obsidian vault to an Org-mode file.
# You need to run previously the following command to export all the notes:

# find . -iname *.md -type f -exec sh -c pandoc "$1" --wrap=preserve -o "${1%.md}.org" _ {} ;

# Then you can run this script to add an Org-roam header to each note.

# Usage: ./obsidian2org.py <path-to-vault>

def main():
    vault = sys.argv[1]
    for root, dirs, files in os.walk(vault):
        for file in files:
            if file.endswith(".org"):
                content = ""
                with open(os.path.join(root, file), 'r') as f:
                    content = f.read()
                with open(os.path.join(root, file), 'w') as f:
                    text = ":PROPERTIES:\n:ID:\t" + str(abs(hash(file[:-4]))) + "\n:END:\n"
                    text += "#+TITLE: " + file[:-4].replace("_", " ").title() + "\n"
                    text += "#+AUTHOR: Lucas Elvira Martín\n"
                    text += "#+ROAM_KEY: " + file[:-4] + "\n"
                    text += "* :TOC:\n"
                    text += content
                    f.write(text)


main()
