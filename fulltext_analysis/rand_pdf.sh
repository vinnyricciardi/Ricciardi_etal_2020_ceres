#!/bin/bash
find . -name *pdf | sort -R |tail -200 | while read file; do cp "$file" "input/samplePDFs/"; done