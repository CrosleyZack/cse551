#!/bin/bash

pdflatex -shell-escape \\nonstopmode\\input $1.tex &&\
bibtex $1 &&\
pdflatex -shell-escape \\nonstopmode\\input $1.tex &&\
pdflatex -shell-escape \\nonstopmode\\input $!.tex
