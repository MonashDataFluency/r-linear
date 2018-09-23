
RMDS=index.Rmd \
     slides/linear_thinking.Rmd \
     topics/linear_models.Rmd \
     topics/answers.Rmd

HTMLS=$(patsubst %.Rmd,%.html,$(RMDS))

# Create stripped down versions of .Rmd files
#RS=r-more-files/programming.R \
#   r-more-files/tidyverse.R \
#   r-more-files/sequences_and_features.R

# Create unevaluated versions (compact teacher's notes)
#UNEVALS=topics/programming_uneval.html \
#        topics/tidyverse_uneval.html \
#        topics/sequences_and_features_uneval.html

all : $(HTMLS) r-linear-files.zip

%.html : %.Rmd diagram.R topics/_output.yaml slides/style.css
	Rscript -e 'rmarkdown::render("$<", "all")'


#%_uneval.html : %.Rmd Makefile
#	python unevalify.py <$< >topics/temp.Rmd
#	Rscript -e 'rmarkdown::render("topics/temp.Rmd", "all")'
#	mv topics/temp.html $@
#	rm topics/temp.Rmd

r-linear-files/%.R : topics/%.Rmd purify.py
	python3 purify.py <$< >$@

r-linear-files.zip : r-linear-files/linear_models.R r-linear-files/*
	zip -FSr r-linear-files.zip r-linear-files

clean :
	rm -f $(HTMLS)
