#### Makefile for generating R packages.

# Bryan Hanson, modified from several versions found on the web
# + ideas & help from SO etc
# First seriously working version July 2015

# Assumes this file is in a folder where package contents are in a subfolder PKG_NAME

SHELL := /bin/bash # bash is needed for manipulation of version number

### Define variables
# For different pkgs change PKG_NAME on next line
# In some cases changes may be necessary

PKG_NAME := LearnPCA
TODAY := $(shell date +%Y-%m-%d)

# Note: the following assumes one space only after the colon
# in DESCRIPTION

PKG_VERSION := $(shell grep -i '^version' $(PKG_NAME)/DESCRIPTION | cut -d ':' -f2 | cut -d ' ' -f2)
PKG_DATE := $(shell grep -i '^date' $(PKG_NAME)/DESCRIPTION | cut -d ':' -f2 | cut -d ' ' -f2)

R_FILES := $(wildcard $(PKG_NAME)/R/*.R)
Rd_FILES := $(wildcard $(PKG_NAME)/man/*.Rd)
INST_FILES := $(wildcard $(PKG_NAME)/inst/**/*)
DESC_FILE := $(PKG_NAME)/DESCRIPTION
NAME_FILE := $(PKG_NAME)/NAMESPACE
PKG_FILES := $(R_FILES) $(Rd_FILES) $(INST_FILES) $(NEWS_FILE) $(DESC_FILE) $(NAME_FILE)

### Increment the z version of x.y.z

XYZ := $(subst ., , $(PKG_VERSION))
X := $(word 1, $(XYZ))
Y := $(word 2, $(XYZ))
Z := $(word 3, $(XYZ))
Z2 := ${shell echo $$(($(Z)+1))}
# $$ means use the shell variable, not the one in the makefile (~ scoping)
NEW_VERSION := $(addsuffix $(addprefix .,$(Z2)), $(addsuffix $(addprefix ., $(Y)), $(X)))
TGZ := $(PKG_NAME)_$(NEW_VERSION).tar.gz
TGZs := $(PKG_NAME)_*.tar.gz

### Now the various targets
.PHONY: info
.PHONY: files
.PHONY: clean

all: info doEdit build check install
novig: info doEdit buildNoVig checkNoVig install

info:
	@echo " "
	@echo "Package: " $(PKG_NAME)
	@echo "Old/New version numbers:" $(PKG_VERSION) $(NEW_VERSION)
	@echo "Old/New dates:" $(PKG_DATE) $(TODAY)
	@echo " "

files:
	@echo " "
	@echo "Package files:"
	@printf "\t%s\n" $(PKG_FILES)
	@echo " "

# This package uses roxygen style documentation
# so we have to run document on it.

build: $(PKG_FILES)
	cd $(PKG_NAME); R -e 'devtools::document()'
	R CMD build --resave-data --compact-vignettes="both" $(PKG_NAME)

buildNoVig: $(PKG_FILES)
	  cd $(PKG_NAME); R -e 'devtools::document()'
	  R CMD build --no-build-vignettes --resave-data $(PKG_NAME)

check: $(TGZ)
	R CMD check --as-cran $(TGZ)

checkNoVig: $(TGZ)
	  R CMD check --ignore-vignettes $(TGZ)

install: $(TGZ)
	R CMD INSTALL $(TGZ)

clean:
	rm -f $(TGZs)
	rm -f -r $(PKG_NAME).Rcheck

### Edit the relevant files to increment version number & date
# note: important not to quote $(NEW_VERSION)

doEdit:
	@echo " "
	@echo "Editing DESCRIPTION to increment version"
	. ./sedstr.sh; sedstr '$(PKG_VERSION)' $(NEW_VERSION) $(DESC_FILE)
	@echo "Editing DESCRIPTION to update the date"
	. ./sedstr.sh; sedstr '$(PKG_DATE)' '$(TODAY)' $(DESC_FILE)
	rm $(PKG_NAME)/DESCRIPTION.tmp
	@echo " "
