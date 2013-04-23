### Makefile --- Personal makefile for the Milonga theme

## Copyright (C) 2013 Didier Verna

## Author: Didier Verna <didier@didierverna.net>
## Created: 5 Mar 2013

## This file is part of Milonga.

## Milonga is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 2,
## as published by the Free Software Foundation.

## Milonga is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


### Commentary:

## Please use GNU Make with this makefile.


### Code:

THEME_NAME := milonga
THEME_FILE := $(THEME_NAME)-theme.el
SIG_FILE   := $(THEME_FILE).asc

VERSION := $(shell grep -m 1 "^;; Version: " $(THEME_FILE) \
		 | sed 's|;; Version: ||')
VERSION_FILE := $(THEME_NAME)-version.txt

WWW_HOST := www
WWW_DIR  := ~/www/software/elisp


all:

clean:
	rm -f *~

distclean: clean
	rm -f $(SIG_FILE) $(VERSION_FILE)

install-www: $(SIG_FILE)
	echo "$(VERSION)" > $(VERSION_FILE)
	scp -p $(THEME_NAME)-version.txt $(THEME_FILE) $(SIG_FILE) \
	  $(WWW_HOST):$(WWW_DIR)/
	rm -f $(VERSION_FILE)

tag:
	git tag 'version-$(VERSION)'


%.asc: %
	gpg -a -b $<


.PHONY: tag

### Makefile ends here
