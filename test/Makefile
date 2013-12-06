TEST = 1

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard -DTEST=1# \
	#+'{parse_transform, eunit_autoexport}'

ERL_LIBS = $(abspath ../deps):$(abspath ../)

erlc = ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) -o $(shell pwd)

.PHONY: build

all: build

build:
	$(erlc) $(wildcard *.erl */*.erl)
