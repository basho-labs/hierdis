PROJECT = hierdis

REBAR := $(shell which rebar 2>&1 >/dev/null; if [ $$? -eq 0 ]; then which rebar; else echo $(shell pwd)/rebar; fi)
REBAR_BUILD_DIR := $(shell pwd)/.rebar-build

COVERAGE ?= 0
TEST ?= 0

V ?= 0

rebar_args_3 = -v 3
rebar_args_2 = -v 2
rebar_args_1 = -v 1
rebar_args = $(rebar_args_$(V))

rebar_verbose_0 = @echo ":: REBAR" $(@F);
rebar_verbose = $(rebar_verbose_$(V))

rebar = $(rebar_verbose) V=$(V) TEST=$(TEST) $(REBAR) $(rebar_args)

DIALYZER ?= dialyzer

dialyzer_verbose_0 = @echo ":: DIALYZER" $(@F);
dialyzer_verbose = $(dialyzer_verbose_$(V))

dialyzer = $(dialyzer_verbose) $(DIALYZER)

.PHONY: deps compile build clean appclean distclean docs xref build-plt \
	check-plt dialyze coverage ct eunit test-deps test-compile \
	test-build test-appclean test-clean test

all: deps build

deps: $(REBAR)
	$(rebar) update-deps
	$(rebar) get-deps
	$(rebar) check-deps

compile: $(REBAR)
	$(rebar) skip_deps=true compile

build: $(REBAR)
	$(rebar) compile

clean: $(REBAR)
	$(rebar) clean

appclean: $(REBAR)
	$(rebar) skip_deps=true clean

distclean: clean
	$(rebar) delete-deps

##
## Docs
##
docs: $(REBAR)
	$(rebar) skip_deps=true doc

xref:
	$(rebar) xref

##
## Dialyzer
##
PLT ?= .$(PROJECT).plt
PLT_DEPS ?= asn1 compiler crypto edoc erts gs hipe inets kernel \
	observer public_key runtime_tools sasl ssl stdlib syntax_tools \
	tools webtool xmerl
PLT_APPS ?= .
DIALYZER_OPTS ?= -Werror_handling -Wno_return -Wrace_conditions \
	-Wunmatched_returns

build-plt: clean compile
	$(dialyzer) --build_plt --output_plt $(PLT) --apps $(PLT_DEPS) $(PLT_APPS) \
		deps/*/ebin ebin

check-plt: $(PLT)
	$(dialyzer) --check_plt --plt $(PLT) --apps $(PLT_DEPS) $(PLT_APPS) \
		deps/*/ebin ebin

dialyze: $(PLT)
	$(dialyzer) $(DIALYZER_OPTS) --plt $(PLT) deps/*/ebin ebin

$(PLT):
	$(MAKE) build-plt

##
## Tests
##
coverage: COVERAGE=1
coverage: test

ct: TEST=1
ct:
	$(rebar) skip_deps=true ct

eunit: TEST=1
eunit:
	$(rebar) skip_deps=true eunit

test-deps: TEST=1
test-deps: deps

test-compile: TEST=1 EUNIT_NOAUTO=1
test-compile: compile

test-build: TEST=1 EUNIT_NOAUTO=1
test-build: build

test-clean: TEST=1
test-clean: clean

test-appclean: TEST=1
test-appclean: appclean

test: test-clean test-deps test-build ct

##
## rebar
##
rebar: $(REBAR)

$(REBAR):
	@rm -rf $(REBAR_BUILD_DIR)
	git clone git://github.com/rebar/rebar.git $(REBAR_BUILD_DIR)
	cd $(REBAR_BUILD_DIR) && ./bootstrap
	mv $(REBAR_BUILD_DIR)/rebar $(REBAR)
	@rm -rf $(REBAR_BUILD_DIR)
