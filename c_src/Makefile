HIREDIS_VSN ?= v0.11.0
HIREDIS_DIR ?= $(CURDIR)/hiredis
HIREDIS_GIT ?= git://github.com/redis/hiredis.git

.PHONY: all clean delete-deps get-deps

all: $(HIREDIS_DIR)/libhiredis.a

clean:
	@if [ -f $(HIREDIS_DIR)/Makefile ]; then \
		$(MAKE) -C $(HIREDIS_DIR) clean; \
	else \
		true; \
	fi

delete-deps:
	@if [ -f $(HIREDIS_DIR)/Makefile ]; then \
		rm -rf $(HIREDIS_DIR); \
	else \
		true; \
	fi

get-deps:
	@if [ ! -f $(HIREDIS_DIR)/Makefile ]; then \
		git clone -n $(HIREDIS_GIT) $(HIREDIS_DIR) && \
		(cd $(HIREDIS_DIR) && git checkout -q $(HIREDIS_VSN)); \
	else \
		true; \
	fi

$(HIREDIS_DIR)/libhiredis.a: get-deps
	$(MAKE) -C $(HIREDIS_DIR)
