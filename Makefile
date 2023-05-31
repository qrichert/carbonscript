ERROR := \033[0;91m
INFO := \033[0;94m
NC := \033[0m

define show_help_message
	echo "Usage: make TARGET"
	echo ""
	echo "Commands:"
	grep -hE '^[A-Za-z0-9_ \-]*?:.*##.*$$' $(MAKEFILE_LIST) | \
	    awk 'BEGIN {FS = ":.*?## "}; {printf "  $(INFO)%-12s$(NC) %s\n", $$1, $$2}'
endef

define show_error_message
	echo "$(ERROR)[Error] $(1)$(NC)"
endef

SOURCE_DIRS := carbonscript tests

.PHONY: all
all: help

.PHONY: help
help: ## Show this help message
	@$(show_help_message)

.PHONY: l
l: lint
.PHONY: lint
lint: ## Run various linting tools
	@isort $(SOURCE_DIRS)
	@black $(SOURCE_DIRS)
	@prettier -w *.md docs/

.PHONY: t
t: test
.PHONY: test
test: ## Run unit tests
	@python -m unittest

.PHONY: hc
hc: healthcheck
.PHONY: healthcheck
healthcheck: ## Run various code inspection tools
	@pylint $(SOURCE_DIRS) || true

%:
	@$(call show_error_message,Unknown command '$@')
	@$(show_help_message)
