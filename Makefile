usage:
	@echo "usage: make <command> "
	@echo ""
	@echo "Available options: "
	@echo "  format             -- Apply source code formatting"

# Target to use as dependency to fail if not inside nix-shell
# The use of nix needs more thought
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "This $(MAKECMDGOALS) target requires to be run inside nix-shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "     run 'nix develop' first" && false)

# Run fourmolu formatter
.PHONY: format
format: # requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(shell fd --exclude heliumTestCases -ehs)
	cabal-fmt -i $(shell fd -ecabal)