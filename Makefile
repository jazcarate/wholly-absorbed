help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

serve: ## Run ghcid and the main
	nix-shell \
		--run "ghcid --poll --test :main"

build: ## Build the app
	nix-build release.nix

test: ## Test the app
	nix-shell \
		--run "ghcid --poll --command \"cabal repl test\" --test :main"

update-deps: ## Update the deps from the .cabal file to default.nix
	nix-shell \
		--pure -p cabal2nix --run "cabal2nix ." > default.nix

.PHONY: serve serve build test