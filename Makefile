help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

serve: ## Run ghcid and the main
	nix-shell \
		--run "script/watch"

build: ## Build the app
	nix-build release.nix

test: ## Test the app
	nix-shell \
		--run "script/watch --command \"cabal repl rapt-test\""

update-deps: ## Update the deps from the .cabal file to default.nix
	nix-shell \
		--pure -p cabal2nix --run "cabal2nix ." > rapt.nix

drop-db: ## Drop the db
	rm data/sqlite.db*

.PHONY: serve serve build test update-deps drop-db