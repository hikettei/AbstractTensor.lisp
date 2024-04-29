
ROS           := ros
QUICKLOAD     := --load cl-polyhedral.asd --eval '(progn (cl:push (cl:pathname "./") ql:*local-project-directories*) (asdf:load-asd "abstracttensor.asd") (asdf:load-system "abstracttensor"))'

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Builds CLI Tool using roswell
	$(ROS) build ./roswell/caten.ros

.PHONY: test
test: ## Runs test harness
	$(ROS) $(QUICKLOAD) --eval '(asdf:test-system "abstracttensor")'

