.PHONY: build test test-backend test-frontend run clean install-deps setup-mathjax

build:
	dune build

test: test-backend test-frontend

test-backend:
	dune exec test_backend
	dune exec test_api_security

test-frontend:
	@echo "Starting server in background for Selenium tests..."
	@./_build/install/default/bin/mlocid &
	@SERVER_PID=$$!; \
	sleep 3; \
	python3 test_selenium.py || EXIT_CODE=$$?; \
	kill $$SERVER_PID 2>/dev/null || true; \
	exit $$EXIT_CODE

run:
	dune exec mlocid

clean:
	dune clean
	rm -f mlocid.db

install-deps:
	opam install . --deps-only

setup-mathjax:
	@echo "Downloading MathJax..."
	@mkdir -p static/mathjax
	@cd static/mathjax && \
	if [ ! -d "es5" ]; then \
		wget -q https://github.com/mathjax/MathJax/archive/refs/tags/3.2.2.tar.gz && \
		tar -xzf 3.2.2.tar.gz && \
		mv MathJax-3.2.2/es5 . && \
		rm -rf MathJax-3.2.2 3.2.2.tar.gz; \
	fi

install-selenium-deps:
	pip3 install selenium webdriver-manager
