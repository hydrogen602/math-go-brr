.PHONY: run build-dev test

run: build-dev
	python test.py

build-dev:
	maturin develop

test: build-dev
	pytest tests