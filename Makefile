
run:
	maturin develop
	python test.py

test:
	pytest tests