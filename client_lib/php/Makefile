PHPDOCTOR?=/usr/local/phpdoctor

foo:
	@echo 'Usage: make {docs|test}'

docs:
	@echo "Uses PHPDoctor, get it from http://peej.github.com/phpdoctor"
	${PHPDOCTOR}/phpdoc.php docs.ini

test:
	php unit_tests.py
