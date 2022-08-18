*name*=cns

.PHONY: run all test coverage clean help

default: all

run:
	swish main.ss

all:
	swish-build -o $(*name*) main.ss -b scheme --rtlib swish --libs-visible

test:
	swish-test --progress test --report mat-report.html .

coverage:
	swish-test --progress test --report mat-report.html \
		--save-profile profile.data --coverage coverage.html .

clean:
	$(RM) $(*name*) $(*name*).boot
	$(RM) *.so *.mo *.wpo *.sop *.ss.html
	$(RM) mat-report.html profile.data coverage.html

help:
	@echo "make run       -- run main.ss"
	@echo "make/make all  -- build project(include \`$(*name*)\` and \`$(*name*).boot\` file)"
	@echo "make test      -- test this project"
	@echo "make coverage  -- test and coverage this project"
	@echo "make clean     -- clean this project"
	@echo "make help      -- show this help"
