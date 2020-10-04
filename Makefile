stack-all:
	stack-nightly build
	@echo
	stack --resolver lts-16 build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 build
	@echo
	stack --resolver lts-11 build
	@echo
	stack --resolver lts-10 build
	@echo
	stack --resolver lts-9 build
	@echo
	stack --resolver lts-8 build
	@echo
	stack --resolver lts-6 build
	@echo
	stack --resolver lts-5 build
