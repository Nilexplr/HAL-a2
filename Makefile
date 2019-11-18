##
## hal
## Makefile
##

PROJECT_NAME	=	hal

all: $(PROJECT_NAME)

$(PROJECT_NAME):
	stack install --local-bin-path . --exec 'mv hal-exe hal'

tests_run:
	stack test

tests_run_coverage:
	stack test --coverage

clean:
	rm -f $(PROJECT_NAME)

fclean: clean

re: fclean all