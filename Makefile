##
## hal
## Makefile
##

PROJECT_NAME	=	hal

all: $(PROJECT_NAME)

$(PROJECT_NAME):
	stack install --local-bin-path . --exec 'mv HAL-a2-exe hal'

tests_run:
	stack test

tests_run_coverage:
	stack test --coverage --allow-different-user

clean:
	rm -f $(PROJECT_NAME)
	stack clean

fclean: clean

re: fclean all