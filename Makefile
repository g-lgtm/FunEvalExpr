##
## EPITECH PROJECT, 2021
## makefile
## File description:
## makefile
##

NAME	=	funEvalExpr

BINNAME	=	fun-evalexpr-exe

RM	=	rm -rf

all:	$(NAME)

$(NAME):
		@stack build --copy-bins --local-bin-path ./
		@mv $(BINNAME) $(NAME)

tests_run: re
		@stack test

clean:
		$(RM) *.hi *.o .stack-work *~

fclean:	clean
		$(RM) $(NAME)

re:	fclean all

.PHONY:	all re clean fclean
