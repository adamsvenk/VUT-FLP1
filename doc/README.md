## Brno University of Technology - Faculty of Information Technology
## Functional and Logic Programming: Simplify-BKG
### Description
Implementation of an program, which removes useless symbols from provided context-free grammar. The implementation is based on the algorithm from the course Theoretical Computer Science. Program is implemented in functional programming language Haskell.

### Structure
```
Makefile
src
 └── Main.hs												# Main source code
doc
 └── README.md
test
 ├── test01-1.out
 ├── test01-2.out
 ├── test01.in
 ├── test01-i.out
 ├── test02-1.out
 ├── test02-2.out
 ├── test02.in
 ├── test02-i.out
 ├── test03-1.out
 ├── test03-2.out
 ├── test03.in
 └── test03-i.out
```


 
### Getting started
The whole program is based in the file `Main.hs`. For the compiling, you will need `make` and `ghc` utilities.

    $ ./flp21-fun [option][input]
   

 - `[option]`
	 - `[-i]` print loaded context-free grammar based on the input
	 - `[-1]` print context-free grammar after the first step of the Algorithm 4.3
	 - `[-2]` print context-free grammar after the second step of the Algorithm 4.3
- `[input]`
	- path to input file
	- if no file is provided, the program will read it input from `stdin`

### Contact
If you have any questions, feel free to contact me at xsvenk00@stud.fit.vutbr.cz

