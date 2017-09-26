Readme
Tell Johnson

Goal: The goal of the changes to my code is to enable variables to have
      subranges. This means that the programmer can say that a variable has to
			have values within a certain range and if a variable doesn't then the
			code shouldn't work. My code should be able to declare integers and 
			characters with subranges, assign proper values to them, be able to use
			them for operations, and tell if a read in value for the variable is valid.
			


Compilation: To compile the code use the following commands:
						 flex oal.l
						 bison oal.y
						 g++ oal.y -o oal
						 flex johnsont.l
						 bison johnsont.y
						 g++ johnsont.y -o mipl
						 
						 To run the code use the following commands:
						 mipl testName.txt > oalFile
						 oal oalFile

Test 1: Should show a successful use of subrange variables with multiple
        assignments and operations.
Test 2: The mipl code should fail to compile because of assigning a value that
        is too small.
Test 3: The mipl code should fail to compile because of assigning a value that
        is too large. This is caused by an addition of two numbers and the
				resulting value being too large. Because these numbers are constants
				the error can be caught at compile time.
Test 4: The mipl code should fail to compile because of assigning a value that
        is too large.
Test 5: Should show a successful read of both a character and integer.
Test 6: Should show an unsuccessful read of an character. This
        should fail at runtime and output an SRE error message which stands
				for subrange error and the program should end once the mistake is found.
Test 7: Should show an unsuccessful read of an integer. This
        should fail at runtime and output an SRE error message which stands
				for subrange error and the program should end once the mistake is found.
Test 8: Should fail at runtime with a SRE error because of assigning a value 
        that is too large. Because one of these numbers is a variable this
				error can't be caught till runtime.
Test 9: Should fail at runtime with a SRE error because of assigning a value 
        that is too large. Because one of these numbers is a variable this
				error can't be caught till runtime.