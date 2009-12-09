===========================================================================
Using:	python update_banks_manual.py [input_file] [output_file]
===========================================================================
input_file: 
	1) Must has UTF-8 coding.
	2) Can't contain double quotes symbols.
	3) File must have next structure:
        333 Bank_name_1
            999999999 Bank_1_branch_1
            999999999 Bank_1_branch_2
        333 Bank_name_2
            999999999 Bank_2_branch_1
        ...
	    It means: First word in line must be 3- or 9-digit number (bank's or bank branche's BICs).
	    The rest of the line is name of bank (or bank's branche). Name can't be empty.
	    So, line must contain at least 2 words.