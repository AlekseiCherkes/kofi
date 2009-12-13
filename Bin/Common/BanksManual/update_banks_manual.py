#-*- coding: utf-8 -*-
import os
import sys
from create_db import create_db

def read_manual(file_name):
    '''
    Parse lines in text file and save parsing info in array.
    File must have structure:
        333 Bank_name_1
            999999999 Bank_1_branch_1
            999999999 Bank_1_branch_2
        333 Bank_name_2
            999999999 Bank_2_branch_1
        ...
    It means: First word in line must be 3- or 9-digit number (bank's or bank
        branche's BICs).
        The rest of the line is name of bank (or bank's branche). Name can't be
        empty.
    So, line must contain at least 2 words.
    Arguments:
        file_name - name of text file for parsing.
    Return:
        Array of tuples. Each tuple has next structure:
            (bank_bic, bank_name, bank_branch_bic, bank_branch_name)
        It's allowed have bank without branches. In this case tuple has next
        structure:
            (bank_bic, bank_name, None, None)
    '''
    manual = []
    bank_bic = None
    bank_name = None
    branch_count = 0
    is_line_first = True
    # I have next problem: if file's coding is UTF-8, sometimes this file has
    #extra 3 symbols at begining. I don't know what is it.
    for line in open(file_name):
        if is_line_first == True:
            line = line[3:]
            is_line_first = False
        words = line.split()
        x = len(words[0])
        # Each string must contain at least 2 substrings: BIC and name.
        if len(words) < 2:
            print 'ERROR! Each string must contain at least 2 words: BIC and bank name.'
            return None
        # Bank's BIC has 3 digits.
        if len(words[0]) == 3:
            try:
                int(words[0])
            except:
                print 'ERROR! Bank\'s BIC must be 3-digit number.'
                return None
            if (bank_bic != None) and (bank_name != None) and branch_count == 0:
                record = [bank_bic, bank_name, None, None]
                manual.append(record)
            bank_bic = words[0]
            bank_name = "".join([word + ' ' for word in words[1:]])
            branch_count = 0
            continue
        # Bank branch's BIC has 9 digits.
        if len(words[0]) == 9:
            try:
                int(words[0])
            except:
                print 'ERROR! Bank branche\'s BIC must be 9-digit number.'
                return None
            record = [bank_bic, bank_name, words[0], \
                "".join([word + ' ' for word in words[1:]])]
            manual.append(record)
            branch_count += 1
            continue
        else:
            print 'ERROR! Invalid file format. Each string must starts with 3- or 9- \
digits number (BIC).'
            return None
    # It's bank without branches.
    if (bank_bic != None) and (bank_name != None) and branch_count == 0:
        record = [bank_bic, bank_name, None, None]
        manual.append(record)
    return manual

if __name__ == '__main__':
    manual_name = None
    db_name = None
    # Read arguments.
    if len(sys.argv) == 1:
        manual_name = 'Manual.txt'
        db_name = 'manual.db'
    elif len(sys.argv) == 2:
        manual_name = sys.argv[1]
        db_name = manual_name[:-3]
    elif len(sys.argv) == 3:
        manual_name = sys.argv[1]
        db_name = sys.argv[2]
    else:
        print 'Error. Invalid arguments count. Use "python \
update_banks_manual.py [input_file] [output_file]" syntax.'
        exit()

    if not os.path.exists(manual_name):
        print 'Error. File "' + manual_name + '" doesn\'t exist.'
        exit()
    manual = read_manual(manual_name)
    if manual != None:
        try:
            create_db(db_name, manual)
        except Exception,  x:
            print "Error: ", x