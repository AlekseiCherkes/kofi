#-*- coding: utf-8 -*-

#import pysqlite2._sqlite as db
from pysqlite2 import dbapi2 as db

def create_db(db_file_name, manual):
    c = db.connect(database=db_file_name)

    delete_all_tables(c)
    create_tables(c)
    fill_tables(c, manual)

    c.close()

def delete_all_tables(c):
    '''c - connection object of datebase.'''
    cu = c.cursor()

    try:
        cu.execute("DROP TABLE Bank;")
    except db.DatabaseError, x:
        pass

    try:
        cu.execute("DROP TABLE Branch;")
    except db.DatabaseError, x:
        pass

    c.commit()


def create_tables(c):
    """c - connection object of datebase."""
    cu = c.cursor()

    #Create tables.
    cu.execute('''
        CREATE TABLE Bank (
            bank_bic varchar(3) PRIMARY KEY,
            name VARCHAR(200) NOT NULL
        );
    ''')

    cu.execute('''
        CREATE TABLE Branch (
            branch_bic varchar(9) PRIMARY KEY,
            name VARCHAR(200) NOT NULL,
            bank_bic varchar(3),
            CONSTRAINT fk_bank
            FOREIGN KEY(bank_bic)
                REFERENCES Bank
                ON DELETE CASCADE
                ON UPDATE CASCADE
        );
    ''')

    c.commit()

def fill_tables(c, manual):
    """c - connection object of database."""
    cu = c.cursor()

    for record in manual:
        if len(record) != 4:
            print 'ERROR! Corrupted parsed array.'
            return None
        # Try add new bank.
        cu.execute('''select * from Bank
                    where bank_bic = ''' + record[0] + ';')
        result = cu.fetchall()
        if result is None:
            cu.execute('INSERT INTO Bank VALUES ("' + record[0] + '","' +
             record[1] + '");')
        # Add new branch.
        if (record[2] != None) and (record[3] != None):
            c.execute('INSERT INTO Branch VALUES ("' + record[2] + '","' +
            record[3] + '","' + record[0] + '");')
    c.commit()

