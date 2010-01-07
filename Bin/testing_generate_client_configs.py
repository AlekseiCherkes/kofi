#-*- coding: utf-8 -*-

import os
import random
import sqlite3 as db

server_db_path = 'Server/server.db'
client_dir = 'Client/Profiles'

def create_client_db(con):
    cur = con.cursor()
    # Config table always has only one row
    cur.execute('''create table Config (
	                   unp char(13) not null,
	                   name varchar(256) not null,
                       date datetime not null,
                       recv_key varchar(1024) not null,
                       send_key varchar(1024) not null
                    ); ''')
    cur.execute('''create table Company (
	                   company_unp char(13) not null,
	                   company_name varchar(256) not null,
	                   primary key (company_unp)
                    ); ''')
    cur.execute('''create table Account (
	                   acc_id char(13) not null,
	                   company_unp char(13) not null,
	                   bank_bic char(9) not null,
	                   primary key (acc_id, bank_bic),
	                   foreign key (company_unp)
		                  references Company(company_unp)
                    );''')
    cur.execute('''create table Statement(
	                   statement_id integer not null,
	                   start_date datetime not null,
	                   end_date datetime not null,
	                   acc_id char(13) not null,
	                   bank_bic char(9) not null,
	                   statement_text varchar(2000) not null,
	                   primary key (statement_id),
	                   foreign key (acc_id)
                            references Account(acc_id),
	                   foreign key (bank_bic)
                            references Account(bank_bic)
                    );''')
    cur.execute('''create table TransactionTemplate(
                    	transaction_template_id integer not null,
                    	tmpl_name varchar(16) not null,
                    	payer_bank_bic char(9) not null,
                    	payer_acc_id char(13) not null,
                    	bnfc_bank_bic char(9) not null,
                    	bnfc_acc_id char(13) not null,
                    	amount money not null,
                    	reason varchar(256) not null,
                    	is_urgent bit not null,
                    	primary key (transaction_template_id),
                    	foreign key (payer_acc_id)
                    		references Account(acc_id)
                    	foreign key (payer_bank_bic)
                    		references Account(bank_bic)
                    	foreign key (bnfc_acc_id)
                    		references Account(acc_id)
                    	foreign key (bnfc_bank_bic)
                    		references Account(bank_bic)
                    );''')

def fill_client_db(client_con, server_db_path, client_info):
    client_cur = client_con.cursor()
    server_con = db.connect(database = server_db_path)
    server_cur = server_con.cursor()
    try:
        client_cur.execute('''insert into Config values (?, ?, ?, ?, ?);''',
                           (client_info[0], client_info[1], client_info[2],
                           client_info[3], client_info[4]))
        client_con.commit()
        server_cur.execute('select company_unp, company_name \
                            from Company ;')
                            ##where unregistry_date is not null;')

        companies = server_cur.fetchall()
        # Create list of companies, available for our client.
        for company in companies:
            client_cur.execute('''insert into Company
                                    values (?, ?);''',
                                    (company[0], company[1]))
            client_con.commit()
        # Find all accounts for all companies.
        server_cur.execute('''select acc_id, bank_bic, owner_unp
                              from Account;''')
        accounts = server_cur.fetchall()

        # Add each of this account into client DB.
        for account in accounts:
            client_cur.execute('''insert into Account
                                  values (?,?,?);''',
                                  (account[0], account[2], account[1]))
            client_con.commit()
        # Fill Statement table.
        for account in accounts:
            # Create statements only for accounts of this client.
            if account[2] == client_info[0]:
                statement_id = str(random.randint(0, 1000000000))
                client_cur.execute('''insert into Statement
                                      values (?,
                                              current_timestamp,
                                              current_timestamp,
                                              ?,?,
                                              'statement text');''',
                                      (statement_id, account[0], account[1]))
                client_con.commit()

        if len(accounts) > 1:
            tmpl_name = str(random.randint(0, 1000000000))
            client_cur.execute('''insert into TransactionTemplate
                                  values (NULL, ?,?,?,?,?,
                                          150,
                                          'reason',
                                          1);''',
                                  (tmpl_name,
                                  accounts[0][1],
                                  accounts[0][0],
                                  accounts[1][1],
                                  accounts[1][0]))
            client_con.commit()
    finally:
        server_con.close()

if __name__ == '__main__':
    server_con = db.connect(database = server_db_path)
    server_cur = server_con.cursor()
    try:
        server_cur.execute('select company_unp, company_name, registry_date, \
                            client_recv_key, client_send_key from Company;')
        clients_info = server_cur.fetchall()
    finally:
        server_con.close()

    # Remove old dir.
    for root, dirs, files in os.walk(client_dir):
        for _file in files:
            full_path = os.path.join(root, _file)
            os.remove(full_path)
    try:
        os.rmdir(client_dir)
    except:
        pass

    # Create directory for clients DBs.
    try:
        os.mkdir(client_dir)
    except:
        pass
    # Create clients DBs.
    for client_info in clients_info:
        client_name = client_dir + '/' + client_info[0] + '.db'
        client_con = db.connect(database = client_name)
        try:
            create_client_db(client_con)
            fill_client_db(client_con, server_db_path, client_info)
        finally:
            client_con.close()

