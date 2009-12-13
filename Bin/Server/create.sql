CREATE TABLE Account (
	account_id char(13) not null,
	bank_id char(9) not null,
	owner_id char(13) not null,
	ballance double precision not null,
	open_date timestamp not null,
	close_date timestamp null,
	primary key (account_id, bank_id),
	FOREIGN KEY(owner_id)
		REFERENCES Client(client_unp)
		ON DELETE CASCADE
		ON UPDATE CASCADE
);
					  
CREATE TABLE Client (
	client_unp char(13) not null,
	name text  not null,
	registry_date timestamp  not null,
	unregistry_date timestamp  null,
	open_key varchar(200)  not null,
	primary key (client_unp)
);
					  
CREATE TABLE CommitedTransaction (
	transaction_id int not null,
	commit_date timestamp not null,
	recive_date timestamp not null,
	status_id int not null,
	content text not null,
	reason text not null,
	payer_account_id varchar(9) not null,
	payer_bank_id varchar(9) not null,
	payer_final_balance double precision not null,
	beneficiary_account_id varchar(9) not null,
	beneficiary_bank_id varchar(9) not null,
	beneficiary_final_balance double precision not null,
	amount double precision not null,
	priority int not null,
	primary key (transaction_id),
	FOREIGN KEY(payer_account_id)
		REFERENCES Account(account_id),
	FOREIGN KEY(payer_bank_id)
		REFERENCES Account(bank_id),
	FOREIGN KEY(beneficiary_account_id)
		REFERENCES Account(account_id),
	FOREIGN KEY(beneficiary_bank_id)
		REFERENCES Account(bank_id),
	FOREIGN KEY(status_id)
		REFERENCES Status(status_id)
);

CREATE TABLE Status (
	status_id int primary key,
	message text not null
);