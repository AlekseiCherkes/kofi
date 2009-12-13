create table Account (
	account_id char(13) not null,
	bank_id char(9) not null,
	owner_id char(13) not null,
	ballance double precision not null,
	open_date timestamp not null,
	close_date timestamp null,
	primary key (account_id, bank_id),
	foreign key(owner_id)
		references Company(company_unp)
		on delete cascade
		on update cascade
);
					  
create table Company (
	company_unp char(13) not null,
	name varchar(256) not null,
	registry_date timestamp  not null,
	unregistry_date timestamp  null,
	open_key varchar(1024)  not null,
	primary key (company_unp)
);
					  
create table CommitedTransaction (
	transaction_id int not null,
	commit_date timestamp not null,
	recive_date timestamp not null,
	status_id int not null,
	content text not null,
	reason text not null,
	payer_account_id char(13) not null,
	payer_bank_id char(9) not null,
	payer_final_balance double precision not null,
	beneficiary_account_id char(13) not null,
	beneficiary_bank_id char(9) not null,
	beneficiary_final_balance double precision not null,
	amount double precision not null,
	priority int not null,
	primary key (transaction_id),
	foreign key(payer_account_id)
		references Account(account_id),
	foreign key(payer_bank_id)
		references Account(bank_id),
	foreign key(beneficiary_account_id)
		references Account(account_id),
	foreign key(beneficiary_bank_id)
		references Account(bank_id),
	foreign key(status_id)
		references status(status_id)
);

create table Status (
	status_id int primary key,
	message text not null
);