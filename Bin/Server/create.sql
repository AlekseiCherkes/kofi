create table Account (
	acc_id char(13) not null,
	bank_bic char(9) not null,
	owner_unp char(13) not null,
	ballance money not null,
	open_date datetime not null,
	close_date datetime null,
	primary key (account_id, bank_id),
	foreign key(owner_unp)
		references Company(company_unp)
		on delete cascade
		on update cascade
);
					  
create table Company (
	company_unp char(13) not null,
	company_name varchar(256) not null,
	registry_date datetime  not null,
	unregistry_date datetime  null,
	open_key varchar(1024)  not null,
	primary key (company_unp)
);
					  
create table CommitedTransaction (
	trn_id int not null,
	commit_date datetime not null,
	recive_date datetime not null,
	status_id int not null,
	content varchar(4000) not null,
	reason varchar(1000) not null,
	payer_acc_id char(13) not null,
	payer_bank_bic char(9) not null,
	payer_final_balance money not null,
	bnfc_acc_id char(13) not null,
	bnfc_bank_unp char(9) not null,
	bnfc_final_balance money not null,
	amount money not null,
	priority int not null,
	primary key (transaction_id),
	foreign key(payer_acc_id)
		references Account(acc_id),
	foreign key(payer_bank_bic)
		references Account(bank_bic),
	foreign key(bnfc_acc_id)
		references Account(acc_id),
	foreign key(bnfc_bank_unp)
		references Account(bank_unp),
	foreign key(status_id)
		references status(status_id)
);

create table Status (
	status_id int primary key,
	message varchar(256) not null
);