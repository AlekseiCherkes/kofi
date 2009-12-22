create table Account (
	acc_id char(13) not null,
	bank_bic char(9) not null,
	owner_unp char(13) not null,
	ballance money not null,
	open_date datetime not null,
	close_date datetime null,
	primary key (acc_id, bank_bic),
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
	payer_final_balance money null,
	bnfc_acc_id char(13) not null,
	bnfc_bank_unp char(9) not null,
	bnfc_final_balance money null,
	amount money not null,
	priority int not null,
	primary key (trn_id),
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

insert into Status 
values (0, "Success.");
insert into Status 
values (1, "Invalid payer's BIC.");
insert into Status 
values (2, "Invalid beneficiary's BIC.");
insert into Status 
values (3, "Invalid payer's account.");
insert into Status 
values (4, "Invalid beneficiary's account.");
insert into Status 
values (5, "Payer hasn't this account.");
insert into Status 
values (6, "Payer is closed already.");
insert into Status 
values (7, "Beneficiary is closed already.");
insert into Status 
values (8, "Payer's account is closed already.");
insert into Status 
values (9, "Beneficiary's account is closed already.");
insert into Status 
values (10, "Payer hasn't enough money for commited this transaction.");