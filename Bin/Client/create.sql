create table Company (
	company_unp char(13) not null,
	company_name varchar(256) not null,
	primary key (company_unp)
);
		
create table Beneficiary (
	beneficiary_unp char(13) not null,
	primary key (beneficiary_unp),
	foreign key (beneficiary_unp)
		references Company(company_unp)
);

create table Account (
	acc_id char(13) not null,
	company_unp char(13) not null,
	bank_bic char(9) not null,
	primary key (acc_id, bank_bic),
	foreign key (company_unp)
		references Company(company_unp),
/*???*/	foreign key (bank_bic)
		references Bank(bank_bic)
);

create table Statement(
	statement_id int not null,
	start_date timestamp not null,
	end_date timestamp not null,
	acc_id char(13) not null,
	bank_bic char(9) not null,
	statement_text varchar(2000) not null,
	primary key (statement_id),
	foreign key (acc_id)
		references Account(acc_id),
	foreign key (bank_bic)
		references Account(bank_bic)
);

create table TransactionTemplate(
	tmpl_name char(16) not null,
	payer_bank_bic char(9) not null,
	payer_acc_id char(13) not null, 
	bnfc_bank_bic char(9) not null, 
	bnfc_acc_id char(13) not null, 
	amount double precision not null, 
	reason varchar(255) not null, 
	is_urgent bit not null, 
	primary key (tmpl_name),
	foreign key (payer_acc_id)
		references Account(acc_id)
	foreign key (payer_bank_bic)
		references Account(bank_bic)
	foreign key (bnfc_acc_id)
		references Account(acc_id)
	foreign key (bnfc_bank_bic)
		references Account(bank_bic)
);