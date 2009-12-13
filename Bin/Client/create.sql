CREATE TABLE Company (
	company_unp char(13) not null,
	company_name varchar(256) not null,
	primary key (company_unp)
);
		
CREATE TABLE CounterParty (
	payer_unp char(13) not null,
	beneficiary_unp char(13) not null,
	primary key (payer_unp, beneficiary_unp),
	foreign key (payer_unp)
		references Company(company_unp),
	foreign key (beneficiary_unp)
		references Company(company_unp)
);

CREATE TABLE Bank (
	bank_bic char(9) not null,
	bank_name varchar(256) not null,
	primary key (bank_bic)
);

CREATE TABLE Account (
	acc_id int not null,
	company_unp char(13) not null,
	bank_bic char(9) not null,
	primary key (acc_id, company_unp, bank_bic),
	foreign key (company_unp)
		references Company(company_unp),
	foreign key (bank_bic)
		references Bank(bank_bic)
);