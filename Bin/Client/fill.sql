-- Company
insert into Company
values (1111111111111, "Company_1");
insert into Company
values (2222222222222, "Company_2");
insert into Company
values (3333333333333, "Company_3");

-- -- Bank
-- insert into Bank
-- values (111111111, "Bank_1");
-- insert into Bank
-- values (222222222, "Bank_2");
-- insert into Bank
-- values (333333333, "Bank_3");

-- Account
-- 1-th company has accounts in all banks.
-- 2-d company has several accounts only in one bank.
-- 3-d company hasn't accounts at all.
insert into Account
values (1, 1111111111111, 111111111);
insert into Account
values (1, 1111111111111, 222222222);
insert into Account
values (1, 1111111111111, 333333333);
insert into Account
values (1, 2222222222222, 333333333);
insert into Account
values (2, 2222222222222, 333333333);
insert into Account
values (1, 2222222222222, 111111111);
insert into Account
values (2, 2222222222222, 111111111);

-- CounterParty
-- 1-st company has links with 2-d and 3-d.
-- 2-d company hasn't links at all.
-- 3-d company has link with 2-d.
insert into Beneficiary
values (1111111111111, 2222222222222);
insert into Beneficiary
values (1111111111111, 3333333333333);
insert into Beneficiary
values (3333333333333, 2222222222222);
