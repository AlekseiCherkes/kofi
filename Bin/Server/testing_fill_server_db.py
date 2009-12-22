#-*- coding: cp1251 -*-

import os
import random
import datetime
from pysqlite2 import dbapi2 as db

##company_names = [
##    'АКСЕНОВ Сергей Анатольевич',
##    'БОРИСОВ Владимир Сергеевич',
##    'БОРОВИКОВ Александр Владимирович',
##    'БУРДА Василий Николаевич',
##    'ВОЛОДЬКИН Сергей Александрович',
##    'ГЛИНСКИЙ Геннадий Александрович',
##    'ГОРБАЧЁВА Марина Ивановна',
##    'КИМСТАЧ Александр Викторович',
##    'ЛЫСЕНКО Римма Николаевна',
##    'МАЛАХОВ Валерий Вячеславович',
##    'МАРУДОВ Олег Владимирович',
##    'МИХАЙЛИЧЕНКО Юлия Александровна',
##    'ТИМАШКОВ Юрий Семёнович',
##    'ТУРКОВ Валерий Трофимович',
##    'УЛАСЕВИЧ Александр Викторович',
##    'ФЕСИНА Светлана Викторовна',
##    'ФИЛИППОВИЧ Константин Николаевич',
##    'ХРУЦКИЙ Сергей Петрович',
##    'ЧЕПИК Дина Константиновна',
##    'ЧИСТЯКОВА Марина Анатольевна',
##    'ЩАДНЕВА Галина Александровна',
##    'ИМП ООО Правовая группа «Закон и Порядок» г. Витебск',
##    'ООО «Витебская юридическая компания»',
##    'Частное предприятие «Агентство правовых услуг города Витебска»',
##    'ООО «Юридическая компания «Темис» г. Новополоцк',
##    'ООО «Юридическое агентство «Берега Двины»  г. Витебск',
##    'Филиал ООО «Партнёр-Консультант» в городе Витебске'
##    ]

company_names = [
    'АКСЕНОВ Сергей Анатольевич',
    'БОРИСОВ Владимир Сергеевич'
    ]

accounts_per_company = 2
#transaction_per_account = 2
banks_count = 2
banks_manual_db_path =  '..\Common\BanksManual\manual.db'
server_db_path = 'server.db'

def get_banks_manual(banks_manual_db_path, banks_count):
    '''
        [str] get_banks_manual(str, int)

        Get bics of banks.
        'banks_count' - how many banks you need.
    '''
    c = db.connect(database = banks_manual_db_path)
    cu = c.cursor()
    cu.execute('select branch_bic from Branch;')
    all_records = cu.fetchall()
    all_records = all_records[:banks_count]
    banks_manual = []
    for i in all_records:
        banks_manual.append(i[0])
    c.close()
    return banks_manual

def fill_Company(company_names):
    '''
        [str, str, str, str] fill_Company([str])

        Fill table Company.
    '''
    companies = []
    for company_name in company_names:
        result = os.popen('create_company.exe "' + company_name + '"')
        company_info = result.readlines()

        # Remove starting and ending double quotes.
        company_info[0] = company_info[0][1:len(company_info[0]) - 3]
        company_info[1] = company_info[1][1:len(company_info[1]) - 3]
        companies.append(company_info)
    return companies

def fill_Account(companies, accounts_per_company, banks):
    '''
        [str, str, str] fill_Account([str], int, [str])

        Fill table Account. For each company create in each bank some accounts.
        'companies' - list of company;
        'accounts_per_company' - count of company's accounts in each banks;
        'banks_manual' - list of banks.
    '''
    total_acc_count = 0
    accounts = []
    for company in companies:
        for bank_bic in banks:
            for i in range(accounts_per_company):
                #balance = (int(company[0]) + int(bank_bic)) / 1000000
                balance = 100 + 10 * total_acc_count
                cmd_command = 'create_account.exe ' + company[0] + ' ' + \
                    bank_bic + ' ' + str(balance)
                result = os.popen(cmd_command)
                account_id = result.readlines()
                # Remove starting and ending double quotes.
                account_id = account_id[0][1:len(account_id[0]) - 3]
                account_info = []
                account_info.append(account_id)
                account_info.append(company[0])
                account_info.append(bank_bic)
                account_info.append(balance)
                accounts.append(account_info)
                total_acc_count += 1
    return accounts

def fill_CommitedTransaction(server_db_path, accounts):
    '''
        null fill_CommitedTransaction([str], [str])

        Fill table CommitedTransaction.
        'server_db_path' - path for server database;
        'accounts' - list of accounts;
    '''
    c = db.connect(database = server_db_path)
    cu = c.cursor()
    try:
        for payer_account in accounts:
            for bnfc_account in accounts:
                if payer_account != bnfc_account:
                    #trn_id = str(random.randint(0, 1000000000))
                    trn_id = str(random.randint(0, 1000000000))
                    money_amount = payer_account[3] / 2.0
                    cu.execute('''insert into CommitedTransaction
                                  values(?,
                                         current_timestamp,
                                         current_timestamp,
                                         0,
                                        'trn_content',
                                        'trn_reason',
                                        ?, ?, ?,
                                        ?, ?, ?,
                                        2,
                                        0
                                        );''',
                                (trn_id,
                                payer_account[0],
                                payer_account[2],
                                payer_account[3] - money_amount,
                                bnfc_account[0],
                                bnfc_account[2],
                                bnfc_account[3] + money_amount)
                              )
                    c.commit()
    finally:
        c.close()


def print_statistic(server_db_path):
    c = db.connect(database = server_db_path)
    cu = c.cursor()
    try:
        cu.execute('select * from Company;')
        all_records = cu.fetchall()
        print 'Records in Company: ' + str(len(all_records))

        cu.execute('select * from Account;')
        all_records = cu.fetchall()
        print 'Records in Account: ' + str(len(all_records))

        cu.execute('select * from CommitedTransaction;')
        all_records = cu.fetchall()
        print 'Records in CommitedTransaction: ' + str(len(all_records))
    finally:
        c.close()


if __name__ == '__main__':
    os.system('make')
    banks = get_banks_manual(banks_manual_db_path, banks_count)
    companies = fill_Company(company_names)
    accounts = fill_Account(companies, accounts_per_company, banks)
    fill_CommitedTransaction(server_db_path, accounts)
    print_statistic(server_db_path)



