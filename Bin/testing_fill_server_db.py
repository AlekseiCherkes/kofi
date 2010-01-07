#-*- coding: utf-8 -*-

import os
import random
import datetime
import sqlite3 as db
import subprocess
import time


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
banks_manual_db_path =  'Common\BanksManual\manual.db'
server_db_path = 'Server/server.db'

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
        process = subprocess.Popen(['Server\create_company', company_name], \
            cwd = 'Server', shell=False, stdin=subprocess.PIPE, \
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        # Get return code.
        process.wait()
        returncode = process.returncode
        if returncode != 0:
            # Error occure.
            print 'Error ' + str(returncode) + ': ', process.stdout.readlines()
            print 'In [testing_fill_server_db.py].fill_Company(...)'
            exit()
        company_info = process.stdout.readlines()
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
                #balance = 100 + 10 * total_acc_count
                balance = random.randint(100, 10000)
                args = ['Server\create_account', company[0], bank_bic, str(balance)]
                process = subprocess.Popen(args, cwd = 'Server', shell=False, \
                    stdin=subprocess.PIPE, stdout=subprocess.PIPE, \
                    stderr=subprocess.PIPE)
                # Get return code.
                process.wait()
                returncode = process.returncode
                if returncode != 0:
                    # Error occure.
                    print 'Error ' + str(returncode) + ' in create_account.exe: '
                    print 'stdout: ', process.stdout.readlines()
                    print 'stderr: ', process.stderr.readlines()
                    print 'args: ', args
                    print 'In [testing_fill_server_db.py].fill_Account(...)'
                    exit()
                account_id = process.stdout.readlines()
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
                    trn_id = str(random.randint(0, 1000000000))
                    seconds = str(random.randint(0, 59))
                    minutes = str(random.randint(0, 59))
                    hours = str(random.randint(0, 23))
                    day = str(random.randint(1, 28))
                    month = str(random.randint(1, 12))
                    year = random.choice(['2009', '2008', '2007'])
                    #recive_date = year + '-' + month + '-' + day + ' ' + hours + ':' + minutes + ':' + '0'
                    t = time.strptime(year + ' ' + month + ' ' + day +  ' ' + \
                        hours + ' ' + minutes + ' ' + seconds, "%Y %m %d %H %M %S")
                    t = time.mktime(t)
                    recive_date = time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(t))
                    money_amount = payer_account[3] / 2.0
                    trn_reason = random.choice(['Оплата коммунальных услуг.',
                                                'Выплата гонорара.',
                                                'Благотворительность.'
                                                'Выплата кредита.'])
                    cu.execute('''insert into CommitedTransaction
                                  values(?,
                                         ?,
                                         ?,
                                         0,
                                        'trn_content',
                                        ?,
                                        ?, ?, ?,
                                        ?, ?, ?,
                                        ?,
                                        0
                                        );''',
                                (trn_id,
                                recive_date,
                                recive_date,
                                trn_reason,
                                payer_account[0],
                                payer_account[2],
                                bnfc_account[0],
                                bnfc_account[2],
                                payer_account[3] - money_amount,
                                bnfc_account[3] + money_amount,
                                money_amount)
                              )
                    c.commit()
    finally:
        c.close()

def fill_CurrencyRate(server_db_path):
    c = db.connect(database = server_db_path)
    cu = c.cursor()
    usd_bur = 2900.
    eur_bur = 4200.
    eur_usd = eur_bur / usd_bur
    try:
        cu.execute('''insert into CurrencyRate
                   values ('USD', 'BYR', ?);''', (usd_bur,))
        c.commit()
        cu.execute('''insert into CurrencyRate
                   values ('BYR', 'USD', ?);''', (1./usd_bur,))
        c.commit()
        cu.execute('''insert into CurrencyRate
                   values ('EUR', 'BYR', ?);''', (eur_bur,))
        c.commit()
        cu.execute('''insert into CurrencyRate
                   values ('BYR', 'EUR', ?);''', (1./eur_bur,))
        c.commit()
        cu.execute('''insert into CurrencyRate
                   values ('USD', 'EUR', ?);''', (1./eur_usd,))
        c.commit()
        cu.execute('''insert into CurrencyRate
                   values ('EUR', 'USD', ?);''', (eur_usd,))
        c.commit()

    finally:
        c.close()


def print_statistic(server_db_path):
    '''
    Print counts of records in Company, Account and CommitedTransaction tables.
    '''
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
    banks = get_banks_manual(banks_manual_db_path, banks_count)
    companies = fill_Company(company_names)
    accounts = fill_Account(companies, accounts_per_company, banks)
    fill_CommitedTransaction(server_db_path, accounts)
    fill_CurrencyRate(server_db_path)
    print_statistic(server_db_path)



