-- FIXME: заменить весь файл, так чтобы он создавал базу клиента а не сервера.

CREATE TABLE Account (id varchar(9)  not null,
                      owner_id varchar(13) not null,
                      ballance double precision  not null,
                      open_date timestamp not null,
                      close_date timestamp null);
					  
CREATE TABLE Company (unp varchar(13)  not null,
                      name text  not null,
                      registry_date timestamp  not null,
                      unregistry_date timestamp  null,
                      open_key varchar  not null);
					  
CREATE TABLE Status (id bigint  not null,
                     message text  not null);