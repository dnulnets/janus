insert into "user" (id, username, "password", "guid", email) values (1000, 'tomas', crypt('mandelmassa', gen_salt('bf', 10)), gen_random_uuid(), 'tomas@stenlund.eu');
