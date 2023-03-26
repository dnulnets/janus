insert into "user" (id, username, "password", "guid", email, active) values (gen_random_uuid(), 'tomas', crypt('mandelmassa', gen_salt('bf', 10)), gen_random_uuid(), 'tomas@stenlund.eu', true);
