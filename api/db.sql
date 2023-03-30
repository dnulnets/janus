insert into "user" (id, username, "password", "guid", email, active) values (gen_random_uuid(), 'tomas', crypt('mandelmassa', gen_salt('bf', 10)), gen_random_uuid(), 'tomas@stenlund.eu', true);
insert into "user" (id, username, "password", "guid", email, active) values (gen_random_uuid(), 'anna', crypt('mandelmassa', gen_salt('bf', 10)), gen_random_uuid(), 'anna@stenlund.eu', true);

curl http://localhost:8080/api/user/3da5b056-65f1-4cec-b768-d41d563fce86 -H "Accept: plain/text" -H "Authorization: Bearer {token}"
