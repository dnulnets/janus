insert into "user" (id, username, "password", "guid", email, active) values (gen_random_uuid(), 'tomas', crypt('mandelmassa', gen_salt('bf', 10)), gen_random_uuid(), 'tomas@stenlund.eu', true);
insert into "user" (id, username, "password", "guid", email, active) values (gen_random_uuid(), 'anna', crypt('mandelmassa', gen_salt('bf', 10)), gen_random_uuid(), 'anna@stenlund.eu', true);

curl http://localhost:8080/api/user/3da5b056-65f1-4cec-b768-d41d563fce86 -H "Accept: plain/text" -H "Authorization: Bearer {token}"

curl http://localhost:8080/api/users -H "Accept: plain/text" -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2ODA4MDE5MTcsImlhdCI6MTY4MDgwMDExNywiaXNzIjoiaHR0cHM6Ly9zdGVubHVuZC5ldSIsInN1YiI6Ijg5MDYzYjQ5LThhNTMtNGVjMC1iZTM3LTQwYjY3MmMwNGU2ZCJ9.i6xPMGhWR1Ko9gAuPfkWgbvAOFFgVd0Q90-2gRU9_2U"
