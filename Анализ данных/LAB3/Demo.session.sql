CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    username VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL
);


INSERT INTO users (username, email) VALUES
    ('join_doe', 'wfwfsfsaf'),
    ('jane_doe', 'qwertyui'),
    ('bob_doe', 'qazsxsfdywfwf');



    