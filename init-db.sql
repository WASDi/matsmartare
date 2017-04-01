BEGIN TRANSACTION;

CREATE TABLE categories (
 id INTEGER PRIMARY KEY,
 url TEXT NOT NULL,
 name TEXT NOT NULL
);

CREATE TABLE items (
 id INTEGER PRIMARY KEY,
 categories TEXT NOT NULL,
 url TEXT NOT NULL,
 img_url TEXT NOT NULL,
 name TEXT NOT NULL,
 price DECIMAL(8,2) NOT NULL,
 discount INTEGER NOT NULL,
 best_before TEXT,
 first_seen DATETIME NOT NULL,
 last_seen DATETIME NOT NULL
);

CREATE TABLE update_logs (
 when_timestamp INTEGER PRIMARY KEY,
 num_web_items INTEGER NOT NULL,
 num_new_items INTEGER NOT NULL
);

COMMIT;
