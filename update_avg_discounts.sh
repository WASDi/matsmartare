#!/bin/bash
sqlite3 matsmartare.db "INSERT INTO avg_discounts (the_date, avg_discount) SELECT current_date, avg(discount) FROM items WHERE last_seen=(SELECT MAX(last_seen) FROM items)"
