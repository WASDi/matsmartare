#!/bin/bash
cd git_ignore/jsons/latest
curl 'https://api.matsmart.se/api/v1.0/routes?market=SE' > routes.json
curl 'https://api.matsmart.se/api/v1.0/product-displays?market=SE' > products.json
python3 -m json.tool < routes.json > routes_formatted.json
python3 -m json.tool < products.json > products_formatted.json
