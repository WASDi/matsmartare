#!/bin/bash
cd git_ignore
curl 'https://www.matsmart.se/' > index.html
grep __PRELOADED_STATE__ index.html | perl -pe 's|.*<script>window.__PRELOADED_STATE__=(.*?)</script>.*|\1|' | python3 -m json.tool > preloaded.json

cd jsons/latest
curl 'https://api.matsmart.se/api/v1.0/routes?market=SE' > routes.json
curl 'https://api.matsmart.se/api/v1.0/product-displays?market=SE' > products.json
python3 -m json.tool < routes.json > routes_formatted.json
python3 -m json.tool < products.json > products_formatted.json
