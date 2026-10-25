import json
import logging
import os
import time
from datetime import datetime

import jsondiff
import requests

logging.basicConfig(format='%(asctime)s: %(message)s', level=logging.INFO)


def get_url(offset):
    return f'https://api.motatos.se/jsonapi/index/products?page[offset]={offset}&page[limit]=50&sort=-created&filter[out_of_stock][condition][path]=out_of_stock&filter[out_of_stock][condition][operator]=%3C%3E&filter[out_of_stock][condition][value]=1'


def fetch_all():
    items = {}
    offset = 0
    start_time = time.time()

    logging.info(f'Fetching items...')
    while True:
        response = requests.get(get_url(offset))

        if response.status_code != 200:
            raise Exception(f'Unexpected response: {response.status_code}')

        data = response.json()
        new_items = {str(item['attributes']['product_id']): item['attributes'] for item in data['data']}
        if not new_items:
            break
        items.update(new_items)

        offset += 45  # max 50 pagination, leave margin for changes
    end_time = time.time()
    logging.info(f'Done fetching {len(items)} items in {end_time - start_time:.2f} seconds.')
    return items


items = fetch_all()

diff_folder = 'data/diffs'
initial_items_file = 'data/initial.json'
latest_items_file = 'data/latest.json'
os.makedirs(diff_folder, exist_ok=True)

if os.path.exists(latest_items_file):
    with open(latest_items_file, 'r') as f:
        old_data = json.load(f)

    differ = jsondiff.JsonDiffer(marshal=True)

    # logging.info('Replaying patches')  # if reading from initial items
    # for diff_file in sorted(os.listdir(diff_folder)):
    #     with open(os.path.join(diff_folder, diff_file), 'r') as f:
    #         old_data = differ.patch(old_data, json.load(f))

    differences = differ.diff(old_data, items)

    if differences:
        now = datetime.now()
        timestamp = now.strftime("%Y%m%d_%H%M%S")
        month_folder = os.path.join(diff_folder, now.strftime("%Y_%m"))
        os.makedirs(month_folder, exist_ok=True)
        diff_filename = os.path.join(month_folder, f"diff_{timestamp}.json")

        logging.info(f'Dumping patch for new diff {diff_filename}')
        with open(diff_filename, 'w') as f:
            json.dump(differences, f)

        logging.info('Dumping latest items')
        with open(latest_items_file, 'w') as f:
            json.dump(items, f)
    else:
        logging.info('No new diffs')
else:
    logging.info('Dumping initial items')
    with open(initial_items_file, 'w') as f:
        json.dump(items, f)
    with open(latest_items_file, 'w') as f:
        json.dump(items, f)
