import json
import logging
import os
import time
from datetime import datetime

import jsondiff
import requests

logging.basicConfig(format='%(asctime)s: %(message)s', level=logging.INFO)


def get_url(offset):
    return f'https://api.motatos.se/jsonapi/index/products?page[offset]={offset}&page[limit]=50&sort=-sticky%2C-ranking_score_3&filter[exclude_from_lists][condition][path]=exclude_from_lists&filter[exclude_from_lists][condition][operator]=%3C%3E&filter[exclude_from_lists][condition][value]=1&filter[out_of_stock][condition][path]=out_of_stock&filter[out_of_stock][condition][operator]=%3C%3E&filter[out_of_stock][condition][value]=1&filter[categories][condition][path]=categories&filter[categories][condition][operator]=IN&filter[categories][condition][value][0]=6fc9ebee-2f19-4bb6-9a0a-9138c718ad64&filter[categories][condition][value][1]=bf90d3ee-8a1f-4547-ab89-cdf2d365e74a&filter[categories][condition][value][2]=f01ce77c-1b16-4b90-a2f6-9126f4d9f2d9&filter[categories][condition][value][3]=b3c2d83b-ec96-4b42-bef4-46cd24e702d0&filter[categories][condition][value][4]=3106afab-9ce9-445a-842f-5843c2a54468&filter[categories][condition][value][5]=cde27885-9540-41e4-9c10-1b27e51ac8f5&filter[categories][condition][value][6]=184d28e8-4e7f-4346-86d0-fc2cba54259d&filter[categories][condition][value][7]=b349c95e-7205-49f2-8288-93e91ea80c92&filter[categories][condition][value][8]=ebd8d7da-e22e-45bd-a0dc-1f7cddceeabc&filter[categories][condition][value][9]=5d1b827d-12d1-4eb1-8eba-accd8e993b57&filter[categories][condition][value][10]=e6b55e13-4308-4bfe-9387-dc2e41024eeb'


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

        offset += 48  # max 50 pagination, leave margin for changes
    end_time = time.time()
    logging.info(f'Done fetching {len(items)} items in {end_time - start_time:.2f} seconds.')
    return items


items = fetch_all()

diff_folder = 'data/diffs'
initial_items_file = 'data/initial.json'
latest_items_file = 'data/latest.json'
if not os.path.exists(diff_folder):
    os.makedirs(diff_folder)

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
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        diff_filename = f"{diff_folder}/diff_{timestamp}.json"

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
