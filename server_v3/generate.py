import json
import logging
import os
from datetime import datetime

import jsondiff

logging.basicConfig(format='%(asctime)s: %(message)s', level=logging.INFO)


def diff_file_timestamp(filename):
    date_str = filename[5:13]
    time_str = filename[14:20]

    datetime_str = date_str + time_str
    return int(datetime.strptime(datetime_str, "%Y%m%d%H%M%S").timestamp())


def extract_categories(raw_items):
    categories = set()
    for item in raw_items.values():
        for cat in item['computed_categories']:
            categories.add((cat['id'], cat['name']))
    return [{'id': cat[0], 'name': cat[1]} for cat in categories]


def parse_item(raw_item, timestamp):
    return {
        'id': raw_item['product_id'],
        'categories': [cat['id'] for cat in raw_item['computed_categories']],
        'url': raw_item['path']['alias'],
        'img_url': raw_item['computed_variations'][0]['computed_images'][0]['styles']['thumbnail'],
        'name': raw_item['title'],
        'price': float(raw_item['computed_variations'][0]['resolved_price']['number']),  # TODO verify lowest bulk
        'discount': raw_item['computed_variations'][0]['savings_percentage'],
        'best_before': raw_item['computed_variations'][0]['best_before'],
        'first_seen': timestamp,
        'not_waste': raw_item['body'] is not None
                     and ('inte räddad' in raw_item['body']['value'] or
                          'kompletterar vårt räddade' in raw_item['body']['value'])
    }


def calculate_price_changes(old_prices, new_prices, timestamp):
    for id, new_price in new_prices.items():
        if id in old_prices and old_prices[id] != new_prices[id]:
            yield {
                'item_id': id,
                'price_before': old_prices[id],
                'price_after': new_prices[id],
                'created': timestamp
            }


diff_folder = 'data/diffs'
diff_files = sorted(os.listdir(diff_folder))
differ = jsondiff.JsonDiffer(marshal=True)

with open('data/initial.json', 'r') as f:
    raw = json.load(f)

items = [parse_item(item, 0) for item in raw.values()]
prices = {item['id']: item['price'] for item in items}

# TODO option to not replay from start

price_changes = []
for diff_file in diff_files:
    timestamp = diff_file_timestamp(diff_file)
    with open(os.path.join(diff_folder, diff_file), 'r') as f:
        raw = differ.patch(raw, json.load(f))

    new_items = [parse_item(item, timestamp) for item in raw.values()]
    new_prices = {item['id']: item['price'] for item in new_items}

    for change in calculate_price_changes(prices, new_prices, timestamp):
        price_changes.append(change)

    items = new_items  # TODO don't overwrite first_seen
    prices = new_prices

# TODO remove price changes of removed items

everything = {
    'categories': extract_categories(raw),
    'items': items,
    'priceChanges': price_changes
}

with open('everything.json', 'w') as f:
    json.dump(everything, f)
