import json
import logging
import os
import sys
import time
from datetime import datetime

import jsondiff
from tqdm import tqdm

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
    result = [{'id': cat[0], 'name': cat[1]} for cat in categories]
    result.sort(key=lambda x: (x['name'], x['id']))
    return result


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


def restore_first_seen(first_seen, items):
    for item in items:
        if item['id'] in first_seen:
            item['first_seen'] = first_seen[item['id']]


def perform_replay():
    diff_folder = 'data/diffs'
    diff_files = sorted(os.listdir(diff_folder))
    differ = jsondiff.JsonDiffer(marshal=True)

    with open('data/initial.json', 'r') as f:
        raw = json.load(f)

    items = [parse_item(item, 0) for item in raw.values()]
    prices = {item['id']: item['price'] for item in items}
    first_seen = {item['id']: 0 for item in items}

    price_changes = []
    for diff_file in tqdm(diff_files):
        timestamp = diff_file_timestamp(diff_file)
        with open(os.path.join(diff_folder, diff_file), 'r') as f:
            diff = json.load(f)
            raw = differ.patch(raw, diff)

        diffed_items = [parse_item(item, timestamp) for id, item in raw.items() if id in diff.keys()]
        maybe_diffed_prices = {item['id']: item['price'] for item in diffed_items}

        # Record price changes
        for change in calculate_price_changes(prices, maybe_diffed_prices, timestamp):
            price_changes.append(change)
            prices[change['item_id']] = change['price_after']

        # Record new first_seen and prices
        for item in diffed_items:
            if item['id'] not in first_seen:
                first_seen[item['id']] = timestamp
            if item['id'] not in prices:
                prices[item['id']] = item['price']

        # Remove deleted items from first_seen, so that if they appear again they get considered as new
        if '$delete' in diff:
            for item_id in diff['$delete']:
                first_seen.pop(int(item_id), None)

    # raw is now latest state, rebuild all items and not just diffed
    items = [parse_item(item, -1) for item in raw.values()]
    items.sort(key=lambda x: x['id'])
    restore_first_seen(first_seen, items)

    return {
        'categories': extract_categories(raw),
        'items': items,
        'priceChanges': price_changes[::-1]
    }


def perform_increment():
    timestamp = diff_file_timestamp(max(os.listdir('data/diffs')))

    with open('data/everything.json', 'r') as f:
        old_everything = json.load(f)
    with open('data/latest.json', 'r') as f:
        raw = json.load(f)

    items = old_everything['items']
    prices = {item['id']: item['price'] for item in items}
    first_seen = {item['id']: item['first_seen'] for item in items}

    new_items = [parse_item(item, timestamp) for item in raw.values()]
    new_items.sort(key=lambda x: x['id'])
    new_prices = {item['id']: item['price'] for item in new_items}

    price_changes = list(calculate_price_changes(prices, new_prices, timestamp))
    restore_first_seen(first_seen, new_items)

    return {
        'categories': extract_categories(raw),
        'items': new_items,
        'priceChanges': price_changes + old_everything['priceChanges']
    }


def main():
    replay = len(sys.argv) > 1 and sys.argv[1] == '--replay'
    if replay:
        everything = perform_replay()
    else:
        everything = perform_increment()

    # Discard price changes of removed items
    item_ids = set(item['id'] for item in everything['items'])
    everything['priceChanges'] = list(filter(lambda x: x['item_id'] in item_ids, everything['priceChanges']))

    with open('data/everything.json', 'w') as f:
        json.dump(everything, f)


if __name__ == "__main__":
    start_time = time.time()
    main()
    end_time = time.time()
    logging.info(f'Done generating items in {end_time - start_time:.2f} seconds.')
