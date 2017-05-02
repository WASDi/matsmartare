-- Unarchives all items that were added in the last update

-- Toggle all for deletion that were in the last update and is also in archive
UPDATE items
SET name = 'DEL_' || name
WHERE
name NOT LIKE 'ARCHIVE_%'
AND url IN (
	SELECT i.url FROM
	items i
	INNER JOIN
	items ia
	ON ia.name = 'ARCHIVE_' || i.name
	WHERE i.first_seen = (SELECT max(first_seen) FROM items)
);

-- Update last_seen to prevent being marked again as archive
UPDATE items
SET last_seen = (SELECT max(last_seen) FROM items)
WHERE
name LIKE 'ARCHIVE_%'
AND url IN (
	   SELECT url FROM
	   items
	   WHERE name LIKE 'DEL_%'
);

-- Remove archive-flag from items marked to be deleted
UPDATE items
SET name = substr(name, 9)
WHERE
name LIKE 'ARCHIVE_%'
AND url IN (
	SELECT i.url FROM
	items i
	WHERE
	name LIKE 'DEL_%'
);

-- Delete the items that have been un-archived
DELETE FROM items
WHERE NAME LIKE 'DEL_%';
