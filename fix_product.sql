UPDATE product
LEFT JOIN (
	SELECT product_id, participant_id FROM (
		SELECT * FROM assignment ORDER BY ts DESC, action DESC
	) AS sorted GROUP BY product_id
) AS latest
ON id = product_id SET current_owner_id = participant_id;
