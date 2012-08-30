TRUNCATE super_category_pages;

INSERT INTO super_category_pages
(
  SELECT cat_page AS scp_page,
         CASE WHEN count(DISTINCT sc_name) = 1 THEN min(sc_name) ELSE 'mul' END AS scp_name
    FROM category_pages, super_categories
   WHERE cat_title = sc_category
GROUP BY cat_page
ORDER BY cat_page
);

VACUUM FULL super_category_pages;

CLUSTER super_category_pages;

ANALYZE super_category_pages;
