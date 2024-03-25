SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

CREATE SCHEMA foods;

CREATE TABLE foods.veg (
    code character varying(8) NOT NULL,
    vegetable character varying(255),
    magnesium float DEFAULT 0,
    potassium float DEFAULT 0,
    category integer DEFAULT 10 NOT NULL
);

COPY foods.veg(code, vegetable, magnesium, potassium, category)
    FROM 'D:\r\KenyaDSM\data\foods\cat_veg.csv'
    DELIMITER ','
    CSV HEADER;

ALTER TABLE foods.veg
    ADD COLUMN id SERIAL PRIMARY KEY;

TABLE foods.veg