install.package("sqldf")

CREATE DEFINER=`gta_prod_master`@`%` TRIGGER `before_intervention_update` BEFORE UPDATE ON `gta_intervention` FOR EACH ROW BEGIN
    SET @intervention_id = NEW.id;
  SET @intervention_area = NEW.intervention_area;
  SET @affected_flow = NEW.affected_flow_id;
  SET @is_fta_included = NEW.is_fta_included;
  SET @year = YEAR(NEW.inception_date);
  SET @measure_id = OLD.measure_id;
  SET @year = @year - 1;
  SET @dm_freeze = NEW.dm_freeze;
  SET @aj_freeze = NEW.aj_freeze;
  
  IF((NEW.to_recalculate <=> OLD.to_recalculate) = 1 AND (NEW.measure_status_id <=> OLD.measure_status_id) = 1) THEN

    DELETE FROM gta_it_revised
    WHERE intervention_id = @intervention_id;

/* AREA 1 ---------------------------------------------------------------------- */
    IF (@intervention_area = 1) THEN
        IF (@affected_flow = 1) THEN
        
          UPDATE gta_affected_tariff_line atl
          LEFT JOIN
          (SELECT gsgt.tariff_line_code FROM gta_support_goods_trade gsgt 
           JOIN gta_implementing_jurisdiction gij ON gij.intervention_id = @intervention_id AND gsgt.reporter_jurisdiction_id = gij.jurisdiction_id AND gsgt.year = @year) t2
           ON atl.intervention_id = @intervention_id AND t2.tariff_line_code = atl.tariff_line_code
          SET is_positively_affected = (case when t2.tariff_line_code IS NOT NULL THEN 1 ELSE 0 END)
          WHERE intervention_id = @intervention_id;
            
        END IF;
        IF (@affected_flow != 1) THEN
        
          UPDATE gta_affected_tariff_line atl
          LEFT JOIN
          (SELECT gsgt.tariff_line_code FROM gta_support_goods_trade gsgt 
           JOIN gta_implementing_jurisdiction gij ON gij.intervention_id = @intervention_id AND gsgt.partner_jurisdiction_id = gij.jurisdiction_id AND gsgt.year = @year) t2
           ON atl.intervention_id = @intervention_id AND t2.tariff_line_code = atl.tariff_line_code
          SET is_positively_affected = (case when t2.tariff_line_code IS NOT NULL THEN 1 ELSE 0 END)
          WHERE intervention_id = @intervention_id;
            
        END IF;
        
        UPDATE gta_affected_sector ac
        SET is_positively_affected =
        (
          SELECT EXISTS (
            SELECT NULL
            FROM
              gta_affected_tariff_line atl,
              gta_tariff_line tl
            WHERE atl.intervention_id = @intervention_id
            AND atl.tariff_line_code = tl.code
            AND atl.is_positively_affected = 1
            AND tl.sector_code_3 = ac.sector_code
          )
          OR type = 'A'
        )
        WHERE intervention_id = @intervention_id;
      
      IF (@dm_freeze = 0) THEN
        IF (@affected_flow = 1) THEN
        
          DELETE FROM gta_distorted_market 
          WHERE intervention_id = @intervention_id
          AND
          (
            type = 'N'
            OR
            (
              type = 'D'
              AND jurisdiction_id
              NOT IN
              (
                SELECT jurisdiction_id
                FROM gta_implementing_jurisdiction ij
                WHERE ij.intervention_id = @intervention_id
              )
            )
          );
    
          INSERT INTO gta_distorted_market
          (
            SELECT
              @intervention_id,
              jurisdiction_id jid,
              'N'
            FROM gta_implementing_jurisdiction ij
            WHERE ij.intervention_id = @intervention_id
          )
          ON DUPLICATE KEY UPDATE
          type = IF(type = 'A', 'N', type);
            
        END IF;
        IF (@affected_flow != 1) THEN
        
          DELETE FROM gta_distorted_market 
          WHERE intervention_id = @intervention_id
          AND
          (
            type = 'N'
            OR
            (
              type = 'D'
              AND jurisdiction_id NOT IN
              (
                SELECT DISTINCT g.reporter_jurisdiction_id
                FROM gta_support_goods_trade g
                WHERE g.year = @year
                AND g.partner_jurisdiction_id IN
                (
                  SELECT jurisdiction_id
                  FROM gta_implementing_jurisdiction ij
                  WHERE ij.intervention_id = @intervention_id
                )
                AND g.reporter_jurisdiction_id NOT IN
                (
                  SELECT jurisdiction_id
                  FROM gta_implementing_jurisdiction ij
                  WHERE ij.intervention_id = @intervention_id
                )
                AND g.tariff_line_code IN
                (
                  SELECT tariff_line_code
                  FROM gta_affected_tariff_line atl
                  WHERE atl.intervention_id = @intervention_id
                )
              )
            )
          );
    
          INSERT INTO gta_distorted_market
          (
            SELECT DISTINCT
              @intervention_id,
              g.reporter_jurisdiction_id jid,
              'N'
            FROM gta_support_goods_trade g
            WHERE g.year = @year
            AND g.partner_jurisdiction_id IN
            (
              SELECT jurisdiction_id
              FROM gta_implementing_jurisdiction ij
              WHERE ij.intervention_id = @intervention_id
            )
            AND g.reporter_jurisdiction_id NOT IN
            (
              SELECT jurisdiction_id
              FROM gta_implementing_jurisdiction ij
              WHERE ij.intervention_id = @intervention_id
            )
            AND g.tariff_line_code IN
            (
              SELECT tariff_line_code
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
            )
          )
          ON DUPLICATE KEY UPDATE
          type = IF(type = 'A', 'N', type);
            
        END IF;
      END IF;
 --------------------------------------------------------------------------     
      IF (@aj_freeze = 0) THEN
        IF (@affected_flow = 1 AND @is_fta_included = 1) THEN
        
          DELETE FROM gta_affected_jurisdiction
          WHERE intervention_id = @intervention_id
          AND
          (
            type = 'N'
            OR
            (
              type = 'D'
              AND jurisdiction_id NOT IN
              (
                SELECT DISTINCT g.partner_jurisdiction_id
                FROM
                  gta_support_goods_trade g
                WHERE g.year = @year
                AND (g.partner_jurisdiction_id, g.reporter_jurisdiction_id) NOT IN
                (
                  SELECT
                    fta.implementing_jurisdiction_id,
                    fta.affected_jurisdiction_id
                  FROM gta_support_fta fta
                  WHERE fta.year = @year + 1
                )
                AND g.reporter_jurisdiction_id IN
                (
                  SELECT jurisdiction_id
                  FROM gta_distorted_market dm
                  WHERE dm.intervention_id = @intervention_id
                  AND type != 'D'
                )
                AND g.partner_jurisdiction_id NOT IN
                (
                  SELECT jurisdiction_id
                  FROM gta_implementing_jurisdiction ij
                  WHERE ij.intervention_id = @intervention_id
                )
                AND g.tariff_line_code IN
                (
                  SELECT tariff_line_code
                  FROM gta_affected_tariff_line atl
                  WHERE atl.intervention_id = @intervention_id
                )
              )
            )
          );
    
          INSERT INTO gta_affected_jurisdiction
          (
            SELECT DISTINCT
              @intervention_id,
              g.partner_jurisdiction_id jid,
              'N'
            FROM
              gta_support_goods_trade g
            WHERE g.year = @year
            AND (g.partner_jurisdiction_id, g.reporter_jurisdiction_id) NOT IN
            (
              SELECT
                fta.implementing_jurisdiction_id,
                fta.affected_jurisdiction_id
              FROM gta_support_fta fta
              WHERE fta.year = @year + 1
            )
            AND g.reporter_jurisdiction_id IN
            (
              SELECT jurisdiction_id
              FROM gta_distorted_market dm
              WHERE dm.intervention_id = @intervention_id
              AND type != 'D'
            )
            AND g.partner_jurisdiction_id NOT IN
            (
              SELECT jurisdiction_id
              FROM gta_implementing_jurisdiction ij
              WHERE ij.intervention_id = @intervention_id
            )
            AND g.tariff_line_code IN
            (
              SELECT tariff_line_code
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
            )
          )
          ON DUPLICATE KEY UPDATE
          type = IF(type = 'A', 'N', type);
        
        END IF;
        IF (@affected_flow = 1 AND @is_fta_included = 0) THEN
        
          DELETE FROM gta_affected_jurisdiction
          WHERE intervention_id = @intervention_id
          AND
          (
            type = 'N'
            OR
            (
              type = 'D'
              AND jurisdiction_id NOT IN
              (
                SELECT DISTINCT g.partner_jurisdiction_id
                FROM gta_support_goods_trade g
                WHERE g.year = @year
                AND g.reporter_jurisdiction_id IN
                (
                  SELECT jurisdiction_id
                  FROM gta_distorted_market dm
                  WHERE dm.intervention_id = @intervention_id
                  AND type != 'D'
                )
                AND g.partner_jurisdiction_id NOT IN
                (
                  SELECT jurisdiction_id
                  FROM gta_implementing_jurisdiction ij
                  WHERE ij.intervention_id = @intervention_id
                )
                AND g.tariff_line_code IN
                (
                  SELECT tariff_line_code
                  FROM gta_affected_tariff_line atl
                  WHERE atl.intervention_id = @intervention_id
                )
              )
            )
          );

          INSERT INTO gta_affected_jurisdiction
          (
            SELECT DISTINCT
              @intervention_id,
              g.partner_jurisdiction_id jid,
              'N'
            FROM gta_support_goods_trade g
            WHERE g.year = @year
            AND g.reporter_jurisdiction_id IN
            (
              SELECT jurisdiction_id
              FROM gta_distorted_market dm
              WHERE dm.intervention_id = @intervention_id
              AND type != 'D'
            )
            AND g.partner_jurisdiction_id NOT IN
            (
              SELECT jurisdiction_id
              FROM gta_implementing_jurisdiction ij
              WHERE ij.intervention_id = @intervention_id
            )
            AND g.tariff_line_code IN
            (
              SELECT tariff_line_code
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
            )
          )
          ON DUPLICATE KEY UPDATE
          type = IF(type = 'A', 'N', type);
        
        END IF;
        IF (@affected_flow = 3) THEN
        
          DELETE FROM gta_affected_jurisdiction
          WHERE intervention_id = @intervention_id
          AND
          (
            type = 'N'
            OR
            (
              type = 'D'
              AND jurisdiction_id NOT IN
              (
                select distinct g2.partner_jurisdiction_id
                from (select distinct g2.reporter_jurisdiction_id, g2.partner_jurisdiction_id 
                        from gta_support_goods_trade g2 
                        join gta_affected_tariff_line atl on atl.intervention_id = @intervention_id and g2.year = @year and atl.tariff_line_code = g2.tariff_line_code) g2
                join (select distinct g1.reporter_jurisdiction_id, g1.partner_jurisdiction_id 
                        from gta_support_goods_trade g1 
                        join gta_affected_tariff_line atl on atl.intervention_id = @intervention_id and g1.year = @year and atl.tariff_line_code = g1.tariff_line_code) g1
                on g1.reporter_jurisdiction_id = g2.reporter_jurisdiction_id
                join gta_distorted_market dm on dm.intervention_id = @intervention_id and dm.type = 'N' and g1.reporter_jurisdiction_id = dm.jurisdiction_id
                join gta_implementing_jurisdiction ij on ij.intervention_id = @intervention_id and g1.partner_jurisdiction_id = ij.jurisdiction_id
              )
            )
          );

          INSERT INTO gta_affected_jurisdiction
          (
            select distinct @intervention_id, g2.partner_jurisdiction_id, 'N' 
            from (select distinct g2.reporter_jurisdiction_id, g2.partner_jurisdiction_id 
                    from gta_support_goods_trade g2 
                    join gta_affected_tariff_line atl on atl.intervention_id = @intervention_id and g2.year = @year and atl.tariff_line_code = g2.tariff_line_code) g2
            join (select distinct g1.reporter_jurisdiction_id, g1.partner_jurisdiction_id 
                    from gta_support_goods_trade g1 
                    join gta_affected_tariff_line atl on atl.intervention_id = @intervention_id and g1.year = @year and atl.tariff_line_code = g1.tariff_line_code) g1
            on g1.reporter_jurisdiction_id = g2.reporter_jurisdiction_id
            join gta_distorted_market dm on dm.intervention_id = @intervention_id and dm.type = 'N' and g1.reporter_jurisdiction_id = dm.jurisdiction_id
            join gta_implementing_jurisdiction ij on ij.intervention_id = @intervention_id and g1.partner_jurisdiction_id = ij.jurisdiction_id
          )
          ON DUPLICATE KEY UPDATE
          type = IF(gta_affected_jurisdiction.type = 'A', 'N', gta_affected_jurisdiction.type);
        
        END IF;
      END IF;

      IF (@affected_flow = 1) THEN

        
          INSERT INTO gta_it_revised
          (
            intervention_id,
            implementing_jurisdiction_id,
            distorted_market_id,
            affected_jurisdiction_id,
            tariff_line_id,
            sector_code_3,
            tariff_line_code_4,
            value
          )
          (
           SELECT
              @intervention_id,
              g.reporter_jurisdiction_id,
              g.reporter_jurisdiction_id,
              g.partner_jurisdiction_id,
              tl.id,
              tl.sector_code_3,
              tl.product_code_4,
              g.value
            FROM
              gta_support_goods_trade g
            JOIN gta_tariff_line tl ON g.year = @year AND g.tariff_line_code = tl.code
            LEFT JOIN gta_distorted_market gdm ON 
            	gdm.intervention_id = @intervention_id 
	            AND g.reporter_jurisdiction_id = gdm.jurisdiction_id 
	            AND type = 'N'
            WHERE g.year = @year AND gdm.intervention_id is not null
            AND EXISTS
            (
              SELECT 1
              FROM gta_affected_jurisdiction aj
              WHERE aj.intervention_id = @intervention_id
              AND g.partner_jurisdiction_id = aj.jurisdiction_id
              AND type = 'N'
            )
            AND EXISTS
            (
              SELECT 1
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
              AND atl.tariff_line_code = tl.code
            )
          );
        
          INSERT INTO gta_it_revised
          (
            intervention_id,
            implementing_jurisdiction_id,
            distorted_market_id,
            affected_jurisdiction_id,
            tariff_line_id,
            sector_code_3,
            tariff_line_code_4,
            value
          )
          (
           SELECT 
                @intervention_id,
                ij.id as ij_id,
                g.reporter_jurisdiction_id,
                g.partner_jurisdiction_id,
                tl.id as tl_id,
                tl.sector_code_3,
                tl.product_code_4,
                g.value 
            FROM
            gta_support_goods_trade g
            JOIN gta_tariff_line tl on g.tariff_line_code = tl.code and g.year = @year
            CROSS JOIN 
                (
                SELECT jurisdiction_id id
                FROM gta_implementing_jurisdiction ij
                WHERE ij.intervention_id = @intervention_id
                ) ij
            LEFT JOIN gta_distorted_market gdm ON 
            	g.reporter_jurisdiction_id = gdm.jurisdiction_id 
            	AND gdm.`type` = 'A' 
            	AND gdm.intervention_id = @intervention_id
            WHERE g.year = @year AND g.tariff_line_code = tl.code AND gdm.intervention_id is not null
            AND EXISTS
            (
                SELECT 1
                FROM gta_affected_jurisdiction aj
                WHERE aj.intervention_id = @intervention_id
                AND g.partner_jurisdiction_id = aj.jurisdiction_id
                AND type = 'N'
            )
            AND EXISTS
            (
                SELECT 1
                FROM gta_affected_tariff_line atl
                WHERE atl.intervention_id = @intervention_id
                AND atl.tariff_line_code = tl.code
            )
          );
        
          INSERT INTO gta_it_revised
          (
            intervention_id,
            implementing_jurisdiction_id,
            distorted_market_id,
            affected_jurisdiction_id,
            tariff_line_id,
            sector_code_3,
            tariff_line_code_4
          )
          (
            SELECT
              @intervention_id,
              dm.id,
              dm.id,
              aj.id,
              tl.id,
              tl.sector_code_3,
              tl.product_code_4
            FROM
              (
                SELECT jurisdiction_id id
                FROM gta_distorted_market
                WHERE intervention_id = @intervention_id
                AND type = 'N'
              ) dm,
              (
                SELECT jurisdiction_id id
                FROM gta_affected_jurisdiction
                WHERE intervention_id = @intervention_id
                AND type = 'A'
              ) aj,
              gta_tariff_line tl
            WHERE EXISTS
            (
              SELECT 1
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
              AND atl.tariff_line_code = tl.code
            )
          );
        
          INSERT INTO gta_it_revised
          (
            intervention_id,
            implementing_jurisdiction_id,
            distorted_market_id,
            affected_jurisdiction_id,
            tariff_line_id,
            sector_code_3,
            tariff_line_code_4
          )
          (
            SELECT
              @intervention_id,
              ij.id,
              dm.id,
              aj.id,
              tl.id,
              tl.sector_code_3,
              tl.product_code_4
            FROM
              (
                SELECT jurisdiction_id id
                FROM gta_implementing_jurisdiction
                WHERE intervention_id = @intervention_id
              ) ij,
              (
                SELECT jurisdiction_id id
                FROM gta_distorted_market
                WHERE intervention_id = @intervention_id
                AND type = 'A'
              ) dm,
              (
                SELECT jurisdiction_id id
                FROM gta_affected_jurisdiction
                WHERE intervention_id = @intervention_id
                AND type = 'A'
              ) aj,
              gta_tariff_line tl
            WHERE EXISTS
            (
              SELECT 1
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
              AND atl.tariff_line_code = tl.code
            )
          );
          
      END IF;
      IF (@affected_flow = 2) THEN

        
          INSERT INTO gta_it_revised
          (
            intervention_id,
            implementing_jurisdiction_id,
            distorted_market_id,
            affected_jurisdiction_id,
            tariff_line_id,
            sector_code_3,
            tariff_line_code_4,
            value
          )
          (
            SELECT
              @intervention_id,
              g.partner_jurisdiction_id,
              g.reporter_jurisdiction_id,
              g.reporter_jurisdiction_id,
              tl.id,
              tl.sector_code_3,
              tl.product_code_4,
              g.value
            FROM
              gta_support_goods_trade g,
              gta_tariff_line tl
            WHERE g.year = @year
            AND EXISTS
            (
              SELECT 1
              FROM gta_distorted_market dm
              WHERE dm.intervention_id = @intervention_id
              AND g.reporter_jurisdiction_id = dm.jurisdiction_id
              AND type = 'N'
            )
            AND EXISTS
            (
              SELECT 1
              FROM gta_implementing_jurisdiction ij
              WHERE ij.intervention_id = @intervention_id
              AND g.partner_jurisdiction_id = ij.jurisdiction_id
            )
            AND EXISTS
            (
              SELECT 1
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
              AND atl.tariff_line_code = tl.code
            )
            AND g.tariff_line_code = tl.code
          );
        
          INSERT INTO gta_it_revised
          (
            intervention_id,
            implementing_jurisdiction_id,
            distorted_market_id,
            affected_jurisdiction_id,
            tariff_line_id,
            sector_code_3,
            tariff_line_code_4
          )
          (
            SELECT
              @intervention_id,
              ij.id,
              dm.id,
              dm.id,
              tl.id,
              tl.sector_code_3,
              tl.product_code_4
            FROM
              (
                SELECT jurisdiction_id id
                FROM gta_implementing_jurisdiction
                WHERE intervention_id = @intervention_id
              ) ij,
              (
                SELECT jurisdiction_id id
                FROM gta_distorted_market
                WHERE intervention_id = @intervention_id
                AND type = 'A'
              ) dm,
              gta_tariff_line tl
            WHERE EXISTS
            (
              SELECT 1
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
              AND atl.tariff_line_code = tl.code
            )
          );

      END IF;
      IF (@affected_flow = 3) THEN

        
          INSERT INTO gta_it_revised
          (
            intervention_id,
            implementing_jurisdiction_id,
            distorted_market_id,
            affected_jurisdiction_id,
            tariff_line_id,
            sector_code_3,
            tariff_line_code_4,
            value
          )
          (
            SELECT @intervention_id,
			  	   g1.partner_jurisdiction_id,
			       g2.reporter_jurisdiction_id,
			       g2.partner_jurisdiction_id,
			       tl.id,
			       tl.sector_code_3,
			       tl.product_code_4,
			       g2.value
			FROM gta_affected_tariff_line atl USE index (fk_gta_potentially_affected_tariff_line_gta_intervention1_idx)
			JOIN gta_tariff_line tl use index (idx_gta_tariff_line_code) ON atl.tariff_line_code = tl.code
			JOIN gta_support_goods_trade g1 USE INDEX (tariff_line_id) ON g1.year = @year AND tl.id = g1.tariff_line_id
			JOIN gta_support_goods_trade g2  ON g2.year = g1.year AND g1.reporter_jurisdiction_id = g2.reporter_jurisdiction_id AND g1.tariff_line_id  = g2.tariff_line_id
			JOIN gta_distorted_market dm ON dm.intervention_id = @intervention_id AND g1.reporter_jurisdiction_id = dm.jurisdiction_id AND dm.type = 'N'
			JOIN gta_implementing_jurisdiction ij USE INDEX (idx_intervention_implementer) ON  ij.intervention_id = atl.intervention_id AND g1.partner_jurisdiction_id = ij.jurisdiction_id
			WHERE atl.intervention_id = @intervention_id
          );
        
          INSERT INTO gta_it_revised
          (
            intervention_id,
            implementing_jurisdiction_id,
            distorted_market_id,
            affected_jurisdiction_id,
            tariff_line_id,
            sector_code_3,
            tariff_line_code_4,
            value
          )
          (
            SELECT
              @intervention_id,
              ij.id,
              g.reporter_jurisdiction_id,
              g.partner_jurisdiction_id,
              tl.id,
              tl.sector_code_3,
              tl.product_code_4,
              g.value
            FROM
              gta_support_goods_trade g,
              (
                SELECT jurisdiction_id id
                FROM gta_implementing_jurisdiction ij
                WHERE ij.intervention_id = @intervention_id
              ) ij,
              gta_tariff_line tl
            WHERE g.year = @year
            AND EXISTS
            (
              SELECT 1
              FROM gta_distorted_market dm
              WHERE dm.intervention_id = @intervention_id
              AND g.reporter_jurisdiction_id = dm.jurisdiction_id
              AND type = 'A'
            )
            AND EXISTS
            (
              SELECT 1
              FROM gta_affected_jurisdiction aj
              WHERE aj.intervention_id = @intervention_id
              AND g.partner_jurisdiction_id = aj.jurisdiction_id
              AND type = 'N'
            )
            AND EXISTS
            (
              SELECT 1
              FROM gta_affected_tariff_line atl
              WHERE atl.intervention_id = @intervention_id
              AND atl.tariff_line_code = tl.code
            )
            AND g.tariff_line_code = tl.code
          );

      END IF;

    END IF;
 
      IF (@affected_flow = 2) THEN

        DELETE FROM gta_affected_jurisdiction 
        WHERE intervention_id = @intervention_id;

        INSERT INTO gta_affected_jurisdiction
        (
          SELECT
            @intervention_id,
            jurisdiction_id,
            type
          FROM gta_distorted_market dm
          WHERE dm.intervention_id = @intervention_id
        );

      END IF;

      DELETE it FROM gta_it_revised it, gta_affected_jurisdiction aj
      WHERE it.intervention_id = @intervention_id
      AND aj.intervention_id = @intervention_id
      AND aj.type = 'D'
      AND aj.jurisdiction_id = it.affected_jurisdiction_id;

      DELETE it FROM gta_it_revised it, gta_implementing_jurisdiction ij
      WHERE it.intervention_id = @intervention_id
      AND ij.intervention_id = @intervention_id
      AND ij.jurisdiction_id = it.affected_jurisdiction_id;

      DELETE aj FROM gta_affected_jurisdiction aj, gta_implementing_jurisdiction ij
      WHERE ij.intervention_id = @intervention_id
      AND aj.intervention_id = @intervention_id
      AND aj.jurisdiction_id = ij.jurisdiction_id;

      INSERT IGNORE INTO gta_affected_sector
      (
        intervention_id,
        sector_code,
        is_positively_affected,
        type
      )
      (
        SELECT DISTINCT
          @intervention_id,
          sector_code_3,
          is_positively_affected,
          'N'
        FROM
          gta_affected_tariff_line atl,
          gta_tariff_line tl
        WHERE atl.tariff_line_code = tl.code
        AND atl.intervention_id = @intervention_id
      );
    

    SET NEW.to_recalculate = 0;
   
	INSERT INTO api_updated_measures 
	SELECT @measure_id AS measure_id, NOW() AS created_at;
   
  END IF;
END
