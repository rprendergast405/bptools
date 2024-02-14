#' Get Basic Combined Aspiring/BNZ Data
#'
#' A function designed to take a lot of the mundane copy-paste steps out of the process of getting combined Aspiring and BNZ data.
#' Takes away a bit of flexibility, but with the benefit of only needing a handful of parameters, which are then propogated throughout the process. This saves manually editing an older query several times, or having to re-write the standard queries from scratch, both of which are big sources for errors.
#'
#' @param db_con ODBC connection to bespoke
#' @param merch_tbl A data.frame with the GROUP_IDs of interest, plus any additional merchant information such as storetype groups
#' @param start_month The first month of the time period
#' @param end_month The last month of the time period
#' @param final_tablename A name for the final data table
#'
#' @export
get_combined_data <-
  function(db_con,
           merch_tbl,
           start_month,
           end_month,
           final_tablename) {

    # 0. Initialise ----
    cat("Initialising\n")

    merch_count <- aggregate(merch_tbl$GROUP_ID, list(merch_tbl$GROUP_ID), length)


    if (any(merch_count$x > 1)) {
      bad_groups <- merch_count$Group.1[which(merch_count$x > 1)]
      stop(
        "Your merchant table needs to have one row per merchant only. Check the following GROUP_IDs:\n",
        paste(bad_groups, collapse = ",\n")
      )
    }

    datetime <-
      paste0(substr(toupper(Sys.info()["user"]), 1, 3), format(Sys.time(), "%m%d%H%M"))

    tablename_exists <-
      toupper(final_tablename) %in% RODBC::sqlTables(db_con)$TABLE_NAME

    if (tablename_exists) {
      stop(
        "Your table name ",
        toupper(final_tablename),
        " already exists; please choose something different or drop the old table"
      )
    }

    # 1. Merchant Table ----
    # LEE COMMENTS
    # -- Build the Merchant List
    # -- It's best to try and have this contain all the information you'll need by group.
    # --This means you dont have to go back to Fozzie - Which might have changed over time
    cat("Saving Merchant Table (Step 1 of 7)\n")

    tablename1 <- toupper(paste0("combined_", datetime, "_groups"))

    # Make sure column names are uppercase
    names(merch_tbl) <- toupper(names(merch_tbl))

    # Save the merchant table
    #sqlDrop(DB, tablename1, errors = FALSE)
    RODBC::sqlSave(db_con,
                   merch_tbl,
                   tablename = tablename1,
                   rownames = FALSE)


    # 2. Get the aspiring data ----
    # LEE COMMENTS
    # -- Run the Aspiring Data
    # -- Aspiring Contains 4 SPEND_TYPES
    # -- ;'NZ Domestic Physical' - New Zealanders spending at Physical New Zealand Merchants
    # -- 'Int Domestic Phys' -- International Cardholders spending at Physical New Zealand Merchants
    # -- 'NZ Dom Ecom' -- New Zealanders spending at Domestic Online Merchants
    # -- 'NZ Int Ecom' -- New Zealanders spending at International Online Merchants
    #
    # -- This project only cares about New Zealanders, so we only want spend_Type 'NZ Domestic Physical'
    # -- While Aspiring contains attributes like Customer Location - We ignore these, and recalculate based on BNZ again
    cat("Getting Aspiring Data (Step 2 of 7)\n")


    tablename2 <- toupper(paste0("combined_", datetime, "_aspiring"))

    # sqlDrop(DB, tablename2, errors = FALSE)

    RODBC::sqlQuery(
      db_con,
      glue::glue(
        "
        CREATE TABLE {tablename2} compress AS
        SELECT g.*,
        mfs.seqmonth,
        SUM(mfs.spend) spend,
        SUM(mfs.trans) trans

        FROM {tablename1} g
        INNER JOIN aspiring.mbie_final_spends mfs
        ON (g.group_id       = mfs.group_id)

        WHERE mfs.spend_type = 'NZ Domestic Physical'
        AND mfs.seqmonth BETWEEN {start_month} AND {end_month}

        GROUP BY {paste0('g.', names(merch_tbl), collapse = ', ')},
        mfs.seqmonth"
      )
      )

    # We make some copies of full tables to speed up the queries where possible

    # 3. Customer Table ----
    # -- To make the whole process on the customer side easier - We build a customer table first with all the relevant attributes.
    # -- This involves including all the relevant attributes, along with Age in 5 year bands, and CAU - as the data will need to be weighted.
    # -- When we do this, we have a couple of criteria
    # -- * Only people between 15 and 120 - This makes sure we exclude a couple of BNZ data oddities. Some people have weird ages, and all Business cards have a default age of 1
    # -- * Only people with Gender M/F - This is because Business cards have a gender of U
    # -- * Only people with a Meshblock above 0 - This just makes sure we're only counting people who have a specific customer location
    cat("Copying Customer Data (Step 3 of 7)\n")


    tablename3 <- toupper(paste0("combined_", datetime, "_customer"))

    # sqlDrop(DB, tablename3, errors = FALSE)

    RODBC::sqlQuery(
      db_con,
      glue::glue(
        "CREATE TABLE {tablename3} compress as
        Select c.customer_id,
        CASE
        WHEN age BETWEEN 0 AND 4 THEN '0-4'
        WHEN age BETWEEN 5 AND 9 THEN '5-9'
        WHEN age BETWEEN 10 AND 14 THEN '10-14'
        WHEN age BETWEEN 15 AND 19 THEN '15-19'
        WHEN age BETWEEN 20 AND 24 THEN '20-24'
        WHEN age BETWEEN 25 AND 29 THEN '25-29'
        WHEN age BETWEEN 30 AND 34 THEN '30-34'
        WHEN age BETWEEN 35 AND 39 THEN '35-39'
        WHEN age BETWEEN 40 AND 44 THEN '40-44'
        WHEN age BETWEEN 45 AND 49 THEN '45-49'
        WHEN age BETWEEN 50 AND 54 THEN '50-54'
        WHEN age BETWEEN 55 AND 59 THEN '55-59'
        WHEN age BETWEEN 60 AND 64 THEN '60-64'
        WHEN age BETWEEN 65 AND 69 THEN '65-69'
        WHEN age BETWEEN 70 AND 74 THEN '70-74'
        WHEN age BETWEEN 75 AND 79 THEN '75-79'
        WHEN age BETWEEN 80 AND 84 THEN '80-84'
        WHEN age >= 85 THEN '85+' END age,
        cem.meshblock_13 meshblock, mb.cau_id cau, gender, dp.nzdep2013 dep_idx

        from bnztrans.customer c
        INNER JOIN bnztrans.customer_ext_meshblocks cem
        ON (c.customer_id = cem.customer_id)
        LEFT JOIN census.meshblock_2013 mb
        ON (cem.meshblock_13 = mb.id)
        LEFT JOIN CENSUS.NZDEP13_MB dp
        ON (cem.meshblock_13 = dp.mb13)

        WHERE c.age between 15 and 120
        and cem.meshblock_13 > 0
        and c.gender in ('M', 'F')"
      )
      )


    # 4. Weighting Table ----
    cat("Copying Weighting Table (Step 4 of 7)\n")

    tablename4 <- toupper(paste0("combined_", datetime, "_weights"))

    # sqlDrop(DB, tablename4, errors = FALSE)

    RODBC::sqlQuery(
      db_con,
      glue::glue(
        "CREATE TABLE {tablename4} compress as
        Select seqmonth, cau, age, weight

        from MVCORE.BNZ_13_CUST_WEIGHTING_FINAL

        WHERE seqmonth between {start_month} and {end_month}"
      )
      )

    RODBC::sqlQuery(db_con,
                    glue::glue("DELETE from {tablename4} WHERE weight is null or weight = 0"))



    # 5. Raw BNZ Spending ----
    # -- Run the Raw BNZ Data
    # -- Pretty much a normal query - Except we're weighting to account for Demographics
    cat("Getting Raw BNZ Spending (Step 5 of 7)\n")

    tablename5 <- toupper(paste0("combined_", datetime, "_bnz"))

    RODBC::sqlQuery(
      db_con,
      glue::glue(
        "
        CREATE TABLE {tablename5} compress as
        Select g.*, bnz.seqday, bnz.seqmonth,
        tc.age, tc.gender, tc.meshblock,
        sum(transaction_value * weight) spend, sum(weight) trans

        from {tablename1} g
        INNER JOIN fozzie.grouped_elements ge
        ON (g.group_id = ge.group_id)
        INNER JOIN bnztrans.bnztrans bnz
        ON (ge.element_id = bnz.receiver_id and ge.element_type = bnz.receiver_type)
        INNER JOIN {tablename3} tc
        ON (bnz.customer_id = tc.customer_id)
        INNER JOIN {tablename4} w
        ON (bnz.seqmonth = w.seqmonth and tc.age = w.age and tc.cau = w.cau)

        WHERE bnz.seqmonth between {start_month} and {end_month}

        group by {paste0('g.', names(merch_tbl), collapse = ', ')},
        bnz.seqday, bnz.seqmonth,
        tc.age, tc.gender, tc.meshblock"
      )
      )


    # 6. Apportioning Ratios ----
    # -- Calculate the Ratios that each customer perrmutation makes up, so we can apply this to the aspiring number;
    # -- This can take a while  - Especially if you've opted for 'Rest of New Zealand' as a group, or if your customer splitting is very specific
    cat("Calculating Apportioning Ratios (Step 6 of 7)\n")

    # Need to get the column names of the table so we know what to apportion by
    bnz_names <- RODBC::sqlColumns(DB, tablename5)$COLUMN_NAME

    bnz_names <-
      bnz_names[bnz_names %in% c("SEQMONTH", "GROUP_ID", "SPEND", "TRANS") == FALSE]

    tablename6 <- toupper(paste0("combined_", datetime, "_ratios"))

    RODBC::sqlQuery(
      db_con,
      glue::glue(
        "
        CREATE TABLE {tablename6} compress as
        Select group_id, seqmonth,
        {paste(bnz_names, collapse = ', ')},
        spend/sum(spend) over (partition by group_id, seqmonth) spend_ratio,
        trans/sum(trans) over (partition by group_id, seqmonth) trans_ratio

        from {tablename5}"
      )
      )


    # 7. Final Dataset ----
    cat("Building Final Combined Dataset (Step 7 of 7)\n")

    tablename7 <- toupper(final_tablename)

    RODBC::sqlQuery(
      db_con,
      glue::glue(
        "
        CREATE TABLE {tablename7} compress as
        Select
        d.seqmonth, g.group_id,
        {paste('r.', bnz_names, collapse = ', ')},
        round(sum(spend*spend_ratio),2) spend, ceil(sum(trans*trans_ratio)) trans

        from {tablename1} g
        INNER JOIN {tablename2} d
        ON (g.group_id = d.group_id)
        INNER JOIN {tablename6} r
        ON (d.seqmonth = r.seqmonth and d.group_id = r.group_id)

        group by
        d.seqmonth, g.group_id,
        {paste('r.', bnz_names, collapse = ', ')}"
      )
      )


    # 8. Drop the working tables ----
    on.exit({
      cat("Dropping Working Tables\n")
      RODBC::sqlDrop(db_con, tablename1)
      RODBC::sqlDrop(db_con, tablename2)
      RODBC::sqlDrop(db_con, tablename3)
      RODBC::sqlDrop(db_con, tablename4)
      RODBC::sqlDrop(db_con, tablename5)
      RODBC::sqlDrop(db_con, tablename6)
    })
  }
