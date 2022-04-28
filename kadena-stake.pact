(enforce-pact-version "3.7")

(namespace (read-msg 'ns))

(module kadena-stake GOVERNANCE "An example staking smart contract"

    (defconst KADENA_STAKE_BANK "kadena-stake-bank")
    (defconst STAKE_KEY_DELIMETER " | STAKE_KEY_DELIMETER | ")
    (defconst SECONDS_IN_YEAR 31536000)

    (defun kadena-stake-vault-guard:guard () (create-module-guard "kadena-stake-holdings"))

    ;;;;; CAPABILITIES

    (defcap GOVERNANCE ()
      @doc " Give the admin full access to call and upgrade the module. "
      (enforce-keyset 'admin-kadena-stake)
    )

    (defcap ACCOUNT_GUARD(account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    ;;;;;;;;;; SCHEMAS AND TABLES ;;;;;;;;;;;;;;
    (defschema pools-schema
        @doc "Pool information, a unique id is the key"
        id:string
        name:string
        apy:decimal
        starting-balance:decimal
        token:module{fungible-v2}
        account:string
        tokens-locked:decimal
        last-updated:time
        owed:decimal
        paid:decimal
    )

    (defschema stakes-schema
        @doc "Stores staking information for users, key is account + pool name"
        id:string
        pool-id:string
        balance:decimal
        last-updated:time
        account:string
    )

    (deftable pools:{pools-schema})
    (deftable stakes:{stakes-schema})

    ;;;;;;;;; CODE THAT NEEDS PERMISSIONS / CAPABILITIES ;;;;;;;;;;;;;;;

    ;;;;; Pool Related
    (defun create-pool (id:string name:string apy:decimal starting-balance:decimal token:module{fungible-v2} account:string)
        @doc "Creates a new pool"
        (with-capability (ACCOUNT_GUARD account)
            (token::transfer-create account id (kadena-stake-vault-guard) starting-balance)
            (enforce-pool-id id)
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "apy": apy,
                    "starting-balance": starting-balance,
                    "token": token,
                    "account": account,
                    "tokens-locked": 0.0,
                    "last-updated": (at "block-time" (chain-data)),
                    "owed": 0.0,
                    "paid": 0.0
                }
            )
        )
    )

    (defun deactivate-pool (pool-id:string)
        @doc "Deactivates a pool and withdraws un-used funds back to "
        (+ 1 1)
    )

    ;  ;;;;; User Related
    (defun create-stake (pool-id:string account:string amount:decimal)
        @doc "Adds a stake for a user, claims rewards before doing so"
        (with-capability (ACCOUNT_GUARD account)
            (let
                (
                    (pool-data (read pools pool-id))
                    (stake-id (get-stake-id-key account pool-id))
                    (calculate-owed-upto-now)
                )
                ;Enforce unique stake id
                (enforce-stake-id stake-id)

                ;Insert stake
                (insert stakes stake-id {
                    "id": stake-id,
                    "pool-id": pool-id,
                    "balance": amount,
                    "last-updated":  (at "block-time" (chain-data)),
                    "account": account
                })
                (let
                    (
                        (token:module{fungible-v2} (at "token" pool-data))
                    )
                    (token::transfer account pool-id amount)
                    (floor (calculate-owed-upto-now (at "tokens-locked" pool-data) (at "last-updated" pool-data) (at "apy" pool-data) ) (token::precision) )

                    ;Update pools row
                    (update pools pool-id
                      { "id" : (at "id" pool-data)
                      , "name" : (at "name" pool-data)
                      , "apy" : (at "apy" pool-data)
                      , "starting-balance" : (at "starting-balance" pool-data)
                      , "token" : (at "token" pool-data)
                      , "account" : (at "account" pool-data)
                      , "tokens-locked" : (+ (at "tokens-locked" pool-data) amount)
                      , "last-updated" : (at "block-time" (chain-data))
                      , "owed" : (at "owed" pool-data)
                      , "paid" : (at "paid" pool-data) }
                    )
                )
            )

            ; TODO CALCULATE AMOUNT OWED FOR POOL ?

        )
    )

    (defun claim-rewards (pool-id:string account:string)
        @doc "Claims the rewards a user is owed"
        (let
                (
                    (pool-data (read pools pool-id))
                    (stake-data (read stakes (get-stake-id-key account pool-id) ))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "token" pool-data))
                    )
                    (floor (calculate-owed-upto-now (at "balance" stake-data) (at "last-updated" pool-data) (at "apy" pool-data) ) (token::precision) )
                )
            )
    )

    (defun withdraw-stake ()
        @doc "Withdraws users stake complete and claims rewards before doing so"
        (+ 1 1)
    )

    ;  ;;;;;;;;;;;; HELPER FUNCTINONS ;;;;;;;;;;;;;;;;;;;
    ;  (defun calculate-rewards (pool-id:string, account:string)
    ;      @doc "Calculates rewards for an account in a pool"
    ;      (let
    ;          (
    ;              (stake-info (read stakes (get-stake-id account pool-id) ["pool-id" "balance" "last-updated" "account"]))
    ;          )
    ;          ; TODO CACLULATE REWARDS LEFT
    ;      )
    ;  )

    (defun calculate-owed-upto-now (balance:decimal start-time:time apy:decimal)
        @doc "Calculates tokens for an APY and a balance of tokens from start-tme to now"
        (+ 1 1)

        (let
            (
                (time-passed (diff-time  (at "block-time" (chain-data)) start-time) )
            )
            (enforce ( > time-passed 0.0) "From date is in the future")
            (* (apy-for-ratio-of-seconds-in-year (seconds-over-seconds-in-year time-passed) apy) balance)
        )
    )

    (defun apy-for-ratio-of-seconds-in-year (ratio:decimal apy:decimal)
        (* ratio (/ apy 100))
    )

    (defun seconds-over-seconds-in-year (time-passed:decimal)
        @doc "Given seconds, calculate the % of a total year those seconds made up"
        ( / time-passed SECONDS_IN_YEAR)
    )


    (defun get-stake-id (account:string pool-id:string )
        @doc "Gets the stake id for a user in a pool"
        (+
            account
            (+ STAKE_KEY_DELIMETER pool-id)
        )
    )

    (defun get-stake-id-key ( account:string pool-id:string )
      @doc " Returns id/account data structure "
      (format "{}:{}" [account pool-id])
    )

    (defun enforce-pool-id ( id:string )
      @doc " Enforces a unique pool-id "
      (with-default-read pools id
          { 'id: 'nonulls }
          { 'id := id }
          (enforce (= 'nonulls id) "This ID already exists.")
      )
    )

      (defun enforce-stake-id ( id:string )
      @doc " Enforces a unique stake-id "
      (with-default-read stakes id
          { 'id: 'nonulls }
          { 'id := id }
          (enforce (= 'nonulls id) "This ID already exists.")
      )
    )


)

;(create-table pools)
;(create-table stakes)
