;  Uncomment out before deploying
;  (namespace "free")

;  Uncomment before first deployment
;  (define-keyset "kadena-stake" (read-keyset "kadena-stake"))

(module kadena-stake "kadena-stake" "An example staking smart contract"

    (defconst ADMIN_KEYSET (read-keyset 'kadena-stake))
    (defconst STAKE_KEY_DELIMETER " | STAKE_KEY_DELIMETER | ")
    (defconst SECONDS_IN_YEAR 31536000)

    ;;;;; CAPABILITIES
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
            (token::transfer-create account id  ADMIN_KEYSET starting-balance)
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
                    (pool-data (read pools pool-id ["token" "tokens-locked" "last-updated" "apy"]))
                    (stake-id (get-stake-id account pool-id))
                    (calculate-owed-upto-now)
                )
                (let 
                    ( 
                        (token:module{fungible-v2} (at "token" pool-data))
                    )
                    (token::transfer account pool-id amount)
                )
                (insert stakes stake-id {
                    "id": stake-id,
                    "pool-id": pool-id,
                    "balance": amount,
                    "last-updated":  (at "block-time" (chain-data)),
                    "account": account
                })
                (calculate-owed-upto-now 
                    (at "tokens-locked" pool-data) 
                    (at "last-updated" pool-data) 
                    (at "apy" pool-data) 
                )
            )     
            ; TODO CALCULATE AMOUNT OWED FOR POOL AND UPDATE TOKENS LOCKED
        )
    )

    (defun claim-rewards (pool-id:string account:string)
        @doc "Claims the rewards a user is owed"
        (+ 1 1)
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
            (
                *
                (
                    *
                    (
                        /
                        time-passed
                        SECONDS_IN_YEAR
                    )
                    (/ apy 100)
                )
                balance
            )
        )
    )

    (defun get-stake-id (account:string pool-id:string )
        @doc "Gets the stake id for a user in a pool"
        (+ 
            account
            (+ STAKE_KEY_DELIMETER pool-id)
        )
    )


)

(create-table pools)
(create-table stakes)
