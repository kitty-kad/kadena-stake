;  Uncomment out before deploying
;  (namespace "free")

;  Uncomment before first deployment
;  (define-keyset "kadena-stake" (read-keyset "kadena-stake"))

(module kadena-stake "kadena-stake" "An example staking smart contract"

    (defconst ADMIN_KEYSET (read-keyset 'kadena-stake))

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
        balance:decimal
        token:module{fungible-v2}
        account:string
    )

    (defschema stakes-schema
        @doc "Stores staking information for users, key is account + pool name"
        id:string
        apy:decimal
        balance:decimal
        stake-time:time
        account:string
    )

    (deftable pools:{pools-schema})
    (deftable stakes:{stakes-schema})

    ;;;;;;;;; CODE THAT NEEDS PERMISSIONS / CAPABILITIES ;;;;;;;;;;;;;;;
    
    ;;;;; Pool Related
    (defun create-pool (pool-info:object{pools-schema})
        @doc "Creates a new pool"
        (insert pools (at "id" pool-info) pool-info)
        (let 
            (
                (id (at "id" pool-info))
                (token:module{fungible-v2} (at "token" pool-info))
                (account (at "account" pool-info))
                (balance (at "balance" pool-info))
            )
            (token::transfer-create account id  ADMIN_KEYSET balance)
        )
    )

    (defun deactivate-pool (pool-id:string)
        @doc "Deactivates a pool and withdraws un-used funds back to "
        (+ 1 1)
    )

    ;;;;; User Related
    (defun add-stake (pool-id:string, account:string, amount:decimal)
        @doc "Adds a stake for a user, claims rewards before doing so"
        (+ 1 1)
    )

    (defun claim-rewards (pool-id:string account:string)
        @doc "Claims the rewards a user is owed"
        (+ 1 1)
    )

    (defun withdraw-stake ()
        @doc "Withdraws users stake complete and claims rewards before doing so"
        (+ 1 1)
    )

    ;;;;;;;;;;;; HELPER FUNCTINONS ;;;;;;;;;;;;;;;;;;;
    (defun calculate-rewards (pool-id:string, account:string)
        @doc "Calculates rewards for an account in a pool"
        (+ 1 1)
    )


)

(create-table pools)
(create-table stakes)
