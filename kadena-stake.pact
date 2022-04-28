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
        (enforce (= "k:" (take 2 account)) "For security only support k accounts")
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap PRIVATE ()
        @doc "Only to be called with private contexts"
        true
    )

    (defcap UPDATE ()
      @doc " Capability to perform UPDATEing operations. "
      true
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
    )

    (defschema pools-usage-schema
        @doc "Pool usage data"
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
    (deftable pools-usage:{pools-usage-schema})
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
                    "account": account
                }
            )
            (insert pools-usage id {
                "tokens-locked": 0.0,
                "last-updated": (at "block-time" (chain-data)),
                "owed": 0.0,
                "paid": 0.0
            })
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
                    (pool-data (read pools pool-id ["token" "apy"]))
                    (stake-id (get-stake-id-key account pool-id))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "token" pool-data))
                    )
                    (token::transfer account pool-id amount)
                    (insert stakes stake-id {
                        "id": stake-id,
                        "pool-id": pool-id,
                        "balance": amount,
                        "last-updated":  (at "block-time" (chain-data)),
                        "account": account
                    })
                    (with-capability (PRIVATE)
                        (update-pool-usage-after-locked-amount-changed pool-id token amount)
                    )
                )
            )
        )
    )

    (defun update-pool-usage-after-locked-amount-changed (pool-id:string token:module{fungible-v2} locked-amount-change:decimal)
        @doc "Updates pool data after money was moved in or out of the pool"
        (require-capability (PRIVATE))
        (let
            (
                (pool-usage-data (read pools-usage pool-id ["tokens-locked" "last-updated" "owed" "paid"]))
                (pool-data (read pools pool-id ["apy"]))
            )
            (update pools-usage pool-id
                {
                    "tokens-locked": (+ (at "tokens-locked" pool-usage-data) locked-amount-change),
                    "last-updated": (at "block-time" (chain-data)),
                    "owed": (+
                        (at "owed" pool-usage-data)
                        (calculate-owed-upto-now
                            (at "tokens-locked" pool-usage-data)
                            (at "last-updated" pool-usage-data)
                            (at "apy" pool-data)
                            (token::precision)
                        )
                    ),
                    "paid": (at "paid" pool-usage-data)
                }
            )
        )
    )

    (defun update-pool-usage-after-rewards-claimed (pool-id:string claimed:decimal)
        @doc "Updates pool usage after rewards claimed"
        (require-capability (UPDATE))
        (let
            (
                (pool-usage-data (read pools-usage pool-id))
            )
            (update pools-usage pool-id
                (+
                    {
                        "last-updated": (at "block-time" (chain-data)),
                        "paid": (+ (at "paid" pool-usage-data) claimed)
                    }
                    pool-usage-data
                )
            )
        )
    )

    (defun claim-rewards (pool-id:string account:string)
        @doc "Claims the rewards a user is owed"
        (require-capability (UPDATE))
            (let
                (
                    (stake-id (get-stake-id-key account pool-id))
                    (pool-data (read pools pool-id ["token" "apy"]))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "token" pool-data))
                        (apy (at "apy" pool-data))
                        (stake (read stakes stake-id))
                    )
                    (let
                        (
                           (to-pay
                                (calculate-owed-upto-now
                                    (at "balance" stake)
                                    (at "last-updated" stake)
                                    (at "apy" pool-data)
                                    (token::precision)
                                )
                           )
                        )
                        (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                        (token::transfer pool-id account to-pay)
                        (update stakes stake-id  (+ {"last-updated":  (at "block-time" (chain-data))} stake))
                        (update-pool-usage-after-rewards-claimed pool-id to-pay)
                    )
                )
            )
    )

    (defun withdraw-stake (account:string pool-id:string)
        @doc "Withdraws users stake complete and claims rewards before doing so"
        (with-capability (ACCOUNT_GUARD account)
          (with-capability (UPDATE)
          (claim-rewards pool-id account)
          (let
                (
                    (stake-id (get-stake-id-key account pool-id))
                    (pool-data (read pools pool-id))
                    (pool-usage-data (read pools-usage pool-id))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "token" pool-data))
                        (stake (read stakes stake-id))
                    )
                    (let
                        (
                           (to-pay
                             (at "balance" stake)
                           )
                        )
                        (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                        (token::transfer pool-id account to-pay)
                        (update stakes stake-id  (+ {"last-updated":  (at "block-time" (chain-data)), "balance": 0.0} stake))
                        (update pools-usage pool-id
                            (+
                                {
                                    "last-updated": (at "block-time" (chain-data)),
                                    "tokens-locked": (- (at "tokens-locked" pool-usage-data) to-pay)
                                }
                                pool-usage-data
                            )
                        )
                    )
                )
            )
          )
        )
    )

    (defun calculate-owed-upto-now (balance:decimal start-time:time apy:decimal precision:integer)
        @doc "Calculates tokens for an APY and a balance of tokens from start-tme to now"
        (let
            (
                (time-passed (diff-time  (at "block-time" (chain-data)) start-time) )
            )
            (enforce ( > time-passed 0.0) "From date is in the future")
            (floor
                (
                    *
                    (apy-for-ratio-of-seconds-in-year (seconds-over-seconds-in-year time-passed) apy)
                    balance
                )
                precision
            )
        )
    )

    (defun apy-for-ratio-of-seconds-in-year (ratio:decimal apy:decimal)
        (* ratio (/ apy 100))
    )

    (defun seconds-over-seconds-in-year (time-passed:decimal)
        @doc "Given seconds, calculate the % of a total year those seconds made up"
        ( / time-passed SECONDS_IN_YEAR)
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

    (defun get-pools ()
      (keys pools)
    )

    (defun get-pool-info (pool-id: string)

    (+ (read pools pool-id) (read pools-usage pool-id))

    )


)

;  (create-table pools)
;  (create-table pools-usage)
;  (create-table stakes)
