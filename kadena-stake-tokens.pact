(enforce-pact-version "3.7")

(namespace (read-msg 'ns))

(module kadena-stake-tokens GOVERNANCE "Kadena Stake Factory Tokens - For creating pools where aswap LP tokens are staked for fungiv2 token rewards"

    ;;;;; CONSTANTS
    (defconst KDS_BANK:string "kadena-stake-tokens-bank")

    (defconst SECONDS_IN_YEAR 31536000)

    (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
    " Allowed character set for Account IDs. ")

    (defconst ACCOUNT_ID_MIN_LENGTH 3
      " Minimum character length for account IDs. ")

    (defconst ACCOUNT_ID_MAX_LENGTH 256
      " Maximum character length for account IDs. ")

    (defconst NAME_MIN_LENGTH 3
      " Minimum character length for account IDs. ")

    (defconst NAME_MAX_LENGTH 30
      " Maximum character length for account IDs. ")

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
        balance:decimal
        reward-token:module{fungible-v2}
        account:string
        active:bool
        max-reward-per-account:decimal
        claim-wait-seconds:decimal
        max-reward-per-claim:decimal
        start-time:time
        reward-duration:decimal
        reward-amount:decimal
        apy-fixed:bool
    )

    (defschema pools-lp-schema
      @doc "LP Pool information if pool is LP style stake pool "
      lp-token1:module{fungible-v2}
      lp-token2:module{fungible-v2}

    )

    (defschema pools-usage-schema
        @doc "Pool usage data"
        tokens-locked:decimal
        last-updated:time
        owed:decimal
        paid:decimal
    )

    (defschema pools-user-stats-schema
      @doc "User total reward earnings in a pool"
      total-earned:decimal)


    (defschema stakes-schema
        @doc "Stores staking information for users, key is account + pool name"
        id:string
        pool-id:string
        balance:decimal
        last-updated:time
        account:string
    )

    (deftable pools:{pools-schema})
    (deftable pool-lp-stats:{pools-lp-schema})
    (deftable pools-usage:{pools-usage-schema})
    (deftable pool-user-stats:{pools-user-stats-schema})
    (deftable stakes:{stakes-schema})

    ;;;;;;;;; CODE THAT NEEDS PERMISSIONS / CAPABILITIES ;;;;;;;;;;;;;;;

    ;;;;; Pool Related

    ;create pool - for creating staking pool of reward token/stake token type fungi-v2/fungi-v2
    ;id- id of pool, ex: "test-pool"
    ;name- name of pool, ex: "Test Pool"
    ;balance- amount of reward token given by pool owner to be distributed to stakers, ex: 200
    ;apy- constant apy of pool reward, ex: 10.0
    ;reward-token- name of fungible-v2 reward token provided by pool creator, ex: coin
    ;stake-token1- name of fungible-v2 token which is first side of LP pair, ex: coin
    ;stake-token2- name of fungible-v2 token which is second side of LP pair, ex: coin
    ;account- pool creator account, ex: k:mykaccount
    ;max-reward-per-account- max rewards a stakers account can ever claim in the pool, ex: 200
    ;claim-wait-seconds- minimum number of seconds between staker reward claims, ex: 0
    ;max-reward-per-claim- max rewards a staker account can claim per wait duration, ex: 200
    ;reward-duration: if apy is not fixed, this is time it takes for rewards to become available to stakers, ex: 86400
    ;reward-amount: if apy is not fixed, this is the amount of rewards available each reward-duration, ex: 10
    ;apy-fixed: true or false for creating a pool with fixed apy (use apy variable) or variable apy (use reward-duration/reward-amount)

    (defun create-pool-lp (id:string
                        name:string
                        apy:decimal
                        balance:decimal
                        reward-token:module{fungible-v2}
                        stake-token1:module{fungible-v2}
                        stake-token2:module{fungible-v2}
                        account:string
                        max-reward-per-account:decimal
                        claim-wait-seconds:decimal
                        max-reward-per-claim:decimal
                        reward-duration:decimal
                        reward-amount:decimal
                        apy-fixed:bool )

        @doc "Creates a new pool for LP Tokens where Stakers stake and earn aswap LP tokens"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce-unit balance (reward-token::precision))
            ;Transfer token
            (reward-token::create-account id (kadena-stake-vault-guard))
            (reward-token::transfer-create account id (kadena-stake-vault-guard) balance)
            (test.tokens.create-account (get-pair-key stake-token1 stake-token2) id (kadena-stake-vault-guard))
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "apy": apy,
                    "balance": balance,
                    "reward-token": reward-token,
                    "account": account,
                    "active": true,
                    "max-reward-per-account": max-reward-per-account,
                    "claim-wait-seconds": (if (= apy-fixed true) claim-wait-seconds reward-duration ),
                    "max-reward-per-claim": max-reward-per-claim,
                    "start-time": (at "block-time" (chain-data)),
                    "reward-duration": reward-duration,
                    "reward-amount": reward-amount,
                    "apy-fixed": apy-fixed
                }
            )
            ;Insert pool blank usage
            (insert pools-usage id {
                "tokens-locked": 0.0,
                "last-updated": (at "block-time" (chain-data)),
                "owed": 0.0,
                "paid": 0.0
            })
            ;Insert pool blank usage
            (insert pool-lp-stats id {
                "lp-token1": stake-token1,
                "lp-token2": stake-token2
            })
            ;Return a message
            (format "Created pool {} with {} {}" [name balance reward-token])
        )
    )

    (defun deactivate-pool (pool-id:string account:string)
        @doc "Deactivates a pool and withdraws un-used funds back to pool creator if possible"
        (with-capability (ACCOUNT_GUARD account)
        (let
            (
                (pool-data (read pools pool-id))
                (pool-usage-data (read pools-usage pool-id))
            )
            (let
                (
                    (token:module{fungible-v2} (at "reward-token" pool-data))
                )
                (let
                    (
                       (variable-rewards (calculate-available-now (at "start-time" pool-data) pool-id) )
                       (fixed-rewards (+ (calculate-owed-upto-now (at "tokens-locked" pool-usage-data) (at "last-updated" pool-usage-data) (at "apy" pool-data) (token::precision) pool-id) (at "owed" pool-usage-data) ) )
                    )
                    (let
                        (
                           (calculated-owed (if (= (at "apy-fixed" pool-data) true) fixed-rewards (- variable-rewards (- (at "paid" pool-usage-data) variable-rewards )  ) ))
                        )
                        (let
                            (
                               (calculated-unclaimed-max (round (- calculated-owed (at "paid" pool-usage-data)) (token::precision)) )
                            )
                            (let
                                (
                                   (calculated-unclaimed (if (< calculated-unclaimed-max 0.0) 0.0 calculated-unclaimed-max) )
                                )
                                (let
                                    (
                                       (to-pay-max (floor (- (- (at "balance" pool-data) (at "paid" pool-usage-data)) calculated-unclaimed) (token::precision)) )
                                       (pool-balance (token::get-balance pool-id))
                                    )
                                    (let
                                        (
                                           (to-pay (if (= (at "tokens-locked" pool-usage-data) 0.0) (token::get-balance pool-id) (if (< to-pay-max 0.0) 0.0 (floor (- to-pay-max (/ to-pay-max 1000) ) (token::precision)) )) )
                                        )
                                        ;Enforce pool owner
                                        (enforce (= (at "account" pool-data) account) "Access prohibited.")
                                        ;Enforce pool not empty
                                        (enforce (> pool-balance 0.0) "Pool is empty.")
                                        ;Transfer pool starting stake
                                        (install-capability (token::TRANSFER pool-id account to-pay) )
                                        (token::transfer pool-id account to-pay)
                                        ;Update pool
                                        (update pools pool-id
                                          (+
                                              {
                                                  "active": false,
                                                  "balance": (- (at "balance" pool-data) to-pay)
                                              }
                                              pool-data
                                          )
                                        )
                                        ;Update pool usage
                                        (update pools-usage pool-id
                                          (+
                                              {
                                                  "last-updated": (at "block-time" (chain-data))
                                              }
                                              pool-usage-data
                                          )
                                        )
                                        ;Return a message
                                        (format "Returned {} {} and deactivated pool {}" [to-pay token pool-id])
                                    )
                                )
                            )
                         )
                      )
                  )
              )
        )
        )
    )


    (defun add-balance-to-pool (pool-id:string account:string amount:decimal)
        @doc "Adds more balance to a pool for staking distribution - Pool creator only"
        (with-capability (ACCOUNT_GUARD account)
        (let
                (
                    (pool-data (read pools pool-id))
                    (pool-usage-data (read pools-usage pool-id))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "reward-token" pool-data))
                    )
                    ;Enforce pool owner
                    (enforce (= (at "account" pool-data) account) "Access prohibited.")
                    ;Enforce token precision
                    (enforce-unit amount (token::precision))
                    ;Transfer token to pool
                    (token::transfer account pool-id amount)
                    ;Update pool
                    (update pools pool-id
                      (+
                          {
                              "balance": (+ (at "balance" pool-data) amount)
                          }
                          pool-data
                      )
                    )
                    ;Update pool usage
                    (with-capability (PRIVATE)
                        (update-pool-usage-after-locked-amount-changed pool-id token amount)
                    )
                )
        )
        )
    )

    (defun reactivate-pool (pool-id:string account:string)
        @doc "Reactivates a deactivated pool"
        (with-capability (ACCOUNT_GUARD account)
        (let
                (
                    (pool-data (read pools pool-id))
                    (pool-usage-data (read pools-usage pool-id))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "reward-token" pool-data))
                    )
                    ;Enforce pool owner
                    (enforce (= (at "account" pool-data) account) "Access prohibited.")
                    ;Update pool
                    (update pools pool-id
                      (+
                          {
                              "active": true
                          }
                          pool-data
                      )
                    )
                    ;Update pool usage for date
                    (with-capability (PRIVATE)
                        (update-pool-usage-after-locked-amount-changed pool-id token 0.0)
                    )
                )
        )
        )
    )

    ;  ;;;;; User Related
    (defun create-stake (pool-id:string account:string amount:decimal)
        @doc " Adds a stake for a user "
        (with-capability (ACCOUNT_GUARD account)
        (claim-rewards pool-id account)
            (let
                (
                    (pool-data (read pools pool-id ["apy" "active" "account" "reward-token"]))
                    (stake-id (get-stake-id-key account pool-id))
                )
                (let
                    (
                        (pool-lp-stats (read pool-lp-stats pool-id))
                    )
                    (let
                        (
                            (lp-token1:module{fungible-v2} (at "lp-token1" pool-lp-stats))
                            (lp-token2:module{fungible-v2} (at "lp-token2" pool-lp-stats))
                        )
                        ;Enforce active pool
                        (enforce (= (at "active" pool-data) true) "Staking pool is not active.")
                        ;Enforce stakers only
                        (enforce (!= (at "account" pool-data) account) "Pool owners may not stake their own pools.")
                        ;Transfer stake to pool
                        (test.tokens.transfer (get-pair-key lp-token1 lp-token2) account pool-id amount)
                        ;Insert stake
                        (with-default-read stakes stake-id
                          { "id" : stake-id, "pool-id" : pool-id, "balance" : 0.0, "last-updated" : (at "block-time" (chain-data)), "account" : account }
                          { "id" := t_id, "pool-id" := t_pool-id, "balance" := t_balance, "last-updated" := t_last-updated, "account" := t_account }
                          (write stakes stake-id {
                              "id": t_id,
                              "pool-id": t_pool-id,
                              "balance": (+ t_balance amount),
                              "last-updated": t_last-updated,
                              "account": t_account
                          })
                        )
                        (write pool-user-stats stake-id {
                            "total-earned": 0.0
                        })
                        (with-capability (PRIVATE)
                            (update-pool-usage-after-locked-amount-changed pool-id amount)
                        )
                        (format "Staked {} {} in pool {} with account {}" [amount (get-pair-key lp-token1 lp-token2)  pool-id account])
                    )
                )
            )
        )
    )

    (defun update-pool-usage-after-locked-amount-changed (pool-id:string locked-amount-change:decimal)
        @doc "Updates pool usage data after stakes are changed"
        (require-capability (PRIVATE))
        (let
            (
                (pool-usage-data (read pools-usage pool-id ["tokens-locked" "last-updated" "owed" "paid"]))
                (pool-data (read pools pool-id ["apy" "start-time" "apy-fixed"]))
                (pool-lp-stats (read pool-lp-stats pool-id))
            )
            (let
                (
                    (variable-rewards (calculate-available-now (at "start-time" pool-data) pool-id) )
                    (fixed-rewards (+ (calculate-owed-upto-now (at "tokens-locked" pool-usage-data) (at "last-updated" pool-usage-data) (at "apy" pool-data) (test.tokens.precision (get-pair-key (at "lp-token1" pool-lp-stats) (at "lp-token2" pool-lp-stats))) pool-id) (at "owed" pool-usage-data) ) )
                )
                (update pools-usage pool-id
                    {
                        "tokens-locked": (+ (at "tokens-locked" pool-usage-data) locked-amount-change),
                        "last-updated": (at "block-time" (chain-data)),
                        "owed": (if (= (at "apy-fixed" pool-data) true) fixed-rewards variable-rewards),
                        "paid": (at "paid" pool-usage-data)
                    }
                )
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
        (with-capability (ACCOUNT_GUARD account)
        (with-default-read stakes (get-stake-id-key account pool-id)
          { "account" : 'nonulls, "balance" : 0.0 }
          { "account" := t_account, "balance" := t_balance }
          (if (= account t_account)
            (let
                  (
                      (stake-id (get-stake-id-key account pool-id))
                      (pool-data (read pools pool-id ["reward-token" "apy" "active" "balance" "max-reward-per-account" "max-reward-per-claim" "claim-wait-seconds" "start-time" "apy-fixed"]))
                      (pool-usage-data (read pools-usage pool-id))

                  )
                  (let
                      (
                          (token:module{fungible-v2} (at "reward-token" pool-data))
                          (apy (at "apy" pool-data))
                          (stake (read stakes stake-id))
                          (pool-user-data (read pool-user-stats stake-id))
                          (available-rewards (calculate-available-now (at "start-time" pool-data) pool-id))
                      )
                      (let
                          (
                             (fixed-reward-max (calculate-owed-upto-now (at "balance" stake) (at "last-updated" stake) (at "apy" pool-data) (token::precision) pool-id))
                             (total-rewards (calculate-owed-upto-now (at "tokens-locked" pool-usage-data) (at "last-updated" stake) (at "apy" pool-data) (token::precision) pool-id))
                          )
                          (let
                              (
                                 (fixed-reward (if (>= (-(at "balance" pool-data) (at "paid" pool-usage-data)) fixed-reward-max) fixed-reward-max (-(at "balance" pool-data) (at "paid" pool-usage-data))) )
                              )
                              (let
                                  (
                                     (owner-percentage (/ fixed-reward  total-rewards) )
                                  )
                                  (let
                                      (
                                         (variable-reward-max (floor (* available-rewards owner-percentage) (token::precision)))
                                      )
                                      (let
                                          (
                                             (variable-reward (if (>= (-(at "balance" pool-data) (at "paid" pool-usage-data)) variable-reward-max) variable-reward-max (-(at "balance" pool-data) (at "paid" pool-usage-data))) )
                                          )
                                          (let
                                              (
                                                 (calculated-reward (if (= (at "apy-fixed" pool-data) true) fixed-reward variable-reward) )
                                              )
                                              (let
                                                  (
                                                     (to-pay (if (> (diff-time (at "block-time" (chain-data)) (at 'last-updated stake)) (at "claim-wait-seconds" pool-data) ) (if (> calculated-reward (at "max-reward-per-claim" pool-data)) (at "max-reward-per-claim" pool-data) (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) calculated-reward 0.0)) 0.0))

                                                  )
                                                  ;Enforce account
                                                  (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                                                  ;Enforce balance
                                                  (enforce (>= to-pay 0.0) "You have no rewards to claim in this pool.")
                                                  ;Enforce claim reward duration
                                                  (enforce (> (diff-time (at "block-time" (chain-data)) (at 'last-updated stake)) (at "claim-wait-seconds" pool-data) ) "You must wait the full claim reward duration before claiming rewards.")
                                                  ;Can the user recieve rewards? If so lets install a transfer capability to grant the proper amount while making sure not to exceed the pools available balance
                                                  (if (> to-pay 0.0)
                                                    (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) to-pay)
                                                      (install-capability (token::TRANSFER pool-id account to-pay))
                                                      (install-capability (token::TRANSFER pool-id account (- (at "balance" pool-data) (at "paid" pool-usage-data) )))
                                                    )
                                                  true)
                                                  ;Transfer reward token amount composed above if we can
                                                  (if (> to-pay 0.0)
                                                    (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) to-pay)
                                                      (token::transfer pool-id account to-pay)
                                                      (token::transfer pool-id account (- (at "balance" pool-data) (at "paid" pool-usage-data) ))
                                                    )
                                                  true)
                                                  ;Update user stake data
                                                  (update stakes stake-id  (+ {"last-updated":  (at "block-time" (chain-data))} stake))
                                                  ;update pool usage data
                                                  (update pools-usage pool-id
                                                      (+
                                                          {
                                                              "last-updated":  (if (!= t_balance 0.0) (at "block-time" (chain-data)) (at "last-updated" stake)),
                                                              "paid": (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) to-pay)
                                                                        (+ (at "paid" pool-usage-data) to-pay)
                                                                        (+ (at "paid" pool-usage-data) (- (at "balance" pool-data) (at "paid" pool-usage-data) ))
                                                                      ),
                                                              "owed": (if (= (at "apy-fixed" pool-data) true)
                                                                        (+ (calculate-owed-upto-now (at "tokens-locked" pool-usage-data) (at "last-updated" pool-usage-data) (at "apy" pool-data) (token::precision) pool-id) (at "owed" pool-usage-data) )
                                                                        available-rewards
                                                                      )
                                                          }
                                                          pool-usage-data
                                                      )
                                                  )
                                                  (write pool-user-stats stake-id {
                                                      "total-earned": (+ (at "total-earned" pool-user-data) to-pay)
                                                  })
                                                  ;Check if pool is tapped empty and if it needs to deactivate
                                                  (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) to-pay)
                                                    (update pools pool-id { "active" : true })
                                                    (update pools pool-id { "active" : false })
                                                  )
                                                  ;Check if pool now owes all its balance and if it needs to deactivate
                                                  (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) (at "owed" (read pools-usage pool-id)))
                                                    (update pools pool-id { "active" : true })
                                                    (update pools pool-id { "active" : false })
                                                  )
                                                  ;Return a result message
                                                  (if (= (at "apy-fixed" pool-data) true)
                                                    (format "Awarded {} with {} {}" [account to-pay token])
                                                    (format "Awarded {} with {} {} for {}% of pool" [account to-pay token  (floor (* owner-percentage 100) 2) ])
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
                )
          true)
        )

        )
    )


    (defun withdraw-stake (account:string pool-id:string)
        @doc " Withdraws users stake and claims rewards "
        (with-capability (ACCOUNT_GUARD account)
          (with-capability (UPDATE)
          (let
                (
                    (stake-id (get-stake-id-key account pool-id))
                    (pool-data (read pools pool-id))
                    (pool-usage-data (read pools-usage pool-id))
                )
                (let
                    (
                        (reward-token:module{fungible-v2} (at "reward-token" pool-data))
                        (stake (read stakes stake-id))
                        (pool-user-data (read pool-user-stats stake-id))
                        (pool-lp-stats (read pool-lp-stats pool-id))
                        (available-rewards (calculate-available-now (at "start-time" pool-data) pool-id))
                    )
                    (let
                        (
                           (lp-token1:module{fungible-v2} (at "lp-token1" pool-lp-stats))
                           (lp-token2:module{fungible-v2} (at "lp-token1" pool-lp-stats))
                           (fixed-reward-max (calculate-owed-upto-now (at "balance" stake) (at "last-updated" stake) (at "apy" pool-data) (reward-token::precision) pool-id))
                           (to-pay-stake (at "balance" stake))
                        )
                        (let
                            (
                               (fixed-reward (if (>= (-(at "balance" pool-data) (at "paid" pool-usage-data)) fixed-reward-max)
                                              fixed-reward-max
                                              (-(at "balance" pool-data) (at "paid" pool-usage-data))
                                             )
                               )
                            )
                            (let
                                (
                                   (owner-percentage (/ fixed-reward  (calculate-owed-upto-now (at "tokens-locked" pool-usage-data) (at "last-updated" stake) (at "apy" pool-data) (reward-token::precision) pool-id) ) )
                                )
                                (let
                                    (
                                       (variable-reward-max (floor (* available-rewards owner-percentage) (reward-token::precision)) )
                                    )
                                    (let
                                        (
                                           (variable-reward (if (>= (-(at "balance" pool-data) (at "paid" pool-usage-data)) variable-reward-max)
                                                              variable-reward-max
                                                              (-(at "balance" pool-data) (at "paid" pool-usage-data))
                                                            )
                                           )
                                        )
                                        (let
                                            (
                                               (calculated-reward (if (= (at "apy-fixed" pool-data) true) fixed-reward variable-reward) )
                                            )
                                            (let
                                                (
                                                   (to-pay-reward (if (> (diff-time (at "block-time" (chain-data)) (at 'last-updated stake)) (at "claim-wait-seconds" pool-data) )
                                                                    (if (> calculated-reward (at "max-reward-per-claim" pool-data))
                                                                      (at "max-reward-per-claim" pool-data)
                                                                        (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data))
                                                                          calculated-reward
                                                                          0.0
                                                                        )
                                                                    )
                                                                    0.0
                                                                  )
                                                  )
                                                )
                                                ;Enforce account
                                                (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                                                ;Enforce stake
                                                (enforce (> to-pay-stake 0.0) "You dont have a stake in this pool to withdraw")
                                                ;Can the user recieve rewards or not? If so, lets create their reward transfer capability
                                                ;(if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) (install-capability (reward-token::TRANSFER pool-id account to-pay-reward)) true)
                                                ;Can the user recieve rewards? If so, lets make the reward transfer
                                                ;(if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) (reward-token::transfer pool-id account to-pay-reward) true )
                                                (if (> to-pay-reward 0.0) (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) (install-capability (reward-token::TRANSFER pool-id account to-pay-reward)) true) true)
                                                (if (> to-pay-reward 0.0) (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) (reward-token::transfer pool-id account to-pay-reward) true ) true)
                                                ;Lets install the stake token transfer capability to transfer the users stake back
                                                (install-capability (test.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) pool-id account to-pay-stake))
                                                ;Lets transfer the stake back
                                                (test.tokens.transfer (get-pair-key lp-token1 lp-token2) pool-id account to-pay-stake)
                                                ;Update users staking data
                                                (update stakes stake-id  (+ {"last-updated":  (at "block-time" (chain-data)), "balance": (-(at "balance" stake) to-pay-stake) } stake))
                                                ;Update pool usage data
                                                (update pools-usage pool-id
                                                      (+
                                                          {
                                                              "last-updated": (at "block-time" (chain-data)),
                                                              "tokens-locked": (- (at "tokens-locked" pool-usage-data) to-pay-stake),
                                                              "paid": (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) to-pay-reward)
                                                                        (+ (at "paid" pool-usage-data) to-pay-reward)
                                                                        (+ (at "paid" pool-usage-data) (- (at "balance" pool-data) (at "paid" pool-usage-data) ))
                                                                      ),
                                                              "owed": (if (= (at "apy-fixed" pool-data) true)
                                                                        (abs(-(+(calculate-owed-upto-now (at "tokens-locked" pool-usage-data) (at "last-updated" pool-usage-data) (at "apy" pool-data) (reward-token::precision) pool-id) (at "owed" pool-usage-data))to-pay-reward))
                                                                        available-rewards
                                                                      )
                                                          }
                                                          pool-usage-data
                                                      )
                                                  )
                                                  ;Update total paid rewards to account
                                                  (write pool-user-stats stake-id {
                                                      "total-earned": (+ (at "total-earned" pool-user-data) to-pay-reward)
                                                  })
                                                  ;Check if we need to deactivate our pool due to it becoming empty
                                                  (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) to-pay-reward)
                                                    (update pools pool-id { "active" : true })
                                                    (update pools pool-id { "active" : false })
                                                  )
                                                  ;Check if we need to deactivate our pool due to owing all thats available to stakers already
                                                  (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) (at "owed" (read pools-usage pool-id)))
                                                    (update pools pool-id { "active" : true })
                                                    (update pools pool-id { "active" : false })
                                                  )
                                                  ;Return a result message
                                                  (if (= (at "apy-fixed" pool-data) true)
                                                    (format "Awarded {} with {} {} and unstaked {} {}" [account to-pay-reward reward-token to-pay-stake (get-pair-key lp-token1 lp-token2)])
                                                    (format "Awarded {} with {} {} for {}% of pool, and unstaked {} {}" [account to-pay-reward reward-token (floor (* owner-percentage 100) 2) to-pay-stake (get-pair-key lp-token1 lp-token2)])
                                                  )
                                        )
                                      )
                                  )
                              )
                          )
                       )
                   )
                )
             )
          )
       )
    )

    ;;///////////////////////
    ;;UTILITIES / GETTERS
    ;;//////////////////////

    (defun calculate-available-now (start-time:time pool-id:string)
        @doc " Calculates reward tokens available for variable APY distribution at any given time "
        (let
            (
                (time-passed (diff-time  (at "block-time" (chain-data)) start-time) )
                (pool-data (read pools pool-id ["balance" "reward-duration" "reward-amount" "reward-token"]))
                (pool-usage-data (read pools-usage pool-id ["paid"]))
            )
            (let
                (
                    (token:module{fungible-v2} (at "reward-token" pool-data))

                )
                ;We clamp our reward calculation under the pool's available balance so we don't exceed it
                (let
                    (
                        (total-available (floor (* (/ time-passed (at "reward-duration" pool-data)) (at "reward-amount" pool-data)) (token::precision) ))
                        (max-available (floor (- (at "balance" pool-data) (at "paid" pool-usage-data) ) (token::precision)))
                    )
                    (enforce ( >= time-passed 0.0) "From date is in the future")
                    (if (<= total-available max-available ) total-available max-available)
                )
            )
        )
    )

    (defun calculate-owed-upto-now (balance:decimal start-time:time apy:decimal precision:integer pool-id:string)
        @doc " Calculates tokens owed for an APY and a balance of tokens from start-time to now "
        (let
            (
                (time-passed (diff-time  (at "block-time" (chain-data)) start-time) )
                (pool-data (read pools pool-id ["balance"]))
                (pool-usage-data (read pools-usage pool-id ["paid"]))

            )
            (enforce ( >= time-passed 0.0) "From date is in the future")
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

    (defun get-pool-info (pool-id:string)
      (+ (read pools pool-id) (read pools-usage pool-id))
    )

    (defun get-user-stakes (pool-id:string account:string)
      (read stakes (get-stake-id-key account pool-id))
    )

    (defun get-user-pools ( account:string )
      @doc " Get a list of pool IDs that a user is staking in "
        (select stakes ['pool-id]
          (and? (where 'account (= account))
            (where 'balance (< 0.0))))
    )

    (defun get-user-created-pools ( account:string )
      @doc " Get a list of pool IDs that a user has created "
        (select pools (where 'account (= account)))
    )

    (defun enforce-unit:bool (amount:decimal precision)
      @doc " Enforces precision "
      (enforce
        (= (floor amount precision)
           amount)
        "Minimum denomination exceeded.")
    )

   (defun enforce-valid-id ( id:string )
    @doc " Enforce that an account ID meets charset and length requirements. "
    (enforce
      (is-charset ACCOUNT_ID_CHARSET id)
      (format
        "Account ID does not conform to the required charset: {}"
        [id]))
    (let ((accountLength (length id)))
      (enforce
        (>= accountLength ACCOUNT_ID_MIN_LENGTH)
        (format
          "Account ID does not conform to the min length requirement: {}"
          [id]))
      (enforce
        (<= accountLength ACCOUNT_ID_MAX_LENGTH)
        (format
          "Account ID does not conform to the max length requirement: {}"
          [id]))))

  (defun enforce-valid-name ( name:string )
    @doc " Enforce that a Pool Name meets charset and length requirements. "
    (enforce
      (is-charset ACCOUNT_ID_CHARSET name)
      (format
        "Pool Name does not conform to the required charset: {}"
        [name]))
    (let ((nameLength (length name)))
      (enforce
        (>= nameLength NAME_MIN_LENGTH)
        (format
          "Pool Name does not conform to the min length requirement: {}"
          [name]))
      (enforce
        (<= nameLength NAME_MAX_LENGTH)
        (format
          "Pool Name does not conform to the max length requirement: {}"
          [name]))))

  (defun get-pair-key
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    " Create canonical key for lp pair staking."
    (format "{}:{}" (canonicalize tokenA tokenB))
  )

  (defun canonicalize:[module{fungible-v2}]
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (if (is-canonical tokenA tokenB) [tokenA tokenB] [tokenB tokenA])
  )

  (defun is-canonical
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (< (format "{}" [tokenA]) (format "{}" [tokenB]))
  )

  (defun get-token-key
    ( tokenA:module{fungible-v2} )
    " Create key from token module"
    (format "{}" [tokenA])
  )

  (defun initialize ()
    @doc " Initialize the contract. Can only happen once. "
    (coin.create-account KDS_BANK (create-module-guard "kadena-stake-tokens-bank"))
  )

)

;(create-table free.kadena-stake-tokens.pools)
;(create-table free.kadena-stake-tokens.pools-usage)
;(create-table free.kadena-stake-tokens.pool-lp-stats)
;(create-table free.kadena-stake-tokens.pool-user-stats)
;(create-table free.kadena-stake-tokens.stakes)
;(free.kadena-stake-tokens.initialize)
