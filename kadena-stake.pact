(enforce-pact-version "3.7")

(namespace (read-msg 'ns))

(module kadena-stake GOVERNANCE "An example staking smart contract"

    ;;;;; CONSTANTS

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
        starting-balance:decimal
        reward-token:module{fungible-v2}
        stake-token:module{fungible-v2}
        account:string
        active:bool
        max-reward-per-account:decimal
        claim-wait-seconds:decimal
        max-reward-per-claim:decimal
        stake-token-is-lp:bool
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
    ;starting-balance- amount of reward token given by pool owner to be distributed to stakers, ex: 200
    ;apy- constant apy of pool reward, ex: 10.0
    ;reward-token- name of fungible-v2 reward token provided by pool creator, ex: coin
    ;stake-token- name of fungible-v2 token stakers must stake for reward token, ex: coin
    ;account- pool creator account, ex: k:mykaccount
    ;max-reward-per-account- max rewards a stakers account can ever claim in the pool, ex: 200
    ;claim-wait-seconds- minimum number of seconds between staker reward claims, ex: 0
    ;max-reward-per-claim- max rewards a staker account can claim per wait duration, ex: 200

    (defun create-pool (id:string
                        name:string
                        apy:decimal
                        starting-balance:decimal
                        reward-token:module{fungible-v2}
                        stake-token:module{fungible-v2}
                        account:string
                        max-reward-per-account:decimal
                        claim-wait-seconds:decimal
                        max-reward-per-claim:decimal)

        @doc "Creates a new pool - Stakers stake fungi-v2"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce-unit starting-balance (reward-token::precision))
            ;Transfer token to pool
            (reward-token::transfer-create account id (kadena-stake-vault-guard) starting-balance)
            ;If reward-token != stake-token then create a token account for stake-token
            (if (= (reward-token::details id) (stake-token::details id)) true (stake-token::create-account id (kadena-stake-vault-guard)))
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "apy": apy,
                    "starting-balance": starting-balance,
                    "reward-token": reward-token,
                    "stake-token": stake-token,
                    "account": account,
                    "active": true,
                    "max-reward-per-account": max-reward-per-account,
                    "claim-wait-seconds": claim-wait-seconds,
                    "max-reward-per-claim": max-reward-per-claim,
                    "stake-token-is-lp": false
                }
            )
            ;Insert pool blank usage
            (insert pools-usage id {
                "tokens-locked": 0.0,
                "last-updated": (at "block-time" (chain-data)),
                "owed": 0.0,
                "paid": 0.0
            })
        )
    )

    (defun create-pool-lp (id:string
                        name:string
                        apy:decimal
                        starting-balance:decimal
                        reward-token:module{fungible-v2}
                        stake-token1:module{fungible-v2}
                        stake-token2:module{fungible-v2}
                        account:string
                        max-reward-per-account:decimal
                        claim-wait-seconds:decimal
                        max-reward-per-claim:decimal)

        @doc "Creates a new pool for LP - Stakers stake aswap LP tokens"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce-unit starting-balance (reward-token::precision))
            ;Transfer token
            (reward-token::transfer-create account id (kadena-stake-vault-guard) starting-balance)
            (test.tokens.create-account (get-pair-key stake-token1 stake-token2) id (kadena-stake-vault-guard))
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "apy": apy,
                    "starting-balance": starting-balance,
                    "reward-token": reward-token,
                    "stake-token": coin,
                    "account": account,
                    "active": true,
                    "max-reward-per-account": max-reward-per-account,
                    "claim-wait-seconds": claim-wait-seconds,
                    "max-reward-per-claim": max-reward-per-claim,
                    "stake-token-is-lp": true
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
                           (to-pay (- (at "starting-balance" pool-data) (at "paid" pool-usage-data)) )
                           (pool-balance (token::get-balance pool-id))
                        )
                        ;Enforce pool owner
                        (enforce (= (at "account" pool-data) account) "Access prohibited.")
                        ;Enforce active pool
                        (enforce (= (at "active" pool-data) true) "Staking pool is not active.")
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
                                  "apy": 0.0
                              }
                              pool-data
                          )
                        )
                        ;Update pool usage
                        (update pools-usage pool-id
                          (+
                              {
                                  "owed": 0.0,
                                  "last-updated": (at "block-time" (chain-data))
                              }
                              pool-usage-data
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
                              "starting-balance": (+ (at "starting-balance" pool-data) amount)
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
            (let
                (
                    (pool-data (read pools pool-id ["stake-token" "apy" "active" "account" "stake-token-is-lp"]))
                    (stake-id (get-stake-id-key account pool-id))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "stake-token" pool-data))
                        (pool-lp-stats (if (= (at "stake-token-is-lp" pool-data) false) true (read pool-lp-stats pool-id)))
                    )
                    (let
                        (
                            (lp-token1:module{fungible-v2} (if (= (at "stake-token-is-lp" pool-data) false) coin (at "lp-token1" pool-lp-stats) ))
                            (lp-token2:module{fungible-v2} (if (= (at "stake-token-is-lp" pool-data) false) coin (at "lp-token2" pool-lp-stats) ))
                        )
                        ;Enforce active pool
                        (enforce (= (at "active" pool-data) true) "Staking pool is not active.")
                        ;Enforce stakers only
                        (enforce (!= (at "account" pool-data) account) "Pool owners may not stake their own pools.")
                        ;Transfer stake to pool
                        (if (= (at "stake-token-is-lp" pool-data) false) (token::transfer account pool-id amount) (test.tokens.transfer (get-pair-key lp-token1 lp-token2) account pool-id amount) )
                        ;(token::transfer account pool-id amount)
                        ;Insert stake
                        (write stakes stake-id {
                            "id": stake-id,
                            "pool-id": pool-id,
                            "balance": amount,
                            "last-updated":  (at "block-time" (chain-data)),
                            "account": account
                        })
                        (write pool-user-stats stake-id {
                            "total-earned": 0.0
                        })
                        (with-capability (PRIVATE)
                            (update-pool-usage-after-locked-amount-changed pool-id token amount)
                        )
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
                    "owed": (abs(+
                        (at "owed" pool-usage-data)
                        (calculate-owed-upto-now
                            (at "tokens-locked" pool-usage-data)
                            (at "last-updated" pool-usage-data)
                            (at "apy" pool-data)
                            (token::precision)
                            pool-id
                        )
                    )),
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
        (with-capability (ACCOUNT_GUARD account)
            (let
                (
                    (stake-id (get-stake-id-key account pool-id))
                    (pool-data (read pools pool-id ["reward-token" "apy" "active" "starting-balance" "max-reward-per-account" "max-reward-per-claim" "claim-wait-seconds"]))
                    (pool-usage-data (read pools-usage pool-id))

                )
                (let
                    (
                        (token:module{fungible-v2} (at "reward-token" pool-data))
                        (apy (at "apy" pool-data))
                        (stake (read stakes stake-id))
                        (pool-user-data (read pool-user-stats stake-id))
                    )
                    ;Determine if user is to be paid
                    (let
                        (
                           (calculated-reward (calculate-owed-upto-now (at "balance" stake) (at "last-updated" stake) (at "apy" pool-data) (token::precision) pool-id))
                        )
                        ;Check if user can stake due to time then determine if reward is bigger then max-reward-per-claim
                        ; (if (> calculated-reward (at "max-reward-per-claim" pool-data)) (at "max-reward-per-claim" pool-data) (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) calculated-reward 0.0))
                        ; (to-pay (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) calculated-reward 0.0))
                        (let
                            (
                               (to-pay (if (> (diff-time (at "block-time" (chain-data)) (at 'last-updated stake)) (at "claim-wait-seconds" pool-data) ) (if (> calculated-reward (at "max-reward-per-claim" pool-data)) (at "max-reward-per-claim" pool-data) (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) calculated-reward 0.0)) 0.0))

                            )
                            ;Enforce account
                            (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                            ;Enforce active pool
                            (enforce (= (at "active" pool-data) true) "Staking pool is not active.")
                            ;Install transfer capability determining if pool will empty and deactivate
                            (if (>= (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ) to-pay) (install-capability (token::TRANSFER pool-id account to-pay)) (install-capability (token::TRANSFER pool-id account (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ))) )
                            ;Transfer reward token depending on pool deactivating due to becoming empty
                            (if (>= (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ) to-pay) (token::transfer pool-id account to-pay) (token::transfer pool-id account (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) )) )
                            ;Update stake time
                            (update stakes stake-id  (+ {"last-updated":  (at "block-time" (chain-data))} stake))
                            (update pools-usage pool-id
                                (+
                                    {
                                        "last-updated": (at "block-time" (chain-data)),
                                        "paid": (if (>= (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ) to-pay) (+ (at "paid" pool-usage-data) to-pay) (+ (at "paid" pool-usage-data) (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) )) ),
                                        "owed": (abs (calculate-owed-upto-now (-(at "tokens-locked" pool-usage-data) to-pay) (at "last-updated" pool-usage-data) (at "apy" pool-data) (token::precision) pool-id))
                                    }
                                    pool-usage-data
                                )
                            )
                            (write pool-user-stats stake-id {
                                "total-earned": (+ (at "total-earned" pool-user-data) to-pay)
                            })
                            (if (>= (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ) to-pay) (update pools pool-id { "active" : true }) (update pools pool-id { "active" : false }) )
                            (if (>= (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ) (at "owed" (read pools-usage pool-id))) (update pools pool-id { "active" : true }) (update pools pool-id { "active" : false }) )
                        )
                    )
                )
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
                (let*
                    (
                        (reward-token:module{fungible-v2} (at "reward-token" pool-data))
                        (stake-token:module{fungible-v2} (at "stake-token" pool-data))
                        (stake (read stakes stake-id))
                        (pool-user-data (read pool-user-stats stake-id))
                        (pool-lp-stats (if (= (at "stake-token-is-lp" pool-data) false) true (read pool-lp-stats pool-id)))
                    )
                    ;(to-pay-reward (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) (calculate-owed-upto-now (at "balance" stake) (at "last-updated" stake) (at "apy" pool-data) (reward-token::precision) pool-id) 0.0))
                    (let
                        (
                           (lp-token1:module{fungible-v2} (if (= (at "stake-token-is-lp" pool-data) false) coin (at "lp-token1" pool-lp-stats) ))
                           (lp-token2:module{fungible-v2} (if (= (at "stake-token-is-lp" pool-data) false) coin (at "lp-token2" pool-lp-stats) ))
                           (to-pay-stake (at "balance" stake))
                           (calculated-reward (calculate-owed-upto-now (at "balance" stake) (at "last-updated" stake) (at "apy" pool-data) (reward-token::precision) pool-id))
                        )
                        (let
                            (
                               (to-pay-reward (if (> (diff-time (at "block-time" (chain-data)) (at 'last-updated stake)) (at "claim-wait-seconds" pool-data) ) (if (> calculated-reward (at "max-reward-per-claim" pool-data)) (at "max-reward-per-claim" pool-data) (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) calculated-reward 0.0)) 0.0))
                            )
                            ;Enforce account
                            (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                            ;Check if user can get a reward or not due to max reward limit per account
                            ;And
                            ;Check if reward token and stake token are same type for transfer capability modification
                            ;And
                            ;Make stake and reward transfers accordingly
                            (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) (if (= (reward-token::details pool-id) (stake-token::details pool-id)) (install-capability (reward-token::TRANSFER pool-id account (+ to-pay-stake to-pay-reward))) (install-capability (reward-token::TRANSFER pool-id account to-pay-reward))) true)
                            (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) (if (= (reward-token::details pool-id) (stake-token::details pool-id)) (reward-token::transfer pool-id account (+ to-pay-stake to-pay-reward)) (reward-token::transfer pool-id account to-pay-reward)) true )
                            (if (= (at "stake-token-is-lp" pool-data) false) (if (= (reward-token::details pool-id) (stake-token::details pool-id)) true (install-capability (stake-token::TRANSFER pool-id account to-pay-stake)) ) (install-capability (test.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) pool-id account to-pay-stake)) )
                            (if (= (at "stake-token-is-lp" pool-data) false) (if (= (reward-token::details pool-id) (stake-token::details pool-id)) true (stake-token::transfer pool-id account to-pay-stake) ) (test.tokens.transfer (get-pair-key lp-token1 lp-token2) pool-id account to-pay-stake) )
                            ;(if (= (reward-token::details pool-id) (stake-token::details pool-id)) true (install-capability (stake-token::TRANSFER pool-id account to-pay-stake)) )
                            ;(if (= (reward-token::details pool-id) (stake-token::details pool-id)) true (stake-token::transfer pool-id account to-pay-stake) )
                            ;Update stake
                            (update stakes stake-id  (+ {"last-updated":  (at "block-time" (chain-data)), "balance": (-(at "balance" stake) to-pay-stake) } stake))
                            ;Update pool usage
                            (update pools-usage pool-id
                                (+
                                    {
                                        "last-updated": (at "block-time" (chain-data)),
                                        "tokens-locked": (- (at "tokens-locked" pool-usage-data) to-pay-stake),
                                        "paid": (if (>= (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ) to-pay-reward) (+ (at "paid" pool-usage-data) to-pay-reward) (+ (at "paid" pool-usage-data) (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) )) ),
                                        "owed": (abs (-
                                                  (+
                                                    (at "owed" pool-usage-data)
                                                    (calculate-owed-upto-now
                                                        (at "tokens-locked" pool-usage-data)
                                                        (at "last-updated" pool-usage-data)
                                                        (at "apy" pool-data)
                                                        (reward-token::precision)
                                                        pool-id
                                                    )
                                                  )
                                                  to-pay-reward
                                                ))
                                    }
                                    pool-usage-data
                                )
                            )
                            ;Update total paid rewards to account
                            (write pool-user-stats stake-id {
                                "total-earned": (+ (at "total-earned" pool-user-data) to-pay-reward)
                            })
                            ;Check if we need to deactivate our pool due to it becoming empty
                            (if (>= (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ) to-pay-reward) (update pools pool-id { "active" : true }) (update pools pool-id { "active" : false }) )
                            (if (>= (- (at "starting-balance" pool-data) (at "paid" pool-usage-data) ) (at "owed" (read pools-usage pool-id))) (update pools pool-id { "active" : true }) (update pools pool-id { "active" : false }) )
                    )
                    )
                )
            )
          )
        )
    )

    (defun calculate-owed-upto-now (balance:decimal start-time:time apy:decimal precision:integer pool-id:string)
        @doc " Calculates tokens owed for an APY and a balance of tokens from start-time to now "
        (let
            (
                (time-passed (diff-time  (at "block-time" (chain-data)) start-time) )
                (pool-data (read pools pool-id ["starting-balance"]))
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

    (defun get-pool-info (pool-id: string)
      (+ (read pools pool-id) (read pools-usage pool-id))
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

)

;  (create-table pools)
;  (create-table pools-usage)
;  (create-table stakes)
