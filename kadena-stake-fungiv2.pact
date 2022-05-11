(enforce-pact-version "3.7")

;(namespace 'free)

(namespace (read-msg 'ns))

(module kadena-stake-fungiv2 GOVERNANCE "Kadena Stake Fungiv2 - Stake FungiV2 Tokens + Create FungiV2 Staking Pools"

;Pool factory for creating Fungible-V2 / Fungible-V2 staking pools with various options

    ;;;;; CONSTANTS
    (defconst KDS_BANK:string "kadena-stake-bank")

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
        stake-token:module{fungible-v2}
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
    (deftable pools-usage:{pools-usage-schema})
    (deftable pool-user-stats:{pools-user-stats-schema})
    (deftable stakes:{stakes-schema})

    ;;;;;;;;; CODE THAT NEEDS PERMISSIONS / CAPABILITIES ;;;;;;;;;;;;;;;

    ;;;;; Pool Related

    ;create pool: for creating staking pool of reward token/stake token type fungi-v2/fungi-v2
    ;id: id of pool, ex: "test-pool"
    ;name: name of pool, ex: "Test Pool"
    ;balance: amount of reward token given by pool owner to be distributed to stakers, ex: 200
    ;apy: constant apy of pool reward, ex: 10.0
    ;reward-token: name of fungible-v2 reward token provided by pool creator, ex: coin
    ;stake-token: name of fungible-v2 token stakers must stake for reward token, ex: coin
    ;account: pool creator account, ex: k:mykaccount
    ;max-reward-per-account: max rewards a stakers account can ever claim in the pool, ex: 200
    ;claim-wait-seconds: minimum number of seconds between staker reward claims, ex: 0
    ;max-reward-per-claim: max rewards a staker account can claim per wait duration, ex: 200
    ;reward-duration: if apy is not fixed, this is time it takes for rewards to become available to stakers, ex: 86400
    ;reward-amount: if apy is not fixed, this is the amount of rewards available each reward-duration, ex: 10
    ;apy-fixed: true or false for creating a pool with fixed apy (use apy variable) or variable apy (use reward-duration/reward-amount)

    (defun create-pool (id:string
                        name:string
                        apy:decimal
                        balance:decimal
                        reward-token:module{fungible-v2}
                        stake-token:module{fungible-v2}
                        account:string
                        max-reward-per-account:decimal
                        claim-wait-seconds:decimal
                        max-reward-per-claim:decimal
                        reward-duration:decimal
                        reward-amount:decimal
                        apy-fixed:bool )

        @doc "Creates a new pool - Stakers stake and earn Fungible-v2 tokens"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce (<= reward-amount balance) "Rewards amount must not exceed the pools reward balance")
            (enforce (<= max-reward-per-claim balance) "Max rewards per claim must not exceed the pools reward balance")
            (enforce-unit balance (reward-token::precision))
            ;Create reward token account
            (reward-token::create-account id (kadena-stake-vault-guard))
            ;Transfer reward token
            (reward-token::transfer-create account id (kadena-stake-vault-guard) balance)
            ;If reward-token != stake-token then create a token account for stake-token too
            (if (= (get-token-key reward-token) (get-token-key stake-token)) true (if (= (reward-token::details id) (stake-token::details id)) true (stake-token::create-account id (kadena-stake-vault-guard))) )
            ;(if (= (reward-token::details id) (stake-token::details id)) true (stake-token::create-account id (kadena-stake-vault-guard)))
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "apy": apy,
                    "balance": balance,
                    "reward-token": reward-token,
                    "stake-token": stake-token,
                    "account": account,
                    "active": true,
                    "max-reward-per-account": max-reward-per-account,
                    "claim-wait-seconds": claim-wait-seconds,
                    "max-reward-per-claim": max-reward-per-claim,
                    "start-time": (at "block-time" (chain-data)),
                    "reward-duration": reward-duration,
                    "reward-amount": reward-amount,
                    "apy-fixed": apy-fixed

                }
            )
            ;Insert fresh blank pool usage for this new pool
            (insert pools-usage id {
                "tokens-locked": 0.0,
                "last-updated": (at "block-time" (chain-data)),
                "owed": 0.0,
                "paid": 0.0
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
                           (calculated-owed (if (= (at "apy-fixed" pool-data) true) fixed-rewards variable-rewards))
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
                                           (to-pay (if (< to-pay-max 0.0) 0.0 (floor (- to-pay-max (/ to-pay-max 1000) ) (token::precision)) ) )
                                        )
                                        ;to-pay
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
        @doc " Adds a users stake to a pool "
        (with-capability (ACCOUNT_GUARD account)
        (claim-rewards pool-id account)
            (let
                (
                    (pool-data (read pools pool-id ["stake-token" "apy" "active" "account"]))
                    (stake-id (get-stake-id-key account pool-id))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "stake-token" pool-data))
                    )
                        ;Enforce active pool
                        (enforce (= (at "active" pool-data) true) "Staking pool is not active.")
                        ;Enforce stakers only
                        (enforce (!= (at "account" pool-data) account) "Pool owners may not stake their own pools.")
                        ;Transfer stake to pool
                        (token::transfer account pool-id amount)
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
                        ;Reset total earned for user
                        (write pool-user-stats stake-id {
                            "total-earned": 0.0
                        })
                        ;Update pool usage
                        (with-capability (PRIVATE)
                            (update-pool-usage-after-locked-amount-changed pool-id token amount)
                        )
                        (format "Staked {} {} in pool {} with account {}" [amount (at "stake-token" pool-data) pool-id account])
                )
            )
        )
    )

    (defun update-pool-usage-after-locked-amount-changed (pool-id:string token:module{fungible-v2} locked-amount-change:decimal)
        @doc "Updates pool usage data after stakes are changed"
        (require-capability (PRIVATE))
        (let
            (
                (pool-usage-data (read pools-usage pool-id ["tokens-locked" "last-updated" "owed" "paid"]))
                (pool-data (read pools pool-id ["apy" "start-time" "apy-fixed"]))
            )
            (let
                (
                    (variable-rewards (calculate-available-now (at "start-time" pool-data) pool-id) )
                    (fixed-rewards (+ (calculate-owed-upto-now (at "tokens-locked" pool-usage-data) (at "last-updated" pool-usage-data) (at "apy" pool-data) (token::precision) pool-id) (at "owed" pool-usage-data) ) )
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
        @doc "Updates pool usage after rewards are claimed"
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
        @doc "Claims the rewards a user has earned from staking"
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
                      ;Lets determine rewards for our user depending on the pools options / APY, and get them sent out correctly.
                      ;fixed-rewards: rewards for pools with fixed apy options.
                      ;variable-rewards: rewards for pools with variable apy options.
                      ;owner-percentage: stakers percentage of total staked tokens in pool- used for converting between fixed/variable rewards.
                      ;calculated-reward: our final reward, either fixed or variable
                      ;to-pay: what the staker is actually paid. Always clamped under the pool's 'max-reward-per-claim' option and 'max-rewards-per-account' option.
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
                                                  ;Install reward token transfer capability, clamping transfer allowance below the pools available balance
                                                  (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) to-pay)
                                                    (install-capability (token::TRANSFER pool-id account to-pay))
                                                    (install-capability (token::TRANSFER pool-id account (- (at "balance" pool-data) (at "paid" pool-usage-data) )))
                                                  )
                                                  ;Transfer proper reward token allowance
                                                  (if (>= (- (at "balance" pool-data) (at "paid" pool-usage-data) ) to-pay)
                                                    (token::transfer pool-id account to-pay)
                                                    (token::transfer pool-id account (- (at "balance" pool-data) (at "paid" pool-usage-data) ))
                                                  )
                                                  ;Update the users stake time
                                                  (update stakes stake-id  (+ {"last-updated":  (at "block-time" (chain-data))} stake))
                                                  ;Update pool usage with proper paid + owed by determining if owed amount is based off a fixed or variable apy pool
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
                                                  ;Update total earned for user
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
                        (stake-token:module{fungible-v2} (at "stake-token" pool-data))
                        (stake (read stakes stake-id))
                        (pool-user-data (read pool-user-stats stake-id))
                        (available-rewards (calculate-available-now (at "start-time" pool-data) pool-id))
                    )
                      ;Lets determine stake and rewards to return to the user depending on the pools options / APY, and get them both sent out correctly.
                      ;fixed-rewards: rewards for pools with fixed apy options.
                      ;variable-rewards: rewards for pools with variable apy options.
                      ;owner-percentage: stakers percentage of total staked tokens in pool- used for converting between fixed/variable rewards.
                      ;calculated-reward: our final reward, either fixed or variable
                      ;to-pay-reward: what the staker is actually paid as a reward. Always clamped under the pool's 'max-reward-per-claim' option and 'max-rewards-per-account' option.
                      ;to-pay-stake: the stakers staked tokens amount to return
                      (let
                          (
                             (fixed-reward-max (calculate-owed-upto-now (at "balance" stake) (at "last-updated" stake) (at "apy" pool-data) (reward-token::precision) pool-id))
                             (to-pay-stake (at "balance" stake))
                          )
                          (let
                              (
                                 (fixed-reward (if (>= (-(at "balance" pool-data) (at "paid" pool-usage-data)) fixed-reward-max) fixed-reward-max (-(at "balance" pool-data) (at "paid" pool-usage-data))) )
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
                                             (variable-reward (if (>= (-(at "balance" pool-data) (at "paid" pool-usage-data)) variable-reward-max) variable-reward-max (-(at "balance" pool-data) (at "paid" pool-usage-data))) )
                                          )
                                          (let
                                              (
                                                 (calculated-reward (if (= (at "apy-fixed" pool-data) true) fixed-reward variable-reward) )
                                              )
                                              (let
                                                  (
                                                     (to-pay-reward (if (> (diff-time (at "block-time" (chain-data)) (at 'last-updated stake)) (at "claim-wait-seconds" pool-data) ) (if (> calculated-reward (at "max-reward-per-claim" pool-data)) (at "max-reward-per-claim" pool-data) (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data)) calculated-reward 0.0)) 0.0))

                                                  )
                                                  ;Enforce account
                                                  (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                                                  ;Enforce stake
                                                  (enforce (> to-pay-stake 0.0) "You dont have a stake in this pool to withdraw")
                                                  ;Lets now transfer rewards and stake back to the user
                                                  ;If stake and reward tokens are different, we compose 2 different transfer allowances and functions.
                                                  ;If stake token and reward token are the same, we compose a single transfer allowance and function.
                                                  (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data))
                                                      (if (= (reward-token::details pool-id) (stake-token::details pool-id))
                                                        (if (= (get-token-key reward-token) (get-token-key stake-token))
                                                          (install-capability (reward-token::TRANSFER pool-id account (+ to-pay-stake to-pay-reward)))
                                                          (install-capability (reward-token::TRANSFER pool-id account to-pay-reward)))
                                                      (install-capability (reward-token::TRANSFER pool-id account to-pay-reward)) )
                                                  true)
                                                  ;Transfer reward or transfer reward + stake depending on token type
                                                  (if (< (at "total-earned" pool-user-data) (at "max-reward-per-account" pool-data))
                                                    (if (= (reward-token::details pool-id) (stake-token::details pool-id))
                                                      (if (= (get-token-key reward-token) (get-token-key stake-token))
                                                        (reward-token::transfer pool-id account (+ to-pay-stake to-pay-reward))
                                                        (reward-token::transfer pool-id account to-pay-reward) )
                                                      (reward-token::transfer pool-id account to-pay-reward) )
                                                  true)
                                                  ;Now lets transfer the users stake back if we haven't already in the logic above
                                                  ;If stake token type is different from reward token type, compose a stake token transfer allowance
                                                  (if (= (reward-token::details pool-id) (stake-token::details pool-id))
                                                    true
                                                    (if (= (get-token-key reward-token) (get-token-key stake-token))
                                                      true
                                                      (install-capability (stake-token::TRANSFER pool-id account to-pay-stake))
                                                    )
                                                  )
                                                  ;If stake token type is different from reward token type, make stake token transfer now.
                                                  (if (= (reward-token::details pool-id) (stake-token::details pool-id))
                                                    true
                                                    (if (= (get-token-key reward-token) (get-token-key stake-token))
                                                      true
                                                      (stake-token::transfer pool-id account to-pay-stake)
                                                    )
                                                  )
                                                  ;Update stake
                                                  (update stakes stake-id  (+ {"last-updated":  (at "block-time" (chain-data)), "balance": (-(at "balance" stake) to-pay-stake) } stake))
                                                  ;Update pool usage
                                                  ;Calculate paid, ensuring it doesnt exceed pools balance
                                                  ;Calculate owed, depending if our pool has a fixed apy or not we apply correct calculation
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
                                                    (format "Awarded {} with {} {} and unstaked {} {}" [account to-pay-reward reward-token to-pay-stake stake-token])
                                                    (format "Awarded {} with {} {} for {}% of pool, and unstaked {} {}" [account to-pay-reward reward-token (floor (* owner-percentage 100) 2) to-pay-stake stake-token])
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
        @doc " Calculates tokens owed for a fixed APY and a balance of tokens from start-time to now "
        (let
            (
                (time-passed (diff-time  (at "block-time" (chain-data)) start-time) )
                (pool-data (read pools pool-id ["balance"]))
                (pool-usage-data (read pools-usage pool-id ["paid"]))

            )
            (let
              (
                (owed-max (floor (*(apy-for-ratio-of-seconds-in-year (seconds-over-seconds-in-year time-passed) apy)balance)precision))
              )
              (enforce ( >= time-passed 0.0) "From date is in the future")
              ;We clamp our reward calculation under the pool's avaialble balance so we don't exceed it
              (if (>= (-(at "balance" pool-data) (at "paid" pool-usage-data)) owed-max)
                owed-max
                (-(at "balance" pool-data) (at "paid" pool-usage-data))
              )
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

  (defun initialize ()
    @doc " Initialize the contract. Can only happen once. "
    (coin.create-account KDS_BANK (create-module-guard "kadena-stake-bank"))
  )

  (defun get-token-key
    ( tokenA:module{fungible-v2} )
    " Create key from token module"
    (format "{}" [tokenA])
  )

)

;(create-table free.kadena-stake-fungiv2.pools)
;(create-table free.kadena-stake-fungiv2.pools-usage)
;(create-table free.kadena-stake-fungiv2.pool-user-stats)
;(create-table free.kadena-stake-fungiv2.stakes)
;(free.kadena-stake-fungiv2.initialize)
