(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-INITIALIZED (err u101))
(define-constant ERR-NOT-INITIALIZED (err u102))
(define-constant ERR-INVALID-GUARDIAN (err u103))
(define-constant ERR-ALREADY-LOCKED (err u104))
(define-constant ERR-NOT-LOCKED (err u105))
(define-constant ERR-COOLDOWN-ACTIVE (err u106))
(define-constant ERR-INSUFFICIENT-GUARDIANS (err u107))

(define-data-var contract-owner principal tx-sender)
(define-data-var min-guardian-signatures uint u2)
(define-data-var lock-duration uint u144)
(define-data-var cooldown-period uint u72)

(define-map wallet-configs
    principal
    {
        initialized: bool,
        last-activity: uint,
        inactivity-threshold: uint,
        is-locked: bool,
        lock-expiry: uint,
        guardian-count: uint
    }
)

(define-map wallet-guardians 
    { wallet: principal, guardian: principal } 
    bool
)

(define-map guardian-votes
    { wallet: principal, guardian: principal }
    bool
)

(define-public (initialize-wallet (inactivity-threshold uint))
    (let
        ((wallet tx-sender)
         (existing-config (default-to 
            { initialized: false, last-activity: u0, inactivity-threshold: u0, is-locked: false, lock-expiry: u0, guardian-count: u0 }
            (map-get? wallet-configs wallet))))
        (asserts! (not (get initialized existing-config)) ERR-ALREADY-INITIALIZED)
        (ok (map-set wallet-configs wallet
            {
                initialized: true,
                last-activity: stacks-block-height,
                inactivity-threshold: inactivity-threshold,
                is-locked: false,
                lock-expiry: u0,
                guardian-count: u0
            }))))

(define-public (add-guardian (guardian principal))
    (let
        ((wallet tx-sender)
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (not (get is-locked config)) ERR-ALREADY-LOCKED)
        (asserts! (not (default-to false (map-get? wallet-guardians {wallet: wallet, guardian: guardian}))) ERR-INVALID-GUARDIAN)
        (map-set wallet-guardians {wallet: wallet, guardian: guardian} true)
        (map-set wallet-configs wallet
            (merge config {guardian-count: (+ (get guardian-count config) u1)}))
        (ok true)))

(define-public (remove-guardian (guardian principal))
    (let
        ((wallet tx-sender)
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (not (get is-locked config)) ERR-ALREADY-LOCKED)
        (asserts! (default-to false (map-get? wallet-guardians {wallet: wallet, guardian: guardian})) ERR-INVALID-GUARDIAN)
        (map-delete wallet-guardians {wallet: wallet, guardian: guardian})
        (map-set wallet-configs wallet
            (merge config {guardian-count: (- (get guardian-count config) u1)}))
        (ok true)))

(define-public (vote-lock (wallet principal))
    (let
        ((guardian tx-sender)
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (default-to false (map-get? wallet-guardians {wallet: wallet, guardian: guardian})) ERR-INVALID-GUARDIAN)
        (map-set guardian-votes {wallet: wallet, guardian: guardian} true)
        (try-lock wallet)))

(define-private (try-lock (wallet principal))
    (let
        ((config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED))
         (vote-count (count-votes-for-wallet wallet)))
        (if (>= vote-count (var-get min-guardian-signatures))
            (lock-wallet wallet)
            (ok false))))

(define-private (count-votes-for-wallet (wallet principal))
    u0)

(define-private (lock-wallet (wallet principal))
    (let
        ((config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (not (get is-locked config)) ERR-ALREADY-LOCKED)
        (map-set wallet-configs wallet
            (merge config 
                {
                    is-locked: true,
                    lock-expiry: (+ stacks-block-height (var-get lock-duration))
                }))
        (ok true)))

(define-public (unlock-wallet)
    (let
        ((wallet tx-sender)
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (get is-locked config) ERR-NOT-LOCKED)
        (asserts! (>= stacks-block-height (get lock-expiry config)) ERR-COOLDOWN-ACTIVE)
        (map-set wallet-configs wallet
            (merge config 
                {
                    is-locked: false,
                    lock-expiry: u0
                }))
        (ok true)))

(define-read-only (get-wallet-status (wallet principal))
    (ok (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))

(define-read-only (is-guardian-of (wallet principal) (guardian principal))
    (ok (default-to false (map-get? wallet-guardians {wallet: wallet, guardian: guardian}))))
