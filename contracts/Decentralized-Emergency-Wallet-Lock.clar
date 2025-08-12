(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-INITIALIZED (err u101))
(define-constant ERR-NOT-INITIALIZED (err u102))
(define-constant ERR-INVALID-GUARDIAN (err u103))
(define-constant ERR-ALREADY-LOCKED (err u104))
(define-constant ERR-NOT-LOCKED (err u105))
(define-constant ERR-COOLDOWN-ACTIVE (err u106))
(define-constant ERR-INSUFFICIENT-GUARDIANS (err u107))
(define-constant ERR-CONTACT-ALREADY-EXISTS (err u108))
(define-constant ERR-CONTACT-NOT-FOUND (err u109))
(define-constant ERR-MAX-CONTACTS-REACHED (err u110))

(define-data-var contract-owner principal tx-sender)
(define-data-var min-guardian-signatures uint u2)
(define-data-var lock-duration uint u144)
(define-data-var cooldown-period uint u72)
(define-data-var max-emergency-contacts uint u5)

(define-map wallet-configs
    principal
    {
        initialized: bool,
        last-activity: uint,
        inactivity-threshold: uint,
        is-locked: bool,
        lock-expiry: uint,
        guardian-count: uint,
        emergency-contact-count: uint
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

(define-map emergency-contacts
    { wallet: principal, contact: principal }
    {
        contact-name: (string-ascii 50),
        notification-enabled: bool,
        added-at: uint
    }
)

(define-map contact-notifications
    { wallet: principal, contact: principal, event-type: (string-ascii 20) }
    {
        timestamp: uint,
        notified: bool
    }
)

(define-public (initialize-wallet (inactivity-threshold uint))
    (let
        ((wallet tx-sender)
         (existing-config (default-to 
            { initialized: false, last-activity: u0, inactivity-threshold: u0, is-locked: false, lock-expiry: u0, guardian-count: u0, emergency-contact-count: u0 }
            (map-get? wallet-configs wallet))))
        (asserts! (not (get initialized existing-config)) ERR-ALREADY-INITIALIZED)
        (ok (map-set wallet-configs wallet
            {
                initialized: true,
                last-activity: stacks-block-height,
                inactivity-threshold: inactivity-threshold,
                is-locked: false,
                lock-expiry: u0,
                guardian-count: u0,
                emergency-contact-count: u0
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
        (unwrap-panic (notify-emergency-contacts wallet "lock"))
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
        (unwrap-panic (notify-emergency-contacts wallet "unlock"))
        (ok true)))

(define-read-only (get-wallet-status (wallet principal))
    (ok (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))

(define-read-only (is-guardian-of (wallet principal) (guardian principal))
    (ok (default-to false (map-get? wallet-guardians {wallet: wallet, guardian: guardian}))))

(define-public (add-emergency-contact (contact principal) (contact-name (string-ascii 50)))
    (let
        ((wallet tx-sender)
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (not (get is-locked config)) ERR-ALREADY-LOCKED)
        (asserts! (< (get emergency-contact-count config) (var-get max-emergency-contacts)) ERR-MAX-CONTACTS-REACHED)
        (asserts! (is-none (map-get? emergency-contacts {wallet: wallet, contact: contact})) ERR-CONTACT-ALREADY-EXISTS)
        (map-set emergency-contacts {wallet: wallet, contact: contact}
            {
                contact-name: contact-name,
                notification-enabled: true,
                added-at: stacks-block-height
            })
        (map-set wallet-configs wallet
            (merge config {emergency-contact-count: (+ (get emergency-contact-count config) u1)}))
        (ok true)))

(define-public (remove-emergency-contact (contact principal))
    (let
        ((wallet tx-sender)
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (not (get is-locked config)) ERR-ALREADY-LOCKED)
        (asserts! (is-some (map-get? emergency-contacts {wallet: wallet, contact: contact})) ERR-CONTACT-NOT-FOUND)
        (map-delete emergency-contacts {wallet: wallet, contact: contact})
        (map-set wallet-configs wallet
            (merge config {emergency-contact-count: (- (get emergency-contact-count config) u1)}))
        (ok true)))

(define-public (toggle-contact-notifications (contact principal))
    (let
        ((wallet tx-sender)
         (existing-contact (unwrap! (map-get? emergency-contacts {wallet: wallet, contact: contact}) ERR-CONTACT-NOT-FOUND)))
        (map-set emergency-contacts {wallet: wallet, contact: contact}
            (merge existing-contact {notification-enabled: (not (get notification-enabled existing-contact))}))
        (ok true)))

(define-private (notify-emergency-contacts (wallet principal) (event-type (string-ascii 20)))
    (begin
        (map-set contact-notifications {wallet: wallet, contact: wallet, event-type: event-type}
            {timestamp: stacks-block-height, notified: true})
        (ok true)))

(define-read-only (get-emergency-contact (wallet principal) (contact principal))
    (ok (map-get? emergency-contacts {wallet: wallet, contact: contact})))

(define-read-only (get-emergency-contact-count (wallet principal))
    (let
        ((config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (ok (get emergency-contact-count config))))

(define-read-only (is-emergency-contact (wallet principal) (contact principal))
    (ok (is-some (map-get? emergency-contacts {wallet: wallet, contact: contact}))))
