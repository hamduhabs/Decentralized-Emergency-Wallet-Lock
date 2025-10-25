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
(define-constant ERR-RECOVERY-NOT-INITIATED (err u111))
(define-constant ERR-RECOVERY-DELAY-NOT-MET (err u112))
(define-constant ERR-RECOVERY-ALREADY-INITIATED (err u113))

(define-data-var contract-owner principal tx-sender)
(define-data-var min-guardian-signatures uint u2)
(define-data-var lock-duration uint u144)
(define-data-var cooldown-period uint u72)
(define-data-var max-emergency-contacts uint u5)
(define-data-var recovery-delay-blocks uint u1008)

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

(define-map recovery-keys
    principal
    {
        recovery-principal: principal,
        delay-blocks: uint,
        initiated-at: uint,
        is-active: bool
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
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED))
         (has-voted (default-to false (map-get? guardian-votes {wallet: wallet, guardian: guardian}))))
        (asserts! (default-to false (map-get? wallet-guardians {wallet: wallet, guardian: guardian})) ERR-INVALID-GUARDIAN)
        (asserts! (not has-voted) ERR-INVALID-GUARDIAN)
        (map-set guardian-votes {wallet: wallet, guardian: guardian} true)
        (increment-vote-tally wallet)
        (try-lock wallet)))

(define-private (try-lock (wallet principal))
    (let
        ((config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED))
         (vote-count (count-votes-for-wallet wallet)))
        (if (>= vote-count (var-get min-guardian-signatures))
            (lock-wallet wallet)
            (ok false))))

(define-map vote-tallies
    principal
    uint
)

(define-private (count-votes-for-wallet (wallet principal))
    (default-to u0 (map-get? vote-tallies wallet)))

(define-private (increment-vote-tally (wallet principal))
    (let
        ((current-votes (default-to u0 (map-get? vote-tallies wallet))))
        (map-set vote-tallies wallet (+ current-votes u1))
        (+ current-votes u1)))

(define-private (reset-vote-tally (wallet principal))
    (map-delete vote-tallies wallet))

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
        (reset-vote-tally wallet)
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

(define-public (update-activity)
    (let
        ((wallet tx-sender)
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (not (get is-locked config)) ERR-ALREADY-LOCKED)
        (map-set wallet-configs wallet
            (merge config {last-activity: stacks-block-height}))
        (ok true)))

(define-public (check-inactivity (wallet principal))
    (let
        ((config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED))
         (current-block stacks-block-height)
         (last-activity (get last-activity config))
         (threshold (get inactivity-threshold config)))
        (asserts! (not (get is-locked config)) ERR-ALREADY-LOCKED)
        (if (and (> threshold u0) (>= (- current-block last-activity) threshold))
            (auto-lock-inactive-wallet wallet)
            (ok false))))

(define-private (auto-lock-inactive-wallet (wallet principal))
    (let
        ((config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (map-set wallet-configs wallet
            (merge config 
                {
                    is-locked: true,
                    lock-expiry: (+ stacks-block-height (var-get lock-duration))
                }))
        (unwrap-panic (notify-emergency-contacts wallet "inactivity-lock"))
        (ok true)))

(define-read-only (get-activity-status (wallet principal))
    (let
        ((config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED))
         (current-block stacks-block-height)
         (last-activity (get last-activity config))
         (threshold (get inactivity-threshold config))
         (blocks-inactive (- current-block last-activity)))
        (ok {
            last-activity: last-activity,
            blocks-inactive: blocks-inactive,
            inactivity-threshold: threshold,
            is-at-risk: (and (> threshold u0) (>= blocks-inactive (/ (* threshold u4) u5))),
            will-auto-lock: (and (> threshold u0) (>= blocks-inactive threshold))
        })))

(define-read-only (get-blocks-until-auto-lock (wallet principal))
    (let
        ((config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED))
         (current-block stacks-block-height)
         (last-activity (get last-activity config))
         (threshold (get inactivity-threshold config))
         (blocks-inactive (- current-block last-activity)))
        (if (and (> threshold u0) (< blocks-inactive threshold))
            (ok (- threshold blocks-inactive))
            (ok u0))))

(define-public (set-recovery-key (recovery-principal principal))
    (let
        ((wallet tx-sender)
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (not (get is-locked config)) ERR-ALREADY-LOCKED)
        (map-set recovery-keys wallet
            {
                recovery-principal: recovery-principal,
                delay-blocks: (var-get recovery-delay-blocks),
                initiated-at: u0,
                is-active: false
            })
        (ok true)))

(define-public (initiate-recovery (wallet principal))
    (let
        ((recovery-data (unwrap! (map-get? recovery-keys wallet) ERR-NOT-AUTHORIZED))
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED)))
        (asserts! (is-eq tx-sender (get recovery-principal recovery-data)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get is-active recovery-data)) ERR-RECOVERY-ALREADY-INITIATED)
        (map-set recovery-keys wallet
            (merge recovery-data 
                {
                    initiated-at: stacks-block-height,
                    is-active: true
                }))
        (unwrap-panic (notify-emergency-contacts wallet "recovery-initiated"))
        (ok true)))

(define-public (execute-recovery (wallet principal))
    (let
        ((recovery-data (unwrap! (map-get? recovery-keys wallet) ERR-NOT-AUTHORIZED))
         (config (unwrap! (map-get? wallet-configs wallet) ERR-NOT-INITIALIZED))
         (current-block stacks-block-height))
        (asserts! (is-eq tx-sender (get recovery-principal recovery-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active recovery-data) ERR-RECOVERY-NOT-INITIATED)
        (asserts! (>= (- current-block (get initiated-at recovery-data)) (get delay-blocks recovery-data)) ERR-RECOVERY-DELAY-NOT-MET)
        (map-set wallet-configs wallet
            (merge config 
                {
                    is-locked: false,
                    lock-expiry: u0
                }))
        (map-set recovery-keys wallet
            (merge recovery-data {is-active: false, initiated-at: u0}))
        (reset-vote-tally wallet)
        (unwrap-panic (notify-emergency-contacts wallet "recovery-executed"))
        (ok true)))

(define-public (cancel-recovery)
    (let
        ((wallet tx-sender)
         (recovery-data (unwrap! (map-get? recovery-keys wallet) ERR-NOT-AUTHORIZED)))
        (asserts! (get is-active recovery-data) ERR-RECOVERY-NOT-INITIATED)
        (map-set recovery-keys wallet
            (merge recovery-data {is-active: false, initiated-at: u0}))
        (unwrap-panic (notify-emergency-contacts wallet "recovery-cancelled"))
        (ok true)))

(define-read-only (get-recovery-status (wallet principal))
    (ok (map-get? recovery-keys wallet)))

(define-read-only (get-blocks-until-recovery (wallet principal))
    (let
        ((recovery-data (unwrap! (map-get? recovery-keys wallet) ERR-NOT-AUTHORIZED))
         (current-block stacks-block-height))
        (if (get is-active recovery-data)
            (let
                ((blocks-passed (- current-block (get initiated-at recovery-data)))
                 (delay-required (get delay-blocks recovery-data)))
                (if (>= blocks-passed delay-required)
                    (ok u0)
                    (ok (- delay-required blocks-passed))))
            (ok u0))))
