# 🔒 Decentralized Emergency Wallet Lock

A secure, decentralized solution for protecting wallets from unauthorized access and potential theft.

## 🎯 Features

- Emergency wallet locking mechanism
- Guardian-based multi-signature protection
- Time-based lock duration
- Cooldown period for security
- Wallet activity monitoring

## 🛠 Technical Details

The contract implements the following key functionalities:

- Wallet initialization with customizable inactivity threshold
- Guardian management (add/remove)
- Guardian voting system for wallet locking
- Automatic lock execution on sufficient votes
- Time-based unlock mechanism

## 📝 Usage

### Initialize Wallet
```clarity
(contract-call? .decentralized-emergency-wallet-lock initialize-wallet u720)
```

### Add Guardian
```clarity
(contract-call? .decentralized-emergency-wallet-lock add-guardian 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Vote to Lock
```clarity
(contract-call? .decentralized-emergency-wallet-lock vote-lock 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Unlock Wallet
```clarity
(contract-call? .decentralized-emergency-wallet-lock unlock-wallet)
```

## 🔐 Security Considerations

- Minimum 2 guardian signatures required for locking
- 144 block lock duration (~24 hours)
- 72 block cooldown period (~12 hours)
- Only wallet owner can add/remove guardians
- Locked wallets cannot modify guardian structure

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📜 License

MIT
```
