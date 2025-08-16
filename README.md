# 🩸 DonorX: Blood Donation Reward System

A blockchain-based reward system for blood donors using NFT badges to recognize and incentivize regular blood donation.

## 🎯 Features

- ✨ NFT badges for verified blood donors
- 🏥 Authorized donation center management
- 📊 Donation tracking system
- 🎖️ Automatic badge minting based on donation threshold
- 🔒 Secure verification process

## 🚀 Getting Started

### Prerequisites

- Clarinet
- Stacks wallet

### Contract Deployment

1. Clone the repository
2. Install dependencies:
```bash
clarinet integrate
```

### 📝 Usage

1. **Register as a Donor**
```clarity
(contract-call? .donorx register-donor)
```

2. **Record Donation** (Donation Centers Only)
```clarity
(contract-call? .donorx record-donation <donor-principal>)
```

3. **Check Donor Status**
```clarity
(contract-call? .donorx get-donor-info <donor-principal>)
```

## 🔑 Key Functions

- `register-donor`: Register as a new donor
- `record-donation`: Record a blood donation (donation centers only)
- `register-donation-center`: Add authorized donation centers
- `get-donor-info`: View donor statistics and badge status

## 🏆 Rewards

Donors receive unique NFT badges after reaching the donation threshold (default: 1 donation)

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📜 License

MIT
```
