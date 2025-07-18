# 🏢 Workstamp - On-Chain HR Records

> Work experience NFTs tied to identity - Building trust in professional history on the blockchain

## 📋 Overview

Workstamp is a Clarity smart contract that creates verifiable, on-chain employment records as NFTs. Each workstamp represents a work relationship between an employer and employee, creating an immutable professional history that can be verified by anyone.

## ✨ Features

- 🎯 **Issue Workstamps**: Employers can create work experience NFTs for employees
- ⏰ **Employment Tracking**: Track start/end dates using block heights
- ✅ **Employee Verification**: Employees can verify their workstamps
- 🔄 **Role Updates**: Employers can update job roles for active employment
- 📊 **Comprehensive Queries**: Get workstamps by employee, employer, or ID
- 🛡️ **Security Controls**: Contract pause functionality and authorization checks

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity and Stacks blockchain

### Installation

```bash
clarinet new workstamp-project
cd workstamp-project
```

Copy the contract code into `contracts/Workstamp.clar`

## 📖 Usage

### Issue a Workstamp

```clarity
(contract-call? .Workstamp issue-workstamp 'SP1234...EMPLOYEE "Software Engineer")
```

### End Employment

```clarity
(contract-call? .Workstamp end-employment u1)
```

### Verify Workstamp (as employee)

```clarity
(contract-call? .Workstamp verify-workstamp u1)
```

### Query Functions

```clarity
;; Get specific workstamp
(contract-call? .Workstamp get-workstamp u1)

;; Get all workstamps for an employee
(contract-call? .Workstamp get-employee-workstamps 'SP1234...EMPLOYEE)

;; Get all workstamps issued by an employer
(contract-call? .Workstamp get-employer-workstamps 'SP5678...EMPLOYER)

;; Check if employment is active
(contract-call? .Workstamp is-employment-active u1)
```

## 🏗️ Contract Structure

### Data Maps
- `workstamps`: Core workstamp data
- `employee-workstamps`: Employee's workstamp list
- `employer-workstamps`: Employer's issued workstamps
- `active-employment`: Current active employments

### Key Functions
- `issue-workstamp`: Create new employment record
- `end-employment`: Terminate active employment
- `verify-workstamp`: Employee verification
- `update-role`: Modify job role
- `get-workstamp`: Retrieve workstamp details

## 🔒 Security Features

- Only employers can issue workstamps to others
- Only employees can verify their own workstamps
- Only employers can end their issued employments
- Contract owner can pause/unpause contract
- Prevents duplicate active employments

## 🧪 Testing

```bash
clarinet test
```

## 📝 Error Codes

- `u100`: Not authorized
- `u101`: Workstamp not found
- `u102`: Already exists
- `u103`: Invalid employee
- `u104`: Workstamp active
- `u105`: Not employer
- `u106`: Invalid role
- `u107`: Invalid dates

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is open source and available under the MIT License.
