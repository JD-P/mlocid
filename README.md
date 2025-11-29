# mlocid

A spaced repetition web application built with the Dream web framework for OCaml. mlocid uses the SM-2 algorithm to optimize your learning schedule.

## Features

- **User Authentication**: Secure login and registration system
- **Flashcard Management**: Create, read, update, and delete flashcards
- **SM-2 Algorithm**: Intelligent spaced repetition scheduling
- **Mnemosyne Import**: Import flashcards from Mnemosyne format (tab-separated)
- **LaTeX Support**: Render mathematical expressions with MathJax
- **Unicode Support**: Full support for international characters including Chinese
- **Responsive Design**: Works on both desktop and mobile browsers
- **BCNF Database**: Properly normalized database schema

## Requirements

- OCaml >= 4.14.0
- opam
- dune >= 3.0
- SQLite3
- Python 3 (for Selenium tests)
- Chrome/Chromium (for Selenium tests)

## Installation

1. Install OCaml dependencies:
```bash
opam install . --deps-only
```

   **Note**: To install test dependencies (required for running tests), use:
```bash
opam install . --deps-only --with-test
# or the short form:
opam install . --deps-only -t
```

2. Build the application:
```bash
make build
# or
dune build
```

3. (Optional) Download MathJax for LaTeX rendering:
```bash
make setup-mathjax
```

## Configuration

Create a `config.yaml` file (or use the default):

```yaml
database_path: "./mlocid.db"
port: 8080
host: "127.0.0.1"
static_dir: "./static"
session_secret: "change-this-secret-in-production"
```

## Running

Start the server:
```bash
make run
# or
dune exec mlocid
```

Or with a custom config:
```bash
dune exec mlocid -- --config /path/to/config.yaml
```

The application will be available at `http://localhost:8080` (or your configured host/port).

## Testing

### Backend Unit Tests

**Prerequisites**: Make sure you've installed test dependencies with `opam install . --deps-only --with-test`

Run the backend API unit tests:
```bash
make test-backend
# or
dune exec test_backend
```

Run the API security tests (including SQL injection tests):
```bash
dune exec test_api_security
```

These tests verify:
- User creation and authentication
- Flashcard CRUD operations
- SM-2 algorithm correctness
- Database operations
- **Security**: Users cannot access other users' flashcards
- **Security**: Unauthenticated users cannot access protected endpoints
- **Security**: Authorization checks for all API endpoints
- **Security**: SQL injection protection across all input fields and endpoints

### Frontend Selenium Tests

1. Install Python dependencies:
```bash
make install-selenium-deps
# or
pip3 install selenium webdriver-manager
```

2. Run the frontend tests:
```bash
make test-frontend
```

These tests verify:
- Page navigation
- User registration and login
- Flashcard creation
- UI interactions

## Usage

1. **Register**: Create a new account at `/register`
2. **Login**: Sign in at `/login`
3. **Create Cards**: Add flashcards with questions and answers
4. **Import**: Import cards in Mnemosyne format (tab-separated question/answer pairs)
5. **Study**: Review due cards and provide quality feedback (0-5)
6. **Manage**: Edit or delete your flashcards

## SM-2 Algorithm

The application uses the SM-2 spaced repetition algorithm:

- **Quality Scale**: 0 (blackout) to 5 (perfect recall)
- **E-Factor**: Starts at 2.5, adjusts based on performance (minimum 1.3)
- **Intervals**: 
  - First review: 1 day
  - Second review: 6 days
  - Subsequent: Previous interval × E-Factor
- **Rescheduling**: Cards with quality < 3 restart from the beginning

## API Endpoints

### Authentication
- `POST /api/register` - Register new user
- `POST /api/login` - Login
- `POST /api/logout` - Logout
- `GET /api/user` - Get current user

### Flashcards
- `GET /api/flashcards` - List all flashcards
- `GET /api/flashcards/:id` - Get specific flashcard
- `POST /api/flashcards` - Create flashcard
- `PUT /api/flashcards/:id` - Update flashcard
- `DELETE /api/flashcards/:id` - Delete flashcard

### Study
- `GET /api/study/due` - Get due flashcards
- `POST /api/study/review/:id` - Submit review with quality (0-5)

### Import
- `POST /api/import/mnemosyne` - Import Mnemosyne format (text/plain, tab-separated)

## Database Schema

The database is in Boyce-Codd Normal Form (BCNF):

- **users**: id (PK), username (UNIQUE), password_hash, created_at
- **flashcards**: id (PK), user_id (FK), question, answer, efactor, interval, repetitions, next_review, created_at, updated_at

## License

Copyright 2025 John David Pressman

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
