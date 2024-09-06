# Editor

## Prerequisites

Before building and running the project, ensure you have the following prerequisites installed:

1. **GHC (Glasgow Haskell Compiler)**: Version 9.2.3 or higher.
    - The project is built using the Haskell 2010 standard.
    - Ensure GHC is installed with threading support enabled, as the project uses the `-threaded` flag.

2. **Cabal**: Version 3.0 or higher.
    - This project uses Cabal as the build system.
    - You can install Cabal using `ghcup` or directly from the [Haskell Platform](https://www.haskell.org/platform/).

### Setting Up the Environment

1. **Install GHC and Cabal**:
   - Using `ghcup`:
     ```bash
     curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
     ghcup install ghc 9.2.3
     ghcup install cabal 3.8.1.0
     ```

2. **Clone the Repository**:
   ```bash
   git clone <repository-url>
   cd PS3

## Installing Dependencies and Running the Project

### Step 1: Install Dependencies

Before running the project, you need to install all the necessary dependencies:

```bash
cabal update
cabal build

cd interpreter
pip install -r requirements.txt 

### Step 2: Run the Project

```bash
cabal run
