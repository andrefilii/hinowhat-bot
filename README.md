# Hinowhat Bot

A Discord bot written in **Haskell** designed to manage random interactions, greetings, and refusals based on language localization and deterministic seeding.

Built with [discord-haskell](https://github.com/discord-haskell/discord-haskell).

## Features

- **Multilingual Greetings**: Supports multiple languages with JSON-based message pools
- **Easter Eggs**: A configurable probability system for triggering special surprise messages
- **Name Recognition**: Extracts and personalizes greetings with user-provided names
- **Refusal Pool**: Humorously rejects requests with witty responses
- **Debug Logging**: Optional verbose logging for development and troubleshooting

## Prerequisites

- **GHC 9.6.7** or later
- **Cabal 3.12** or later
- A Discord bot token (see [Discord Developer Portal](https://discord.com/developers/applications))

### Installation

1. **Clone the repository**:

   ```bash
   git clone https://github.com/andrefilii/hinowhat-bot.git
   cd hinowhat-bot
   ```

2. **Install Haskell dependencies**:

   ```bash
   cabal update
   cabal build
   ```

3. **Set up environment variables**:
   Create a `.env` file in the project root:

   ```bash
   DISCORD_TOKEN=your_bot_token_here
   BOT_SECRET=your_secret_phrase_for_easter_eggs
   BOT_LANGUAGE=it
   BOT_DEBUG=false
   ```

   - `DISCORD_TOKEN` (required): Your Discord bot token
   - `BOT_SECRET` (optional): A secret phrase used for randomness (default: `"default_insecure_secret"`)
   - `BOT_LANGUAGE` (optional): Default language code (`it`, `en`, etc.; default: `"it"`)
   - `BOT_DEBUG` (optional): Enable debug logging (`true`/`false`; default: `false`)

## Usage

### Running the Bot

Using the Makefile:

```bash
make run
```

Or directly with Cabal:

```bash
cabal run
```

The bot will start and listen for Discord events.

### Commands

| Command          | Description                               | Example         |
| ---------------- | ----------------------------------------- | --------------- |
| `h!hello [name]` | Sends a greeting, optionally personalized | `h!hello Alice` |
| `h!hi [name]`    | Alias for hello                           | `h!hi Bob`      |
| `h!ciao [name]`  | Alias for hello                           | `h!ciao`        |
| `h!no`           | Receives a witty refusal                  | `h!no`          |

### Message Pools

Messages are stored as JSON files in the `data/` directory:

- **`data/greetings/{lang}.json`**: Generic and personalized greetings
- **`data/no/{lang}.json`**: Refusal messages
- **`data/special/{lang}.json`**: Easter egg messages (git-ignored)

#### JSON Structure

**Greetings** (`data/greetings/en.json`):

```json
{
  "generic": ["Hello!", "Hi there!"],
  "with_name": ["Hello {{name}}!", "Hi {{name}}, welcome!"]
}
```

**Refusals** (`data/no/en.json`):

```json
{
  "answers": ["No way!", "Absolutely not."]
}
```

**Special** (`data/special/en.json`):

```json
{
  "sentences": ["I'm watching you...", "Did you hear that?"]
}
```

## Architecture

### Core Modules

- **`Bot.Types`**: Core data structures (`PoolCategory`, `BotEnv`, message pools)
- **`Bot.Persistence`**: Data access layer for JSON file loading
- **`Bot.Logic`**: Deterministic RNG and Easter egg logic
- **`Bot.Discord`**: Discord event handler and command routing
- **`Bot.Logger`**: Logging utilities
- **`Bot.Constants`**: Global configuration

### Deterministic Randomness

The bot uses a deterministic seeding mechanism:

1. Combines the `BOT_SECRET` with the current Unix timestamp
2. Hashes the combination using SHA256
3. Folds the hash bytes into a seed for `System.Random`

This ensures the same secret + timestamp always produces the same random selection.

## Testing

Run the test suite:

```bash
cabal test
```

The tests verify:

- **`Bot.LogicSpec`**: Determinism and variance of the RNG
- **`Bot.PersistenceSpec`**: JSON parsing and pool extraction

## Development

### Build the project

```bash
make build
```

### Format and check code

```bash
cabal build --ghc-options="-Wall"
```

## License

This project is licensed under the **MIT License**. See [LICENSE](LICENSE) for details.

## Contributing

Contributions are welcome! Feel free to open issues or submit pull requests for bug fixes, feature requests, or improvements.

## Author

**Andrea Filippi** - [GitHub](https://github.com/andrefilii) - [Email](mailto:andrea.filippi02@outlook.com)

---

**Note**: The `data/special/` directory is intentionally git-ignored to allow for personal customization without sharing private inside jokes with the world.
