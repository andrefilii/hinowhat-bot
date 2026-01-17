# Special Messages Pool

This folder contains the "Special" or "Creepy" messages that the bot might randomly trigger.
The content of this folder is ignored by git to allow for personal customization without sharing private inside jokes.

## How to add messages

Create a JSON file named after the language code (e.g., `it.json`, `en.json`).

### JSON Structure
The file must contain a single object with a `sentences` key, which holds an array of strings.

**Example (`it.json`):**
```json
{
  "sentences": [
    "I am watching you.",
    "Don't look behind you.",
    "I know what you did."
  ]
}