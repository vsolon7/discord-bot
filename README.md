# discord-bot
This is a discord bot I'm writing for my relatively small discord server. For now, the bot can only handle one server per
instance. Currently, the bot has some main basic commands: an argument timer to keep track of the time since the last argument, and a prediction command that lets users make a prediction (with an optional confidence rating); the bot will notify them when its time for their prediction to come true.

The bot can also read appdata/config/responses.json to get the ability to repond (with a preset response) to messages that start with the given word. In the future, I'll upgrade this to messages *containing* the given word.

In the future, I hope to add a currency system, but I'm still trying to think of how exactly it should be implemented. The currency will be used to make wagers (and maybe making a prediction should force you to stake some percentage of your money in accordance with your confidence).

# Structure
First, lets talk about how the Haskell file structure interacts with the module structure. A file `X.hs` in `src/Vesbot/` corresponds to the module `Vesbot.X`. When `X` is a folder with the same name, a file `Y.hs` in `src/Vesbot/X/` corresponds to the module `Vesbot.X.Y`. 

The bot has certain capacities which I tried to split into their own standalone modules as much as possible. Currently, it has the argument timer system, a prediction system, some defined slash commands, and some responses. Each capacity `X` has a main file `Vesbot.X`, but some modules require submodules, usually of the form `Vesbot.X.Database`, `Vesbot.X.Types`, and/or `Vesbot.X.Commands`. There are some helper files/modules scattered in `src/Vesbot` as well, such as `Vesbot.Utils`, `Vesbot.Parsing`, `Vesbot.Config`, etc.

# Control Flow
Here, I just want to give a basic overview of the control flow of the bot. I am using the discord-haskell package, and so the bot is ultimately run with `runDiscord`.

When program starts, the bot reads in a bunch of initial data and initializes various databases (See `src/Vesbot/Config.hs`). It hands this initial data to `app/Main.hs` via the `initBot` function.

In `Main.hs`, we take the configuration data and use it to run the bot with the `runDiscord` function mentioned above. Nothing else happens.

Now, `runDiscord` has `discordOnStart` and `discordOnEvent` functions that define what actions to take when bot establishes the connection with Discord and when the bot receives Discord events, respectively. When the bot starts, it initializes the job scheduler process via `runScheduler` defined in `JSONScheduler.Scheduler` (which corresponds to the file `src/JSONScheduler/Scheduler.hs`). This function essentially just periodically checks a database for the next scheduled action (it computes the time `T` until the next scheduled action and waits `max(T, 1 hour)`. When a job is scheduled, the scheduler recomputes the time `T` until the next soonest action and waits for `max(T, 1 hour)`). After this, the bot continuously monitors for Discord events. When one comes in, it *forks a new thread* and passes the event data to `eventHandler` defined in `Vesbot.Handler`

## Event Handler
The `eventHandler` pattern matches on the type of event and delegates to more specialized function. The first thing that happens is the "Ready" event. When this happens, the bot tries to register the various application commands defined in `mySlashCommands :: [SlashCommand]` found in `Vesbot.SlashCommands` (many of them are/will be simply imported from modules defined in `Vesbot.X.Commands`) and it unregisters the ones that are "out of date", meaning they were registered by the last instance of the bot, but not the current one.

Here are the other sorts of events the bot can currently handle:
1.  `InteractionCreate` events, which are essentially just slash commands at this point. These are passed to `interactionHandler` in `Vesbot.Handler.Interactions`, which ignores all InteractionCreate events that are not slash commands.
2. `MessageCreate` events, which happen anytime anyone sends any message (or does similar things, such as create a thread). These are passed to `messageHandler` in `Vesbot.Handler.Messages`
3. `MessageReactionAdd` events, which happen any time anyone (including bots) react to any message. These are passed to `updateReactionActivity` defined in `Vesbot.Activity`.
4. (Coming soon) I need to add a similar function for MessageReactionRemove events.

That's essentially it, the bot just responds to these small number of interaction types.

## Slash Commands
This is the main way of interfacing with the bot. Each module `X` defines some commands in `Vesbot.X.Commands`. Some commands have to write to/read from a database, and all the database commands are defined in `Vesbot.X.Database`. Each `Vesbot.X.Commands` exports its commands so that the `Vesbot.SlashCommands` module can import all of them and bundle the commands together in a list of all the slash commands to be defined, called `mySlashCommands`. It then exports `mySlashCommands` to `Vesbot.Handler` so that the handler can register them.

In `Vesbot.SlashCommands.Types`, you will see there is a type `SlashCommand`. A `SlashCommand` is a record containing the command name, the command registration, and then a function `commandHandler :: Interaction -> Maybe OptionsData -> DiscordHandler ()`. This function is called by the `interactionHandler` after it finds the slash command corresponding to the name of the command. `DiscordHandler ()` is a `ReaderT DiscordHandle IO ()`, which just means it's an `IO` monad that has access to the read-only content of the `DiscordHandle`; the latter being a record of all the data discord-haskell uses to communicate with discord. So, the `commandHandler` function takes in an interaction and some options data, and it runs some `IO`, possibly involving the Discord API and the bot's connection to the server (for example, the bot will send a response in the form of a chat input).

In the various modules `Vesbot.X.Commands`, therefore, you will find the actual code that is run when the bot receives a slash command interaction corresponding to that slash command.

## Activity Logging
Currently, the bot keeps track of some basic usage data. Namely, the number of reactions each user has given and received, the number of messages each user has sent, and some emoji use data to hopefully track the usage statistics over time. All of this happens in `Vesbot.Activity`, and the database commands are in `Vesbot.Activity.Database`

# Scheduler
Sometimes, the bot wants to schedule future things to happen in response to user commands (or just in general, for example, holiday notifications). This is how the prediction system works, and I hope to add a betting system and a remind system in the future (the latter should be simple).

The way it does this is with a library I wrote called JSONScheduler, which is still fairly rough and has known bugs in edge cases that won't really come up for the use cases of this bot (hopefully...). The idea is you have a database of all scheduled future tasks and a scheduler that runs on its own thread in a loop of `check for due jobs -> do their associated actions -> wait until the next job is due`. The scheduler spends most of its time waiting at the end of this loop (by trying to take from an empty MVar). When a future job is schedule (i.e., is added to the database), it wakes up the scheduler, which then starts over at the start of the loop.

The way the scheduler knows what action to take when a job is due is that it takes in a function ``handler :: a -> IO ()`` (the monad is actually slightly more general than `IO`, but you can/should think of it as `IO`). What is `a`? It's a data type with `Data.Aeson` `FromJSON` and `ToJSON` instance that bundles together all of the necessary information to actually do the action. The scheduler stores the JSON representation in the database when `scheduleJob` is called for that action `a`. When it's time for `a` to fire, it converts it from JSON to something of type `a` again, and then passes that to the event handler. So the event handler function that is passed to `runScheduler` is going to be something like

```hs
handler :: a -> IO ()
handler action = case action of
Action1 d1 d2 ... dn1   -> runAction1 d1 d2 ... dn1
Action2 d1 d2 ... dn2   -> runAction2 d1 d2 ... dn2
...
Action10 d1 d2 ... dn10 -> runAction10 d1 ... dn10
```
You can see this in the bot in `Vesbot.Handler.Actions`.

The JSONScheduler has support for all sorts of periodically recurring actions, which I mainly added for holiday support (but there are tricky holidays like easter that it can't support... maybe with a future `Easterly` type!). When an action is found to be due, the scheduler either removes it from the database, or updates the time it should be due again, in accordance with its recurrence type.
