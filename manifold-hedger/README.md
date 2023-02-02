# Table of contents
- [Summary](#summary)
    - [Analytics results](#analytics-results)
- [Installation](#installation)
    - [Normal execution](#normal-execution)
    - [Interactive execution](#interactive-execution)
    - [Addendum: Installing haskell](#addendum-installing-haskell)
- [Explaining the model](#explaining-the-model)
    - [Recap: The Ledger-Hedger paper](#recap-the-Ledger-Hedger-paper)
        - [Gas price and demand](#gas-price-and-demand)
        - [Ledger-Hedger: Players and phases](#ledger-hedger-players-and-phases)
        - [Ledger-Hedger: The subgame space](#ledger-hedger-the-subgame-space)
    - [Assumptions made explicit](#assumptions-made-explicit)
        - [Refined payoffs](#refined-payoffs)
        - [Ambiguous **Buyer** behavior](#ambiguous-buyer-behavior)
        - [Service costs](#service-costs)
        - [Modelling risk explicitly](#modelling-risk-explicitly)
- [Code deep dive](#code-deep-dive)
    - [Recap: DSL primer](#recap-dsl-primer)
        - [The building blocks](#the-building-blocks)
        - [Exhogenous parameters](#exhogenous-parameters)
        - [Basic operations](#basic-operations)
        - [Branching](#branching)
        - [Supplying strategies](#supplying-strategies)
            - [Evaluating strategies](#evaluating-strategies)
            - [Stochasticity](#stochasticity)
            - [Branching](#branching-1)
        - [Stochasticity](#stochasticity)
    - [File structure](#file-structure)
- [Analytics](#analytics)
    - [Strategies employed in the analysis](#strategies-employed-in-the-analysis)
    - [Reading the analytics](#reading-the-analytics)
    - [Running the analytics](#running-the-analytics)
    - [Replicating the Ledger-Hedger paper results](#replicating-the-ledger-hedger-paper-results)
    - [Other analyses](#other-analyses)
      - [Sanity checks](#sanity-checks)
      - [Interactive perks](#interactive-perks)
# Summary

This project implements the model detailed in the [Ledger-Hedger](https://eprint.iacr.org/2022/056.pdf) paper. In particular, we have focused on relaxing some of the assumptions made in the paper around all players being risk-averse (in particular **Seller**). Our model allows for custom definitions of risk-sensitivity for all players.

## Analytics results
**The most important finding in our simulations is that Ledger-Hedger is incredibly sensible to initial parameters, and not as robust as we would have expected.**

A more detailed analysis can be found in the section [Analytics](#analytics).

First of all, we replicated the analysis outlined in the paper. We used the same parameters provided there, and verified the equilibrium.

In general, we found that the game-theoretic scenarios detailed in the paper are a bit fragile, and incentives to prefer Ledger-Hedger swing within a close margin.

In our opinion, this depends on the fact that, in the paper, using the service has costs: **Seller** has to pay a fee to accept the Ledger Hedger contract and to close it. These costs, expressed in terms of gas, amount to a non-negligible fraction of the entire gas amount **Buyer** wants to reserve.

This choice of parameters makes the protocol advantageous only in a small window which depends strictly on the fact that we are using very concave utility functions, which represent quite high risk-aversity.

Indeed, in a risk-neutral scenario there is no difference between using or not using Ledger-Hedger *if and only if* the usage fees and payments are *both* set to **0**, meaning that the protocol is essentially 'free to use for everyone'. In any other case, a risk-neutral individual would prefer not using Ledger-Hedger.

Moreover, we found that our results are heavily dependent on the future gas price distribution: Even small variation in the standard deviation of the normal curve describing future gas price can greatly influence the stability of the equilibrium.

Summarizing, our main findings are the following:
- Running the protocol with the parameters provided in the paper results in Ledger-Hedger being advantageous only for very risk-averse players.
- Using Ledger-Hedger makes players incur extra costs: Both **Seller** and **Buyer** pay a fee for using the platform.
- These fees are only offset by the risk-aversity of the players. The more risk-averse they are, the more it makes sense for them to carry these extra costs.
- Varying the price at which **Buyer** reserves the gas, called $\pi_{contract}$ in the paper, won't help much, since this parameter constitutes revenue for **Seller** but a cost for **Buyer**. So the more we make it convenient for the former, the less we make it convenient for the latter, and vice-versa.
- As it is instantiated, the protocol is brittle. Even small changes in the standard deviation of future gas price distribution can break the equilibrium.
- To make the protocol more robust, the only viable option is lowering the fees for the usage of the platform, which constitute a cost both for **Buyer** and for **Seller**.
- Finally, we hypothesize that the situation could change if **Buyer** and **Seller** have differential, private information about the future gas price distribution. That is, the future gas price distribution is not anymore assumed to be a normal distribution centered around the current price. This has not been simulated yet, as it was not included in the current working package.


# Installation

To run the model, it is necessary to have `haskell` and `stack` installed on your machine. Refer to the subsection [Addendum: Installing haskell](#addendum-installing-haskell) for instructions. A deeper dive into the code structure can be found in the [Code deep dive](#code-deep-dive) subsection.

There are two main ways of running the model: normal and interactive execution.

## Normal execution

To 'just' run the model, type

```sh
stack run
```
in the main directory, where the file `stack.yaml` is located.
The model will be compiled and a predefined set of analytics will be run. The results of the predefined analytics will be shown on terminal.

## Interactive execution

One of the most powerful features of `haskell` is *REPL mode*. This allows you to recompile the code on the fly, to query the type of a function and a lot of other things. To start interactive mode, just run

```sh
stack ghci
```

in the main directory. The code will compile, and then an interactive terminal (REPL) window will open. There are various commands that can be fed to the REPL. Among the most useful ones we highlight:

| Command         | Description               |
|:----------------|--------------------------:|
| `:q`            | quit interactive mode     |
| `:r`            | recompile the source code |
| `:l module`     | load module               |
| `:t expression` | query expression type     |

Of these commands, `:t` is the most important one, as it allows to visualize clearly what type of input we must feed to a given function. For instance, `:t (&&)` produces the output:

```haskell
(&&) :: Bool -> Bool -> Bool
```
Which tells us that `(&&)` - the logical `and` operator - takes a boolean (a truth value), then another boolean, and returns a boolean (the logical `and` of the first two).

Since under the hood games are nothing more than functions, REPL allows us to see the game type by doing `:t gameName`. If the game is parametrized, say, over a string, then `:t gameName "string"` will return the type where the first component has already been filled.

This tool is expecially powerful to better understand the structure of the strategies we have to feed to the model, which can grow very complicated as the model scales.

## Addendum: Installing haskell
If you dont' have either `haskell` or `stack`, it is necessary to install them. there are many ways to do so; on Linux/macOS systems, we suggest using [ghcup](https://www.haskell.org/ghcup/).
In a terminal, type:

```sh
 curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh 
```

If asked, respond 'yes' (`Y`) to the following questions:

```
Do you want to install haskell-language-server (HLS)?
Do you want to enable better integration of stack with GHCup?
```

Afterwards, `ghcup` may ask you to install some additional packages before continuing with the installation. Follow the advice before continuing. Then, just follow the instructions through the end.

`ghcup` is a very convenient solution in that it installs only in one folder (on Linux systems, `/home/$USER/.ghcup`). Should you decide to get rid of `haskell` altogether, just delete the folder.

**A note of warning:** GHC, the `haskell` compiler installed with `ghcup`, relies heavily on the GCC compiler. GHC assumes that GCC comes together with all the relevant libraries. As such, in compiling the model you may get errors such as:
```sh
/usr/bin/ld.gold: error: cannot find -ltinfo
```
these errors hint at missing GCC libraries, which will have to be installed independently. The precise iter to do so depends on the libraries involved and on your operating system. Unfortunately there is little we can do about it, as this is a problem with the general `haskell` developer infrastructure.


# Explaining the model

Here we give a more detailed explanation of what our model does.

## Recap: The Ledger-Hedger paper
As our model is based on the Ledger-Hedger paper, we start by briefly recalling the main highlights of this work.

[Ledger Hedger](https://eprint.iacr.org/2022/056.pdf) is a game-theoretic mechanism for gas price reservation. *Gas size* denotes the amount of computational work needed to execute a given function in a smart contract. For instance, in the Ethereum ecosystem every EVM instruction has a fixed gas size. As the blockspace demand varies overtime, transaction issuers dynamically specify the fees they are willing to pay to get their transactions included by providing a *gas price*: The total amount the issuer will pay to the miner (or whatever equivalent role a given blockchain provides) for a given transaction to be executed is then determined as the product between gas size and gas price.

### Ledger-Hedger: Players and phases

The fact that gas price varies with market conditions, generally rising when demand is high and falling when demand is low, can be a problem for some transaction issuers, that would like to reserve a a fixed gas price beforehand. Similarly, this can be a problem for miners, who may be unable to forecast their future profits. Ledger-Hedger provides a mechanism to address this problem. We consider a system with two participants:

- **Buyer**, that wants to issue a given transaction in a future block interval $ start<end $. The transaction's gas size - which from now on we will call $g_{alloc}$ to keep consistent with the paper - is presumed fixed.
- **Seller**, that has a given gas allocation within the above-mentioned timeframe.

This mechanism defines an interactive game articulated in two phases, called $\varphi_{init}$ and $\varphi_{exec}$, where at each stage **Buyer** and **Seller** can take different choices, as exemplified by the following figure.

![Ledger-Hedger statespace](pics/game_statespace.png)

Let us give a more detailed view of all the moving components in this picture.

- Phase I happens in a timeframe ranging from the current block to a block called $acc$, by which **Seller** will have to either accept or reject **Buyer**'s offer.
- Phase II happens within the block interval $start<end$. Again, this is the block interval within which **Buyer** wants their transaction executed, and within which **Seller** has gas space to offer. We postulate that $$now < acc < start < end$$

### Ledger-Hedger: The subgame space

Let us now describe the subgame space:
- **InitLH** is where **Buyer** can decide to either use or not use Leger-Hedger.
    - In the former case ($Wait$), **Buyer** just has to wait until $start$;
    - In the latter case ($Initiate$), buyer initiates the mechanism by:
        - Paying an amount $SentTokens$.
        - Specifying $acc$, the block number by which **Seller** must accept **Buyer**'s request. This effectively fixes when Phase I will end.
        - Specifying $start < end$, the block interval within which **Buyer** wants to execute the transaction.
        - Specifying $g_{alloc}$, the amount of gas units **Buyer** wishes to use.
        - Specifying $col$, some non-negative amount of tokens that **Seller** must provide in order to accept the request.
        - Specifying $\epsilon$, a non-negative parameter needed to swing the game-theoretic equilibrium in a favourable direction.
        - All the above-mentioned parameters are fixed only once, and then are considered immutable. As it will become clear soon, the gas price **Buyer** is offering, which in the paper is denoted $\pi_{contract}$, can be calculated as: $$\pi_{contract} := \frac{SentTokens}{g_{alloc}}$$

- **AcceptLH**, where **Seller** must decide if accepting or declining **Buyer**'s offer.
    - In the former case ($Decline$), **Seller** simply waits.
    - In the latter case ($Accept$), **Seller** commits the collateral $col$.
- **Nature draws** is when Phase II starts, and the market gas price $\pi_{exec}$ becomes known. This may be higher or lower than $\pi_{contract}$.

- **NoLH** is the subgame resulting from **Buyer** not having used Ledger-Hedger. Here **Buyer** can decide to either publish the transaction anyway ($Publish$), which gets confirmed at market price, or to not publish the transaction ($No-op$).
- **RecoupLH** is the subgame where **Buyer**'s proposition was not accepted. **Buyer** can either:
    - Choose to Recoup the funds ($Recoup$), in which case **Buyer** receives back the amount $SentTokens$ in full.
    - Choose to not recoup the funds ($Forfait$), in which case the amount $SentTokens$ is lost.
- **PublishTx** is the subgame where **Buyer** can decide to either publish the transaction ($Publish$) or not ($No-op$).
- **FullfillTX** is the subgame where **Seller** can:
    - Confirm the transaction ($Confirm$), thus receiving back the collateral $col$ together with the amount $SentTokens$. In practice, **Seller** executes the transaction at gas price $\pi_{contract}$. Moreover, if the gas size of the transaction $g_{pub}$ ends up being lower than the reserved $g_{alloc}$, **Seller** can sell the difference $g_{alloc} - g_{pub}$ at market price.
    - Exhaust the contract ($Exhaust$). In practice this means that **Seller** will replace the transaction execution trace with a bunch of null operations. In doing so, **Seller** receives $$SentTokens - \epsilon$$
    This is fundamental, as the lower payoff makes $Confirm$ a rationally better choice than $Exhaust$, thus incentivizing **Seller** not to 'betray' **Buyer**.
    - Ignore the situation ($Ignore$) by not doing anything. This results in **Seller** losing their collateral $col$.
- **FullFillNoTx** is a subgame similar to **PublishTx**, but in this case **Buyer** never publishes the transaction. In this case, the only available options for **Seller** are $Exhaust$ and $Ignore$, that work as above.


## Assumptions made explicit

In formalizing Ledger-Hedger, we had to make some assumptions, that were kept implicit in the paper, more explicit.

### Refined payoffs

First of all, the payoff types had to be refined: Whereas for some actions such as $Confirm$ or $Exhaust$ it is very clear how much **Seller** gains, in cases such as $No-op$ the utility is not specified: If one supposes that in this case the payoff is simply 0 (no gas gets spent whatsoever), then **Buyer** would default to $Wait$ and $No-op$ all the time. We had to assume, then, that the transaction that **Buyer** wants to issue has some *intrinsic utility*. This represents the desire for **Buyer** to see the transaction included in a block and is one of the main drivers to use Ledger-Hedger in the first place. 

In our model, a transaction is defined as a record of type:

``` haskell
data Transaction = Transaction
{ gasAllocTX    :: Gas
, utilityFromTX :: Utility
} deriving (Eq,Ord,Show)
```
here, `utilityFromTX` is the *intrinsic utility* of the transaction.

Another problem that required attention is the overall payoff structure. Ledger-Hedger is an interactive mechanism composed of various subgames, where a player choice determines the subgame that gets played next. This defines a tree-like structure, and payoffs are defined with respect to *game flows*, that is, a different payoff is defined for each one of the possible branches. This is in contrast with the usual approach of defining an *outcome space* (that is, a space representing all the posible outcomes of the overall game) and then defining payoffs on it (as function from this space to, say, the real numbers).

Adopting this flow perspective, things change a great deal if one decides to add payoffs 'as things progress' or if one calculates them only when the end of a branch is reached.

In the first case, payoffs can become negative at some stages of the game: For instance, in the subgame **AcceptLH**, **Seller** gets a negative payoff when the $Accept$ decision is taken. Clearly, this allows the game to progress and **Seller** will be able to recoup the collateral together with the mining fee later on, making the payoff positive overall.

Still, adding payoffs as things progress can cause problems when one notices that the utility functions representing risk-aversion - namely `sqrt` and `log` - are well-defined only for positive numbers. To avoid this inconsistency problems we opted for calculating payoffs only when the end of a branch is reached. We also think that this represents better rational players, which are able to reason about the game until 'the very end'.


### Ambiguous **Buyer** behavior

In the Ledger-Hedger paper, there are some circumstances in which **Seller** does not publish **Buyer**'s transaction. In these circumstances, the paper ambiguously states that **Buyer** *can* decide to publish the transaction at market price. This assumption is problematic, as in publishing **Buyer** incurs in some extra costs, while gaining the intrinsic utility of the transaction. We had to make these choices explicit in our model: In a situation where **Buyer**'s transaction does not get published, **Buyer** can explicitly decide to publish it at market price, with payoffs being influenced accordingly.


### Service costs

In the paper, actions such as **Initiate** or **Accept** have a cost, which is expressed in gas units. This makes sense as the paper is aimed at providing on-chain gas price reservation mechanisms. We incorporated these costs in the Ledger-Hedger contract type, defined as the following record:

``` haskell
data HLContract = HLContract
{ collateral    :: Collateral
, payment       :: Paymentn
, epsilon       :: Payment
, gasInitiation :: Gas
, gasAccept     :: Gas
, gasDone       :: Gas
} deriving (Eq,Show,Ord)
```

Here, `gasInitiation` represents the fee **Buyer** has to pay to initiate an Ledger-Hedger contract,`gasAccept` is what **Seller** has to pay to accept it, and `gasDone` represent the fee **Seller** has to pay to finally fullfill the contract. 

We stuck to the nomenclature used in the paper to aid comprehension, but since the type `Gas` is just an alias for `Double`, our model is not necessarily bound to the on-chain interpretation, and can be easily modified to accomodate more general scenarios.


### Modelling risk explicitly

Perhaps the most important assumption we made explicit is around the notion of risk that the players in the paper - **Buyer** and **Seller**, respectively - have.

In a nutshell, *risk propensity* denotes how much one player prefers a *certain* payoff versus an *uncertain* one. 
    
    As a simple example, let us imagine a lottery where one can win between 0 and 100 dollars with equal probability. The expected payoff in this scenario is 50 dollars. A *risk-averse* player will prefer to receive 50 dollars with certainty than playing the lottery. On the contrary, a *risk-loving* player will prefer to play the lottery: the hope for a higher payoff wins over the possibility of getting a lower one. A *risk-neutral* player will be unbiased with respect to which game to play.

In the Ledger-Hedger paper, players are considered to be risk-averse or at best risk-neutral. This makes sense, as the main incentive to use Ledger Hedger is exactly hedging against the uncertainty of future gas price fluctuations:
 - A risk-averse **Buyer** seeks protection against the possibility of prices rising, resulting in bigger expenses;
 - A risk-averse **Seller** seeks protection against the possibility of prices falling, resulting in less profit.

To model this assumption explicitly, we relied on [Expected Utility Theory](https://en.wikipedia.org/wiki/Risk_aversion#Utility_of_money): In checking if the game is at equilibrium, the payoffs for both players aren't used as they are (this would be the risk-neutral case). Instead, they are first fed to a couple of functions called `utilityFunctionBuyer` and `utilityFunctionSeller`, that can be defined in any way the modeller wants. They represent the risk propensity of both **Buyer** and **Seller**. A concave function will represent a risk-averse player, whereas a convex function will represent a risk-prone player. Supplying the identity functions will result in the standard risk-neutral case.

Besides the identities, we also provided a square root definition and a logarithmic definition for the utility functions, which can be found in `Parametrization.hs`

In practice, these functions are used as follows (cf. [Code deep dive](#code-deep-dive) for syntax details):

`returns : utilityFunction $ plainPayoffFunction data;`

What this means is that we are piping the output of `plainPayoffFunction` to a utility function, that 'skews' the payoff by virtue of being concave or convex.


# Code deep dive

## Recap: DSL primer

Our models are written in a custom DSL compiled to `haskell`. Here we give a brief description of how our software works.

### The building blocks
The basic building block of our model is called **open game**, and can be thought of as a game-theoretic lego brick. This may represent a player, a nature draw, a payoff matrix or a complex combination of these elements. It has the following form:

```haskell
gameName variables = [opengame|

   inputs    : a;
   feedback  : b;

   :----------------------------:

   inputs    : a';
   feedback  : b';
   operation : content;
   outputs   : s';
   returns   : t';

   :----------------------------:

   outputs   :  s;
   returns   :  t;
  |]
```

We can imagine this block as a box with 4 wires on its outside, on which travels information marked as:
- `inputs`, data that gets fed into the game (e.g. a player receiving information from a context).
- `outputs`, data that the game feeds to the outside world (e.g. a player communicating a choice to another player).
- `returns`, the returns of a player actions, which are usually directly fed to a function calculating payoffs.
- The `feedback` wire which sends information back in time. If, intuitively, `returns` represents the returns on a player action, one could imagine it as 'information that an agents receive from the future'. `feedback` is the dual analog of that: If a given piece of information comes from the future, someone in the future must have been sent it to the past. For additional details about the `feedback` wire please refer to the relevant [literature](https://arxiv.org/abs/1603.04641).

The `:--:` delimiters separate the outside from the inside of the box. As one can see, the interfaces inside are replicated. This is intentional as it allows for a notion of *nesting*. For instance, the situation depicted in the following picture:

![An open grame in graphical form](pics/box.png)

Can be represented by the following code block:

```haskell
gameName variables = [opengame|

   inputs    : a, a';
   feedback  : b;

   :----------------------------:

   inputs    : a';
   feedback  : ;
   operation : SubGame1;
   outputs   : x;
   returns   : t';

   inputs    : a, x;
   feedback  : b;
   operation : SubGame2;
   outputs   : s;
   returns   : t;
   :----------------------------:

   outputs   :  s;
   returns   :  t,t';
  |]
```

In turn, `Subgame1` and `Subgame2` can be other games defined using the same DSL. Notice that the wire `x` is internal and totally hidden from the 'outside world'. 

### Exogenous parameters

An exogenous parameter is a given assumption that is not part of the model, and is fed to it externally. As such, it is treated by the model as a 'fact' that cannot really be modified. An example of exogenous parameter could be the market conditions at the time when a game is played.

Exogenous parameters are just defined as variables, as the field `variables` in the previous code blocks testifes. These variables can in turn be fed as exogenous parameters to inside games, as in the following example:

```haskell
gameName stock1Price stock2Price  = [opengame|

   inputs    : a, a';
   feedback  : b;

   :----------------------------:

   inputs    : a';
   feedback  : ;
   operation : SubGame1 stock1Price;
   outputs   : x;
   returns   : t';

   inputs    : a, x;
   feedback  : b;
   operation : SubGame2 stock2Price;
   outputs   : s;
   returns   : t;
   :----------------------------:

   outputs   :  s;
   returns   :  t,t';
  |]
```

### Basic operations

In addition to the DSL defining the 'piping rules' between boxes, we provide some *basic operations* to populate a box, namely:
- A *function*, which just transforms the input in some output.
- A *stochastic distribution*, used to implement draws from nature.
- A *strategic choice*, which can be thought of as a function parametrized over strategies.

### Branching

Another important operation we provide is called *branching*. This is useful in contexts where, say, a player choice determines which subgame is going to be played next.
Branching is represented using the operator `+++`. So, for instance, if `SubGame1` is defined as ```branch1 +++ branch2```, then we are modelling a situation where `SubGame1` can actually evolve into two different games depending on input. As the input of a game can be the outcome of a strategic choice in some other game, this allows for flexible modelling of complex situations.

Graphically, branching can be represented by resorting to [sheet diagrams](https://arxiv.org/abs/2010.13361), but as they are quite complicated to draw, this depiction is rarely used in practice.


### Supplying strategies

As usual in classical game theory, a strategy conditions on the observables and assigns a (possibly randomized) action. 

Every player who can make a decision in the game needs to be assigned a strategy. These individual strategies then get aggregated into a list representing the complete strategy for the whole game.

So, for instance, if our model consists of three subgames, a strategy for the whole model will just be a list:

```haskell
`strGame1 ::- strGame2 ::- strGame3 ::- Nil`.
```

#### Evaluating strategies

To evaluate strategies, it is enough to just run the `main` function defined in `Main.hs`. This is precisely what happens when we give the command `stack run`. In turn, `main` invokes functions defined in `Analytics.hs` which define the right notion of equilibrium to check. If you want to change strategies on the fly, just open a REPL (Cf. [Interactive Execution](#interactive-execution)) and give the command `main`.
You can make parametric changes or even define new strategies and/or notions of equilibrium by editing the relevant files (cf. [File structure](#file-structure)). Once you save your edits, giving `:r` will recompile the code on the fly. Calling `main` again will evaluate the changes.


#### Stochasticity

Our models are Bayesian by default, meaning that they allow for reasoning in probabilitic terms.

Practically, this is obtained by relying on the [Haskell Stochastic Package](https://hackage.haskell.org/package/stochastic), which employs monadic techniques.

A consequence of this is that deterministic strategic decisions (e.g. 'player chooses option A') must be lifted into the stochastic monad, getting thus transformed into their probabilistic equivalent (e.g. 'of all the options available, player chooses A with probability 1')

A practical example of this is the following:

```haskell
strategyName
  :: Kleisli
       Stochastic
       (Parameter1, Parameter2)
       Decision
strategyName = pureAction Decision1
```

In the example above, the player observes some parameters (`Parameter1` and `Parameter2` in this particular case), and then must assign an action (in this case `Decision1`).

`pureAction` lifts the deterministic choice `Decision1` to the corresponding concept in the probabilistic realm. 

The upside of assuming this little amount of overhead is that switching from pure to mixed strategies can be easily done on the fly, without having to change the model beforehand.

#### Branching
As a word of caution notice that, in a game with branching, we need to provide a possible strategy for each branch. For example, suppose to have the following game:

- Player 1 can choose between option A and B;
    - case A: Player 2 can choose between option A1 or A2;
    - case B: Player 2 can choose between option B1 or B2;

Moreover, suppose that the payoffs are as follows: 

- If Player1 chooses A, and then Player2 chooses A1, then both players get 100$.
- In any other case, both players get 0$.

In this game the best strategy is clearly (A,A1). Nevertheless, we need to supply a strategy for Player2 also in the 'B' branch: Even if Player1 will never rationally choose B, Player2 needs to be endowed with a clear choice between B1 and B2 in case this happens.

## File structure

The model is composed of several files:

- `app/Main.hs` contains all the main ingredients already set up for a run. Executing it will execute equilibrium checking on some of the most interesting strategies we defined. We suggest to start from here to get a feel of how the model analysis works.
- `Model.hs` is the file where the main model is defined.
- `Components.hs` is where the subgames making up the whole model are defined.
- `Payoffs.hs` is where the payoff functions used in every subgame are defined. We decided to keep them all in the same file to make tweaking and fine-tuning less dispersive.
- `Strategies.hs` is where the strategies we want to test are defined.
- `Parametrization.hs` defines the concrete parametrizations used for the analysis: e.g. intrinsic utility of **Buyer**'s transaction, fixed costs for initiating a Ledger-Hedger contract etc.
- `Types.hs` is where we define the types of the decisions to be taken (e.g. $Wait$ or $Initiate$ a Ledger-Hedger contract) and the types of `HLContract` and `Transaction`. Here we also define the types for payoffs, gas etc. These are all aliased to `Double`.
- `ActionSpaces.hs` is mainly needed for technical type-transformations. It maps a player's decision type into the type needed to be fed in the subsequent game.
- `Analytics.hs` defines the equilibrium notion for each game we want to test.
- `Diagnostics.hs` is the file detailing which and how much information we want to show when strategies are tested.

Relying on the DSL Primer, parsing the code structure should be a manageable task.

Moreover, the code is divided in two different branches:
- `main` contains the standard model, and runs analytics with the parameters provided in the Ledger-Hedger paper.
- `eq-breaking` instead parametrizes the risk-aversity in the utility functions, and runs stress tests to check under which conditions the equilibrium breaks.

# Analytics

Now, we switch focus on *analytics*, which we defined as the set of techniques we employ to verify if and when a supplied results in an *equilibrium*. The notion of *equilibrium* we rely upon is the one of [Nash equilibrium](https://en.wikipedia.org/wiki/Nash_equilibrium), which intuitively describes a situation where, for each player, unilaterally deviating from the chosen strategy results in a loss.


## Reading the analytics

Analytics in our model are quite straightforward. In case a game is in equilibrium, the terminal will print `Strategies are in equilibrium`.

For games with branching, there will also be a `NOTHING CASE`. To understand this, consider a game (call it `First Game`) that can trigger two different subgames (`Subgame branch 1`, `Subgame branch 2`, respectively) depending on the player's choice. Analytics would read like this:

```
 Game name
First Game:

 Strategies are in equilibrium
Subgame branch 1:

 NOTHING CASE
Subgame branch 2:

 Strategies are in equilibrium
```

Here `NOTHING CASE` signifies that the choice provided by the player results in not visiting `Subgame branch 1`, which is thus never played in this senario: Evidently, the choice made by the player in `First Game` resulting in the play continuing on `Subgame branch 2`.

On the contrary, analytics become more expressive when the game is *not* in equilibrium. In this case, the engine will suggest a more profitable deviation by displaying the following prompt:

```
Strategies are NOT in equilibrium. Consider the following profitable deviations: 

Player: 
Optimal Move: 
Current Strategy:
Optimal Payoff: 
Current Payoff: 
Observable State:
 --other game-- 
 --No more information--
```

`Observable State` contains a dump of all the game parameters that are currenlty observable by all players. This is usually a lot of information, mainly useful for debugging purposes. All the other field names are pretty much self-describing. 


## Strategies employed in the analysis

Our analysis is focused on understanding when the targeted equilibrium - where the Ledger-Hedger contract gets initiated and later published by **Buyer** as well as accepted and confirmed by **Seller** - can be supported. Concretely, this means we employ the following strategies:

```haskell
-- | Buyer: initiate contract strategy 
initiateStrategyBuyerTarget
  :: Kleisli
           Stochastic
           (Transaction, HLContract, GasPrice)
           (InitialDecisionBuyer HLContract)
initiateStrategyBuyerTarget =
  Kleisli (\(_,contract,_) -> playDeterministically $ Initiate contract)

-- | Buyer: publish strategy if no LH
noLHPublishStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, GasPrice)
       (PublishDecision Double)
noLHPublishStrategyTarget =  Kleisli (\(tx,_ ) -> playDeterministically $ Publish (gasAllocTX tx))

-- | Seller: accept decision
acceptStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice)
       AcceptDecisionSeller
acceptStrategyTarget = pureAction Accept


-- | Buyer: publish strategy if recoup
recoupPublishTarget
  :: Kleisli
       Stochastic
       (Transaction, GasPrice)
       (PublishDecision Double)
recoupPublishTarget =  Kleisli (\(tx,_ ) -> playDeterministically $ Publish (gasAllocTX tx))

-- | Buyer: recoup strategy buyer
recoupStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice, GasPrice)
       RecoupDecisionBuyer
recoupStrategyTarget = pureAction Refund

-- | Buyer: publish strategy part 1 if LH
lhPublishStrategyPart1Target
  :: Kleisli
       Stochastic
       GasPrice
       (PublishDecision Double)
lhPublishStrategyPart1Target =  pureAction $ Publish 0.0

-- | Buyer: publish strategy part 2 if LH
lhPublishStrategyPart2Target
  ::  Kleisli
          Stochastic
          (GasPrice, Transaction, PublishDecision a1)
          (PublishDecision Gas)
lhPublishStrategyPart2Target =
  Kleisli
   (\(pi,tx,publishDecision) -> 
        case publishDecision of
          NoOp -> playDeterministically NoOp
          Publish _ -> playDeterministically $ Publish $ gasAllocTX tx)

-- | Seller: fulfill strategy
fulfillStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice,GasPrice, Gas)
       FulfillDecisionSeller
fulfillStrategyTarget = pureAction Confirm

-- | Seller: noFulfill strategy
noFulfillStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice, GasPrice)
       FulfillDecisionSeller
noFulfillStrategyTarget = pureAction Exhaust

-- | Buyer: publish strategy if no fulfill
nofulfillPublishTarget
  :: Kleisli
       Stochastic
       (Transaction, GasPrice)
       (PublishDecision Double)
nofulfillPublishTarget =  Kleisli (\(tx,_ ) -> playDeterministically $ Publish (gasAllocTX tx))
```

As detailed in [File structure](#file-structure), the strategies above reside in `Strategies.hs`. For more information about how to supply strategies and/or how to make changes, please refer to the section [Supplying Strategies](#supplying-strategies).

## Running the analytics

As already stressed in [Evaluating strategies](#evaluating-strategies), there are two main ways to run strategies. In the [Normal execution](#normal-execution) mode, one just needs to give the command `stack run`. This command will execute a pre-defined battery of strategies using the parameters predefined in the source code. These parameters can be varied as one pleases. Once this is done and the edits are saved, `stack run` will automatically recompile the code and run the simulation with the new parameter set.

In the [Interactive execution](#interactive-execution) mode, the users accesses the repl via the command `stack ghci`. Here one can run single functions by just calling them with the relevant parameters, as in:

```haskell
functionName parameters
```

In particular, calling the function `main` in interactive mode will result in the same behavior of calling `stack run` in normal mode. Again, editing the source code and then hitting `:r` will trigger recompilation on the fly.


## Replicating the Ledger-Hedger paper results 

These results can be found in the `main` branch (see subsection [File structure](#file-structure) for more information).

To replicate the results highlighted in the Ledger-Hedger paper, we instantiated the model with the following parameters (the instantiation can be found in `Parameters.hs`):

| **Parameter**  | **Name in the paper** | **Meaning** | **Value** |
|:--------------:|:---------------------:|:-----------:|:----------:|
| `buyerWealth`  | $W^{init}_{Buyer}$    | Initial wealth of **Buyer** | $10^9$           |
| `sellerWealth` | $W^{init}_{Seller}$   | Initial wealth of **Seller** |$10^9$           |
| `collateral`   | $$col$$               | The collateral **Seller** must pay to accept LH contract. | $10^9$           |
| `piInitial` | $\pi_{initial}$ | Initial gas price | $100$
| `piContract`      | $\pi_{contract}$               | The price at which **Buyer** buys `gasAllocTX` from **Seller** in the LH contract. | $100$            |
| `payment` | $payment$ | The ampount **Buyer** pays to **Seller** in LH | `gasAllocTX * piContract`
| `epsilon`      | $\epsilon$            | Technical parameter to disincentivize unwanted behavior from **Seller**. | $1$              |
| `gasInitiation`| $g_{init}$            | Cost of opening a LH contract. | $0.1 \cdot 10^6$ |
| `gasAccept`    | $g_{accept}$          | Cost of accepting a LH contract. | $75 \cdot 10^3$  |
| `gasDone`      | $g_{done}$            | Cost of closing a LH contract. | $20 \cdot 10^3$  |
| `gasAllocTX`   | $g_{alloc}$           | Gas reserved in the LH contract. | $5 \cdot 10^6$   |
| `gasPub`   | $g_{pub}$           | Gas size of the TX if issued at current market price. | $5 \cdot 10^6$   |


These parameters were directly pulled from Sec. 6 of the Ledger-Hedger paper. As for the utility functions, we again followed what the authors did by instantiating the utility functions for both **Buyer** and **Seller** to be first $log(x)$ and then $\sqrt(x)$. These function represent risk-aversity.

Moreover, as specified in the section [Assumptions made explicit](#assumptions-made-explicit), we had to postulate an explicit utility for the transaction that **Buyer** wants to issue. This is represented by the parameter `utilityFromTX`, which has been set to $10^9$.

As for the future price distribution, we defined it in csv format in the file `/probability/distribution.csv` (we also provide a constant distribution, `/probability/distributionOneElement.csv` for debugging reasons). As the distribution is input through this external file, any other distribution (e.g. based on actual data) can be used. `distribution.csv` is a normal distribution centered around the initial gas price. Again, we used the standard deviations suggested in the paper. 

Moreover, we defined `testActionSpaceGasPub`, representing the range in which $g_{pub}$ can swing. $g_{pub}$ is the gas consumed if **Buyer** decides to issue the transaction at market price (for more information see [Ambiguous **Buyer** behavior](#ambiguous-buyer-behavior)). In practice, we followed the paper and equated $g_{pub}$ and $g_{alloc}$, meaning that **Buyer** is using Ledger-Hedger to reserve the precise amount of gas needed to execute the transaction.

As we already stated in [Summary - Analytics results](#analytics-results), we found that the equilibrium with this parameters is quite brittle, and heavily relying on both the risk-aversity of both **Buyer** and **Seller** and on the shape of the probability distribution. By this, we mean that the utility gain in using the Ledger-Hedger is really small, and even a small variation in the given parameters can result in equilibrium breaking.

The reason why the protocol is so sensible is that, **Seller**-side, the usage fees (`gasAccept`, `gasDone`) are quite high. This immediately disincentivates **Seller** to use the protocol, as in terms of raw payoffs doing so results in a loss.

Similarly, **Buyer**-side, the presence of a usage fee (`gasInitiation`) and the cost defined by`payment` disincentivate **Buyer** to use the protocol.

This 'operating at a loss' result is counterbalanced uniquely by the accentuated concavity of the utility functions provided. In a nutshell, both players are so risk-averse that they are willing to pay these huge premiums to protect themselves.

Another reason why Ledger-Hedger is so sensible is also that players are fundamentally unbiased with respect to future gas price: it can go up or down, for both players, with equal probability. We are quite sure the situation would look different if players had private information available about future price distribution. Imagine the current situation:

- **Seller** believes price will go down in the between blocks $start$ and $end$.
- On the contrary, **Buyer** believes that price will go up within the same block interval.

In this scenario, both Players will be much more willing to use Ledger-Hedger, albeit for opposite reasons. Importantly, depending on how skewed these beliefs are, we may dispense of risk-aversity all together: Even for a risk-loving player hedging would make sense if the player strongly believed that prices would swing towards an unfavorable direction.

## Other analyses

We ran also some other analyses these can be found in the `eq-breaking` branch (see subsection [File structure](#file-structure) for more information). 

First of all, and unsurprisingly, we found that running the model with the following parameters results in a much bigger utility:

| **Parameter**  | **Name in the paper** | **Meaning** | **Value** |
|:--------------:|:---------------------:|:-----------:|:----------:|
| `buyerWealth`       | $W^{init}_{Buyer}$    | Initial wealth of **Buyer** | $10^9$           |
| `sellerWealth`       | $W^{init}_{Seller}$   | Initial wealth of **Seller** |$10^9$           |
| `collateral`   | $$col$$               | The collateral **Seller** must pay to accept LH contract. | $10^9$           |
| `piInitial` | $\pi_{initial}$ | Initial gas price | $100$
| `piContract`      | $\pi_{contract}$               | The price at which **Buyer** buys `gasAllocTX` from **Seller** in the LH contract. | $100$            |
| `payment` | $payment$ | The ampount **Buyer** pays to **Seller** in LH | `gasAllocTX * piContract`
| `epsilon`      | $\epsilon$            | Technical parameter to disincentivize unwanted behavior from **Seller**. | $1$              |
| `gasInitiation`| $g_{init}$            | Cost of opening a LH contract. | $0$ |
| `gasAccept`    | $g_{accept}$          | Cost of accepting a LH contract. | $0$  |
| `gasDone`      | $g_{done}$            | Cost of closing a LH contract. | $0$  |
| `gasAllocTX`   | $g_{alloc}$           | Gas reserved in the LH contract. | $5 \cdot 10^6$   |
| `gasPub`   | $g_{pub}$           | Gas size of the TX if issued at current market price. | $5 \cdot 10^6$   |

This represents the scenario where we dispense of the platform fees altogether, and pay for `gasAllocTX` exactly the current price. Most likely, this scenario hadn't been considered in the original paper as the original work was meant to be part of some on-chain infrastructure. As such, what we call 'platform fees' would just be unavoidable smart contract execution costs.

Keeping everything fixed, we then turned `piContract` into a parameter. In the case when both player's utility is set to be $\sqrt x$, we verified that in this setting the equilibrium is solid within the bound:

$$98 \leq \mathtt{piContract} \leq 102$$

This represents the maximum and minimum `piContract` parameters within which both **Buyer** and **Seller** judge using Ledger-Hedger convenient. This result aligns perfectly with the one highlighted in the Ledger-Hedger paper, figure 4.

Having verified this, we used both the original paper parameters and the 'zero fees' parameters to run a multivariate analysis on player's risk aversity. In practice, we generalized the $\sqrt x$ function, up to now being used by both players, to the couple of functions

$$\Large x^{\frac{1}{y_{\mathbf{Buyer}}}} \qquad x^{\frac{1}{y_{\mathbf{Seller}}}}$$

The concavity/convexity of these functions is dependent on the value of $y$, as shown in the following chart:

![Plotting x^{1/y} for different values of y](pics/x_exp_y.png)

Thus, $y_{\mathbf{Buyer}}$ and $y_{\mathbf{Seller}}$ represent the risk-aversity of both players, respectively.
Positive values $< 1$ represent risk-love ,$1$ represents risk-neutrality, while values $> 1$ signal risk-aversity.

Keeping all parameters fixed, the shaded region in the following graphs represents where the equilibrium holds for different risk-aversity ranges. 

At $\texttt{piContract} = 98$, we get the following graph:
![yBuyer/ySeller graph, piContract=98 ](pics/price_98.png)
The blue region represents the 'zero fees' scenario, whereas the red region represents the fees as in the Ledger-Hedger paper. As on can see, the red region is strictly contained in the blue one. We have:
$$
\begin{alignat*}{2}
\text{\color{blue} Zero fees scenario:} \qquad  0.68 \leq y_{\mathbf{Buyer}} \qquad 1.92 \leq y_{\mathbf{Seller}}\\
\text{\color{red} Paper fees scenario:} \qquad 0.94 \leq y_{\mathbf{Buyer}} \qquad 7.09 \leq y_{\mathbf{Seller}}
\end{alignat*}
$$

This is compatible with the idea that the lower the fees the less risk averse players must be to judge the use of Ledger-Hedger convenient.

Setting $\texttt{piContract} = 99$, we get the following graph:
$$
\begin{alignat*}{2}
\text{\color{blue} Zero fees scenario:} \qquad  0.79 \leq y_{\mathbf{Buyer}} \qquad 1.39 \leq y_{\mathbf{Seller}}\\
\text{\color{red} Paper fees scenario:} \qquad 1.16 \leq y_{\mathbf{Buyer}} \qquad 2.93 \leq y_{\mathbf{Seller}}
\end{alignat*}
$$
![yBuyer/ySeller graph, piContract=99 ](pics/price_99.png)

For $\texttt{piContract} = 100$:
$$
\begin{alignat*}{2}
\text{\color{blue} Zero fees scenario:} \qquad  0.94 \leq y_{\mathbf{Buyer}} \qquad 1.08 \leq y_{\mathbf{Seller}}\\
\text{\color{red} Paper fees scenario:} \qquad 1.50 \leq y_{\mathbf{Buyer}} \qquad 1.85 \leq y_{\mathbf{Seller}}
\end{alignat*}
$$
![yBuyer/ySeller graph, piContract=100](pics/price_100.png)

For $\texttt{piContract} = 101$:
$$
\begin{alignat*}{2}
\text{\color{blue} Zero fees scenario:} \qquad  1.16 \leq y_{\mathbf{Buyer}} \qquad 0.89 \leq y_{\mathbf{Seller}}\\
\text{\color{red} Paper fees scenario:} \qquad 2.15 \leq y_{\mathbf{Buyer}} \qquad 1.35 \leq y_{\mathbf{Seller}}
\end{alignat*}
$$
![yBuyer/ySeller graph, piContract=101](pics/price_101.png)

For $\texttt{piContract} = 102$:
$$
\begin{alignat*}{3}
&\text{\color{blue} Zero fees scenario:} \qquad  1.50 &&\leq y_{\mathbf{Buyer}} \qquad 0.75 &&\leq y_{\mathbf{Seller}}\\
&\text{\color{red} Paper fees scenario:} \qquad 3.77 &&\leq y_{\mathbf{Buyer}} \qquad 1.06 &&\leq y_{\mathbf{Seller}}
\end{alignat*}
$$
![yBuyer/ySeller graph, piContract=102](pics/price_102.png)

As one can see, as `piContract` increases the shaded regions migrate to the lower-right end. Again, this makes sense: As the price goes higher, **Buyer** is paying more and more for `gasAllocTX` with respect to current price. This entails that **Buyer** should be more risk-averse to deemm this advantageous, and hence the region moves further to the right on the **Buyer** axis. Specularly, **Seller** is receiving an increasingly better offer compared to the current price, lowering the necessity for risk-aversity. As such, the region grows closer to the **Seller** axis.

### Sanity checks

In addition to this, we performed some sanity checks: Using the constant distribution (see [Replicating the Ledger-Hedger paper results](#replicating-the-ledger-hedger-paper-results) for more information), we verified the following things:

- Setting `epsilon` to 0 results, in the 'zero fees' case, in equilibrium for any choice of risk-aversity for both players. This makes sense: Using the protocol results in zero extra costs. Moreover, both players know with certainty that the future gas price won't change. Hence, risk-aversity does not matter.
- With *any* other choice of `epsilon` and any other fees, the equilibrium breaks for any choice of risk-aversity, for both players. Again, this makes sense: Both players know with certainty that the future gas price won't change, so payng *any* extra fee to hedge is disadvantageous.

### Interactive perks

Finally, in the `eq-breaking` branch there are more things one can do, the most important being that we expose a new function in `Main.hs` that allows for better interactive queries. In interactive mode, giving:

```haskell
interactiveMain piContract utilityParameterBuyer utilityParameterSeller
```

allows to run the Ledger-Hedger strategy on both the 'zero fees' and 'paper fees' scenarios, with customized `piContract` and risk-aversity parameters for both players. In this way, the user can verify equilibria for particular choices of values without the need to recompile.