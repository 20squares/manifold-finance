# manifold-hedger

## Summary
This project implements the model detailed in the [Ledger-Hedger](https://eprint.iacr.org/2022/056.pdf) paper. In particular, we have focused on relaxing some of the assumptions made in the paper around all players being risk-averse (in particular the seller). Our model allows for custom definitions of risk-sensitivity for all players.


## Installation

## The Hedger-Ledger paper
As our model is based on the Ledger-Hedger paper, we start by briefly recalling the main highlights of this work.

[Ledger Hedger](https://eprint.iacr.org/2022/056.pdf) is a game-theoretic mechanism for gas price reservation. *Gas size* denotes the amount of computational work needed to execute a given function in a smart contract. For instance, in the Ethereum ecosystem every EVM instruction has a fixed gas size. As the blockspace demand varies overtime, transaction issuers dinamically specify the fees they are willing to pay to get their transactions included by providing a *gas price*: The total amount the issuer will pay to the miner (or whatever equivalent role a given blockchain provides) for a given transaction to be executed is then determined as the product between gas size and gas price.

The fact that gas price varies with market conditions, generally rising when demand is high and falling when it's not, can be a problem for some transaction issuers, that would like to reserve a a fixed gas price beforehand. Similarly, it can be a problem for miners, that may be unable to forecast their future profits. Ledger Hedger provides a mechanism to address this problem. We consider a system with two participants:

- $Buyer$, that wants to issue a given transaction in a future block interval $start<end$. The transaction's gas size, which from now on we will call $g_{alloc}$ to keep consistent with the paper, is presumed fixed.
- $Seller$, that has a given gas allocation within the above-mentioned timeframe.

This mechanism defines an interactive games articulated in two phases, called $\varphi_{init}$ and $\varphi_{exec}$, where at each stage $Buyer$ and $Seller$ can take different choices, as exemplified by the following figure.

![Hedger-Ledger statespace](game_statespace.png)

Let us give a more detailed view of all the moving components in this picture.

- Phase I happens in a timeframe ranging from the current block to a block called $acc$, by which $Seller$ will have to either accept or reject $Buyer$'s offer.
- Phase II happens within the block interval $start<end$. Again, this is the block interval within which $Buyer$ wants their transaction executed, and $Seller$ has gas space to offer. We postulate that $$now < acc < start < end$$

Let us now describe the action space:
- **InitLH** is where $Buyer$ can decide to either use or not use Hedger Leger. 
    - In the former case ($Wait$), $Buyer$ just has to wait until $start$;
    - In the latter case ($Initiate$), buyer initiates the mechanism by:
        -paying an amount $SentTokens$;
        - specifying $acc$, the block number by which $Seller$ must accept $Buyer$'s request. This effectively fixes when Phase I will end.
        - specifying $start < end$, the block interval within which $Buyer$ wants to execute the transaction.
        - $g_{alloc}$, the amount of gas units $Buyer$ wishes to use.
        - $col$, some non-negative amount of tokens that $Seller$ must provide in order to accept the request.
        - $\epsilon$, a non-negative parameter needed to swing the game-theoretic equilibrium in a favourable direction.
        - All the above-mentioned parameters are fixed only once, and then are considered immutable. As it will become clear soon, the gas price $Buyer$ is offering, which in the paper is denoted $\pi_{contract}$, can be calculated as: $$\pi_{contract} := \frac{SentTokens}{g_{alloc}}$$

- **AcceptLH**, where $Seller$ must decide if accepting or declining $Buyer$'s offer.
    - In the former case ($Decline$), $Seller$ simply waits.
    - In the latter case ($Accept$), $Seller$ commits the collateral $col$.
- **Nature draws** is when Phase II starts, and the market gas price $\pi_{exec}$ becomes known. This may be higher or lower than $\pi_{contract}$.

- **NoLH** is the subgame resulting from $Buyer$ not having used Ledger Hedger. Here $Buyer$ can decide to either publish the transaction anyway ($Publish$), which gets confirmed at market price, or to not publish the transaction ($No-op$).
- **RecoupLH** is the subgame where $Buyer$'s proposition was not accepted. $Buyer$ can either:
    - Choose to Recoup their funds ($Recoup$), in which case $Buyer$ receives back the amount $SentTokens$ in full
    - Choose to not recoup the funds ($Forfait$), in which case the amount $SentTokens$ is lost.
- **PublishTx** is the subgame where $Buyer$ can decide to either publish the transaction ($Publish$) or not ($No-op$).
- **FullfillTX** is the subgame where $Seller$ can:
    - Confirm the transaction ($Confirm$), thus receiving back the collateral $col$ together with the amount $SentTokens$. In practice, $Seller$ executes the transaction at gas price $\pi_{contract}$.
    - Exhaust the contract ($Exhaust$). In practice this means that $Seller$ will replace the transaction execution trace with a bunch of null operations. In doing so, $Seller$ receives $$SentTokens - \epsilon$$
    This is fundamental, as the lower payoff makes $Confirmm$ a rationally better choice than $Exhaust$, thus incentivizing $Seller$ not to 'betray' $Buyer$.
    - Ignore the situation ($Ignore$) by not doing anything. This results in $Seller$ losing their collateral $col$.
- **FullFillNoTx** is a subgame similar to **PublishTx**, but in this case $Buyer$ never published the transaction. In this case, the only available options for $Seller$ are $Exhaust$ and $Ignore$, that work as above.


## Project structure

Here we give an in-depth tour of how our model works. 

### Assumptions made explicit

In formalizing Hedger Ledger, we had to make explicit some assumptions that were kept implicit in the paper.

#### Refined payoffs

First of all, the payoffs had to be refined: Whereas for some actions such as $Confirm$ or $Exhaust$ it is very clear how much $Seller$ gains, in cases such as $No-op$ the utility is not specified: If one supposes that in this case the payoff is simply 0 (no gas gets spent whatsoever), then $Buyer$ would default to $Wait$ and $No-op$ all the time. We had to assume, then, that the transaction that $Buyer$ wants to issue has some *intrinsic utility*. Indeed, in our model transaction is defined as a record of type:
``` haskell
data Transaction = Transaction
{ gasInitiation :: Gas
, gasAllocTX    :: Gas
, utilityFromTX :: Utility
} deriving (Eq,Ord,Show)
```
here, `utilityFromTX` is the *intrinsic utility* of the transaction.

#### Service costs
We moreover assumed that actions had costs: In the paper, actions such as $Initiate$ or $Accept$ are *costless*, whereas in our model one can see that, for instance, a Hedger Ledger contract is defined as a record of type:
``` haskell
data HLContract = HLContract
{ collateral    :: Collateral
, payment       :: Payment
, epsilon       :: Payment
, gasAccept     :: Gas
, gasDone       :: Gas
} deriving (Eq,Show,Ord)
```

Here, `gasInitiation` represents the fee $Buyer$ has to pay to initiate an Hedger Ledger contract,`gasAccept` is what $Seller$ has to pay to accept it, and `gasDone` represent the fee $Seller$ has to pay to finally fullfill the contract. 

The simpler case highlighted in the paper can be recovered by setting these parameters to 0.
Otherwise, we can simulate scenarios where the platform (in this case Manifold) incentivizes to use the price reservation mechanism by setting positive payoffs. Negative payoffs represent service fees that users have to pay to the platform.

#### Modelling risk explicitly

Perhaps the most important assumption we made explicit is around the notion of risk the players in the paper - $Buyer$ and $Seller$, respectively - have.

In a nutshell, *risk propensity* denotes how much one prefers a certain payoff versus an uncertain one. To give a simple example, let us imagine a lottery where one can win between 0 and 100 dollars randomly. The expected payoff in this scenario is 50 dollars. A *risk-averse* player will prefer to receive 50 dollars with certainty than playing the lottery. On the contrary, a *risk-loving* player will prefer to play the lottery: the hope for a higher payoff wins over the possibility of getting a lower one. A *risk-neutral* player will be unbiased with respect to which game to play.

In the Hedger Ledger paper, players are considered to be risk-averse or at best risk-neutral. This makes sense, as the main incentive to use Ledger Hedger is exactly hedging against the uncertainty of future gas price fluctuations: 
 - A risk-averse $Buyer$ seeks protection against the possibility of prices rising, resulting in bigger expenses;
 - A risk-averse $Seller$ seeks protection against the possibility of prices falling, resulting in less profit.
So, for both players it makes sense to agree on a fixed gas price beforehand.

To model this assumption explicitly, we relied on [Expected Utility Theory](https://en.wikipedia.org/wiki/Risk_aversion#Utility_of_money): In checking if the game is at equilibrium, the payoffs for both players aren't used as they are (this would be the risk-neutral case). Instead, they are first fed to a  couple of functions called `utilityFunctionBuyer` and `utilityFunctionSeller`, that can be defined in any way the modeller wants. They represent the risk propensity of both $Buyer$ and $Seller$. A concave function will represent a risk-averse player, whereas a convex function will represent a risk-prone player. Supplying the identity functions will result to the standard risk-neutral case.

### Model components