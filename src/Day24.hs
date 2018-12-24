module Day24
    ( day24
    , day24b
    ) where

import Lib
import Text.ParserCombinators.ReadP
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Array ((!))
import Data.Foldable
import qualified Data.List.Split as Split
import Data.Bits
import Data.Char
import Data.Either


{-
After a weird buzzing noise, you appear back at the man's cottage. He seems relieved to see his friend, but quickly
notices that the little reindeer caught some kind of cold while out exploring.

The portly man explains that this reindeer's immune system isn't similar to regular reindeer immune systems:

The immune system and the infection each have an army made up of several groups; each group consists of one or more
identical units. The armies repeatedly fight until only one army has units remaining.

Units within a group all have the same hit points (amount of damage a unit can take before it is destroyed), attack
damage (the amount of damage each unit deals), an attack type, an initiative (higher initiative units attack first
and win ties), and sometimes weaknesses or immunities. Here is an example group:

18 units each with 729 hit points (weak to fire; immune to cold, slashing)
 with an attack that does 8 radiation damage at initiative 10

Each group also has an effective power: the number of units in that group multiplied by their attack damage. The
above group has an effective power of 18 * 8 = 144. Groups never have zero or negative units; instead, the group
is removed from combat.

Each fight consists of two phases: target selection and attacking.

During the target selection phase, each group attempts to choose one target. In decreasing order of effective power,
groups choose their targets; in a tie, the group with the higher initiative chooses first. The attacking group
chooses to target the group in the enemy army to which it would deal the most damage (after accounting for
weaknesses and immunities, but not accounting for whether the defending group has enough units to actually receive
all of that damage).

If an attacking group is considering two defending groups to which it would deal equal damage, it chooses to target
the defending group with the largest effective power; if there is still a tie, it chooses the defending group with
the highest initiative. If it cannot deal any defending groups damage, it does not choose a target. Defending groups
can only be chosen as a target by one attacking group.

At the end of the target selection phase, each group has selected zero or one groups to attack, and each group is
being attacked by zero or one groups.

During the attacking phase, each group deals damage to the target it selected, if any. Groups attack in decreasing
order of initiative, regardless of whether they are part of the infection or the immune system. (If a group
contains no units, it cannot attack.)

The damage an attacking group deals to a defending group depends on the attacking group's attack type and the
defending group's immunities and weaknesses. By default, an attacking group would deal damage equal to its effective
power to the defending group. However, if the defending group is immune to the attacking group's attack type, the
defending group instead takes no damage; if the defending group is weak to the attacking group's attack type, the
defending group instead takes double damage.

The defending group only loses whole units from damage; damage is always dealt in such a way that it kills the most
units possible, and any remaining damage to a unit that does not immediately kill it is ignored. For example, if a
defending group contains 10 units with 10 hit points each and receives 75 damage, it loses exactly 7 units and is
left with 3 units at full health.

After the fight is over, if both armies still contain units, a new fight begins; combat only ends once one army has
lost all of its units.

For example, consider the following armies:

Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with
 an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
 slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack
 that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire,
 cold) with an attack that does 12 slashing damage at initiative 4

If these armies were to enter combat, the following fights, including details during the target selection and
attacking phases, would take place:

Immune System:
Group 1 contains 17 units
Group 2 contains 989 units
Infection:
Group 1 contains 801 units
Group 2 contains 4485 units

Infection group 1 would deal defending group 1 185832 damage
Infection group 1 would deal defending group 2 185832 damage
Infection group 2 would deal defending group 2 107640 damage
Immune System group 1 would deal defending group 1 76619 damage
Immune System group 1 would deal defending group 2 153238 damage
Immune System group 2 would deal defending group 1 24725 damage

Infection group 2 attacks defending group 2, killing 84 units
Immune System group 2 attacks defending group 1, killing 4 units
Immune System group 1 attacks defending group 2, killing 51 units
Infection group 1 attacks defending group 1, killing 17 units

Immune System:
Group 2 contains 905 units
Infection:
Group 1 contains 797 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 184904 damage
Immune System group 2 would deal defending group 1 22625 damage
Immune System group 2 would deal defending group 2 22625 damage

Immune System group 2 attacks defending group 1, killing 4 units
Infection group 1 attacks defending group 2, killing 144 units

Immune System:
Group 2 contains 761 units
Infection:
Group 1 contains 793 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 183976 damage
Immune System group 2 would deal defending group 1 19025 damage
Immune System group 2 would deal defending group 2 19025 damage

Immune System group 2 attacks defending group 1, killing 4 units
Infection group 1 attacks defending group 2, killing 143 units

Immune System:
Group 2 contains 618 units
Infection:
Group 1 contains 789 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 183048 damage
Immune System group 2 would deal defending group 1 15450 damage
Immune System group 2 would deal defending group 2 15450 damage

Immune System group 2 attacks defending group 1, killing 3 units
Infection group 1 attacks defending group 2, killing 143 units

Immune System:
Group 2 contains 475 units
Infection:
Group 1 contains 786 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 182352 damage
Immune System group 2 would deal defending group 1 11875 damage
Immune System group 2 would deal defending group 2 11875 damage

Immune System group 2 attacks defending group 1, killing 2 units
Infection group 1 attacks defending group 2, killing 142 units

Immune System:
Group 2 contains 333 units
Infection:
Group 1 contains 784 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 181888 damage
Immune System group 2 would deal defending group 1 8325 damage
Immune System group 2 would deal defending group 2 8325 damage

Immune System group 2 attacks defending group 1, killing 1 unit
Infection group 1 attacks defending group 2, killing 142 units

Immune System:
Group 2 contains 191 units
Infection:
Group 1 contains 783 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 181656 damage
Immune System group 2 would deal defending group 1 4775 damage
Immune System group 2 would deal defending group 2 4775 damage

Immune System group 2 attacks defending group 1, killing 1 unit
Infection group 1 attacks defending group 2, killing 142 units

Immune System:
Group 2 contains 49 units
Infection:
Group 1 contains 782 units
Group 2 contains 4434 units

Infection group 1 would deal defending group 2 181424 damage
Immune System group 2 would deal defending group 1 1225 damage
Immune System group 2 would deal defending group 2 1225 damage

Immune System group 2 attacks defending group 1, killing 0 units
Infection group 1 attacks defending group 2, killing 49 units

Immune System:
No groups remain.
Infection:
Group 1 contains 782 units
Group 2 contains 4434 units

In the example above, the winning army ends up with 782 + 4434 = 5216 units.

You scan the reindeer's condition (your puzzle input); the white-bearded man looks nervous. As it stands now, how many units would the winning army have?
-}


parse :: [String] -> ([Group], [Group])
parse ls =
  let systems = Split.splitOn [""] ls
      immune = catMaybes $ map (parseWith troopParser) $ tail $ systems !! 0
      infection = catMaybes $ map (parseWith troopParser) $ tail $ systems !! 1
  in  (immune, infection)

data Attack = Cold | Radiation | Slashing | Bludgeoning | Fire
  deriving (Show, Eq)

data Group = Group { number :: Integer
                   , hp :: Integer
                   , weak :: [Attack]
                   , immune :: [Attack]
                   , attack :: Integer
                   , attackType :: Attack
                   , initiative :: Integer
                   }
  deriving (Show, Eq)

troopParser = do
  number <- intParser
  string " units each with "
  hp <- intParser
  string " hit points"
  (weak, immune) <- parenParser
  string " with an attack that does "
  attack <- intParser
  char ' '
  attackType <- attackParser
  string " damage at initiative "
  initiative <- intParser
  eof
  return Group { number=number
               , hp=hp
               , weak=weak
               , immune=immune
               , attack=attack
               , attackType=attackType
               , initiative=initiative
               }

attackParser = do
  (do string "cold"; return Cold) <++
   (do string "slashing"; return Slashing) <++
   (do string "bludgeoning"; return Bludgeoning) <++
   (do string "fire"; return Fire) <++
   (do string "radiation"; return Radiation)

parenParser = do
  (do string " ("
      weak <- dangerParser "weak"
      string "; "
      immune <- dangerParser "immune"
      string ")"
      return (weak, immune)) <++
    (do string " ("
        immune <- dangerParser "immune"
        string "; "
        weak <- dangerParser "weak"
        string ")"
        return (weak, immune)) <++
    (do string " ("
        weak <- dangerParser "weak"
        string ")"
        return (weak, [])) <++
    (do string " ("
        immune <- dangerParser "immune"
        string ")"
        return ([], immune)) <++
    (do return ([], []))

dangerParser prefix = do
  string prefix
  string " to "
  types <- sepBy1 attackParser (string ", ")
  return types

{-
2667 units each with 9631 hit points (immune to cold; weak to radiation) with an attack that does 33 radiation damage at initiative 3
-}

d0 = lines "\
  \Immune System:\n\
  \17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n\
  \989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\
  \\n\
  \Infection:\n\
  \801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n\
  \4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4\n\
  \"

day24 :: [String] -> IO ()
day24 ls = do
  let (imm, inf) = parse ls
  putStrLn "Immune System:"
  printSystem imm
  putStrLn ""
  putStrLn "Infection:"
  printSystem inf
  putStrLn ""

  (imm', inf') <- runCombat imm inf
  putStrLn ""
  putStrLn "Immune System:"
  printSystem imm'
  putStrLn ""
  putStrLn "Infection:"
  printSystem inf'

  putStrLn ""
  putStrLn $ "Total units left = " ++ (show $ (sumUnits imm') + (sumUnits inf'))

printSystem sys = do
  forM_ sys $ \i -> do
    putStrLn $ (show $ number i) ++ " units each with " ++
               (show $ hp i) ++ " hit points " ++
               "(weak to " ++ (show $ weak i) ++
               "; immune to " ++ (show $ immune i) ++ ") " ++
               "with an attack that does " ++
               (show $ attack i) ++ " " ++ (show $ attackType i) ++ " damage " ++
               "at initiative " ++ (show $ initiative i)

sumUnits = sum . map number

runCombat [] inf = return ([], inf)
runCombat imm [] = return (imm, [])
runCombat imm inf = do
      -- get a list by descending effectivePower, then initiative
  let imm' = zip [0..] $ List.sortBy (\i1 i2 ->
                                compare (effectivePower i2, initiative i2)
                                        (effectivePower i1, initiative i1)) imm
      inf' = zip [0..] $ List.sortBy (\i1 i2 ->
                                compare (effectivePower i2, initiative i2)
                                        (effectivePower i1, initiative i1)) inf

      -- get a map of {imm# -> inf#} attacks
      immAttacks = selectAttacks imm' inf'
      -- and a list of {inf'# -> imm'#} attacks
      infAttacks = selectAttacks inf' imm'

      -- combine into a battle-plan
      plan = Map.union (Map.mapKeys Left immAttacks)
                       (Map.mapKeys Right infAttacks)

  putStrLn "----------"
  printSystem imm
  putStrLn ""
  printSystem inf
  putStrLn ""
  putStrLn $ show $ plan
  putStrLn $ show $ imm
  putStrLn $ show $ inf

  if Map.null plan
  then return (imm, inf)
  else do
    let
        -- run the attacks in descending order of initiative
        attackers = List.sortBy (\u1 u2 -> compare (initiative $ regardless u2)
                                                 (initiative $ regardless u1)) $ (map Left imm') ++ (map Right inf')
        attackerNumbers = map (\u -> case u of
                                        Left (m,_) -> Left m
                                        Right (m,_) -> Right m) attackers

        (immBefore, infBefore) = (Map.fromList imm', Map.fromList inf')
    (immAfter, infAfter) <- runAttacks (Map.fromList imm') (Map.fromList inf') attackerNumbers plan

    if immAfter == immBefore && infAfter == infBefore then return (imm, inf)
    else runCombat (stripIndex $ Map.toList immAfter) (stripIndex $ Map.toList infAfter)

  where
    regardless (Left (_,u)) = u
    regardless (Right (_,u)) = u
    stripIndex = map (\(_,u) -> u)

effectivePower g = number g * attack g

selectAttacks [] _ = Map.empty
selectAttacks _ [] = Map.empty
selectAttacks ((m,attacker):rest) defenders =
  let ((n,d):ds) = List.sortBy (\(_,d1) (_,d2) ->
                                compare (damageDealt attacker d2,
                                         effectivePower d2,
                                         initiative d2)
                                        (damageDealt attacker d1,
                                         effectivePower d1,
                                         initiative d1)) defenders
  in  if damageDealt attacker d == 0
      then selectAttacks rest defenders
      else Map.insert m n $ selectAttacks rest ds

damageDealt from to =
  let rawDamage = effectivePower from
  in  if (attackType from) `elem` (immune to) then 0
      else if (attackType from) `elem` (weak to) then rawDamage * 2
      else rawDamage

runAttacks :: Map.Map Integer Group  ->  -- indexed immune units
              Map.Map Integer Group ->  -- indexed infection units
              [Either Integer Integer] -> -- the next attacker
              Map.Map (Either Integer Integer) Integer -> -- the general battle plan
              IO (Map.Map Integer Group, Map.Map Integer Group)  -- the survivors
runAttacks imm inf [] _ = return (imm, inf)
runAttacks imm inf (unitNumber:units) plan = do
  case unitNumber of
    Left m ->
      let immUnit = imm Map.!? m
      in
        case immUnit of
          Just immUnit ->
            -- find this attack in the plan
            case plan Map.!? unitNumber of
            Nothing -> runAttacks imm inf units plan
            Just n -> do
              let inf' = attack immUnit n inf
              runAttacks imm inf' units plan
          Nothing -> runAttacks imm inf units plan
    Right m ->
      let infUnit = inf Map.!? m
      in
        case infUnit of
          Just infUnit ->
            -- find this attack in the plan
            case plan Map.!? unitNumber of
            Nothing -> runAttacks imm inf units plan
            Just n -> do
              let imm' = attack infUnit n imm
              runAttacks imm' inf units plan
          Nothing -> runAttacks imm inf units plan

  where
    attack from n defenders =
      case defenders Map.!? n of
      Nothing -> defenders
      Just to ->
        let unitsLost = damageDealt from to `div` hp to
            unitsRemaining = (number to - unitsLost)
        in  if unitsRemaining <= 0
            then Map.delete n defenders
            else Map.insert n to { number=unitsRemaining } defenders
{-
The first half of this puzzle is complete! It provides one gold star: *
--- Part Two ---

Things aren't looking good for the reindeer. The man asks whether more milk and cookies would help you think.

If only you could give the reindeer's immune system a boost, you might be able to change the outcome of the combat.

A boost is an integer increase in immune system units' attack damage. For example, if you were to boost the above
example's immune system's units by 1570, the armies would instead look like this:

Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with
 an attack that does 6077 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
 slashing) with an attack that does 1595 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack
 that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire,
 cold) with an attack that does 12 slashing damage at initiative 4

With this boost, the combat proceeds differently:

Immune System:
Group 2 contains 989 units
Group 1 contains 17 units
Infection:
Group 1 contains 801 units
Group 2 contains 4485 units

Infection group 1 would deal defending group 2 185832 damage
Infection group 1 would deal defending group 1 185832 damage
Infection group 2 would deal defending group 1 53820 damage
Immune System group 2 would deal defending group 1 1577455 damage
Immune System group 2 would deal defending group 2 1577455 damage
Immune System group 1 would deal defending group 2 206618 damage

Infection group 2 attacks defending group 1, killing 9 units
Immune System group 2 attacks defending group 1, killing 335 units
Immune System group 1 attacks defending group 2, killing 32 units
Infection group 1 attacks defending group 2, killing 84 units

Immune System:
Group 2 contains 905 units
Group 1 contains 8 units
Infection:
Group 1 contains 466 units
Group 2 contains 4453 units

Infection group 1 would deal defending group 2 108112 damage
Infection group 1 would deal defending group 1 108112 damage
Infection group 2 would deal defending group 1 53436 damage
Immune System group 2 would deal defending group 1 1443475 damage
Immune System group 2 would deal defending group 2 1443475 damage
Immune System group 1 would deal defending group 2 97232 damage

Infection group 2 attacks defending group 1, killing 8 units
Immune System group 2 attacks defending group 1, killing 306 units
Infection group 1 attacks defending group 2, killing 29 units

Immune System:
Group 2 contains 876 units
Infection:
Group 2 contains 4453 units
Group 1 contains 160 units

Infection group 2 would deal defending group 2 106872 damage
Immune System group 2 would deal defending group 2 1397220 damage
Immune System group 2 would deal defending group 1 1397220 damage

Infection group 2 attacks defending group 2, killing 83 units
Immune System group 2 attacks defending group 2, killing 427 units

After a few fights...

Immune System:
Group 2 contains 64 units
Infection:
Group 2 contains 214 units
Group 1 contains 19 units

Infection group 2 would deal defending group 2 5136 damage
Immune System group 2 would deal defending group 2 102080 damage
Immune System group 2 would deal defending group 1 102080 damage

Infection group 2 attacks defending group 2, killing 4 units
Immune System group 2 attacks defending group 2, killing 32 units

Immune System:
Group 2 contains 60 units
Infection:
Group 1 contains 19 units
Group 2 contains 182 units

Infection group 1 would deal defending group 2 4408 damage
Immune System group 2 would deal defending group 1 95700 damage
Immune System group 2 would deal defending group 2 95700 damage

Immune System group 2 attacks defending group 1, killing 19 units

Immune System:
Group 2 contains 60 units
Infection:
Group 2 contains 182 units

Infection group 2 would deal defending group 2 4368 damage
Immune System group 2 would deal defending group 2 95700 damage

Infection group 2 attacks defending group 2, killing 3 units
Immune System group 2 attacks defending group 2, killing 30 units

After a few more fights...

Immune System:
Group 2 contains 51 units
Infection:
Group 2 contains 40 units

Infection group 2 would deal defending group 2 960 damage
Immune System group 2 would deal defending group 2 81345 damage

Infection group 2 attacks defending group 2, killing 0 units
Immune System group 2 attacks defending group 2, killing 27 units

Immune System:
Group 2 contains 51 units
Infection:
Group 2 contains 13 units

Infection group 2 would deal defending group 2 312 damage
Immune System group 2 would deal defending group 2 81345 damage

Infection group 2 attacks defending group 2, killing 0 units
Immune System group 2 attacks defending group 2, killing 13 units

Immune System:
Group 2 contains 51 units
Infection:
No groups remain.

This boost would allow the immune system's armies to win! It would be left with 51 units.

You don't even know how you could boost the reindeer's immune system or what effect it might have, so you need to be
cautious and find the smallest boost that would allow the immune system to win.

How many units does the immune system have left after getting the smallest boost it needs to win?
-}


day24b :: [String] -> IO ()
day24b ls = do
  let (imm, inf) = parse ls
  putStrLn "Immune System:"
  printSystem imm
  putStrLn ""
  putStrLn "Infection:"
  printSystem inf
  putStrLn ""

  -- (boost, imm', inf') <- findBoost [1..] imm inf
  (boost, imm', inf') <- findBoost [1..] imm inf

  putStrLn ""
  putStrLn $ "After boost of " ++ (show boost)
  putStrLn ""
  putStrLn "Immune System:"
  printSystem imm'
  putStrLn ""
  putStrLn "Infection:"
  printSystem inf'

  putStrLn ""
  putStrLn $ "Total units left = " ++ (show $ (sumUnits imm') + (sumUnits inf'))


findBoost (b:bs) imm inf = do
  (m,n) <- runCombatWithBoost b imm inf
  putStrLn $ "Trying a boost of " ++ (show b)
  if length m /= 0 && length n == 0
  then return (b, m, n)
  else findBoost bs imm inf


runCombatWithBoost b imm inf = do
  let imm' = map (\i -> i { attack=b + attack i }) imm
  runCombat imm' inf