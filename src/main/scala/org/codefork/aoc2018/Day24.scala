package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day24 {

  enum SpecialProperties:
    case Weakness, Immunity

  enum ArmyType:
    case Infection, ImmuneSystem

  enum AttackType:
    case Radiation, Fire, Cold, Slashing, Bludgeoning

  // Units within a group all have the same hit points (amount of damage
  // a unit can take before it is destroyed), attack damage (the amount of damage
  // each unit deals), an attack type, an initiative (higher initiative units
  // attack first and win ties), and sometimes weaknesses or immunities.
  case class Group(armyType: ArmyType, id: String = "", units: Int, hitPoints: Int, attackDamage: Int, attackType: AttackType, initiative: Int, weaknesses: Seq[AttackType], immunities: Seq[AttackType]) {
    def effectivePower = units * attackDamage

    // potential damage that this group can do to an enemy group
    def potentialDamageTo(enemy: Group) = {
      val multiplier = if (enemy.immunities.contains(attackType))
        0
      else if (enemy.weaknesses.contains(attackType))
        2
      else
        1
      effectivePower * multiplier
    }

    def selectTarget(enemyGroups: Seq[Group]): Option[Group] = {
      enemyGroups.sortBy(enemyGroup => {
        // If an attacking group is considering two defending groups to which it would deal
        // equal damage, it chooses to target the defending group with the largest
        // effective power; if there is still a tie, it chooses the defending group with the highest initiative.
        (potentialDamageTo(enemyGroup), enemyGroup.effectivePower, enemyGroup.initiative)
      }).reverse.headOption
    }
  }

  // store ids, not the groups themselves, because state of groups can change
  case class Attack(groupId: String, targetId: String)

  // recursivley do target selection for the passed-in groups, removing target groups from reindeerState
  // as they get added to attacks
  @tailrec
  def targetSelection(groups: Seq[Group], reindeerState: ReindeerState, attacks: Seq[Attack] = Seq.empty): Seq[Attack] = {
    if (groups.nonEmpty) {
      val group = groups.head

      val targetOption = if (group.armyType == ArmyType.Infection)
        group.selectTarget(reindeerState.immuneSystem)
      else
        group.selectTarget(reindeerState.infection)

      val newreindeerState = if (targetOption.nonEmpty)
        reindeerState.copy(groups = reindeerState.groups.filterNot(_.id == targetOption.get.id))
      else
        reindeerState

      val newAttacks = if (targetOption.nonEmpty) {
        //println(s"adding attack ${group}, ${targetOption.get}")
        attacks :+ Attack(group.id, targetOption.get.id)
      } else
        attacks
      targetSelection(groups.tail, newreindeerState, newAttacks)
    } else
      attacks
  }

  case class ReindeerState(groups: Seq[Group] = Seq.empty) {

    def infection = groups.filter(_.armyType == ArmyType.Infection)

    def immuneSystem = groups.filter(_.armyType == ArmyType.ImmuneSystem)

    // boot the attack damage of immune system groups by a given value
    def immuneBoost(boost: Int): ReindeerState =
      copy(groups = groups.map(group => {
        if (group.armyType == ArmyType.ImmuneSystem)
          group.copy(attackDamage = group.attackDamage + boost)
        else
          group
      }))

    def getGroupById(id: String) =
      groups.find(_.id == id).get

    // replace the group (either infection or immune system) with newGroup, by id
    def replaceGroup(newGroup: Group) =
      copy(groups = groups.map(group => if group.id == newGroup.id then newGroup else group))

    // execute a single attack, returning an updated reindeerState
    def executeAttack(attack: Attack): ReindeerState = {
      val group = getGroupById(attack.groupId)
      val target = getGroupById(attack.targetId)
      val damage = group.potentialDamageTo(target)
      //println(s"damage=${damage}")
      val unitsToKill = damage / target.hitPoints
      val unitsKilled = if unitsToKill > target.units then target.units else unitsToKill
      //println(s"${group} attacks defending ${target}, killing ${unitsKilled} units")
      val newTarget = target.copy(units = target.units - unitsKilled)
      replaceGroup(newTarget)
    }

    // do a single "fight" (one round)
    def fight(): ReindeerState = {
      val groupsSorted = groups.sortBy(group => (group.effectivePower, group.initiative)).reverse
      val attacks = targetSelection(groupsSorted, this)

      // Groups attack in decreasing order of initiative, regardless of whether
      // they are part of the infection or the immune system.
      val attacksOrdered = attacks.sortBy(attack => getGroupById(attack.groupId).initiative).reverse
      val result = attacksOrdered.foldLeft(this)((acc, attack) => {
        acc.executeAttack(attack)
      })
      // remove groups whose units have all been killed
      result.copy(groups = result.groups.filter(_.units > 0))
    }

    def unitsOfWinningArmy: Int =
      groups.map(_.units).sum

    def winningArmy: ArmyType =
      if (infection.isEmpty)
        ArmyType.ImmuneSystem
      else if (immuneSystem.isEmpty)
        ArmyType.Infection
      else
        throw new Exception("no one won yet")

    def display() = {
      println("Immune System:")
      immuneSystem.foreach(group => {
        //println(s"${group.id} has ${group.units} units")
        println(s"${group}")
      })
      println("Infection:")
      infection.foreach(group => {
        //println(s"${group.id} has ${group.units} units")
        println(s"${group}")
      })
    }
  }

  @tailrec
  def fightUntilWin(reindeerState: ReindeerState, i: Int = 0): ReindeerState = {
    reindeerState.display()
    if (reindeerState.infection.isEmpty || reindeerState.immuneSystem.isEmpty) {
      reindeerState
    } else {
      fightUntilWin(reindeerState.fight(), i + 1)
    }
  }

  case class Result(reindeerState: ReindeerState, deadlocked: Boolean)

  @tailrec
  def fightUntilWinWithDeadlockDetection(reindeerState: ReindeerState, i: Int = 0): Result = {
    //reindeerState.display()
    if (reindeerState.infection.isEmpty || reindeerState.immuneSystem.isEmpty) {
      Result(reindeerState, false)
    } else {
      val newReindeerState = reindeerState.fight()
      if (newReindeerState == reindeerState) {
        //println("deadlock detected, exiting")
        Result(newReindeerState, true)
      } else {
        fightUntilWinWithDeadlockDetection(newReindeerState, i + 1)
      }
    }
  }

  case class ParseState(reindeerState: ReindeerState = ReindeerState(), currentArmyType: ArmyType = ArmyType.Infection)

  def parseInput(path: String): ReindeerState = {
    val unitsRe = raw"(\d+) units each with (\d+) hit points".r
    val attackRe = raw"with an attack that does (\d+) (\w+) damage at initiative (\d+)".r
    val parensRe = raw"\((.+)\)".r
    val url = getClass.getResource(path)
    val s = Source.fromURL(url)
    val result = s.getLines()
      .foldLeft(ParseState()) { (acc, line_raw) => {
        val line = line_raw.strip.replace("\n", "")
        //println(line)
        if (line == "") {
          acc
        } else if (line == "Immune System:") {
          acc.copy(currentArmyType = ArmyType.ImmuneSystem)
        } else if (line == "Infection:") {
          acc.copy(currentArmyType = ArmyType.Infection)
        } else {
          val unitMatches = unitsRe.findAllMatchIn(line).toList
          val units = unitMatches.head.group(1).toInt
          val hitPoints = unitMatches.head.group(2).toInt

          val attachMatches = attackRe.findAllMatchIn(line).toList
          val attackDamage = attachMatches.head.group(1).toInt
          val attackType = AttackType.valueOf(attachMatches.head.group(2).capitalize)
          val initiative = attachMatches.head.group(3).toInt

          val parensMatches = parensRe.findAllMatchIn(line).toList
          val weaknessesAndImmunities = if (parensMatches.size > 0) {
            val parensContent = parensMatches.head.group(1)
            val map = parensContent.split(";").map(_.strip).foldLeft(Map.empty[SpecialProperties, Seq[AttackType]]) { (acc, fragment) => {
              //println(fragment)
              if (fragment.startsWith("weak to")) {
                acc.updated(SpecialProperties.Weakness, fragment.replace("weak to ", "").split(",").map(attackString => AttackType.valueOf(attackString.strip.capitalize)))
              } else if (fragment.startsWith("immune to")) {
                acc.updated(SpecialProperties.Immunity, fragment.replace("immune to ", "").split(",").map(attackString => AttackType.valueOf(attackString.strip.capitalize)))
              } else {
                acc
              }
            }
            }
            map
          } else {
            Map.empty
          }

          val group = Group(armyType = acc.currentArmyType, units = units, hitPoints = hitPoints, attackDamage = attackDamage, attackType = attackType,
            initiative = initiative, weaknesses = weaknessesAndImmunities.getOrElse(SpecialProperties.Weakness, Seq.empty),
            immunities = weaknessesAndImmunities.getOrElse(SpecialProperties.Immunity, Seq.empty))

          acc.copy(reindeerState = acc.reindeerState.copy(groups = acc.reindeerState.groups :+ group))
        }
      }
      }
    val reindeerState = result.reindeerState
    // assign IDs
    reindeerState.copy(
      groups = reindeerState.immuneSystem.zipWithIndex.map { case (group, i) => group.copy(id = s"immune${i + 1}") } ++
        reindeerState.infection.zipWithIndex.map { case (group, i) => group.copy(id = s"infection${i + 1}") }
    )
  }

}
