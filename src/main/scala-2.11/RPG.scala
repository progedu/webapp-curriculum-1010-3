import RPG.random

import java.util.Random

object RPG extends App {
  val random = new Random
  val monsterCount = 5
  val hero = new Hero(300, 30)
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(120), random.nextInt(120), false)

  println(
    s"""あなたは冒険中の ${hero} であり、
       |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けねばならない。
       |【ルール】:
       |1を入力してEnterキーを押すと攻撃、それ以外を入力すると逃走となる。
       |逃走成功確率は50%。逃走に失敗した場合はダメージをうける。
       |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。
       |またモンスターを倒した場合、武器を奪いその攻撃力を得ることできる。
       |---------------------------------------------
       |未知のモンスターがあらわれた。""".stripMargin)

  while (!monsters.isEmpty) {
    val monster = monsters.head
    val input = scala.io.StdIn.readLine("【選択】: 攻撃[1] or 魔法[2] or 逃走[0] > ")

    if (input == "1") { // 攻撃する
      hero.attack(monster)
      println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
    } else if (input == "2") { // 魔法を使う
      hero.magic(monster)
    }
    else { // 逃走する
      if(hero.escape(monster)) {
        println("あなたは、モンスターから逃走に成功した。")
      } else {
        println(s"あなたは、モンスターから逃走に失敗し、${monster.attackDamage}のダメージを受けた。")
      }
    }
    println(s"【現在の状態】: ${hero}, ${monster}")

    if (!hero.isAlive) { // Hero が死んでいるかどうか
      println(
        """---------------------------------------------
          |【ゲームオーバー】: あなたは無残にも殺されてしまった。 """.stripMargin)
      System.exit(0)
    } else if (!monster.isAlive || monster.isAwayFromHero) { // Monster いないかどうか
      if (!monster.isAwayFromHero) { // 倒した場合
        println("モンスターは倒れた。そしてあなたは、モンスターの武器を奪った。")
        if (monster.attackDamage > hero.attackDamage) hero.attackDamage = monster.attackDamage
      }
      monsters = monsters.tail
      println(s"残りのモンスターは${monsters.length}匹となった。")
      if (monsters.length > 0) {
        println(
          """---------------------------------------------
            |新たな未知のモンスターがあらわれた。 """.stripMargin)
      }
    }
  }

  println(
    s"""---------------------------------------------
       |【ゲームクリア】: あなたは困難を乗り越えた。新たな冒険に祝福を。
       |【結果】: ${hero}""".stripMargin
  )
  System.exit(0)

}

abstract class Creature(var hitPoint: Int, var attackDamage: Int) {
  def isAlive(): Boolean = this.hitPoint > 0
}

class Hero(_hitPoint: Int, _attackDamage: Int) extends Creature(_hitPoint, _attackDamage) {

  def attack(monster: Monster): Unit = {
    monster.hitPoint = monster.hitPoint - this.attackDamage
    this.hitPoint = this.hitPoint - monster.attackDamage
  }

  def escape(monster: Monster): Boolean = {
    val isEscaped = RPG.random.nextInt(2) == 1
    if (!isEscaped) {
      this.hitPoint = this.hitPoint - monster.attackDamage
    } else {
      monster.isAwayFromHero = true
    }
    isEscaped
  }

  def magic(monster: Monster): Unit = {
    val magicDamage = (this.attackDamage * RPG.random.nextDouble).toInt
    val isStunned = RPG.random.nextInt(3) > 0
    monster.hitPoint = monster.hitPoint - magicDamage
    if (isStunned) {
      println(s"あなたは${magicDamage}のダメージを与えた。")
      println("モンスターは硬直して動けない。")
    } else {
      this.hitPoint = this.hitPoint - monster.attackDamage
      println(s"あなたは${magicDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
    }
  }

  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage})"

}

class Monster(_hitPoint: Int, _attackDamage: Int, var isAwayFromHero: Boolean)
  extends  Creature(_hitPoint, _attackDamage) {

  override def toString = s"Monster(体力:${hitPoint}, 攻撃力:${attackDamage}, ヒーローから離れている:${isAwayFromHero})"

}
