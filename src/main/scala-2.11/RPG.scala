import java.util.Random

object RPG extends App{
  val random = new Random
  val monsterCount = 5
  val hero = new Hero(300, 30, 50)
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(120), random.nextInt(120), false, 10)

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
    val input = scala.io.StdIn.readLine("【選択】: 攻撃[1] or 逃走[0] or 防御[d]> ")
    var monsterDefencePattern = false

    if (RPG.random.nextInt(3) == 1) {
      monsterDefencePattern = true
    }

    if (!monsterDefencePattern) {
      if (input == "1") { // 攻撃する
        hero.attack(monster)
        println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
      } else if (input == "0") { // 逃走する
        if (hero.escape(monster)) {
          println("あなたは、モンスターから逃走に成功した。")
        } else {
          println(s"あなたは、モンスターから逃走に失敗し、${monster.attackDamage}のダメージを受けた。")
        }
      } else {
        hero.defence(monster)
        println(s"あなたは${monster.attackDamage}のダメージを受けたが、${hero.defencePoint}のダメージを受け流した")
      }
    } else {
      if (input == "1") {
        hero.attackDefence(monster)
        println(s"あなたは${hero.attackDamage}のダメージを与えたが、モンスターは${monster.defencePoint}のダメージを受け流した")
      } else if (input == "0") {
        if (hero.escapeDefence(monster)) {
          println("あなたは、モンスターから逃走に成功した。")
        } else {
          println(s"あなたは、モンスターから逃走に失敗した。モンスターは身を守っている。")
        }
      } else {
        hero.defenceDefence(monster)
        println(s"どちらも身を守っている")
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

abstract class Creature(var hitPoint: Int, var attackDamage: Int, var defencePoint: Int) {
  def isAlive(): Boolean = this.hitPoint > 0
}


class Hero(_hitPoint: Int, _attackDamage: Int, _defencePoint: Int) extends Creature(_hitPoint, _attackDamage, _defencePoint) {


  def attack(monster: Monster): Unit = {
    monster.hitPoint = monster.hitPoint - this.attackDamage
    this.hitPoint = this.hitPoint - monster.attackDamage

  }

  def attackDefence(monster: Monster): Unit = {
    var attackResult = this.attackDamage - monster.defencePoint
    if (attackResult < 0) {
      attackResult = 0
    }
    monster.hitPoint = monster.hitPoint - attackResult
    this.hitPoint = this.hitPoint

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

  def escapeDefence(monster: Monster): Boolean = {
    val isEscaped = RPG.random.nextInt(2) == 1
    if (!isEscaped) {
      this.hitPoint = this.hitPoint
    } else {
      monster.isAwayFromHero = true
    }
    isEscaped
  }



  def defence(monster: Monster): Unit = {
    var defenceDamage = monster.attackDamage - this.defencePoint
    if (defenceDamage < 0) {
      defenceDamage = 0
    }
    this.hitPoint = this.hitPoint - defenceDamage
  }

  def defenceDefence(monster: Monster): Unit = {
    this.hitPoint = this.hitPoint
  }

  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage})"

}

class Monster(_hitPoint: Int, _attackDamage: Int, var isAwayFromHero: Boolean, _defencePoint: Int)
  extends Creature(_hitPoint, _attackDamage, _defencePoint) {

  override def toString = s"Monster(体力:${hitPoint}, 攻撃力:${attackDamage}, ヒーローから離れている:${isAwayFromHero})"

}
