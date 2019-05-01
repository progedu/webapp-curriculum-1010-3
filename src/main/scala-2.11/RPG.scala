import java.util.Random

object RPG extends App {
  val random = new Random
  val monsterCount = 5
  val hero = new Hero(200, 30, 10)
  var monsters = for (i <- 1 to monsterCount)
    yield new Monster(random.nextInt(120), random.nextInt(120), random.nextInt(15), false)

  println(
    s"""あなたは冒険中の ${hero} であり、
        |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けねばならない。
        |【ルール】:
        |1を入力してEnterキーを押すと攻撃、2を入力すると防御、それ以外を入力すると逃走となる。
        |逃走成功確率は50%。逃走に失敗した場合はダメージをうける。
        |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。
        |またモンスターを倒した場合、武器を奪いその攻撃力を得ることできる。
        |---------------------------------------------
        |未知のモンスターがあらわれた。""".stripMargin)

  while (!monsters.isEmpty) {
    val monster = monsters.head
    val input = scala.io.StdIn.readLine("【選択】: 攻撃[1] or 逃走[0] > ")

    if (input == "1") { // 攻撃する
      if (hero.speed > monster.speed) {
        println("あなたの先制攻撃！")
      } else {
        println("モンスターの先制攻撃！")
      }
      if (hero.attack(monster)) {
        println(s"***** 会心の一撃！！ *****")
        println(s"あなたは${hero.attackDamage * 2}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
      } else {
        println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
      }
    } else { // 逃走する
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
      if(!monster.isAwayFromHero) { // 倒した場合
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

abstract class Creature(var hitPoint: Int, var attackDamage: Int, var speed: Int) {
  def isAlive(): Boolean = this.hitPoint > 0
}

class Hero(_hitPoint: Int, _attackDamage: Int, _speed: Int) extends Creature(_hitPoint, _attackDamage, _speed) {

  def attack(monster: Monster): Boolean = {
    val isCritical = RPG.random.nextInt(4) == 1 // 1/4で会心の一撃

    if (this.speed > monster.speed) {
      monster.hitPoint = monster.hitPoint - (this.attackDamage * (if (isCritical) 2 else 1))
      if (monster.hitPoint > 0) {
        this.hitPoint = this.hitPoint - monster.attackDamage
      }
    } else {
      this.hitPoint = this.hitPoint - monster.attackDamage
      if (this.hitPoint > 0) {
        monster.hitPoint = monster.hitPoint - (this.attackDamage * (if (isCritical) 2 else 1))
      }
    }
    isCritical
  }

  def escape(monster: Monster): Boolean = {
    val isEscape = RPG.random.nextInt(2) == 1
    if (!isEscape) {
      this.hitPoint = this.hitPoint - monster.attackDamage
    } else {
      monster.isAwayFromHero = true
    }
    isEscape
  }

  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage}、素早さ:${speed})"

}

class Monster(_hitPoint: Int, _attackDamage: Int, var _speed: Int, var isAwayFromHero: Boolean)
  extends  Creature(_hitPoint, _attackDamage, _speed) {

  override def toString = s"Monster(体力: ${hitPoint}, 攻撃力:${attackDamage}、素早さ:${speed}, ヒーローから離れている:${isAwayFromHero})"

}