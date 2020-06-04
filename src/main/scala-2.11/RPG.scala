import java.util.Random
object RPG extends App{
  val random = new Random
  val monsterCount = 5
  val hero = new Hero(250, 30, evasive = false)
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(120), random.nextInt(120), false)

  println(
    s"""あなたは冒険中の${hero}であり、
       |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けねばならない。
       |【ルール】:
       |1を押してEnterキーを押すと攻撃し、
       |2の場合、防御してダメージを1/3に抑える、
       |3の場合、カウンター状態となり1/4の確率で成功する。
       |それ以外の入力は逃走となる。
       |逃走成功率は50%。逃走に失敗した場合はダメージを受ける。
       |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。
       |またモンスターを倒した場合、武器を奪いその攻撃力を得ることができる。
       |----------------------------------------
       |未知のモンスターが現れた。""".stripMargin)

    while(!monsters.isEmpty) {
      val monster = monsters.head
      val input = scala.io.StdIn.readLine(" 【選択】: 攻撃[1] 防御[2] カウンター[3] 逃走[0] > ")

      if(input == "1") { //攻撃する
        hero.attack(monster)
        if(monster.isAlive) {
          println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
          } else {
          println(s"あなたは${hero.attackDamage}ダメージを与えた。")
        }
      } else if (input == "2") {
        hero.guard(monster)
        println(s"あなたは、モンスターからの攻撃を防御し、${monster.attackDamage / 3}のダメージを受けた。")
      } else if (input == "3") {
        hero.counterAttack(monster)
        if(hero.evasive) {
          println(s"あなたはカウンターに成功し、${hero.attackDamage + 50}のダメージを与えた。")
        } else {
          println(s"あなたは回避に失敗し、${monster.attackDamage + 30}のダメージを受けたが、 モンスターに、${hero.attackDamage + 25}のダメージを与えた。")
        }
      } else {
        if(hero.escape(monster)) {
          println("あなたは、モンスターから逃走に成功した。")
        } else {
          println(s"あなたは、モンスターから逃走に失敗し、${monster.attackDamage}のダメージを受けた。")
        }
      }
      println(s"【現在の状態】: ${hero}, ${monster}")

      if(!hero.isAlive) { // Hero が死んでいるかどうか
        println(
          """
            |【ゲームオーバー】: あなたは無残にも殺されてしまった。""".stripMargin)
        System.exit(0)
      } else if (!monster.isAlive || monster.isAwayFromHero) { // Monster がいないかどうか
          if(!monster.isAwayFromHero) { // 倒した場合
            println("モンスターは倒れた。そしてあなたは、モンスターの武器を奪った。")
            if (monster.attackDamage > hero.attackDamage) hero.attackDamage = monster.attackDamage
          }
        monsters = monsters.tail
        println(s"残りのモンスターは${monsters.length}匹となった。")
        if(monsters.length > 0) {
          println(
            """
              |----------------------------------------
              |新たな未知のモンスターが現れた。""".stripMargin)
        }
      }
    }
  println(
    s"""----------------------------------------
       |【ゲームクリア】: あなたは困難を乗り越えた。新たな冒険に祝福を。
       |【結果】: ${hero}""".stripMargin
  )
  System.exit(0)
}

abstract class Creature(var hitPoint: Int, var attackDamage: Int) {
  def isAlive(): Boolean = this.hitPoint > 0
}

class Hero(_hitPoint: Int, _attackDamage: Int, var evasive: Boolean) extends Creature(_hitPoint, _attackDamage) {
  def attack(monster: Monster): Unit = {
    monster.hitPoint = monster.hitPoint - this.attackDamage
    if(monster.isAlive){this.hitPoint = this.hitPoint - monster.attackDamage}
  }

  def guard(monster: Monster): Unit = {
    this.hitPoint = this.hitPoint - (monster.attackDamage / 3)
  }

  def counterAttack(monster: Monster): Unit = {
    val isEvasive = RPG.random.nextInt(4) == 1
    if(isEvasive) {
      this.evasive = true
      monster.hitPoint = monster.hitPoint - (this.attackDamage + 50)
    } else {
      this.evasive = false
      this.hitPoint = this.hitPoint - (monster.attackDamage + 30)
      if(this.isAlive) {monster.hitPoint = monster.hitPoint - (this.attackDamage + 25)}
    }
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

  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage})"

}

class Monster(_hitPoint: Int, _attackDamage: Int, var isAwayFromHero: Boolean)
  extends Creature(_hitPoint, _attackDamage) {
  override def toString = s"Monster(体力:${hitPoint}, 攻撃力:${attackDamage}, ヒーローから離れている:${isAwayFromHero})"
}
