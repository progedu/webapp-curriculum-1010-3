import java.util.Random

object RPG extends App {
  val random = new Random

  // ゲームの難易度選択：Easy,Normal,Hard
  val difficulty: String = scala.io.StdIn.readLine(
    """
      |ゲームの難易度を数値を入力して選択
      |Easy  【1】
      |Normal【2】
      |Hard  【3】
      | >""".stripMargin)

   // 難易度ごとのパラメータ
  var diffConfig: Map[String, Int] = difficulty match {
    case "1" => Map("monsterCount" -> 3, "heroHP" -> 350, "attackDamage" -> 50, "energyDrink" -> 1)
    case "2" => Map("monsterCount" -> 5, "heroHP" -> 350, "attackDamage" -> 40, "energyDrink" -> 1)
    case "3" => Map("monsterCount" -> 8, "heroHP" -> 450, "attackDamage" -> 30, "energyDrink" -> 1)
    case _   => Map("monsterCount" -> 5, "heroHP" -> 350, "attackDamage" -> 40, "energyDrink" -> 1)
  }


  val monsterCount = diffConfig("monsterCount")
  val hero = new Hero(diffConfig("heroHP"), diffConfig("attackDamage"), diffConfig("energyDrink"))
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(120), random.nextInt(120), false)

  println(
    s"""あなたは冒険中の ${hero} であり、
        |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けねばならない。
        |【ルール】:
        |1を入力してEnterキーを押すと攻撃、2を入力するとエナジードリンク、それ以外を入力すると逃走となる。
        |エナジードリンクは一度使用すると無くなる。
        |逃走成功確率は50%。逃走に失敗した場合はダメージをうける。
        |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。
        |またモンスターを倒した場合、武器を奪いその攻撃力を得ることできる。
        |---------------------------------------------
        |未知のモンスターがあらわれた。""".stripMargin)

  while (!monsters.isEmpty) {
    val monster = monsters.head
    val input = 
      if (hero.hasEnergyDrink)  // エナジードリンクを所持している場合、選択肢にエナジードリンクがあらわれる
        scala.io.StdIn.readLine("【選択】: 攻撃[1] or エナジードリンク【2】or 逃走[0] > ")
      else
        scala.io.StdIn.readLine("【選択】: 攻撃[1] or 逃走[0] > ")

    if (input == "1") { // 攻撃する
      hero.attack(monster)
      println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
    } else if (hero.hasEnergyDrink && input == "2"){ // エナジードリンクを飲む,HP回復
      println(s"あなたは、エナジードリンクを飲みHPが${hero.drink}回復した。")
      monster.attack(hero)
      println(s"あなたは、${monster.attackDamage}のダメージを受けた。")
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

abstract class Creature(var hitPoint: Int, var attackDamage: Int) {
  def isAlive(): Boolean = this.hitPoint > 0
}

class Hero(_hitPoint: Int, _attackDamage: Int, private var energyDrink: Int) 
  extends Creature(_hitPoint, _attackDamage) {

  def attack(monster: Monster): Unit = {
    monster.hitPoint = monster.hitPoint - this.attackDamage
    this.hitPoint = this.hitPoint - monster.attackDamage
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

  // エナジードリンクをもってるかどうか
  def hasEnergyDrink: Boolean = this.energyDrink > 0

  // エナジードリンクを飲むメソッド
  def drink(): Int = {
    this.hitPoint =  + this.hitPoint + (RPG.diffConfig("heroHP") / 2)
    if (this.hitPoint > RPG.diffConfig("heroHP")) this.hitPoint = RPG.diffConfig("heroHP")
    this.energyDrink = this.energyDrink - 1

    RPG.diffConfig("heroHP") / 2
  }

  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage})"

}

class Monster(_hitPoint: Int, _attackDamage: Int, var isAwayFromHero: Boolean)
  extends  Creature(_hitPoint, _attackDamage) {

  // モンスターの攻撃
  def attack(hero: Hero): Unit = hero.hitPoint = hero.hitPoint - this.attackDamage

  override def toString = s"Monster(体力: ${hitPoint}, 攻撃力:${attackDamage}, ヒーローから離れている:${isAwayFromHero})"

}