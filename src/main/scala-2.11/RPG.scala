import java.util.Random

object RPG extends App {
  val random = new Random
  val monsterCount = 5
  val recoverPoint = 50
  val maxHp = 200
  val maxAttk = 30
  val maxMagic = 2
  val hero = new Hero(maxHp, maxAttk, maxMagic)
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(120), random.nextInt(120), false)

  println(
    s"""あなたは冒険中の ${hero} であり、
        |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けねばならない。
        |【ルール】:
        |1を入力してEnterキーを押すと攻撃、2を入力すると防御、3を入力すると体力回復、それ以外を入力すると逃走となる。
        |防御はダメージ量が1/2となる。また、1/2の確率でノーダメージでカウンター攻撃する。
        |体力回復は${maxMagic}回までしか使用できないが、体力が${recoverPoint}回復する。
        |逃走成功確率は50%。逃走に失敗した場合はダメージをうける。
        |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。
        |またモンスターを倒した場合、武器を奪いその攻撃力を得ることできる。
        |---------------------------------------------
        |未知のモンスターがあらわれた。""".stripMargin)

  while (!monsters.isEmpty) {
    val monster = monsters.head
    val input = scala.io.StdIn.readLine("【選択】: 攻撃[1] or 防御[2] or 回復[3] or 逃走[0] > ")

    if (input == "1") { // 攻撃する
      if (hero.attack(monster)) {
        println(s"あなたは${hero.attackDamage * 2}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
      } else {
        println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
      }
    } else if (input == "2") {  // 防御
      if (hero.diffence(monster)) {
        println(s"カウンター攻撃が決まった、あなたはノーダメージで、${hero.attackDamage}のダメージを与えた。")
      } else {
        println(s"防御した、あなたは${monster.attackDamage / 2}のダメージを受けた。")
      }
    } else if (input == "3") {  // 回復
      if (hero.recover()) println(s"あなたの体力が回復した。${hero}")
      else {
        if (hero.magicPoint != 0) println("体力は満タンです。")
        else println("MagicPointが不足しています。")
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

abstract class Creature(var hitPoint: Int, var attackDamage: Int) {
  def isAlive(): Boolean = this.hitPoint > 0
}

class Hero(_hitPoint: Int, _attackDamage: Int, var magicPoint: Int) extends Creature(_hitPoint, _attackDamage) {

  def attack(monster: Monster): Boolean = {
    // 1/10でクリティカル(2倍攻撃)
    val isCritical = RPG.random.nextInt(10) == 1
    val attk = if (isCritical) {
      println("クリティカル攻撃 2倍ダメージ")
      this.attackDamage * 2
    } else this.attackDamage
    monster.hitPoint = monster.hitPoint - attk
    // 攻撃後、モンスターが生きている場合は反撃を受ける
    if (monster.isAlive()) this.hitPoint = this.hitPoint - monster.attackDamage
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

  /**
    * 防御時は1/2ダメージを受ける。
    * 1/2の確率でカウンター攻撃をする。
    * その場合、ノーダメージ
    */
  def diffence(monster: Monster): Boolean = {
    val isCounter = RPG.random.nextInt(2) == 1
    if (isCounter) {
      monster.hitPoint = monster.hitPoint - this.attackDamage
      true
    } else {
      this.hitPoint = this.hitPoint - (monster.attackDamage / 2)
      false
    }
  }

  /**
    * HPがMAXの場合は回復せず、使用回数も減らない
    * HPが減っている場合は、使用回数を1回減らし、体力を回復する。
    * 最大値を超える場合は最大値にする。
    */
  def recover(): Boolean = {
    if (hitPoint >= RPG.maxHp) {return false}
    if (magicPoint > 0) {
      magicPoint = magicPoint - 1
      hitPoint += RPG.recoverPoint
      if (hitPoint > RPG.maxHp) hitPoint = RPG.maxHp
      true
    } else false
  }

  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage})"

}

class Monster(_hitPoint: Int, _attackDamage: Int, var isAwayFromHero: Boolean)
  extends  Creature(_hitPoint, _attackDamage) {

  override def toString = s"Monster(体力: ${hitPoint}, 攻撃力:${attackDamage}, ヒーローから離れている:${isAwayFromHero})"

}