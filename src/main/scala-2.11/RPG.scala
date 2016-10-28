/**
  * Created by masashi on 2016/10/26.
  */
import java.util.Random

object RPG extends App {
  val random = new Random
  val monsterCount = 5
  val hero = new Hero(300, 30, false)
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(120), random.nextInt(120), false)
  val damagebooster = new DamageBooster(random.nextInt(20) + 10, 0)
  val cutefigure = new CuteFigure(random.nextInt(50) + 70, 0)

  println(
    s"""あなたは冒険中の ${hero} であり、
       |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けなければならない。<br>
       |【ルール】:
       |1を入力してEnterキーを押すと攻撃、それ以外を入力すると逃走となる。<br>
       |スタート時にアイテムを1つ選択でき、攻撃時に使用できる。<br>
       |逃走成功確率は50%。逃走に失敗した場合はダメージを受ける。<br>
       |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。<br>
       |また自分より攻撃力の高いモンスターを倒した場合、武器を奪いその攻撃力を得ることができる。<br>
       |---------------------------------------------
       |【アイテム説明】
       |・アイテム名 :説明<br>
       |・ほうれん草 :攻撃力が1回の使用につき最大30増加する。<br>
       |・美少女フィギュア :モンスターが攻撃を弱めてくれる。逃走時には使用できない。<br>
       |*各アイテムは1~3回の使用可能回数がランダムで決められる。<br>
       |---------------------------------------------
       |モンスターが現れた。
       |---------------------------------------------
       |""".stripMargin)


  var inputItem = scala.io.StdIn.readLine("アイテムを選択してください。<br>【選択】アイテムなし:[0], ほうれん草:[1], 美少女フィギュア[2] > ")
  if(inputItem == "1") { // ほうれん草所持
    damagebooster.gainItem(random, hero)
    println(s"ほうれん草を手に入れた。使用可能回数は${damagebooster.usableTimes}回。")
  } else if (inputItem == "2") { // 盾所持
    cutefigure.gainItem(random, hero)
    println(s"美少女フィギュアを手に入れた。使用可能回数は${cutefigure.usableTimes}回。")
  } else if (inputItem == "0") { }


  while (!monsters.isEmpty) {
    val monster = monsters.head
    val input = scala.io.StdIn.readLine("【選択】攻撃:[1] or 逃走:[0] > ")

    if (input == "1") { // 攻撃する
      if (hero.havingItem) {
        val itemUse = scala.io.StdIn.readLine("アイテムを使用しますか？【選択】Yes:[1] or No:[0] > ")
        if (itemUse == "1") { // アイテム使用
          if (damagebooster.isPresense()) { damagebooster.damageIncrease(hero) }
          if (cutefigure.isPresense()) { cutefigure.damageDecrease(hero, monster) }
        } else if (itemUse == "0") {
        }
      }
      hero.attack(monster)
      println(s"モンスターに${hero.attackDamage}のダメージを与た。Heroは${monster.attackDamage}のダメージを受けた。")
      println(s"【現在の状態】:${hero}, ${monster}")
    } else if (input == "0") { // 逃走する
      if(hero.escape(monster)) {
        println("モンスターから逃走に成功した。")
      } else {
        println(s"逃走に失敗。${monster.attackDamage}のダメージを受けた。")
        println(s"【現在の状態】:${hero}, ${monster}")
      }
    }


    if (!hero.isAlive) { // Hero が死んでいるかどうか
      println(
        s"""---------------------------------------------
          |【ゲームオーバー】:Heroは死んだ。""".stripMargin)
      System.exit(0)
    } else if (!monster.isAlive || monster.isAwayFromHero) { // Monster いないかどうか
      if (!monster.isAwayFromHero) { // 倒した場合
        println(s"モンスターは倒れた。")
        if (monster.attackDamage > hero.attackDamage) {
          hero.attackDamage = monster.attackDamage
          println("そしてHeroはモンスターの武器を手に入れた。")
        }
      }
      monsters = monsters.tail
      println(s"残りのモンスターは${monsters.length}匹となった。")
      if (monsters.length > 0) {
        println(
          """---------------------------------------------
            |新たなモンスターがあらわれた。 """.stripMargin)
      }
    }
  }

  println(
    s"""---------------------------------------------
       |【ゲームクリア】:あなたは困難を乗り越えた。新たな冒険に祝福を。<br>
       |【結果】${hero}""".stripMargin
  )
  System.exit(0)

}

abstract class Creature(var hitPoint: Int, var attackDamage: Int) {
  def isAlive(): Boolean = this.hitPoint > 0
}

class Hero(_hitpoint: Int, _attackDamage: Int, var havingItem: Boolean) extends Creature(_hitpoint, _attackDamage) {

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

  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage})"

}

class Monster(_hitPoint: Int, _attackDamage: Int, var isAwayFromHero: Boolean)
  extends Creature(_hitPoint, _attackDamage) {

  override def toString = s"Monster(体力:${hitPoint}, 攻撃力:${attackDamage}, ヒーローから離れている:${isAwayFromHero})"

}


abstract class Item(var usableTimes: Int) {

  def gainItem(random: Random, hero: Hero): Unit = {
    this.usableTimes = random.nextInt(2) + 1
    hero.havingItem = true
  }

  def isPresense(): Boolean = this.usableTimes > 0

  def afterUse(hero: Hero): Unit = {
    this.usableTimes = this.usableTimes - 1
    if (!this.isPresense()){
      hero.havingItem = false
    } else {
      println(s"<<残り使用可能回数${this.usableTimes}>>")
    }
  }
}

class DamageBooster(var attackIncreaseRatio: Int, _usableTimes: Int) extends Item(_usableTimes) {

  def damageIncrease(hero: Hero): Unit = { // 攻撃力上昇
    hero.attackDamage = hero.attackDamage + this.attackIncreaseRatio
    println(s"<<攻撃力が${hero.attackDamage}になった。>>")
    afterUse(hero)
  }

}

class CuteFigure(var power:Int, _usableTimes: Int) extends Item(_usableTimes) {

  def damageDecrease(hero: Hero, monster: Monster): Unit = {
    if (monster.attackDamage > this.power) {
      monster.attackDamage = monster.attackDamage - this.power
    } else {
      monster.attackDamage = 0
    }
    println(s"<<モンスターの攻撃力が${this.power}減って、${monster.attackDamage}になった。>>")
    afterUse(hero)

  }
}