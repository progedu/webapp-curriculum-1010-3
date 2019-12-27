import java.util.Random

object RPG extends App {
  val random = new Random
  val monsterCount = 5
  val hero = new Hero(300,30)
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(70),random.nextInt(70),false)

  println(
    s"""あなたは冒険中の${hero}であり、
       |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けねばならない。
       |【ルール】
       |1を入力してEnterキーを押すと攻撃、それ以外を入力すると逃走となる。
       |逃走成功確率は50%。逃走に失敗した場合はダメージをうける。
       |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。
       |またモンスターを倒した場合、武器を奪いその攻撃力を得ることできる。
       |---------------------------------------------------
       |未知のモンスターが現れた
       |""".stripMargin)

  while (!monsters.isEmpty){
    val monster = monsters.head
    val input = scala.io.StdIn.readLine("[選択]: 攻撃「1」 or 逃走「0」 or 防御「2」 or 魔法「3」 > ")

    if (input == "1"){  //攻撃する
      hero.attack(monster)
      println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
    }
    else if(input == "2"){//防御する
      hero.defence(monster)
      println(s"あなたは${(hero.attackDamage/10)}のダメージを与え、${(monster.attackDamage/10)}のダメージを受けた。")
    }
    else if(input == "3"){//魔法を使う
      hero.magical(monster)
      println(s"あなたは${(hero.attackDamage*2)}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
    }

    else{ //逃走する
      if(hero.escape(monster)){
        println("あなたは逃走に成功した")
      }else{
        println(s"あなたは逃走に失敗し${monster.attackDamage}のダメージを受けた")
      }
    }
    println(s"【現在の状態】：${hero}, ${monster}")

    if (!hero.isAlive){ //Heroが死んでいるかどうか
      println(
        """--------------------------------------------------
          |ゲームオーバー あなたは無残にも殺されてしまった""".stripMargin)
      System.exit(0)
    }else if(!monster.isAlive||monster.isAwayFromHero){ //Monsterいないかどうか
      if(!monster.isAwayFromHero) { // 倒した場合
      println("モンスターは倒れた。そしてあなたはモンスターの武器を奪った")
        if(monster.attackDamage > hero.attackDamage) hero.attackDamage = monster.attackDamage
      }
      monsters = monsters.tail
      println(s"残りのモンスターは${monsters.length}匹となった")
      if(monsters.length > 0){
        println(
          """---------------------------------------------
            |新たな未知のモンスターが現れた
            |""".stripMargin)
      }
    }
  }

  println(
    s"""-----------------------------------------------
       |【ゲームクリア】：   あなたは困難を乗り越えた。新たな冒険に祝福を
       |【結果】  ：${hero}""".stripMargin
  )
  System.exit(0)
}

abstract class Creature(var hitPoint: Int, var attackDamage: Int){
  def isAlive(): Boolean = this.hitPoint > 0
}

class Hero(_hitPoint: Int, _attackDamage: Int) extends Creature(_hitPoint, _attackDamage){

  def attack(monster: Monster): Unit = {
    monster.hitPoint = monster.hitPoint - this.attackDamage
    this.hitPoint = this.hitPoint - monster.attackDamage
  }

  def defence(monster: Monster): Unit = {
    monster.hitPoint = monster.hitPoint - (this.attackDamage/10)
    this.hitPoint = this.hitPoint - (monster.attackDamage/10)
  }

  var magic = 3
  def magical(monster: Monster): Unit = {
    if(magic >0){
      monster.hitPoint = monster.hitPoint - (this.attackDamage*2)
      magic = magic-1
      this.hitPoint = this.hitPoint - monster.attackDamage
    }
    else{println("魔法を打てません")
      this.hitPoint = this.hitPoint - monster.attackDamage
    }
  }

  def escape(monster: Monster): Boolean = {
    val isEscaped = RPG.random.nextInt(2)==1
    if(!isEscaped){
      this.hitPoint = this.hitPoint - monster. attackDamage
    }else{
      monster.isAwayFromHero = true
    }
    isEscaped
  }

  override def toString = s"Hero(体力:${hitPoint},攻撃力${attackDamage})"
}

class Monster(_hitPoint: Int, _attackDamage: Int, var isAwayFromHero: Boolean)
extends Creature(_hitPoint, _attackDamage){
  override def toString = s"Monster(体力:${hitPoint}, 攻撃力:${attackDamage},ヒーローから離れている:${isAwayFromHero})"
}
