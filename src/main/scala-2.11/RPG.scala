import java.util.Random



object RPG extends App {
  val random = new Random
  val monsterCount = 5
  val maxHitpoint = 300
  var maxAttack = 50
  var maxMagicpoint = 100
  val magicHeelrate = 0.3
  val magicTyoirate = 1.3
  val magicHeelCost = 30
  val magicTyoiCost = 40
  val hero = new Hero(maxHitpoint, maxAttack,maxMagicpoint)
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(120), random.nextInt(120), false)


  println(
    s"""あなたは冒険中の ${hero} であり、
        |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けねばならない。
        |【ルール】:
        |1を入力してEnterキーを押すと攻撃、2を入力すると魔法、それ以外を入力すると逃走となる。
        |逃走成功確率は50%。逃走に失敗した場合はダメージをうける。
        |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。
        |またモンスターを倒した場合、武器を奪いその攻撃力を得ることできる。
        |
        |魔法を使うために、魔力を消費する。
        |魔力が足りない場合は、魔法を使うことができない。
        |あなたが使える魔法
        |その１　ヒール
        |　回復魔法　
        |　消費魔力:${magicHeelCost}
        |　体力最大値の${magicHeelrate * 100}%分だけ、体力を回復できる
        |その２　チョイキルト
        |　ステータス魔法
        |　消費魔力:${magicTyoiCost}
        |　攻撃力が${magicTyoirate}倍になる
        |
        |---------------------------------------------
        |未知のモンスターがあらわれた。""".stripMargin)

  while (!monsters.isEmpty) {
    val monster = monsters.head
    val input = scala.io.StdIn.readLine("【選択】: 攻撃[1]  魔法[2] or 逃走[0] > ")

    if (input == "1") { // 攻撃する
      hero.attack(monster)
      println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
    } else if (input == "2") { //魔法メニュー
      val magicmenu = scala.io.StdIn.readLine("【魔法】: ヒール[1]  チョイキルト[2]> ")

      if(magicmenu == "1" && hero.magic(magicHeelCost)){
        //ヒール　回復魔法
        hero.magicHeel(maxHitpoint,magicHeelrate,monster)
        println(s"あなたは${(maxHitpoint * magicHeelrate).toInt}ヒットポイントを回復した")
        println(s"魔法を唱えている間にモンスターから${monster.attackDamage}のダメージを受けた")

      }
      else if(magicmenu == "2" && hero.magic(magicTyoiCost)){
        //チョイキルト　攻撃力アップ
        hero.magicTyoikilt(magicTyoirate,monster)
        println(s"あなたの攻撃力は${magicTyoirate}倍になった")
        println(s"魔法を唱えている間にモンスターから${monster.attackDamage}のダメージを受けた")
      }
      else{ //MP切れで魔法が使えない場合
        println("魔力が切れたので、魔法を使うことができない！")
      }

    }
    else { // 逃走する
      if (hero.escape(monster)) {
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

abstract class Creature(var hitPoint: Double, var attackDamage: Double, var magicPoint: Int) {
  def isAlive(): Boolean = this.hitPoint > 0
}

class Hero(_hitPoint: Int, _attackDamage: Int, _magicPoint: Int) extends Creature(_hitPoint, _attackDamage,_magicPoint) {

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

  def magic(_useMagicCcost: Int): Boolean ={
    var isMagic = false
    if(this.magicPoint >= _useMagicCcost ){
      this.magicPoint = this.magicPoint - _useMagicCcost
      isMagic = true
    }
    else{
      isMagic = false
    }
    isMagic
  }

  def magicHeel(_maxHitpoint: Int, _heelRate: Double, monster: Monster) = {
    //最大HPの◯◯%分回復する魔法
    this.hitPoint = this.hitPoint + _maxHitpoint * _heelRate
    if (this.hitPoint > _maxHitpoint) {
      this.hitPoint = _maxHitpoint
    }
    this.hitPoint = this.hitPoint - monster.attackDamage
  }

  def magicTyoikilt(_tyoiRate: Double, monster : Monster) = {
    //攻撃力がちょっと上がる魔法
    this.attackDamage = this.attackDamage * _tyoiRate
    this.hitPoint = this.hitPoint - monster.attackDamage
  }


  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage.toInt},魔力:${magicPoint})"

}

class Monster(_hitPoint: Int, _attackDamage: Int, var isAwayFromHero: Boolean)
  extends  Creature(_hitPoint, _attackDamage, magicPoint = 100) {

  override def toString = s"Monster(体力: ${hitPoint}, 攻撃力:${attackDamage}, ヒーローから離れている:${isAwayFromHero})"

}