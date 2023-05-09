import java.util.Random

abstract class Creature(var hitPoint: Int, var attackDamage: Int, var speed: Int) {
  def isAlive: Boolean = this.hitPoint > 0
}

class Hero(_hitPoint: Int, _attackDamage: Int, _speed: Int) extends Creature(_hitPoint, _attackDamage, _speed) {
  def fight(monster: Monster): Unit = {
    if (this.speed > monster.speed) {
      monster.hitPoint -= this.attackDamage
      if (monster.isAlive) {
        this.hitPoint -= monster.attackDamage
      }
    } else {
      this.hitPoint -= monster.attackDamage
      if (this.isAlive) {
        monster.hitPoint -= this.attackDamage
      }
    }
  }

  def escape(monster: Monster): Boolean = {
    val isEscaped = RPG.random.nextInt(2) == 1
    if (!isEscaped) {
      this.hitPoint -= monster.attackDamage
    } else {
      monster.isAwayFromHero = true
    }
    isEscaped
  }

  override def toString = s"Hero(体力:${hitPoint}, 攻撃力:${attackDamage}, 素早さ:${speed})"
}

class Monster(_hitPoint: Int, _attackDamage: Int, _speed: Int, var isAwayFromHero: Boolean) extends Creature(_hitPoint, _attackDamage, _speed
) {
  override def toString = s"Monster(体力:${hitPoint}, 攻撃力:${attackDamage}, 素早さ:${speed}, ヒーローから離れている:${isAwayFromHero})"
}

object RPG extends App {
  val random = new Random()
  val monsterCount = 5
  val hero = new Hero(200, 30, 60)
  var monsters = for (i <- 1 to monsterCount) yield new Monster(random.nextInt(120), random.nextInt(120), random.nextInt(120), false)

  println(
    s"""あなたは冒険中の ${hero} であり、
       |${monsterCount}匹のモンスターが潜んでいる洞窟を抜けねばならない。
       |【ルール】:
       |1を入力してEnterキーを押すと攻撃、それ以外を入力すると逃走となる。
       |逃走成功確率は50%。逃走に失敗した場合はダメージをうける。
       |一度でもダメージを受けるとモンスターの体力と攻撃力が判明する。
       |またモンスターを倒した場合、武器を奪いその攻撃力を得ることができる。
       |---------------------------------------------
       |未知のモンスターがあらわれた。""".stripMargin)

  while (!monsters.isEmpty) {
    val monster = monsters.head
    val input = scala.io.StdIn.readLine("【選択】: 攻撃[1] or 逃走[0] > ")

    if (input == "1") {
      hero.fight(monster)
      if (hero.speed > monster.speed) {
        if (monster.isAlive) {
          println(s"あなたは${hero.attackDamage}のダメージを与え、${monster.attackDamage}のダメージを受けた。")
        } else {
          println(s"あなたは${hero.attackDamage}のダメージを与えた。")
        }
      } else {
        if (hero.isAlive) {
          println(s"あなたは${monster.attackDamage}のダメージを受け、${hero.attackDamage}のダメージを与えた。")
        } else {
          println(s"あなたは${monster.attackDamage}のダメージを受けた。")
        }
      }

    } else {
      if (hero.escape(monster)) {
        println("あなたは、モンスターから逃走に成功した。")
      } else {
        println(s"あなたは、モンスターから逃走に失敗し、${monster.attackDamage}のダメージを受けた。")
      }
    }
    println(s"【現在の状態】: ${hero}, ${monster}")

    if (!hero.isAlive) {
      println(
        """---------------------------------------------
          |【ゲームオーバー】: あなたは無残にも殺されてしまった。 """.stripMargin)
      System.exit(0)
    } else if (!monster.isAlive || monster.isAwayFromHero) {
      if (!monster.isAwayFromHero) {
        println("モンスターは倒れた。そしてあなたは、モンスターの武器を奪った。")
        if (monster.attackDamage > hero.attackDamage) hero.attackDamage = monster.attackDamage
      }
      monsters = monsters.tail
      println(s"残りのモンスターは${monsters.length}匹となった。")
      if (monsters.length > 0) {
        if (random.nextInt(4) == 1) {
          println(
            """---------------------------------------------
              |あなたは回復薬が落ちているのを見つけた。
              |あなたはこれを飲んでみても良いし、口をつけず先へ進んでも良い。
              |---------------------------------------------""".stripMargin)
          val input2 = scala.io.StdIn.readLine("【選択】: 回復薬を飲む[1] or やめておく[0] > ")
          if (input2 == "1") {
            val ran = random.nextInt(60)
            val heal = ran - 25
            hero.hitPoint += heal
            if (heal < 0) {
              println(
                """---------------------------------------------
                  |とてつもないほどの痛みがあなたの内側から広がっていく。
                  |どうやらこの回復薬は変質していたらしい……。
                  |---------------------------------------------""".stripMargin)
              println(s"あなたは${heal.abs}のダメージを受けた。")
              println(s"【現在の状態】: ${hero}")
              if (!hero.isAlive) {
                println(
                  """---------------------------------------------
                    |【ゲームオーバー】: せめて戦って死にたかった……。 """.stripMargin)
                System.exit(0)
              }
              println(
                """---------------------------------------------
                  |少しの休憩を挟み、コンディションを確認してあなたは動き出す。 """.stripMargin)
            } else if (heal == 0) {
              println(
                """---------------------------------------------
                  |しかし何も起こる様子がない。
                  |長い時を経て癒しの力を失ってしまったようだ……先へ進もう。
                  |---------------------------------------------""".stripMargin)
            } else {
              println(
                """---------------------------------------------
                  |傷が癒え、力が沸き起こってくる……
                  |あなたは更に気を引き締め、先へ進むことを決めた。
                  |---------------------------------------------""".stripMargin)
              println(s"あなたは${heal}の体力を回復した。")
              println(s"【現在の状態】: ${hero}")
            }
          } else {
            println(
              """---------------------------------------------
                |こんなところに落ちているものを飲んだら何が起きるかわかったもんじゃない。
                |あなたは更に気を引き締め、先へ進むことを決めた。
                |---------------------------------------------""".stripMargin)
          }
        }
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
