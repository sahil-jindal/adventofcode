package daytwentytwo

import scala.collection.mutable.Buffer

case class State(
    shield: Int = 0,
    poison: Int = 0,
    recharge: Int = 0,
    playerHp: Int = 0,
    bossHp: Int = 0,
    playerMana: Int = 0,
    bossDamage: Int = 0,
    usedMana: Int = 0,
    playerArmor: Int = 0,
    manaLimit: Int = 0
) {

    private val missileMana = 53
    private val drainMana = 73
    private val shieldMana = 113
    private val poisonMana = 173
    private val rechargeMana = 229

    def withManaLimit(manaLimit: Int): State =
        copy(manaLimit = manaLimit)

    def applyEffects(): State =
        if playerHp <= 0 || bossHp <= 0 then return this

        var newState = this
        
        if poison > 0 then
            newState = newState.copy(bossHp = bossHp - 3, poison = poison - 1)
        
        if recharge > 0 then
            newState = newState.copy(playerMana = playerMana + 101, recharge = recharge - 1)
        
        if shield > 0 then
            newState = newState.copy(shield = shield - 1, playerArmor = 7)
        else
            newState = newState.copy(playerArmor = 0)
        
        newState

    def damage(damage: Int): State =
        if playerHp <= 0 || bossHp <= 0 then this
        else copy(playerHp = playerHp - damage)

    def bossStep(): State =
        if playerHp <= 0 || bossHp <= 0 then this
        else copy(playerHp = playerHp - math.max(1, bossDamage - playerArmor))

    def playerSteps(): Seq[State] =
        if playerHp <= 0 || bossHp <= 0 then return Seq(this)
    
        val steps = Buffer[State]()
        
        if playerMana >= missileMana && missileMana + usedMana <= manaLimit then
            steps += copy(
                playerMana = playerMana - missileMana,
                usedMana = usedMana + missileMana,
                bossHp = bossHp - 4
            )
        
        if playerMana >= drainMana && drainMana + usedMana <= manaLimit then 
            steps += copy(
                playerMana = playerMana - drainMana,
                usedMana = usedMana + drainMana,
                bossHp = bossHp - 2,
                playerHp = playerHp + 2
            )

        if playerMana >= shieldMana && shield == 0 && shieldMana + usedMana <= manaLimit then 
            steps += copy(
                playerMana = playerMana - shieldMana,
                usedMana = usedMana + shieldMana,
                shield = 6
            )

        if playerMana >= poisonMana && poison == 0 && poisonMana + usedMana <= manaLimit then 
            steps += copy(
                playerMana = playerMana - poisonMana,
                usedMana = usedMana + poisonMana,
                poison = 6
            )

        if playerMana >= rechargeMana && recharge == 0 && rechargeMana + usedMana <= manaLimit then 
            steps += copy(
                playerMana = playerMana - rechargeMana,
                usedMana = usedMana + rechargeMana,
                recharge = 5
            )

        steps.toSeq
}