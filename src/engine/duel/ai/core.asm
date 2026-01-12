; initialize attack scores to zero
ResetAIAttackScores:
	xor a
	; jr InitAIAttackScores
	; fallthrough

; initialize attack scores to value in a
InitAIAttackScores:
; basic stage
	ld [wAIScoreAllAttacks + 0], a
	ld [wAIScoreAllAttacks + 1], a
; stage 1
	ld [wAIScoreAllAttacks + 2], a
	ld [wAIScoreAllAttacks + 3], a
; stage 2
	ld [wAIScoreAllAttacks + 4], a
	ld [wAIScoreAllAttacks + 5], a
	ret


; input:
;   hl = pointer to evaluation function
;   [hTempPlayAreaLocation_ff9d] = location of the Pokémon to check
ForAllAttacksOfPokemon:
; preload previous stages
	push hl
	call GetCardOneStageBelow
	pop hl
	; jr ForAllAttacksOfAllPokemonStages
	; fallthrough

; input:
;   hl = pointer to evaluation function
;   [hTempPlayAreaLocation_ff9d] = location of the Pokémon to check
;   [wAllStagesIndices] = stack of all Pokémon stages at location
ForAllAttacksOfAllPokemonStages:
; store evaluation function pointer
	ld a, l
	ld [wAIEvaluationFunctionPointer], a
	ld a, h
	ld [wAIEvaluationFunctionPointer + 1], a
; initialize attack scores
	call ResetAIAttackScores
; basic stage, always exists
	ld a, [wAllStagesIndices + BASIC]
	ld [wTempCardDeckIndex], a
	ld e, FIRST_ATTACK_OR_PKMN_POWER
	call EvaluateSelectedAttack
	ld [wAIScoreAllAttacks + 0], a
	ld e, SECOND_ATTACK
	call EvaluateSelectedAttack
	ld [wAIScoreAllAttacks + 1], a
; stage 1
	ld a, [wAllStagesIndices + STAGE1]
	cp $ff
	jr z, .stage2
	ld [wTempCardDeckIndex], a
	ld e, FIRST_ATTACK_OR_PKMN_POWER
	call EvaluateSelectedAttack
	ld [wAIScoreAllAttacks + 2], a
	ld e, SECOND_ATTACK
	call EvaluateSelectedAttack
	ld [wAIScoreAllAttacks + 3], a
.stage2
	ld a, [wAllStagesIndices + STAGE2]
	cp $ff
	ret z  ; nothing here
	ld [wTempCardDeckIndex], a
	ld e, FIRST_ATTACK_OR_PKMN_POWER
	call EvaluateSelectedAttack
	ld [wAIScoreAllAttacks + 4], a
	ld e, SECOND_ATTACK
	call EvaluateSelectedAttack
	ld [wAIScoreAllAttacks + 5], a
	ret


; input:
;   hl = pointer to predicate function (carry if match)
;   [hTempPlayAreaLocation_ff9d] = location of the Pokémon to check
FindMatchingAttack:
; preload previous stages
	push hl
	call GetCardOneStageBelow
	pop hl
	; jr FindMatchingAttackFromAllPokemonStages
	; fallthrough

; input:
;   hl = pointer to predicate function (carry if match)
;   [hTempPlayAreaLocation_ff9d] = location of the Pokémon to check
;   [wAllStagesIndices] = stack of all Pokémon stages at location
FindMatchingAttackFromAllPokemonStages:
; store evaluation function pointer
	ld a, l
	ld [wAIEvaluationFunctionPointer], a
	ld a, h
	ld [wAIEvaluationFunctionPointer + 1], a
; basic stage, always exists
	ld a, [wAllStagesIndices + BASIC]
	ld [wTempCardDeckIndex], a
	ld e, FIRST_ATTACK_OR_PKMN_POWER
	call EvaluateSelectedAttack
	ret c  ; found match
	ld e, SECOND_ATTACK
	call EvaluateSelectedAttack
	ret c  ; found match
; stage 1
	ld a, [wAllStagesIndices + STAGE1]
	cp $ff
	jr z, .stage2
	ld [wTempCardDeckIndex], a
	ld e, FIRST_ATTACK_OR_PKMN_POWER
	call EvaluateSelectedAttack
	ret c  ; found match
	ld e, SECOND_ATTACK
	call EvaluateSelectedAttack
	ret c  ; found match
.stage2
	ld a, [wAllStagesIndices + STAGE2]
	cp $ff
	ret z  ; nothing here
	ld [wTempCardDeckIndex], a
	ld e, FIRST_ATTACK_OR_PKMN_POWER
	call EvaluateSelectedAttack
	ret c  ; found match
	ld e, SECOND_ATTACK
	jr EvaluateSelectedAttack


; input:
;   e = selected attack index
;   hl = evaluation function
;   [wTempCardDeckIndex] = deck index of Pokémon card
EvaluateSelectedAttack:
	ld a, [wTempCardDeckIndex]
	ld d, a
	call CopyAttackDataAndDamage_FromDeckIndex
	ld a, [wAIEvaluationFunctionPointer]
	ld l, a
	ld a, [wAIEvaluationFunctionPointer + 1]
	ld h, a
	jp hl



; returns carry if arena card
; can knock out defending Pokémon
CheckIfActiveCardCanKnockOut:
	xor a ; PLAY_AREA_ARENA
	ldh [hTempPlayAreaLocation_ff9d], a
	; jr CheckIfAnyAttackKnocksOutDefendingCard
	; fallthrough

; returns carry if damage dealt from any of a Pokémon's attacks KOs defending Pokémon
; it also checks whether the selected attack is usable and has enough energy
; outputs index of the attack that KOs
; input:
;	[hTempPlayAreaLocation_ff9d] = location of attacking card to consider
; output:
;	[wSelectedAttack] = attack index that KOs
;   [wTempCardDeckIndex] = deck index of the card owning the attack
CheckIfAnyAttackKnocksOutDefendingCard:
	ld hl, CheckIfAttackKnocksOutDefendingCard
	jp FindMatchingAttack

; returns carry if damage dealt from any of a Pokémon's attacks KOs defending Pokémon
; it also checks whether the selected attack is usable and has enough energy
; or if the missing energy is found in the hand
; outputs index of the attack that KOs
; input:
;	[hTempPlayAreaLocation_ff9d] = location of attacking card to consider
; output:
;	[wSelectedAttack] = attack index that KOs
;   [wTempCardDeckIndex] = deck index of the card owning the attack
CheckIfAnyAttackCouldKnockOutDefendingCard:
	ld hl, CheckIfAttackCouldKnockOutDefendingCard
	jp FindMatchingAttack


; returns carry if damage dealt from the loaded attack KOs defending Pokémon
; the attack must be usable and have enough energy
; input:
;	[hTempPlayAreaLocation_ff9d] = location of attacking card to consider
;   [wTempCardDeckIndex] = card owning the selected attack
;   [wLoadedAttack] = attack data to consider
CheckIfAttackKnocksOutDefendingCard:
	call CheckIfLoadedAttackIsUnusable
	ccf
	ret nc  ; unusable
	call CheckEnergyNeededForLoadedAttack
	ccf
	ret nc ; not enough energy
	; jr CheckAttackDoesEnoughDamageToKnockOutDefendingCard
	; fallthrough

; returns carry if damage dealt from the loaded attack KOs defending Pokémon
; input:
;	[hTempPlayAreaLocation_ff9d] = location of attacking card to consider
;   [wTempCardDeckIndex] = card owning the selected attack
;   [wLoadedAttack] = attack data to consider
CheckAttackDoesEnoughDamageToKnockOutDefendingCard:
	call EstimateDamageOfLoadedAttack_VersusDefendingCard
	ld a, DUELVARS_ARENA_CARD_HP
	call GetNonTurnDuelistVariable
	ld hl, wDamage
	sub [hl]
	ret c  ; enough damage
	ret nz  ; not enough damage
; exact damage
	scf
	ret


; returns carry if damage dealt from the loaded attack KOs defending Pokémon
; the attack must be usable and have enough energy attached or available in hand
; FIXME: maybe consider also wAIMaxDamage?
; input:
;	[hTempPlayAreaLocation_ff9d] = location of attacking card to consider
;   [wTempCardDeckIndex] = card owning the selected attack
;   [wLoadedAttack] = attack data to consider
CheckIfAttackCouldKnockOutDefendingCard:
	call CheckIfLoadedAttackIsUnusable
	ccf
	ret nc  ; unusable
	call CheckEnergyNeededForLoadedAttack
	jr nc, CheckAttackDoesEnoughDamageToKnockOutDefendingCard
; not enough energy
	call LookForEnergyNeededForLoadedAttackInHand
	ret nc  ; no energy in hand
	; call LoadCardDataToBuffer1_FromDeckIndex
	; call CheckIfLoadedCardCanBePlayed
	jr CheckAttackDoesEnoughDamageToKnockOutDefendingCard


; checks AI scores for all benched Pokémon
; returns the location of the card with highest score
; in a and [hTempPlayAreaLocation_ff9d]
FindHighestBenchScore:
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	ld b, a
	ld c, 0
	ld e, c
	ld d, c
	ld hl, wPlayAreaAIScore + 1
	jp .next ; can be jr

.loop
	ld a, [hli]
	cp e
	jr c, .next
	ld e, a
	ld d, c
.next
	inc c
	dec b
	jr nz, .loop

	ld a, d
	ldh [hTempPlayAreaLocation_ff9d], a
	or a
	ret

; adds a to wAIScore
; if there's overflow, it's capped at 255
; output:
;	a = a + wAIScore (capped at 255)
AIEncourage:
	push hl
	ld hl, wAIScore
	add [hl]
	jr nc, .no_cap
	ld a, 255
.no_cap
	ld [hl], a
	pop hl
	ret

; subs a from wAIScore
; if there's underflow, it's capped at 0
AIDiscourage:
	push hl
	push de
	ld e, a
	ld hl, wAIScore
	ld a, [hl]
	or a
	jr z, .done
	sub e
	ld [hl], a
	jr nc, .done
	ld [hl], 0
.done
	pop de
	pop hl
	ret

; loads defending Pokémon's weakness/resistance
; and the number of prize cards in both sides
LoadDefendingPokemonColorWRAndPrizeCards:
	call SwapTurn
	call GetArenaCardColor
	call TranslateColorToWR
	ld [wAIPlayerColor], a
	call GetArenaCardWeakness
	ld [wAIPlayerWeakness], a
	call GetArenaCardResistance
	ld [wAIPlayerResistance], a
	call CountPrizes
	ld [wAIPlayerPrizeCount], a
	call SwapTurn
	call CountPrizes
	ld [wAIOpponentPrizeCount], a
	ret

; called when AI has chosen its attack.
; executes all effects and damage.
; handles AI choosing parameters for certain attacks as well.
; input:
;   [wSelectedAttack] = attack index chosen by AI
;   [wTempCardDeckIndex] = deck index of the card owning the attack
AITryUseAttack:
	ld a, [wSelectedAttack]
	ldh [hTemp_ffa0], a
; no need to load the attack right now, OPPACTION_BEGIN_ATTACK will do it
	; ld e, a
	; ld a, DUELVARS_ARENA_CARD
	; call GetTurnDuelistVariable
	; ldh [hTempCardIndex_ff9f], a
	; ld d, a
	; call CopyAttackDataAndDamage_FromDeckIndex

; OPPACTION_BEGIN_ATTACK wants the following inputs:
;   [hTemp_ffa0]: the index of the selected attack
;   [wTempCardDeckIndex]: deck index of the card to load attack data from
	ld a, OPPACTION_BEGIN_ATTACK
	bank1call AIMakeDecision
	ret c

	call AISelectSpecialAttackParameters
	jr c, .use_attack
	ld a, EFFECTCMDTYPE_AI_SELECTION
	call TryExecuteEffectCommandFunction

.use_attack
; not sure if we need to reload the attack data here again
; unless some of the special logic overwrites it...
	ld a, [wSelectedAttack]
	ld e, a
; the attack might not belong to the current arena card
	; ld a, DUELVARS_ARENA_CARD
	; call GetTurnDuelistVariable
; hTempCardIndex_ff9f might have been overwritten
	ld a, [wTempCardDeckIndex]
	ld d, a
	call CopyAttackDataAndDamage_FromDeckIndex
	ld a, OPPACTION_USE_ATTACK
	bank1call AIMakeDecision
	ret c

	ld a, EFFECTCMDTYPE_AI_SWITCH_DEFENDING_PKMN
	call TryExecuteEffectCommandFunction
	ld a, OPPACTION_ATTACK_ANIM_AND_DAMAGE
	bank1call AIMakeDecision
	ret


; pick a random Pokemon in the bench.
; output:
;	- a = PLAY_AREA_* of Bench Pokemon picked.
PickRandomBenchPokemon:
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	dec a
	call Random
	inc a
	ret

AIPickPrizeCards:
	ld a, [wNumberPrizeCardsToTake]
	ld b, a
.loop
	call .PickPrizeCard
	ld a, DUELVARS_PRIZES
	call GetTurnDuelistVariable
	or a
	jr z, .done
	dec b
	jr nz, .loop
.done
	ret

; picks a prize card at random
; and adds it to the hand.
.PickPrizeCard:
	ld a, DUELVARS_PRIZES
	call GetTurnDuelistVariable
	push hl
	ld c, a

; choose a random prize card until
; one is found that isn't taken already.
.loop_pick_prize
	ld a, 6
	call Random
	ld e, a
	ld d, $00
	ld hl, .prize_flags
	add hl, de
	ld a, [hl]
	and c
	jr z, .loop_pick_prize ; no prize

; prize card was found
; remove this prize from wOpponentPrizes
	ld a, [hl]
	pop hl
	cpl
	and [hl]
	ld [hl], a

; add this prize card to the hand
	ld a, e
	add DUELVARS_PRIZE_CARDS
	call GetTurnDuelistVariable
	jp AddCardToHand

.prize_flags
	db $1 << 0
	db $1 << 1
	db $1 << 2
	db $1 << 3
	db $1 << 4
	db $1 << 5
	db $1 << 6
	db $1 << 7

; routine for AI to play all Basic cards from its hand
; in the beginning of the Duel.
AIPlayInitialBasicCards:
	call CreateHandCardList
	ld hl, wDuelTempList
.check_for_next_card
	ld a, [hli]
	ldh [hTempCardIndex_ff98], a
	cp $ff
	ret z ; return when done

	call LoadCardDataToBuffer1_FromDeckIndex
	ld a, [wLoadedCard1Type]
	cp TYPE_ENERGY
	jr nc, .check_for_next_card ; skip if not Pokemon card
	ld a, [wLoadedCard1Stage]
	or a
	jr nz, .check_for_next_card ; skip if not Basic Stage

; play Basic card from hand
	push hl
	ldh a, [hTempCardIndex_ff98]
	call PutHandPokemonCardInPlayArea
	pop hl
	jr .check_for_next_card


; returns carry if Pokémon at hTempPlayAreaLocation_ff9d
; can't use an attack or if that selected attack doesn't have enough energy
; input:
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
;	[wSelectedAttack] = selected attack to examine
Old_CheckIfSelectedAttackIsUnusable:  ; FIXME delete
	call CopyAttackDataAndDamage_FromPlayAreaLocation
	jr CheckIfLoadedAttackIsUnusableOrNotEnoughEnergy


; returns carry if Pokémon at hTempPlayAreaLocation_ff9d
; can't use the given attack
; input:
;	[wSelectedAttack] = selected attack to examine
;   [wTempCardDeckIndex] = card owning the selected attack
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
CheckIfSelectedAttackIsUnusable:
	ld a, [wSelectedAttack]
	ld e, a
	; jr CheckIfAttackIsUnusable
	; fallthrough


; returns carry if Pokémon at hTempPlayAreaLocation_ff9d
; can't use the given attack
; input:
;	e = selected attack to examine
;   [wTempCardDeckIndex] = card owning the selected attack
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
; output:
;	[wSelectedAttack] = selected attack to examine
;   [wLoadedCard1] = Pokémon card to check
;   [wLoadedAttack] = attack to check
CheckIfAttackIsUnusable:
	ld a, [wTempCardDeckIndex]
	ld d, a
	call CopyAttackDataAndDamage_FromDeckIndex
	; jr CheckIfLoadedAttackIsUnusable
	; fallthrough

; returns carry if Pokémon at hTempPlayAreaLocation_ff9d can't use an attack
; also returns carry if the selected attack is a Pokémon Power
; input:
;   [wLoadedCard1] = Pokémon card to check
;   [wLoadedAttack] = attack to check
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
;	[wSelectedAttack] = selected attack to examine
CheckIfLoadedAttackIsUnusable:
	ld a, [wLoadedAttackCategory]
	sub POKEMON_POWER
	cp 1
	ret c  ; this is a Pokémon Power

	ldh a, [hTempPlayAreaLocation_ff9d]
	or a
	jr nz, .bench

	call HandleCantAttackSubstatus
	ret c
	call CheckIfActiveCardParalyzedOrAsleep
	ret c
	call HandleAmnesiaSubstatus
	ret c
; FIXME: this should run at the very start, even for Bench Pokémon
; some effects might have to be adjusted to account for location
	ld a, EFFECTCMDTYPE_INITIAL_EFFECT_1
	call TryExecuteEffectCommandFunction
	ret c

.bench
	ld a, ATTACK_FLAG2_ADDRESS | FLAG_2_BIT_5_F
	jp CheckLoadedAttackFlag


; returns carry if Pokémon at hTempPlayAreaLocation_ff9d
; can't use an attack or if that selected attack doesn't have enough energy
; also returns carry if the selected attack is a Pokémon Power
; input:
;   [wLoadedCard1] = Pokémon card to check
;   [wLoadedAttack] = attack to check
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
;	[wSelectedAttack] = selected attack to examine
CheckIfLoadedAttackIsUnusableOrNotEnoughEnergy:
	call CheckIfLoadedAttackIsUnusable
	ret c  ; unusable
	jp CheckEnergyNeededForLoadedAttack


; load selected attack from Pokémon in hTempPlayAreaLocation_ff9d
; and checks if there is enough energy to execute the selected attack
; input:
;	[wSelectedAttack] = selected attack to examine
;   [wTempCardDeckIndex] = card owning the selected attack
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
; output:
;	b = basic energy still needed
;	c = colorless energy still needed
;	de = output of ConvertColorToEnergyCardID, or $0 if not an attack
;	carry set if no attack
;	       OR if it's a Pokémon Power
;	       OR if not enough energy for attack
CheckEnergyNeededForSelectedAttack:
	ld a, [wSelectedAttack]
	ld e, a
	; jr CheckEnergyNeededForAttack
	; fallthrough

; load selected attack from Pokémon in hTempPlayAreaLocation_ff9d
; and checks if there is enough energy to execute the selected attack
; input:
;	e = selected attack to examine
;   [wTempCardDeckIndex] = card owning the selected attack
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
; output:
;	b = basic energy still needed
;	c = colorless energy still needed
;	de = output of ConvertColorToEnergyCardID, or $0 if not an attack
;	carry set if no attack
;	       OR if it's a Pokémon Power
;	       OR if not enough energy for attack
CheckEnergyNeededForAttack:
	ld a, [wTempCardDeckIndex]
	; fallthrough

CheckEnergyNeededForAttackOfCard:
	ld d, a
	call CopyAttackDataAndDamage_FromDeckIndex
	; jr CheckEnergyNeededForLoadedAttack
	; fallthrough

; check if there is enough energy to execute the selected attack
; input:
;	[wLoadedAttack] = selected attack to examine
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
; output:
;	b = basic energy still needed
;	c = colorless energy still needed
;	de = output of ConvertColorToEnergyCardID, or $0 if not an attack
;	carry set if no attack
;	       OR if it's a Pokémon Power
;	       OR if not enough energy for attack
CheckEnergyNeededForLoadedAttack:
	ld hl, wLoadedAttackName
	ld a, [hli]
	or [hl]
	jr z, .no_attack
	ld a, [wLoadedAttackCategory]
	cp POKEMON_POWER
	jr nz, .is_attack
.no_attack
	lb bc, 0, 0
	ld de, 0
	scf
	ret

.is_attack
	ldh a, [hTempPlayAreaLocation_ff9d]
	ld e, a
	call GetPlayAreaCardAttachedEnergies
	call HandleEnergyBurn
	; jr CheckEnergyNeededForLoadedAttackWithAttachedEnergies
	; fallthrough

; check if there is enough energy to execute the selected attack
; input:
;	[wLoadedAttack] = selected attack to examine
;   [wAttachedEnergies] = attached energies of Pokémon
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
; output:
;	b = basic energy still needed
;	c = colorless energy still needed
;	de = output of ConvertColorToEnergyCardID, or $0 if not an attack
;	carry set if no attack
;	       OR if it's a Pokémon Power
;	       OR if not enough energy for attack
CheckEnergyNeededForLoadedAttackWithAttachedEnergies:
	xor a
	ld [wTempLoadedAttackEnergyCost], a
	ld [wTempLoadedAttackEnergyNeededAmount], a
	ld [wTempLoadedAttackEnergyNeededType], a

	ld hl, wAttachedEnergies
	ld de, wLoadedAttackEnergyCost
	ld b, 0
	ld c, (NUM_TYPES / 2) - 1

.loop
	; check all basic energy cards except colorless
	ld a, [de]
	swap a
	call CheckIfEnoughParticularAttachedEnergy
	ld a, [de]
	call CheckIfEnoughParticularAttachedEnergy
	inc de
	dec c
	jr nz, .loop

; running CheckIfEnoughParticularAttachedEnergy back to back like this
; overwrites the results of a previous call of this function,
; however, no attack in the game has energy requirements for two
; different energy types (excluding colorless), so this routine
; will always just return the result for one type of basic energy,
; while all others will necessarily have an energy cost of 0
; if attacks are added to the game with energy requirements of
; two different basic energy types, then this routine only accounts
; for the type with the highest index

; colorless
	ld a, [de]
	swap a
	and %00001111
	ld b, a ; colorless energy still needed
	ld a, [wTempLoadedAttackEnergyCost]
	ld hl, wTempLoadedAttackEnergyNeededAmount
	sub [hl]
	ld c, a ; basic energy still needed
	ld a, [wTotalAttachedEnergies]
	sub c
	sub b
	jr c, .not_enough

	ld a, [wTempLoadedAttackEnergyNeededAmount]
	or a
	ret z

; being here means the energy cost isn't satisfied,
; including with colorless energy
	xor a
.not_enough
	cpl
	inc a
	ld c, a ; colorless energy still needed
	ld a, [wTempLoadedAttackEnergyNeededAmount]
	ld b, a ; basic energy still needed
	ld a, [wTempLoadedAttackEnergyNeededType]
	call ConvertColorToEnergyCardID
	scf
	ret


; takes as input the energy cost of an attack for a
; particular energy, stored in the lower nibble of a
; if the attack costs some amount of this energy, the lower nibble of a != 0,
; and this amount is stored in wTempLoadedAttackEnergyCost
; sets carry flag if not enough energy of this type attached
; input:
;	a    = this energy cost of attack (lower nibble)
;	[hl] = attached energy
; output:
;	carry set if not enough of this energy type attached
CheckIfEnoughParticularAttachedEnergy:
	and %00001111
	jr nz, .check
.has_enough
	inc hl
	inc b
	or a
	ret

.check
	ld [wTempLoadedAttackEnergyCost], a
	sub [hl]
	jr z, .has_enough
	jr c, .has_enough

	; not enough energy
	ld [wTempLoadedAttackEnergyNeededAmount], a
	ld a, b
	ld [wTempLoadedAttackEnergyNeededType], a
	inc hl
	inc b
	scf
	ret

; input:
;	a = energy type
; output:
;	de = energy card ID
ConvertColorToEnergyCardID:
	push hl
	ld e, a
	ld d, 0
	ld hl, .card_id
	add hl, de
	add hl, de
	ld e, [hl]
	inc hl
	ld d, [hl]
	pop hl
	ret

.card_id
	dw FIRE_ENERGY
	dw GRASS_ENERGY
	dw LIGHTNING_ENERGY
	dw WATER_ENERGY
	dw FIGHTING_ENERGY
	dw PSYCHIC_ENERGY
	dw DOUBLE_COLORLESS_ENERGY


; return carry depending on card index in a:
;	- if energy card, return carry if an energy card has been played already
;	- if basic Pokémon card, return carry if there's no space in bench
;	- if evolution card, return carry if there's no Pokémon in Play to evolve
;	- if trainer card, return carry if it cannot be used
; input:
;	a = card index to check
CheckIfCardCanBePlayed:
	ldh [hTempCardIndex_ff9f], a
	call LoadCardDataToBuffer1_FromDeckIndex

CheckIfLoadedCardCanBePlayed:
	ld a, [wLoadedCard1Type]
	cp TYPE_ENERGY
	jr c, .pokemon_card
	cp TYPE_TRAINER
	jr z, .trainer_card

; energy card
	ld hl, wLoadedCard1ID
	cphl WATER_ENERGY
	jr nz, .energy
	call IsRainDanceActive
	ccf
	ret nc  ; Rain Dance active, can always play Water energy
.energy
	ld a, [wAlreadyPlayedEnergy]
	or a
	ret z
	scf
	ret

.pokemon_card
	ld a, [wLoadedCard1Stage]
	or a
	jr nz, .evolution_card
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	cp MAX_PLAY_AREA_POKEMON
	ccf
	ret

.evolution_card
	bank1call IsPrehistoricPowerActive
	ret c
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	ld c, a
	ld b, 0
.loop
	push bc
	ld e, b
	ldh a, [hTempCardIndex_ff9f]
	ld d, a
	call CheckIfCanEvolveInto
	pop bc
	ret nc
	inc b
	dec c
	jr nz, .loop
	scf
	ret

.trainer_card
	bank1call CheckCantUseTrainerDueToEffect
	ret c
	call LoadNonPokemonCardEffectCommands
	ld a, EFFECTCMDTYPE_INITIAL_EFFECT_1
	jp TryExecuteEffectCommandFunction

; loads all the energy cards
; in hand in wDuelTempList
; return carry if no energy cards found
CreateEnergyCardListFromHand:
	push hl
	push de
	push bc
	ld de, wDuelTempList
	ld b, a
	ld a, DUELVARS_NUMBER_OF_CARDS_IN_HAND
	call GetTurnDuelistVariable
	ld c, a
	inc c
	ld l, DUELVARS_HAND
	jr .decrease

.loop
	ld a, [hli]
	push de
	call GetCardIDFromDeckIndex
	call GetCardType
	pop de
	and TYPE_ENERGY
	jr z, .decrease
	dec hl
	ld a, [hli]
	ld [de], a
	inc de
.decrease
	dec c
	jr nz, .loop

	ld a, $ff
	ld [de], a
	pop bc
	pop de
	pop hl
	ld a, [wDuelTempList]
	cp $ff
	ccf
	ret

; looks for card ID in hand and
; sets carry if a card wasn't found
; as opposed to LookForCardIDInHandList_Bank5
; this function doesn't create a list
; and preserves hl, de and bc
; input:
;	de = card ID
; output:
;	a = card deck index, if found
;	carry set if NOT found
LookForCardIDInHand:
	push hl
	push de
	push bc
	ld b, d
	ld c, e
	ld a, DUELVARS_NUMBER_OF_CARDS_IN_HAND
	call GetTurnDuelistVariable
	ld e, a
	inc e
	ld l, DUELVARS_HAND
	jr .next

.loop
	ld a, [hli]
	push de
	call GetCardIDFromDeckIndex
	call CompareDEtoBC
	pop de
	jr z, .no_carry
.next
	dec e
	jr nz, .loop

	pop bc
	pop de
	pop hl
	scf
	ret

.no_carry
	dec hl
	ld a, [hl]
	pop bc
	pop de
	pop hl
	or a
	ret

INCLUDE "engine/duel/ai/damage_calculation.asm"

AIProcessHandTrainerCards:
	farcall _AIProcessHandTrainerCards
	ret

INCLUDE "engine/duel/ai/deck_ai.asm"

; return carry if card ID loaded in a is found in hand
; and outputs in a the deck index of that card
; as opposed to LookForCardIDInHand, this function
; creates a list in wDuelTempList
; input:
;	de = card ID
; output:
;	a = card deck index, if found
;	carry set if found
LookForCardIDInHandList_Bank5:
	push de
	call CreateHandCardList
	pop de
	ld hl, wDuelTempList

.loop
	ld a, [hli]
	cp $ff
	ret z
	ldh [hTempCardIndex_ff98], a
	call LoadCardDataToBuffer1_FromDeckIndex
	push bc
	ld a, [wLoadedCard1ID + 0]
	ld c, a
	ld a, [wLoadedCard1ID + 1]
	ld b, a
	call CompareDEtoBC
	pop bc
	jr nz, .loop

	ldh a, [hTempCardIndex_ff98]
	scf
	ret

; returns carry if card ID in a
; is found in Play Area, starting with
; location in b
; input:
;	de = card ID
;	b = PLAY_AREA_* to start with
; output:
;	a = PLAY_AREA_* of found card
;	carry set if found
LookForCardIDInPlayArea_Bank5:
.loop
	ld a, DUELVARS_ARENA_CARD
	add b
	call GetTurnDuelistVariable
	cp $ff
	ret z
	call LoadCardDataToBuffer1_FromDeckIndex
	push bc
	ld a, [wLoadedCard1ID + 0]
	ld c, a
	ld a, [wLoadedCard1ID + 1]
	ld b, a
	call CompareDEtoBC
	pop bc
	jr z, .found

	inc b
	ld a, MAX_PLAY_AREA_POKEMON
	cp b
	jr nz, .loop

; not found
	ld b, $ff
	or a
	ret

.found
	ld a, b
	scf
	ret

; check if energy card ID in e is in AI hand and,
; if so, attaches it to card ID in d in Play Area.
; input:
;	de = Energy card ID
;	bc = Pokemon card ID
AIAttachEnergyInHandToCardInPlayArea:
	call LookForCardIDInHandList_Bank5
	ret nc ; not in hand
	ldh [hTemp_ffa0], a
	ld d, b
	ld e, c
	ld b, PLAY_AREA_ARENA

.attach
	call LookForCardIDInPlayArea_Bank5
	ldh [hTempPlayAreaLocation_ffa1], a
	ld a, OPPACTION_PLAY_ENERGY
	bank1call AIMakeDecision
	ret

; same as AIAttachEnergyInHandToCardInPlayArea but
; only look for card ID in the Bench.
AIAttachEnergyInHandToCardInBench:
	call LookForCardIDInHandList_Bank5
	ret nc
	ldh [hTemp_ffa0], a
	ld d, b
	ld e, c
	ld b, PLAY_AREA_BENCH_1
	jr AIAttachEnergyInHandToCardInPlayArea.attach

INCLUDE "engine/duel/ai/init.asm"


; zeroes a bytes starting from hl.
; this function is identical to 'ClearMemory_Bank2',
; 'ClearMemory_Bank6' and 'ClearMemory_Bank8'.
; preserves all registers
; input:
;	a = number of bytes to clear
;	hl = where to begin erasing
ClearMemory_Bank5:
	push af
	push bc
	push hl
	ld b, a
	xor a
.clear_loop
	ld [hli], a
	dec b
	jr nz, .clear_loop
	pop hl
	pop bc
	pop af
	ret

; converts an HP value or amount of damage to the number of equivalent damage counters
; preserves all registers except af
; input:
;	a = HP value to convert
; output:
;	a = number of damage counters
ConvertHPToDamageCounters_Bank5:
	push bc
	ld c, 0
.loop
	sub 10
	jr c, .done
	inc c
	jr .loop
.done
	ld a, c
	pop bc
	ret

; returns in a the result of
; dividing b by a, rounded down
; input:
;	a = divisor
;	b = dividend
CalculateBDividedByA_Bank5:
	push bc
	ld c, a
	ld a, b
	ld b, c
	ld c, 0
.loop
	sub b
	jr c, .done
	inc c
	jr .loop
.done
	ld a, c
	pop bc
	ret

; returns in a the number of energy cards attached
; to Pokémon in location held by e
; this assumes that colorless are paired so
; that one colorless energy card provides 2 colorless energy
; input:
;	e = location to check, i.e. PLAY_AREA_*
; output:
;	a = number of energy cards attached
CountNumberOfEnergyCardsAttached:
	call GetPlayAreaCardAttachedEnergies
	ld a, [wTotalAttachedEnergies]
	or a
	ret z

	xor a
	push hl
	push bc
	ld b, NUM_COLORED_TYPES
	ld hl, wAttachedEnergies
; sum all the attached energies
.loop
	add [hl]
	inc hl
	dec b
	jr nz, .loop

	ld b, [hl]
	srl b
; counts colorless and halves it
	add b
	pop bc
	pop hl
	ret

; returns carry if any card with ID in de is found
; in card location in a
; input:
;	a = CARD_LOCATION_* constant
;	de = card ID to look for
; output:
;	a & e = deck index of a matching card, if any
;	carry set if found
LookForCardIDInLocation_Bank5:
	ld b, d
	ld c, e
	ld d, a
	ld e, 0
.loop
	ld a, DUELVARS_CARD_LOCATIONS
	add e
	call GetTurnDuelistVariable
	cp d
	jr nz, .next
	ld a, e
	push de
	call GetCardIDFromDeckIndex
	call CompareDEtoBC
	pop de
	jr z, .found
.next
	inc e
	ld a, DECK_SIZE
	cp e
	jr nz, .loop

; not found
	or a
	ret
.found
	ld a, e
	scf
	ret

; counts total number of energy cards in opponent's hand
; plus all the cards attached in Turn Duelist's Play Area.
; output:
;	a and wTempAI = total number of energy cards.
CountOppEnergyCardsInHandAndAttached:
	xor a
	ld [wTempAI], a
	call CreateEnergyCardListFromHand
	jr c, .attached

; counts number of energy cards in hand
	ld b, -1
	ld hl, wDuelTempList
.loop_hand
	inc b
	ld a, [hli]
	cp $ff
	jr nz, .loop_hand
	ld a, b
	ld [wTempAI], a

; counts number of energy cards
; that are attached in Play Area
.attached
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	ld d, a
	ld e, PLAY_AREA_ARENA
.loop_play_area
	call CountNumberOfEnergyCardsAttached
	ld hl, wTempAI
	add [hl]
	ld [hl], a
	inc e
	dec d
	jr nz, .loop_play_area
	ret

; returns carry if any card with ID in de is found
; in the list that is pointed by hl.
; if one is found, it is removed from the list.
; input:
;   de  = card ID to look for.
;   hl = list to look in
RemoveCardIDInList:
	push hl
	push de
	push bc
	ld b, d
	ld c, e

.loop_1
	ld a, [hli]
	cp $ff
	jr z, .no_carry

	ldh [hTempCardIndex_ff98], a
	call GetCardIDFromDeckIndex
	call CompareDEtoBC
	jr nz, .loop_1

; found
	ld d, h
	ld e, l
	dec hl

; remove this index from the list
; and reposition the rest of the list ahead.
.loop_2
	ld a, [de]
	inc de
	ld [hli], a
	cp $ff
	jr nz, .loop_2

	ldh a, [hTempCardIndex_ff98]
	pop bc
	pop de
	pop hl
	scf
	ret

.no_carry
	pop bc
	pop de
	pop hl
	or a
	ret

; play Pokemon cards from the hand to set the starting
; Play Area of Boss decks.
; each Boss deck has two ID lists in order of preference.
; one list is for the Arena card is the other is for the Bench cards.
; if Arena card could not be set (due to hand not having any card in its list)
; or if list is null, return carry and do not play any cards.
TrySetUpBossStartingPlayArea:
	ld de, wAICardListArenaPriority
	ld a, d
	or a
	jr z, .set_carry ; return if null

; pick Arena card
	call CreateHandCardList
	ld hl, wDuelTempList
	ld de, wAICardListArenaPriority
	call .PlayPokemonCardInOrder
	ret c

; play Pokemon cards to Bench until there are
; a maximum of 3 cards in Play Area.
.loop
	ld de, wAICardListBenchPriority
	call .PlayPokemonCardInOrder
	jr c, .done
	cp 3
	jr c, .loop

.done
	or a
	ret
.set_carry
	scf
	ret

; runs through input card ID list in de.
; plays to Play Area first card that is found in hand.
; returns carry if none of the cards in the list are found.
; returns number of Pokemon in Play Area in a.
.PlayPokemonCardInOrder
	ld a, [de]
	ld c, a
	inc de
	ld a, [de]
	ld d, a
	ld e, c

; go in order of the list in de and
; add first card that matches ID.
; returns carry if hand doesn't have any card in list.
.loop_id_list
	ld a, [de]
	inc de
	ld c, a
	ld a, [de]
	or c
	jr z, .not_found
	push de
	ld a, [de]
	ld e, c
	ld d, a
	call RemoveCardIDInList
	pop de
	inc de
	jr nc, .loop_id_list

	; play this card to Play Area and return
	push hl
	call PutHandPokemonCardInPlayArea
	pop hl
	or a
	ret

.not_found
	scf
	ret

INCLUDE "engine/duel/ai/retreat.asm"

; copies an $ff-terminated list from hl to de.
; preserves bc
; input:
;	hl = address from which to start copying the data
;	de = where to copy the data
CopyListWithFFTerminatorFromHLToDE_Bank5:
	ld a, [hli]
	ld [de], a
	cp $ff
	ret z
	inc de
	jr CopyListWithFFTerminatorFromHLToDE_Bank5

INCLUDE "engine/duel/ai/hand_pokemon.asm"

; check if player's active Pokémon is Mr Mime
; if it isn't, set carry
; if it is, check if Pokémon at a
; can damage it, and if it can, set carry
; input:
;	a = location of Pokémon card
CheckDamageToMrMime:
	push af
	ld a, DUELVARS_ARENA_CARD
	call GetNonTurnDuelistVariable
	call SwapTurn
	call GetCardIDFromDeckIndex
	call SwapTurn
	cp16 MR_MIME
	pop bc
	jr nz, .set_carry
	ld a, b
	call CheckIfCanDamageDefendingPokemon
	jr c, .set_carry
	or a
	ret
.set_carry
	scf
	ret


; outputs carry if any of the active Pokémon attacks
; can be used and are not residual
CheckIfActivePokemonCanUseAnyNonResidualAttack:
	xor a ; PLAY_AREA_ARENA
	ldh [hTempPlayAreaLocation_ff9d], a
	ld hl, CheckIfLoadedAttackIsUsableAndNonResidual
	jp FindMatchingAttack

CheckIfLoadedAttackIsUsableAndNonResidual:
	call CheckIfLoadedAttackIsUnusableOrNotEnoughEnergy
	ccf
	ret nc  ; unusable or not enough energy
	ld a, [wLoadedAttackCategory]
	and RESIDUAL
	ret nz  ; fail
	scf
	ret


; looks for energy card(s) in hand depending on
; what is needed for selected card and attack
;	- if one basic energy is required, look for that energy;
;	- if one colorless is required, create a list at wDuelTempList
;	  of all energy cards;
;	- if two colorless are required, look for double colorless;
; return carry if successful in finding card and if card is playable
; input:
;	[hTempPlayAreaLocation_ff9d] = location of Pokémon card
;	[wLoadedAttack] = data of selected attack to examine
;	b = basic energy still needed
;	c = colorless energy still needed
;	de = output of ConvertColorToEnergyCardID, or $0 if not an attack
LookForEnergyNeededForLoadedAttackInHand:
	ld a, d
	or e
	ret z  ; return if no attack
	ld a, b
	add c
	cp 1
	jr z, .one_energy
	cp 2
	jr nz, .done
	ld a, c
	cp 2
	jr z, .two_colorless
.done
	or a
	ret

.one_energy
	ld a, b
	or a
	jr z, .one_colorless
.check_specific_energy
	call LookForCardIDInHandList_Bank5
	ret nc  ; not found
; assume: energy card is in wLoadedCard1
	call CheckIfLoadedCardCanBePlayed
	ccf
	ret ; carry if energy can be played

.one_colorless
	call CreateEnergyCardListFromHand
	jr c, .done
; this is a loop only because Rain Dance exists
; a Water Energy might be in the list somewhere
	ld hl, wDuelTempList
.loop_cards
	ld a, [hli]
	cp $ff
	ret z  ; no more energies
	call LoadCardDataToBuffer1_FromDeckIndex
	push hl
	call CheckIfLoadedCardCanBePlayed
	pop hl
	ccf
	ret c  ; energy can be played
	jr .loop_cards

.two_colorless
	ld de, DOUBLE_COLORLESS_ENERGY
	jr .check_specific_energy


; goes through $00 terminated list pointed
; by wAICardListPlayFromHandPriority and compares it to each card in hand.
; Sorts the hand in wDuelTempList so that the found card IDs
; are in the same order as the list pointed by de.
SortTempHandByIDList:
	ld a, [wAICardListPlayFromHandPriority+1]
	or a
	ret z ; return if list is empty

; start going down the ID list
	ld d, a
	ld a, [wAICardListPlayFromHandPriority]
	ld e, a
	ld bc, 0
.loop_list_id
; get this item's ID
; if $00, list has ended
	push hl
	ld h, d
	ld l, e
	ld a, [hli]
	or [hl]
	pop hl
	ret z ; return when list is over
	inc de
	ld hl, wDuelTempList
	add hl, bc

; search in the hand card list
.next_hand_card
	ld a, [hl]
	ldh [hTempCardIndex_ff98], a
	cp -1
	jr z, .loop_list_id
	push bc
	push de
	ld a, [de]
	inc de
	ld c, a
	ld a, [de]
	inc de
	ld b, a
	ldh a, [hTempCardIndex_ff98]
	call GetCardIDFromDeckIndex
	call CompareDEtoBC
	pop de
	pop bc
	jr nz, .not_same

; found
; swap this hand card with the spot
; in hand corresponding to c
	push bc
	push hl
	ld hl, wDuelTempList
	add hl, bc
	ld b, [hl]
	ldh a, [hTempCardIndex_ff98]
	ld [hl], a
	pop hl
	ld [hl], b
	pop bc
	inc c
.not_same
	inc hl
	jr .next_hand_card

; looks for energy card(s) in list at wDuelTempList
; depending on energy flags set in a
; return carry if successful in finding card
; input:
;	a = energy flags needed
CheckEnergyFlagsNeededInList:
	ld c, a
	ld hl, wDuelTempList
.loop_cards
	ld a, [hli]
	cp $ff
	jr z, .no_carry
	call GetCardIDFromDeckIndex

; fire
	cp16 FIRE_ENERGY
	jr nz, .grass
	ld a, FIRE_F
	jr .check_energy
.grass
	cp16 GRASS_ENERGY
	jr nz, .lightning
	ld a, GRASS_F
	jr .check_energy
.lightning
	cp16 LIGHTNING_ENERGY
	jr nz, .water
	ld a, LIGHTNING_F
	jr .check_energy
.water
	cp16 WATER_ENERGY
	jr nz, .fighting
	ld a, WATER_F
	jr .check_energy
.fighting
	cp16 FIGHTING_ENERGY
	jr nz, .psychic
	ld a, FIGHTING_F
	jr .check_energy
.psychic
	cp16 PSYCHIC_ENERGY
	jr nz, .colorless
	ld a, PSYCHIC_F
	jr .check_energy
.colorless
	cp16 DOUBLE_COLORLESS_ENERGY
	jr nz, .loop_cards
	ld a, COLORLESS_F

; if energy card matches required energy, return carry
.check_energy
	and c
	jr z, .loop_cards
	scf
	ret
.no_carry
	or a
	ret

; returns in a the energy cost of both attacks from card index in a
; represented by energy flags
; i.e. each bit represents a different energy type cost
; if any colorless energy is required, all bits are set
; input:
;	a = card index
; output:
;	a = bits of each energy requirement
GetAttacksEnergyCostBits:
	call LoadCardDataToBuffer2_FromDeckIndex
	ld hl, wLoadedCard2Atk1EnergyCost
	call .GetEnergyCostBits
	ld b, a

	push bc
	ld hl, wLoadedCard2Atk2EnergyCost
	call .GetEnergyCostBits
	pop bc
	or b
	ret

; returns in a the energy cost of an attack in [hl]
; represented by energy flags
; i.e. each bit represents a different energy type cost
; if any colorless energy is required, all bits are set
; input:
;	[hl] = Loaded card attack energy cost
; output:
;	a = bits of each energy requirement
.GetEnergyCostBits:
	ld c, $00
	ld a, [hli]
	ld b, a

; fire
	and $f0
	jr z, .grass
	ld c, FIRE_F
.grass
	ld a, b
	and $0f
	jr z, .lightning
	ld a, GRASS_F
	or c
	ld c, a
.lightning
	ld a, [hli]
	ld b, a
	and $f0
	jr z, .water
	ld a, LIGHTNING_F
	or c
	ld c, a
.water
	ld a, b
	and $0f
	jr z, .fighting
	ld a, WATER_F
	or c
	ld c, a
.fighting
	ld a, [hli]
	ld b, a
	and $f0
	jr z, .psychic
	ld a, FIGHTING_F
	or c
	ld c, a
.psychic
	ld a, b
	and $0f
	jr z, .colorless
	ld a, PSYCHIC_F
	or c
	ld c, a
.colorless
	ld a, [hli]
	ld b, a
	and $f0
	jr z, .done
	ld c, %11111111
.done
	ld a, c
	ret

; set carry flag if any card in
; wDuelTempList evolves card index in a
; if found, the evolution card index is returned in a
; input:
;	a = card index to check evolution
; output:
;	a = card index of evolution found
CheckForEvolutionInList:
	ld b, a
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable

	push af
	ld [hl], b
	ld hl, wDuelTempList
.loop
	ld a, [hli]
	cp $ff
	jr z, .no_carry
	ld d, a
	ld e, PLAY_AREA_ARENA
	push de
	push hl
	call CheckIfCanEvolveInto
	pop hl
	pop de
	jr c, .loop

	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	pop af
	ld [hl], a
	ld a, d
	scf
	ret

.no_carry
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	pop af
	ld [hl], a
	or a
	ret

; set carry if it finds an evolution for
; the card index in a in the deck
; if found, return that evolution card index in a
; input:
;	a = card index to check evolution
; output:
;	a = card index of evolution found
CheckForEvolutionInDeck:
	ld b, a
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable

	push af
	ld [hl], b
	ld e, 0
.loop
	ld a, DUELVARS_CARD_LOCATIONS
	add e
	call GetTurnDuelistVariable
	cp CARD_LOCATION_DECK
	jr nz, .not_in_deck
	push de
	ld d, e
	ld e, PLAY_AREA_ARENA
	call CheckIfCanEvolveInto
	pop de
	jr nc, .set_carry

; exit when it gets to the prize cards
.not_in_deck
	inc e
	ld a, DUELVARS_PRIZE_CARDS
	cp e
	jr nz, .loop

	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	pop af
	ld [hl], a
	or a
	ret

.set_carry
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	pop af
	ld [hl], a
	ld a, e
	scf
	ret

INCLUDE "engine/duel/ai/energy.asm"

INCLUDE "engine/duel/ai/attacks.asm"

INCLUDE "engine/duel/ai/special_attacks.asm"

; checks in other Play Area for non-basic cards.
; afterwards, that card is checked for damage,
; and if the damage counters it has is greater than or equal
; to the max HP of the card stage below it,
; return carry and that card's Play Area location in a.
; output:
;	a = card location of Pokémon card, if found;
;	carry set if such a card is found.
LookForCardThatIsKnockedOutOnDevolution:
	ldh a, [hTempPlayAreaLocation_ff9d]
	push af
	call SwapTurn
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	ld b, a
	ld c, PLAY_AREA_ARENA

.loop
	ld a, c
	ldh [hTempPlayAreaLocation_ff9d], a
	push bc
	call GetCardOneStageBelow
	pop bc
	jr c, .next
	; is not a basic card
	; compare its HP with current damage
	ld a, d
	push bc
	call LoadCardDataToBuffer2_FromDeckIndex
	pop bc
	ld a, [wLoadedCard2HP]
	ld [wTempAI], a
	ld e, c
	push bc
	call GetCardDamageAndMaxHP
	pop bc
	ld e, a
	ld a, [wTempAI]
	cp e
	jr c, .set_carry
	jr z, .set_carry
.next
	inc c
	ld a, c
	cp b
	jr nz, .loop

	call SwapTurn
	pop af
	ldh [hTempPlayAreaLocation_ff9d], a
	or a
	ret

.set_carry
	call SwapTurn
	pop af
	ldh [hTempPlayAreaLocation_ff9d], a
	ld a, c
	scf
	ret

; returns carry if the following conditions are met:
;	- arena card HP >= half max HP
;	- arena card cannot potentially evolve
;	- arena card can use second attack
CheckIfArenaCardIsFullyPowered:
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld d, a
	push de
	call LoadCardDataToBuffer1_FromDeckIndex
	ld a, DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	ld d, a
	ld a, [wLoadedCard1HP]
	rrca
	cp d
	pop de
	jr nc, .no_carry

	ld a, [wLoadedCard1AIInfo]
	and HAS_EVOLUTION
	jr z, .check_second_attack
	ld a, d
	call CheckCardEvolutionInHandOrDeck
	jr c, .no_carry

.check_second_attack
	xor a ; PLAY_AREA_ARENA
	ldh [hTempPlayAreaLocation_ff9d], a
	ld a, SECOND_ATTACK
	ld [wSelectedAttack], a
	push hl
	call Old_CheckIfSelectedAttackIsUnusable
	pop hl
	jr c, .no_carry
	scf
	ret
.no_carry
	or a
	ret

; count Pokemon in the Bench that
; meet the following conditions:
;	- card HP > half max HP
;	- card Unknown2's 4 bit is not set or
;	  is set but there's no evolution of card in hand/deck
;	- card can use second attack
; Outputs the number of Pokémon in bench
; that meet these requirements in a
; and returns carry if at least one is found
CountNumberOfSetUpBenchPokemon:
	ldh a, [hTempPlayAreaLocation_ff9d]
	ld d, a
	ld a, [wSelectedAttack]
	ld e, a
	push de
	ld a, DUELVARS_BENCH
	call GetTurnDuelistVariable
	lb bc, 0, 0
	push hl

.next
	inc c
	pop hl
	ld a, [hli]
	push hl
	cp $ff
	jr z, .done

	ld d, a
	push de
	push bc
	call LoadCardDataToBuffer1_FromDeckIndex
	pop bc

; compares card's current HP with max HP
	ld a, c
	add DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	ld d, a
	ld a, [wLoadedCard1HP]
	rrca

; a = max HP / 2
; d = current HP
; jumps if (current HP) <= (max HP / 2)
	cp d
	pop de
	jr nc, .next

	ld a, [wLoadedCard1AIInfo]
	and HAS_EVOLUTION
	jr z, .check_second_attack
	ld a, d
	push bc
	call CheckCardEvolutionInHandOrDeck
	pop bc
	jr c, .next

.check_second_attack
	ld a, c
	ldh [hTempPlayAreaLocation_ff9d], a
	; bug, there is an assumption that the card
	; has a second attack, but it may be the case
	; that it doesn't, which will return carry
	ld a, SECOND_ATTACK
	ld [wSelectedAttack], a
	push bc
	push hl
	call Old_CheckIfSelectedAttackIsUnusable
	pop hl
	pop bc
	jr c, .next
	inc b
	jr .next

.done
	pop hl
	pop de
	ld a, e
	ld [wSelectedAttack], a
	ld a, d
	ldh [hTempPlayAreaLocation_ff9d], a
	ld a, b
	or a
	ret z
	scf
	ret

; handles AI logic to determine some selections regarding certain attacks,
; if any of these attacks were chosen to be used.
; returns carry if selection was successful,
; and no carry if unable to make one.
; outputs in hTempPlayAreaLocation_ffa1 the chosen parameter.
AISelectSpecialAttackParameters:
	ld a, [wSelectedAttack]
	push af
	call .SelectAttackParameters
	pop bc
	ld a, b
	ld [wSelectedAttack], a
	ret

.SelectAttackParameters:
; the attack might not belong to the current arena card
	; ld a, DUELVARS_ARENA_CARD
	; call GetTurnDuelistVariable
	ldh a, [hTempCardIndex_ff9f]
	call GetCardIDFromDeckIndex
	cp16 MEW_LV23
	jr z, .DevolutionBeam
	cp16 MEWTWO_ALT_LV60
	jr z, .EnergyAbsorption
	cp16 MEWTWO_LV60
	jr z, .EnergyAbsorption
	cp16 EXEGGUTOR
	jr z, .Teleport
	cp16 ELECTRODE_LV35
	jr z, .EnergySpike
	; fallthrough

.no_carry
	or a
	ret

.DevolutionBeam
; in case selected attack is Devolution Beam
; store in hTempPlayAreaLocation_ffa1
; the location of card to select to devolve
	ld a, [wSelectedAttack]
	or a
	jp z, .no_carry ; can be jr

	ld a, $01 ; always target the Player's play area
	ldh [hTemp_ffa0], a
	call LookForCardThatIsKnockedOutOnDevolution
	ldh [hTempPlayAreaLocation_ffa1], a

.set_carry_1
	scf
	ret

.EnergyAbsorption
; in case selected attack is Energy Absorption
; make list from energy cards in Discard Pile
	ld a, [wSelectedAttack]
	or a
	jp nz, .no_carry  ; can be jr

	ld a, $ff
	ldh [hTempPlayAreaLocation_ffa1], a
	ldh [hTempRetreatCostCards], a

; search for Psychic energy cards in Discard Pile
	ld de, PSYCHIC_ENERGY
	ld a, CARD_LOCATION_DISCARD_PILE
	call LookForCardIDInLocation_Bank5
	ldh [hTemp_ffa0], a
	farcall CreateEnergyCardListFromDiscardPile_AllEnergy

; find any energy card different from
; the one found by LookForCardIDInLocation_Bank5.
; since using this attack requires a Psychic energy card,
; and another one is in hTemp_ffa0,
; then any other energy card would account
; for the Energy Cost of Psyburn.
	ld hl, wDuelTempList
.loop_energy_cards
	ld a, [hli]
	cp $ff
	jr z, .set_carry_2
	ld b, a
	ldh a, [hTemp_ffa0]
	cp b
	jr z, .loop_energy_cards ; same card, keep looking

; store the deck index of energy card found
	ld a, b
	ldh [hTempPlayAreaLocation_ffa1], a

.set_carry_2
	scf
	ret

.Teleport
; in case selected attack is Teleport
; decide Bench card to switch to.
	ld a, [wSelectedAttack]
	or a
	jp nz, .no_carry  ; can be jr
	call AIDecideBenchPokemonToSwitchTo
	jr c, .no_carry
	ldh [hTemp_ffa0], a
	scf
	ret

.EnergySpike
; in case selected attack is Energy Spike
; decide basic energy card to fetch from Deck.
	ld a, [wSelectedAttack]
	or a
	jp z, .no_carry  ; can be jr

; if none were found in Deck, return carry...
	ld a, CARD_LOCATION_DECK
	ld de, LIGHTNING_ENERGY

; if none were found in Deck, return carry...
	call LookForCardIDInLocation_Bank5
	ldh [hTemp_ffa0], a
	jp nc, .no_carry  ; can be jr

; ...else find a suitable Play Area Pokemon to
; attach the energy card to.
	call AIProcessButDontPlayEnergy_SkipEvolution
	jp nc, .no_carry  ; can be jr
	ldh a, [hTempPlayAreaLocation_ff9d]
	ldh [hTempPlayAreaLocation_ffa1], a
	scf
	ret

; return carry if Pokémon at the given play area location
; does not have energy required for the loaded attack data
; or has exactly the same amount of energy needed
; input:
;	[wLoadedAttack] = attack data to check
;	[hTempPlayAreaLocation_ff9d] = play area location
; output:
;	a = number of extra energy cards attached
CheckIfNoSurplusEnergyForLoadedAttack:
	; ldh a, [hTempPlayAreaLocation_ff9d]
	; add DUELVARS_ARENA_CARD
	; call GetTurnDuelistVariable
	; ld d, a
	; ld a, [wSelectedAttack]
	; ld e, a
	; call CopyAttackDataAndDamage_FromDeckIndex
	ld hl, wLoadedAttackName
	ld a, [hli]
	or [hl]
	jr z, .not_attack
	ld a, [wLoadedAttackCategory]
	cp POKEMON_POWER
	jr nz, .is_attack
.not_attack
	scf
	ret

.is_attack
	ldh a, [hTempPlayAreaLocation_ff9d]
	ld e, a
	call GetPlayAreaCardAttachedEnergies
	call HandleEnergyBurn
	xor a
	ld [wTempLoadedAttackEnergyCost], a
	ld [wTempLoadedAttackEnergyNeededAmount], a
	ld [wTempLoadedAttackEnergyNeededType], a
	ld hl, wAttachedEnergies
	ld de, wLoadedAttackEnergyCost
	ld b, 0
	ld c, (NUM_TYPES / 2) - 1
.loop
	; check all basic energy cards except colorless
	ld a, [de]
	swap a
	call CalculateParticularAttachedEnergyNeeded
	ld a, [de]
	call CalculateParticularAttachedEnergyNeeded
	inc de
	dec c
	jr nz, .loop

	; colorless
	ld a, [de]
	swap a
	and %00001111
	ld b, a
	ld hl, wTempLoadedAttackEnergyCost
	ld a, [wTotalAttachedEnergies]
	sub [hl]
	sub b
	ret c ; return if not enough energy

	or a
	ret nz ; return if surplus energy

	; exactly the amount of energy needed
	scf
	ret

; takes as input the energy cost of an attack for a
; particular energy, stored in the lower nibble of a
; if the attack costs some amount of this energy, the lower nibble of a != 0,
; and this amount is stored in wTempLoadedAttackEnergyCost
; also adds the amount of energy still needed
; to wTempLoadedAttackEnergyNeededAmount
; input:
;	a    = this energy cost of attack (lower nibble)
;	[hl] = attached energy
; output:
;	carry set if not enough of this energy type attached
CalculateParticularAttachedEnergyNeeded:
	and %00001111
	jr nz, .check
.done
	inc hl
	inc b
	ret

.check
	ld [wTempLoadedAttackEnergyCost], a
	sub [hl]
	jr z, .done
	jr nc, .done
	push bc
	ld a, [wTempLoadedAttackEnergyCost]
	ld b, a
	ld a, [hl]
	sub b
	pop bc
	ld [wTempLoadedAttackEnergyNeededAmount], a
	jr .done

; return carry if there is a card that
; can evolve a Pokémon in hand or deck.
; input:
;	a = deck index of card to check;
; output:
;	a = deck index of evolution in hand, if found;
;	carry set if there's a card in hand that can evolve.
CheckCardEvolutionInHandOrDeck:
	ld b, a
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	push af
	ld [hl], b
	ld e, 0

.loop
	ld a, DUELVARS_CARD_LOCATIONS
	add e
	call GetTurnDuelistVariable
	cp CARD_LOCATION_DECK
	jr z, .deck_or_hand
	cp CARD_LOCATION_HAND
	jr nz, .next
.deck_or_hand
	push de
	ld d, e
	ld e, PLAY_AREA_ARENA
	call CheckIfCanEvolveInto
	pop de
	jr nc, .set_carry
.next
	inc e
	ld a, DECK_SIZE
	cp e
	jr nz, .loop

	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	pop af
	ld [hl], a
	or a
	ret

.set_carry
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	pop af
	ld [hl], a
	ld a, e
	scf
	ret

INCLUDE "engine/duel/ai/boss_deck_set_up.asm"

; returns carry if Pokemon at PLAY_AREA* in a
; can damage defending Pokémon with any of its attacks
; input:
;	a = location of card to check
CheckIfCanDamageDefendingPokemon:
	ldh [hTempPlayAreaLocation_ff9d], a
	ld hl, CheckAttackCanBeUsedToDamageDefendingCard
	jp FindMatchingAttack

CheckAttackCanBeUsedToDamageDefendingCard:
	call CheckIfLoadedAttackIsUnusable
	ccf
	ret nc  ; unusable
	call CheckEnergyNeededForLoadedAttack
	ccf
	ret nc  ; not enough energy
	call EstimateDamageOfLoadedAttack_VersusDefendingCard
	ld a, [wDamage]
	cp 1
	ccf
	ret  ; carry if it does damage


; checks if defending Pokémon can knock out
; card at hTempPlayAreaLocation_ff9d with any of its attacks
; and if so, stores the damage to wAITempAttackDamage
; sets carry if any of the attacks knocks out
; also outputs the largest damage dealt in a and wAITempAttackDamage
; input:
;	[hTempPlayAreaLocation_ff9d] = location of card to check
; output:
;	a = largest damage of all attacks
;	[wAITempAttackDamage] = largest damage of all attacks
;	carry set if can knock out
CheckIfDefendingPokemonCanKnockOut:
	xor a
	ld [wAITempAttackDamage], a
; preload previous stages
	call SwapTurn
	call GetCardOneStageBelow
	call SwapTurn
; basic stage, always exists
	ld a, [wAllStagesIndices + BASIC]
	call CheckIfAnyAttackOfDefendingPokemonCardKnockOut
; stage 1
	ld a, [wAllStagesIndices + STAGE1]
	cp $ff
	jr z, .stage2
	call CheckIfAnyAttackOfDefendingPokemonCardKnockOut
.stage2
	ld a, [wAllStagesIndices + STAGE2]
	cp $ff
	call nz, CheckIfAnyAttackOfDefendingPokemonCardKnockOut
; check whether any attack can score a KO
	ld a, [wAITempAttackDamage]
	cp 1
	ccf  ; carry if not zero
	ret

; input:
;   a: deck index of the card owning the attacks
CheckIfAnyAttackOfDefendingPokemonCardKnockOut:
	ld [wTempCardDeckIndex], a
	xor a  ; FIRST_ATTACK_OR_PKMN_POWER
	call .CheckAttack
	jr nc, .second_attack
; enough damage to KO
; check if it is maximal damage
	ld a, [wDamage]
	ld hl, wAITempAttackDamage
	cp [hl]
	jr c, .second_attack  ; not the highest
; new maximum value
	ld [hl], a

.second_attack
	ld a, SECOND_ATTACK
	call .CheckAttack
	ret nc  ; no KO
; enough damage to KO
; check if it is maximal damage
	ld a, [wDamage]
	ld hl, wAITempAttackDamage
	cp [hl]
	jr c, .second_attack  ; not the highest
; new maximum value
	ld [hl], a
	ret

; return carry if defending Pokémon can knock out
; card at hTempPlayAreaLocation_ff9d
; input:
;	a = attack index
;   [wTempCardDeckIndex] = card owning the selected attack
;	[hTempPlayAreaLocation_ff9d] = location of card to check
.CheckAttack:
	ld e, a  ; wSelectedAttack is set when loading attack data
	ldh a, [hTempPlayAreaLocation_ff9d]
	push af
	xor a ; PLAY_AREA_ARENA
	ldh [hTempPlayAreaLocation_ff9d], a
	call SwapTurn
	call CheckIfAttackIsUnusable  ; loads attack data
	call nc, CheckEnergyNeededForLoadedAttack
	call SwapTurn
	pop bc
	ld a, b
	ldh [hTempPlayAreaLocation_ff9d], a
	ccf
	ret nc  ; unusable or not enough energy

; player's active Pokémon can use attack
; attack data is already loaded
	call EstimateDamageOfLoadedAttack_FromDefendingPokemon
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	ld hl, wDamage
	sub [hl]
	ret c  ; KO
	ret nz  ; no KO
	scf
	ret  ; exact KO


; sets carry if Opponent's deck ID
; is between LEGENDARY_MOLTRES_DECK_ID (inclusive)
; and MUSCLES_FOR_BRAINS_DECK_ID (exclusive)
; these are the decks for Grandmaster/Club Master/Ronald
CheckIfOpponentHasBossDeckID:
	push af
	ld a, [wOpponentDeckID]
	cp LEGENDARY_MOLTRES_DECK_ID
	jr c, .no_carry
	cp MUSCLES_FOR_BRAINS_DECK_ID
	jr nc, .no_carry
	pop af
	scf
	ret

.no_carry
	pop af
	or a
	ret

; sets carry if not a boss fight
; and if hasn't received legendary cards yet
CheckIfNotABossDeckID:
	call EnableSRAM
	ld a, [sReceivedLegendaryCards]
	call DisableSRAM
	or a
	jr nz, .no_carry
	call CheckIfOpponentHasBossDeckID
	jr nc, .set_carry
.no_carry
	or a
	ret

.set_carry
	scf
	ret

; probability to return carry:
; - 0% for boss decks
; - 25% for all other decks
; used for certain decks to randomly choose
; not to play Trainer card or use PKMN Power
AIChooseRandomlyNotToDoAction:
; boss decks always use Trainer cards.
	push hl
	push de
	call CheckIfNotABossDeckID
	jr c, .carry_25_percent
	pop de
	pop hl
	ret

.carry_25_percent
	ld a, 4
	call Random
	cp 1
	pop de
	pop hl
	ret

; checks if any bench Pokémon has same ID
; as input, and sets carry if it has more than
; half health and can use its second attack
; input:
;	de = card ID to check for
; output:
;	carry set if the above requirements are met
CheckForBenchIDAtHalfHPAndCanUseSecondAttack:
	ld a, e
	ld [wSamePokemonCardID + 0], a
	ld a, d
	ld [wSamePokemonCardID + 1], a
	ldh a, [hTempPlayAreaLocation_ff9d]
	ld d, a
	ld a, [wSelectedAttack]
	ld e, a
	push de
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	lb bc, 0, PLAY_AREA_ARENA
	push hl

.loop
	inc c
	pop hl
	ld a, [hli]
	push hl
	cp $ff
	jr z, .done
	ld d, a
	push de
	push bc
	call LoadCardDataToBuffer1_FromDeckIndex
	pop bc
	ld a, c
	add DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	ld d, a
	ld a, [wLoadedCard1HP]
	rrca
	cp d
	pop de
	jr nc, .loop
	; half max HP < current HP
	ld a, [wLoadedCard1ID + 0]
	ld hl, wSamePokemonCardID
	cp [hl]
	jr nz, .loop
	ld a, [wLoadedCard1ID + 1]
	inc hl
	cp [hl]
	jr nz, .loop

	ld a, c
	ldh [hTempPlayAreaLocation_ff9d], a
	ld a, SECOND_ATTACK
	ld [wSelectedAttack], a
	push bc
	call Old_CheckIfSelectedAttackIsUnusable
	pop bc
	jr c, .loop
	inc b
.done
	pop hl
	pop de
	ld a, e
	ld [wSelectedAttack], a
	ld a, d
	ldh [hTempPlayAreaLocation_ff9d], a
	ld a, b
	or a
	ret z
	scf
	ret

; add 5 to wPlayAreaEnergyAIScore AI score corresponding to all cards
; in bench that have same ID as register a
; input:
;	bc = card ID to look for
RaiseAIScoreToAllMatchingIDsInBench:
	ld a, DUELVARS_BENCH
	call GetTurnDuelistVariable
	ld e, 0
.loop
	inc e
	ld a, [hli]
	cp $ff
	ret z
	push de
	call GetCardIDFromDeckIndex
	call CompareDEtoBC
	pop de
	jr nz, .loop
	push bc
	ld c, e
	ld b, $00
	push hl
	ld hl, wPlayAreaEnergyAIScore
	add hl, bc
	ld a, 5
	add [hl]
	ld [hl], a
	pop hl
	pop bc
	jr .loop

; used by AI to determine which Pokémon it should favor in the bench
; in order to attach an energy card from the hand, in case there are repeats
; if there is repeated Pokémon in bench, then increase wPlayAreaEnergyAIScore
; from the Pokémon with less damage and more energy cards,
; and decrease from all others
HandleAIEnergyScoringForRepeatedBenchPokemon:
	; clears wSamePokemonEnergyScoreHandled
	ld a, MAX_PLAY_AREA_POKEMON
	ld hl, wSamePokemonEnergyScoreHandled
	call ClearMemory_Bank5

	ld a, DUELVARS_BENCH
	call GetTurnDuelistVariable
	ld e, 0
.loop_bench
	; clears wSamePokemonEnergyScore
	push hl
	ld a, MAX_PLAY_AREA_POKEMON
	ld hl, wSamePokemonEnergyScore
	call ClearMemory_Bank5
	pop hl

	inc e
	ld a, [hli]
	cp $ff
	ret z ; done looping bench

	ld [wSamePokemonCardID], a ; deck index

; checks wSamePokemonEnergyScoreHandled of location in e
; if != 0, go to next in play area
	push de
	push hl
	ld d, $00
	ld hl, wSamePokemonEnergyScoreHandled
	add hl, de
	ld a, [hl]
	or a
	pop hl
	pop de
	jr nz, .loop_bench ; already handled

	; store this card's ID
	push de
	ld a, [wSamePokemonCardID]
	call GetCardIDFromDeckIndex
	ld a, e
	ld [wSamePokemonCardID + 0], a
	ld a, d
	ld [wSamePokemonCardID + 1], a
	pop de

	; calculate score of this Pokémon
	; and all cards with same ID
	push hl
	push de
	call .CalculateScore
.loop_search_same_card_id
	inc e
	ld a, [hli]
	cp $ff
	jr z, .tally_repeated_pokemon
	push de
	call GetCardIDFromDeckIndex
	ld a, [wSamePokemonCardID + 0]
	cp e
	jr nz, .not_equal
	ld a, [wSamePokemonCardID + 1]
	cp d
.not_equal
	pop de
	jr nz, .loop_search_same_card_id
	call .CalculateScore
	jr .loop_search_same_card_id

.tally_repeated_pokemon
	call .CountNumberOfCardsWithSameID
	jr c, .next

	; has repeated card IDs in bench
	; find which one has highest score
	lb bc, 0, 0
	ld hl, wSamePokemonEnergyScore + PLAY_AREA_BENCH_5
	ld d, PLAY_AREA_BENCH_5 + 1
.loop_2
	dec d
	jr z, .got_highest_score
	ld a, [hld]
	cp b
	jr c, .loop_2
	ld b, a ; highest score
	ld c, d ; play area location
	jr .loop_2

; c = play area location of highest score
; increase wPlayAreaEnergyAIScore score for card with highest ID
; decrease wPlayAreaEnergyAIScore score for all cards with same ID
.got_highest_score
	ld hl, wPlayAreaEnergyAIScore
	ld de, wSamePokemonEnergyScore
	ld b, PLAY_AREA_ARENA
.loop_3
	ld a, c
	cp b
	jr z, .card_with_highest
	ld a, [de] ; score
	or a
	jr z, .check_next
; decrease score
	dec [hl]
	jr .check_next
.card_with_highest
; increase score
	inc [hl]
.check_next
	inc b
	ld a, MAX_PLAY_AREA_POKEMON
	cp b
	jr z, .next
	inc de
	inc hl
	jr .loop_3

.next
	pop de
	pop hl
	jp .loop_bench

; loads wSamePokemonEnergyScore + play area location in e
; with energy  * 2 + $80 - floor(dam / 10)
; loads wSamePokemonEnergyScoreHandled + play area location in e
; with $01
.CalculateScore:
	push hl
	push de
	call GetCardDamageAndMaxHP
	call ConvertHPToDamageCounters_Bank5
	ld b, a
	push bc
	call CountNumberOfEnergyCardsAttached
	pop bc
	sla a
	add $80
	sub b
	pop de
	push de
	ld d, $00
	ld hl, wSamePokemonEnergyScore
	add hl, de
	ld [hl], a
	ld hl, wSamePokemonEnergyScoreHandled
	add hl, de
	ld [hl], $01
	pop de
	pop hl
	ret

; counts how many play area locations in wSamePokemonEnergyScore
; are != 0, and outputs result in a
; also returns carry if result is < 2
.CountNumberOfCardsWithSameID:
	ld hl, wSamePokemonEnergyScore
	ld d, $00
	ld e, MAX_PLAY_AREA_POKEMON + 1
.loop
	dec e
	jr z, .done
	ld a, [hli]
	or a
	jr z, .loop
	inc d
	jr .loop
.done
	ld a, d
	cp 2
	ret


; temporarily overwrite current play area card with a hand Pokémon
; overwrites also the card's location and HP for damage calculation
; used by AIDecideEvolution to evaluate the evolution
; input:
;   hTempPlayAreaLocation_ff9d = play area location
;   wTempAIPokemonCard = hand Pokémon deck index
;   wLoadedCard1 = hand Pokémon data
;   wLoadedCard2 = play area Pokémon data
AITemporarilyOverwritePlayAreaPokemon:
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld [wAIBackupPlayAreaPokemon], a
; should be ok if AIDecideSpecialEvolutions does not modify it
	; call LoadCardDataToBuffer2_FromDeckIndex
	ld a, [wTempAIPokemonCard]  ; evolution card
	ld [hl], a
; temporarily overwrite evolution card's location 
; this is necessary to make GetCardOneStageBelow work,
; which is called by CheckIfAnyAttackKnocksOutDefendingCard
	; ld a, [wTempAIPokemonCard]
	ld l, a  ; DUELVARS_CARD_LOCATIONS + index
	ldh a, [hTempPlayAreaLocation_ff9d]
	or CARD_LOCATION_ARENA
	ld [hl], a
	ret

; temporarily replace current HP for damage calculation
; input:
;   hTempPlayAreaLocation_ff9d = play area location
;   wLoadedCard1 = hand Pokémon data
;   wLoadedCard2 = play area Pokémon data
AITemporarilyOverwritePlayAreaPokemonHP:
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	ld [wTempHPBuffer], a
; should be ok if AIDecideSpecialEvolutions does not modify it
	; ld a, [wTempAIPokemonCard]
	; call LoadCardDataToBuffer1_FromDeckIndex
; find the HP difference after evolving
	ld a, [wLoadedCard2HP]
	ld c, a
	ld a, [wLoadedCard1HP]
	sub c
; add this difference to the current HP
	add [hl]
	ld [hl], a
	ret


; restore the play area card that has been overwritten
; also restores the overwriting card's location to the hand
; input:
;   hTempPlayAreaLocation_ff9d = play area location
;   wTempAIPokemonCard = hand Pokémon deck index
;   wAIBackupPlayAreaPokemon = backup of play area Pokémon deck index
AIRestorePlayAreaPokemon_WRAMPlayArea_CardLocationHand:
	ld e, CARD_LOCATION_HAND
	; jr AIRestorePlayAreaPokemon_WRAMPlayArea
	; fallthrough

; restore the play area card that has been overwritten
; also restores the overwriting card's location to the hand
; input:
;   e = CARD_LOCATION_*
;   hTempPlayAreaLocation_ff9d = play area location
;   wTempAIPokemonCard = hand Pokémon deck index
;   wAIBackupPlayAreaPokemon = backup of play area Pokémon deck index
AIRestorePlayAreaPokemon_WRAMPlayArea:
	ld a, [wTempAI]
	ldh [hTempPlayAreaLocation_ff9d], a
	; jr AIRestorePlayAreaPokemon
	; fallthrough

; restore the play area card that has been overwritten
; also restores the overwriting card's location
; input:
;   e = CARD_LOCATION_*
;   hTempPlayAreaLocation_ff9d = play area location
;   wTempAIPokemonCard = hand Pokémon deck index
;   wAIBackupPlayAreaPokemon = backup of play area Pokémon deck index
AIRestorePlayAreaPokemon:
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld a, [wAIBackupPlayAreaPokemon]
	ld [hl], a
; restore evolution card's location to the hand
	ld a, [wTempAIPokemonCard]
	ld l, a  ; DUELVARS_CARD_LOCATIONS
	ld [hl], e
	ret

; restore the play area card's HP that has been overwritten
; input:
;   hTempPlayAreaLocation_ff9d = play area location
;   wTempHPBuffer = backup of play area Pokémon HP
AIRestorePlayAreaPokemonHP:
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	ld a, [wTempHPBuffer]
	ld [hl], a
	ret
