; have AI choose an energy card to play, but do not play it.
; does not consider whether the cards have evolutions to be played.
; return carry if an energy card is chosen to use in any Play Area card,
; and if so, return its Play Area location in hTempPlayAreaLocation_ff9d.
AIProcessButDontPlayEnergy_SkipEvolution:
	ld a, AI_ENERGY_FLAG_SKIP_EVOLUTION
	jr AIProcessButDontPlayEnergy

; have AI choose an energy card to play, but do not play it.
; does not consider whether the cards have evolutions to be played.
; return carry if an energy card is chosen to use in any Bench card,
; and if so, return its Play Area location in hTempPlayAreaLocation_ff9d.
AIProcessButDontPlayEnergy_SkipEvolutionAndArena:
	ld a, AI_ENERGY_FLAG_SKIP_EVOLUTION | AI_ENERGY_FLAG_SKIP_ARENA_CARD
	; jr AIProcessButDontPlayEnergy
	; fallthrough

; have AI choose an energy card to play, but do not play it.
; does not consider whether the cards have evolutions to be played.
; return carry if an energy card is chosen to use, and if so,
; return its Play Area location in hTempPlayAreaLocation_ff9d.
; input:
;	a = logic flags for energy attachment
AIProcessButDontPlayEnergy:
	or AI_ENERGY_FLAG_DONT_PLAY
	ld [wAIEnergyAttachLogicFlags], a
; backup wPlayAreaAIScore in wTempPlayAreaAIScore.
	ld de, wTempPlayAreaAIScore
	ld hl, wPlayAreaAIScore
	ld b, MAX_PLAY_AREA_POKEMON
.loop
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .loop

	ld a, [wAIScore]
	ld [de], a

	jr AIProcessEnergyCards

; copies wTempPlayAreaAIScore to wPlayAreaAIScore
; and loads wAIScore with value in wTempAIScore.
; identical to RetrievePlayAreaAIScoreFromBackup2.
RetrievePlayAreaAIScoreFromBackup1:
	push af
	ld de, wPlayAreaAIScore
	ld hl, wTempPlayAreaAIScore
	ld b, MAX_PLAY_AREA_POKEMON
.loop
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .loop
	ld a, [hl]
	ld [wAIScore], a
	pop af
	ret

; have AI decide whether to play energy card from hand
; and determine which card is best to attach it.
AIProcessAndTryToPlayEnergy:
	ld a, [wAlreadyPlayedEnergy]
	or a
	ret nz ; already played energy this turn

	xor a
	ld [wAIEnergyAttachLogicFlags], a

.has_logic_flags
	call CreateEnergyCardListFromHand
	jr nc, AIProcessEnergyCards

; no energy
	ld a, [wAIEnergyAttachLogicFlags]
	or a
	jp nz, RetrievePlayAreaAIScoreFromBackup1
	ret

; have AI decide whether to play energy card
; and determine which card is best to attach it.
AIProcessEnergyCards:
; initialize Play Area AI score
	ld a, $80
	ld b, MAX_PLAY_AREA_POKEMON
	ld hl, wPlayAreaEnergyAIScore
.loop
	ld [hli], a
	dec b
	jr nz, .loop

; Legendary Decks have their own energy card logic
	call HandleLegendaryDeckEnergyScoring

; start the main Play Area loop
	ld b, PLAY_AREA_ARENA
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	ld c, a

.loop_play_area
	push bc
	ld a, b
	ldh [hTempPlayAreaLocation_ff9d], a
	ld a, $80
	ld [wAIScore], a
	ld a, $ff
	ld [wTempAI], a
	ld a, [wAIEnergyAttachLogicFlags]
	and AI_ENERGY_FLAG_SKIP_EVOLUTION
	jr nz, .check_venusaur

; check if energy needed is found in hand
; and if there's an evolution in hand or deck
; and if so, add to AI score
	call CreateHandCardList
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld [wTempAI2], a
	call GetAttacksEnergyCostBits
	ld hl, wDuelTempList
	call CheckEnergyFlagsNeededInList
	jp nc, .store_score

	ld a, [wTempAI2]
	call CheckForEvolutionInList
	jr nc, .no_evolution_in_hand

	ld [wTempAI], a ; store evolution card found
	ld a, 2
	call AIEncourage
	jr .check_venusaur

.no_evolution_in_hand
	ld a, [wTempAI2]
	call CheckForEvolutionInDeck
	jr nc, .check_venusaur
	ld a, 1
	call AIEncourage

; if there's no Muk in any Play Area
; and there's VenusaurLv67 in own Play Area,
; add to AI score
.check_venusaur
	ld de, MUK
	call CountPokemonWithActivePkmnPowerInBothPlayAreas
	jr c, .check_if_active
	ld de, VENUSAUR_LV67
	call CountTurnDuelistPokemonWithActivePkmnPower
	jr nc, .check_if_active
	ld a, 1
	call AIEncourage

.check_if_active
	ldh a, [hTempPlayAreaLocation_ff9d]
	or a
	jr nz, .bench

; arena
	ld a, [wAIBarrierFlagCounter]
	bit AI_MEWTWO_MILL_F, a
	jr z, .add_to_score

; subtract from score instead
; if Player is running MewtwoLv53 mill deck.
	ld a, 5
	call AIDiscourage
	jr .check_defending_can_ko

.add_to_score
	ld a, 4
	call AIEncourage

; lower AI score if poison/double poison
; will KO Pokémon between turns
; or if the defending Pokémon can KO
	ld a, DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	call ConvertHPToDamageCounters_Bank5
	cp 3
	jr nc, .check_defending_can_ko
	; hp < 30
	cp 2
	jr z, .has_20_hp
	; hp = 10
	ld a, DUELVARS_ARENA_CARD_STATUS
	call GetTurnDuelistVariable
	and POISONED
	jr z, .check_defending_can_ko
	jr .poison_will_ko
.has_20_hp
	ld a, DUELVARS_ARENA_CARD_STATUS
	call GetTurnDuelistVariable
	and DOUBLE_POISONED
	jr z, .check_defending_can_ko
.poison_will_ko
	ld a, 10
	call AIDiscourage
	jr .check_bench
.check_defending_can_ko
	call CheckIfDefendingPokemonCanKnockOut
	jr nc, .ai_score_bonus
	ld a, 10
	call AIDiscourage

; if either poison will KO or defending Pokémon can KO,
; check if there are bench Pokémon,
; if there are not, add AI score
.check_bench
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	dec a
	jr nz, .ai_score_bonus
	ld a, 6
	call AIEncourage
	jr .ai_score_bonus

; lower AI score by 3 - (bench HP)/10
; if bench HP < 30
.bench
	add DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	call ConvertHPToDamageCounters_Bank5
	cp 3
	jr nc, .ai_score_bonus
; hp < 30
	ld b, a
	ld a, 3
	sub b
	call AIDiscourage

; check list in wAICardListEnergyBonus
.ai_score_bonus
	ld a, [wAICardListEnergyBonus + 1]
	or a
	jr z, .check_boss_deck ; is null
	ld h, a
	ld a, [wAICardListEnergyBonus]
	ld l, a

	push hl
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
	pop hl

.loop_id_list
	ld a, [hli]
	or [hl]
	dec hl
	jr z, .check_boss_deck
	ld a, [hli]
	cp e
	jr nz, .next_id_inc_hl_3
	ld a, [hli]
	cp d
	jr nz, .next_id_inc_hl_2

	; number of attached energy cards
	ld a, [hli]
	ld d, a
	push de
	ldh a, [hTempPlayAreaLocation_ff9d]
	ld e, a
	call GetPlayAreaCardAttachedEnergies
	ld a, [wTotalAttachedEnergies]
	pop de
	cp d
	jr c, .check_id_score
	; already reached target number of energy cards
	ld a, 10
	call AIDiscourage
	jr .check_boss_deck

.check_id_score
	ld a, [hli]
	cp $80
	jr c, .decrease_score_1
	sub $80
	call AIEncourage
	jr .check_boss_deck
.decrease_score_1
	ld d, a
	ld a, $80
	sub d
	call AIDiscourage
	jr .check_boss_deck

.next_id_inc_hl_3
	inc hl
.next_id_inc_hl_2
	inc hl
	inc hl
	jr .loop_id_list

; unlock better AI for all decks, not just bosses
.check_boss_deck
	call HandleAIEnergyScoringForRepeatedBenchPokemon
; applies wPlayAreaEnergyAIScore
	ldh a, [hTempPlayAreaLocation_ff9d]
	ld c, a
	ld b, $00
	ld hl, wPlayAreaEnergyAIScore
	add hl, bc
	ld a, [hl]
	cp $80
	jr c, .decrease_score_2
	sub $80
	call AIEncourage
	jr .skip_boss_deck

.decrease_score_2
	ld b, a
	ld a, $80
	sub b
	call AIDiscourage

.skip_boss_deck
	ld a, 1
	call AIEncourage

; check if there was an evolution in hand for this card
; and temporarily replace this card with the evolution
; to check whether energy is still needed for attacks.
	ld a, [wTempAI] ; evolution in hand
	cp $ff
	jr nz, .check_evolution

; no need to replace the card
	ld hl, DetermineAIScoreOfAttackEnergyRequirement
	call ForAllAttacksOfPokemon
	jr .store_score

; temporarily replace this card with evolution in hand
.check_evolution
	ld b, a
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	push af
	ld [hl], b

; add AI score for all attacks, according to their energy requirements
	ld hl, DetermineAIScoreOfAttackEnergyRequirement
	call ForAllAttacksOfPokemon

; recover the original card in the Play Area location
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	pop af
	ld [hl], a

; store bench score for this card.
.store_score
	ldh a, [hTempPlayAreaLocation_ff9d]
	ld c, a
	ld b, $00
	ld hl, wPlayAreaAIScore
	add hl, bc
	ld a, [wAIScore]
	ld [hl], a
	pop bc
	inc b
	dec c
	jp nz, .loop_play_area

; the Play Area loop is over and the score
; for each card has been calculated.
; now to determine the highest score.
	call FindPlayAreaCardWithHighestAIScore
	jr nc, .not_found

	ld a, [wAIEnergyAttachLogicFlags]
	and AI_ENERGY_FLAG_DONT_PLAY
	jr z, .play_card
	scf
	jp RetrievePlayAreaAIScoreFromBackup1

.play_card
	call CreateEnergyCardListFromHand
	jp AITryToPlayEnergyCard

.not_found:
	ld a, [wAIEnergyAttachLogicFlags]
	or a
	jr z, .no_carry
	jp RetrievePlayAreaAIScoreFromBackup1
.no_carry
	or a
	ret

; checks score related to loaded attack,
; in order to determine whether to play energy card.
; the AI score is increased/decreased accordingly.
; input:
;	[wLoadedAttack] = attack data to score
DetermineAIScoreOfAttackEnergyRequirement:
	call CheckEnergyNeededForLoadedAttack
	jp c, .not_enough_energy
	ld a, ATTACK_FLAG2_ADDRESS | ATTACHED_ENERGY_BOOST_F
	call CheckLoadedAttackFlag
	jr c, .attached_energy_boost
	ld a, ATTACK_FLAG2_ADDRESS | DISCARD_ENERGY_F
	call CheckLoadedAttackFlag
	jr c, .discard_energy
; this attack does not need any more energy
	ld a, 1
	jp AIDiscourage  ; done

.attached_energy_boost
	ld a, [wLoadedAttackEffectParam]
	cp MAX_ENERGY_BOOST_IS_LIMITED
	jr z, .check_surplus_energy

	; is MAX_ENERGY_BOOST_IS_NOT_LIMITED,
	; which is equal to 3, add to score.
	jp AIEncourage  ; done

.check_surplus_energy
	call CheckIfNoSurplusEnergyForLoadedAttack
	jr c, .no_surplus_energy
	cp 4 ; check how much surplus energy
	jr c, .no_surplus_energy

.has_surplus_energy
	ld a, 5
	jp AIDiscourage  ; done

.no_surplus_energy
	ld a, 2
	call AIEncourage

; check whether attack has ATTACHED_ENERGY_BOOST flag
; and add to AI score if attaching another energy
; will KO defending Pokémon.
; add more to score if this is currently active Pokémon.
	ld a, ATTACK_FLAG2_ADDRESS | ATTACHED_ENERGY_BOOST_F
	call CheckLoadedAttackFlag
	ret nc  ; done

	call CheckAttackDoesEnoughDamageToKnockOutDefendingCard
	ret c  ; done

; simulate boost gained by attaching another energy card
	ld a, [wDamage]
	add 10  ; FIXME: boost amount hardcoded
	ld b, a
	ld a, DUELVARS_ARENA_CARD_HP
	call GetNonTurnDuelistVariable
	sub b
	jr c, .attaching_kos_player
	ret nz  ; done

.attaching_kos_player
	ld a, 20
	call AIEncourage
	ldh a, [hTempPlayAreaLocation_ff9d]
	or a
	ret nz  ; done

	ld a, 10
	jp AIEncourage  ; done

; checks if there is surplus energy for attack
; that discards attached energy card.
; if current card is ZapdosLv64, don't add to score.
; if there is no surplus energy, encourage playing energy.
.discard_energy
; FIXME: this can be implemented with attack flags, instead of hardcoding ID
	ld hl, wLoadedCard1ID
	cphl ZAPDOS_LV64
	ret z  ; done

	call CheckIfNoSurplusEnergyForLoadedAttack
	jr c, .no_surplus_energy
	jr .has_surplus_energy

; discourage attacks with FLAG_2_BIT_5_F set
.not_enough_energy
	ld a, ATTACK_FLAG2_ADDRESS | FLAG_2_BIT_5_F
	call CheckLoadedAttackFlag
	jr nc, .check_double_colorless_needed
	ld a, 5
	call AIDiscourage

; if the energy card color needed is in hand, increase AI score.
; if a colorless card is needed, increase AI score.
.check_double_colorless_needed
	ld a, c
	cp 2
	jr c, .check_color_needed
; needs two colorless energy cards or more
	push de
	ld de, DOUBLE_COLORLESS_ENERGY
	call LookForCardIDInHand  ; preserves bc
	pop de
	jr c, .check_color_needed
	ld a, 5
	call AIEncourage
	jr .needs_one_energy

.check_color_needed
	ld a, b
	or a
	jr z, .check_colorless_needed
	call LookForCardIDInHand  ; FIXME: hard-coded for Basic Energy cards
	jr c, .check_colorless_needed
	ld a, 4
	call AIEncourage
	jr .check_total_needed

.check_colorless_needed
	ld a, c
	or a
	ret z  ; done

	ld a, 3
	call AIEncourage

; if only one energy card is needed for attack,
; encourage playing energy card.
.check_total_needed
	ld a, b
	add c
	dec a
	ret nz  ; done

.needs_one_energy
	ld a, 3
	call AIEncourage

; if the attack KOs player and this is the active card, add to AI score.
	call CheckAttackDoesEnoughDamageToKnockOutDefendingCard
	ret nc  ; done

	ld a, 20
	call AIEncourage
; add 10 more in case it's the Arena card
	ldh a, [hTempPlayAreaLocation_ff9d]
	or a
	ret nz  ; done

	ld a, 10
	jp AIEncourage


; returns in hTempPlayAreaLocation_ff9d the Play Area location
; of the card with the highest Play Area AI score, unless
; the highest score is below $85.
; if it succeeds in return a card location, set carry.
; if AI_ENERGY_FLAG_SKIP_ARENA_CARD is set in wAIEnergyAttachLogicFlags
; doesn't include the Arena card and there's no minimum score.
FindPlayAreaCardWithHighestAIScore:
	ld a, [wAIEnergyAttachLogicFlags]
	and AI_ENERGY_FLAG_SKIP_ARENA_CARD
	jr nz, .only_bench

	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	ld b, a
	ld c, PLAY_AREA_ARENA
	ld e, c
	ld d, c
	ld hl, wPlayAreaAIScore
; find highest Play Area AI score.
.loop_1
	ld a, [hli]
	cp e
	jr c, .next_1
	jr z, .next_1
	ld e, a ; overwrite highest score found
	ld d, c ; overwrite Play Area of highest score
.next_1
	inc c
	dec b
	jr nz, .loop_1

; if highest AI score is below $85, return no carry.
; else, store Play Area location and return carry.
	ld a, e
	cp $85
	jr c, .not_enough_score
	ld a, d
	ldh [hTempPlayAreaLocation_ff9d], a
	scf
	ret
.not_enough_score
	or a
	ret

; same as above but only check bench Pokémon scores.
.only_bench
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	dec a
	jr z, .no_carry

	ld b, a
	ld e, 0
	ld c, PLAY_AREA_BENCH_1
	ld d, c
	ld hl, wPlayAreaAIScore + 1
.loop_2
	ld a, [hli]
	cp e
	jr c, .next_2
	jr z, .next_2
	ld e, a ; overwrite highest score found
	ld d, c ; overwrite Play Area of highest score
.next_2
	inc c
	dec b
	jr nz, .loop_2

; in this case, there is no minimum threshold AI score.
	ld a, d
	ldh [hTempPlayAreaLocation_ff9d], a
	scf
	ret
.no_carry
	or a
	ret


; returns in e the card ID of the energy required for
; the Discard/Energy Boost attack loaded in wSelectedAttack.
; if it's ZapdosLv64's Thunderbolt attack, return no carry.
; if it's Charizard's Fire Spin or Exeggutor's Big Eggsplosion
; attack, don't return energy card ID, but set carry.
; output:
;	b = TRUE if needs color energy;
;	c = TRUE if only needs colorless energy;
;	carry set if not ZapdosLv64's Thunderbolt attack.
GetEnergyCardForDiscardOrEnergyBoostAttack:
; load card ID and check selected attack index.
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call LoadCardDataToBuffer2_FromDeckIndex
	ld a, [wSelectedAttack]
	or a
	jr z, .first_attack

; check if second attack is ZapdosLv64's Thunderbolt,
; Charizard's Fire Spin or Exeggutor's Big Eggsplosion,
; for these to be treated differently.
; for both attacks, load its energy cost.
	ld hl, wLoadedCard2ID
	cphl ZAPDOS_LV64
	jr z, .zapdos
	cphl CHARIZARD
	jr z, .charizard_or_exeggutor
	cphl EXEGGUTOR
	jr z, .charizard_or_exeggutor
	ld hl, wLoadedCard2Atk2EnergyCost
	jr .fire
.first_attack
	ld hl, wLoadedCard2Atk1EnergyCost

; check which energy color the attack requires,
; and load in e the card ID of corresponding energy card,
; then return carry flag set.
.fire
	ld a, [hli]
	ld b, a
	and $f0
	jr z, .grass
	ld de, FIRE_ENERGY
	jr .set_carry
.grass
	ld a, b
	and $0f
	jr z, .lightning
	ld de, GRASS_ENERGY
	jr .set_carry
.lightning
	ld a, [hli]
	ld b, a
	and $f0
	jr z, .water
	ld de, LIGHTNING_ENERGY
	jr .set_carry
.water
	ld a, b
	and $0f
	jr z, .fighting
	ld de, WATER_ENERGY
	jr .set_carry
.fighting
	ld a, [hli]
	ld b, a
	and $f0
	jr z, .psychic
	ld de, FIGHTING_ENERGY
	jr .set_carry
.psychic
	ld de, PSYCHIC_ENERGY

.set_carry
	lb bc, TRUE, FALSE
	scf
	ret

; for ZapdosLv64's Thunderbolt attack, return with no carry.
.zapdos
	or a
	ret

; Charizard's Fire Spin and Exeggutor's Big Eggsplosion,
; return carry.
.charizard_or_exeggutor
	lb bc, FALSE, TRUE
	scf
	ret

; Called after the AI has decided which card to attach energy from hand.
; The AI does checks to determine whether this card needs more energy
; and chooses the right energy card to play.
; If the card is played, return with carry flag set.
AITryToPlayEnergyCard:
	ld a, $ff
	ld [wTempAI], a
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call CheckCardEvolutionInHandOrDeck
	jr nc, .check_energy_needs

; evolution available, overwrite card to consider next attacks
	ld [wTempAIPokemonCard], a
	add DUELVARS_CARD_LOCATIONS
	call GetTurnDuelistVariable
	ld [wTempAI], a
	call AITemporarilyOverwritePlayAreaPokemon

.check_energy_needs
	ld hl, DeterminePreferredEnergyAttachmentForLoadedAttack
	call FindMatchingAttack
	push af
	ld a, [wTempAI]  ; card location, if overwritten
	cp $ff
	jr z, .card_restored
; the play area card was overwritten, restore it
	ld e, a
	call AIRestorePlayAreaPokemon
.card_restored
	pop af
	ret nc

; play energy card loaded in hTemp_ffa0 and set carry flag.
	ldh a, [hTempPlayAreaLocation_ff9d]
	ldh [hTempPlayAreaLocation_ffa1], a
	ld a, OPPACTION_PLAY_ENERGY
	bank1call AIMakeDecision
	scf
	ret


; This could use custom effect logic, e.g.
;	ld a, EFFECTCMDTYPE_AI_ENERGY_ATTACHMENT_PREFERENCE
;	call TryExecuteEffectCommandFunction
DeterminePreferredEnergyAttachmentForLoadedAttack:
	call CheckEnergyNeededForLoadedAttack
	jr nc, .check_discard_or_energy_boost
; if it is an attack, it needs more energy cards
	ld a, b
	or a
	jr nz, .basic_energy
	ld a, c
	or a
	jr nz, .colorless_energy
; not an attack
	ret

; this attack does not need any more energy cards to be used.
; check whether it can use extra energy cards for its effects.
.check_discard_or_energy_boost
	ld a, ATTACK_FLAG2_ADDRESS | ATTACHED_ENERGY_BOOST_F
	call CheckLoadedAttackFlag
	jr c, .energy_boost_or_discard_energy
	ld a, ATTACK_FLAG2_ADDRESS | DISCARD_ENERGY_F
	call CheckLoadedAttackFlag
	ret nc

; for attacks that discard energy or get boost for
; additional energy cards, get the energy card ID required by attack.
; if it's ZapdosLv64's Thunderbolt attack, return.
.energy_boost_or_discard_energy
	call GetEnergyCardForDiscardOrEnergyBoostAttack
	ret nc

	ld a, b
	or a
	jr z, .colorless_energy

; in this case, Pokémon needs a specific basic energy card.
; look for basic energy card needed in hand and play it.
.basic_energy
	call LookForCardIDInHand
	ldh [hTemp_ffa0], a
	jr nc, .play_energy_card

; in this case Pokémon just needs colorless (any basic energy card).
; some decks allow basic Pokémon to be given double colorless
; in anticipation for evolution, so play card if that is the case.
.colorless_energy
	call CheckSpecificCardsToAttachDoubleColorless
	jr c, .play_energy_card

; check if this card needs 2 colorless.
; if it does, look for double colorless card in hand to play.
	ld a, c
	or a
	ret z
; needs colorless energy
	cp 2
	jr c, .look_for_any_energy

; needs two colorless
	ld hl, wDuelTempList
.loop_1
	ld a, [hli]
	cp $ff
	jr z, .look_for_any_energy
	ldh [hTemp_ffa0], a
	call GetCardIDFromDeckIndex
	cp16 DOUBLE_COLORLESS_ENERGY
	jr nz, .loop_1
	jr .play_energy_card

; otherwise, look for any card and play it.
; if it's a boss deck, avoid playing double colorless in this situation.
.look_for_any_energy
	ld hl, wDuelTempList
	call CountCardsInDuelTempList
	call ShuffleCards
.loop_2
	ld a, [hli]
	cp $ff
	ret z  ; no suitable energy

	call CheckIfOpponentHasBossDeckID
	jr nc, .load_card
; boss deck
	push af
	call GetCardIDFromDeckIndex
	cp16 DOUBLE_COLORLESS_ENERGY
	pop bc
	jr z, .loop_2
	ld a, b
.load_card
	ldh [hTemp_ffa0], a

; plays energy card loaded in hTemp_ffa0 and sets carry flag.
.play_energy_card
	scf
	ret


; decide whether to play double colorless to some specific cards.
; these are cards that do not need double colorless to any of their attacks
; but are required by their evolutions.
; return carry if there's a double colorless in hand to attach
; and it's one of the identified card IDs.
; output:
;	[hTemp_ffa0] = card index of double colorless in hand;
;	carry set if can play energy card.
CheckSpecificCardsToAttachDoubleColorless:
	push bc
	push de
	push hl

; load card ID
	ldh a, [hTempPlayAreaLocation_ff9d]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
; check for specific cards
	cp16 CHARMANDER
	jr z, .check_colorless_attached
	cp16 DRATINI
	jr z, .check_colorless_attached
	cp16 GROWLITHE
	jr z, .check_colorless_attached

.no_carry
	pop hl
	pop de
	pop bc
	or a
	ret

; check if card has any colorless energy cards attached,
; and if there are any, return no carry.
.check_colorless_attached
	ldh a, [hTempPlayAreaLocation_ff9d]
	ld e, a
	call GetPlayAreaCardAttachedEnergies
	ld a, [wAttachedEnergies + COLORLESS]
	or a
	jr nz, .no_carry

; card has no colorless energy, so look for double colorless
; in hand and if found, return carry and its card index.
	ld de, DOUBLE_COLORLESS_ENERGY
	call LookForCardIDInHand
	jr c, .no_carry
	ldh [hTemp_ffa0], a
	pop hl
	pop de
	pop bc
	scf
	ret


; handle how AI scores giving out Energy Cards
; when using Legendary decks
HandleLegendaryDeckEnergyScoring:
	ld a, [wOpponentDeckID]
	cp LEGENDARY_ARTICUNO_DECK_ID
	jr nz, .zapdos_deck
	jp ScoreLegendaryArticunoCardsForEnergyAttachment

.zapdos_deck
	cp LEGENDARY_ZAPDOS_DECK_ID
	jr nz, .moltres_deck
	jp ScoreLegendaryZapdosCardsForEnergyAttachment

.moltres_deck
	cp LEGENDARY_MOLTRES_DECK_ID
	jr nz, .dragonite_deck
	jp ScoreLegendaryMoltresCardsForEnergyAttachment

.dragonite_deck
	cp LEGENDARY_DRAGONITE_DECK_ID
	ret nz
	jp ScoreLegendaryDragoniteCardsForEnergyAttachment
