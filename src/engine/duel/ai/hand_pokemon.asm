; determine whether AI plays
; basic cards from hand
AIDecidePlayPokemonCard:
	call CreateHandCardList
	call SortTempHandByIDList
	ld hl, wDuelTempList
	ld de, wHandTempList
	call CopyListWithFFTerminatorFromHLToDE_Bank5
	ld hl, wHandTempList

.next_hand_card
	ld a, [hli]
	cp $ff
	jp z, AIDecideEvolution

	ld [wTempAIPokemonCard], a
	push hl
	call LoadCardDataToBuffer1_FromDeckIndex
; skip non-Pokémon cards
	ld a, [wLoadedCard1Type]
	cp TYPE_ENERGY
	jr nc, .skip

; skip non-basic Pokémon
	ld a, [wLoadedCard1Stage]
	or a  ; BASIC
	jr nz, .skip

; skip if Bench is full
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	cp MAX_PLAY_AREA_POKEMON
	jr nc, .skip

; initialize AI score
	push af
	ld a, $82
	ld [wAIScore], a
	call AIDecidePlayLegendaryBirds
	pop af

; if Play Area has more than 4 Pokémon, decrease AI score
; else, increase AI score
	cp 4
	jr c, .has_4_or_fewer
	ld a, 20
	call AIDiscourage
	jr .check_defending_can_ko
.has_4_or_fewer
	ld a, 50
	call AIEncourage

; if defending Pokémon can KO active card, increase AI score
.check_defending_can_ko
	xor a ; PLAY_AREA_ARENA
	ldh [hTempPlayAreaLocation_ff9d], a
	call CheckIfDefendingPokemonCanKnockOut
	jr nc, .check_energy_cards
	ld a, 20
	call AIEncourage

; if energy cards are found in hand
; for this card's attacks, raise AI score
.check_energy_cards
	ld a, [wTempAIPokemonCard]
	call GetAttacksEnergyCostBits
	call CheckEnergyFlagsNeededInList
	jr nc, .check_evolution_hand
	ld a, 20
	call AIEncourage

; if evolution card is found in hand
; for this card, raise AI score
.check_evolution_hand
	ld a, [wTempAIPokemonCard]
	call CheckForEvolutionInList
	jr nc, .check_evolution_deck
	ld a, 20
	call AIEncourage

; if evolution card is found in deck
; for this card, raise AI score
.check_evolution_deck
	ld a, [wTempAIPokemonCard]
	call CheckForEvolutionInDeck
	jr nc, .check_score
	ld a, 10
	call AIEncourage

; if AI score is >= 180, play card from hand
.check_score
	ld a, [wAIScore]
	cp 180
	jr c, .skip
	ld a, [wTempAIPokemonCard]
	ldh [hTemp_ffa0], a
	call CheckIfCardCanBePlayed
	jr c, .skip
	ld a, OPPACTION_PLAY_BASIC_PKMN
	bank1call AIMakeDecision
	jr c, .done
.skip
	pop hl
	jp .next_hand_card
.done
	pop hl
	ret

; determine whether AI evolves
; Pokémon in the Play Area
AIDecideEvolution:
	call CreateHandCardList
	ld hl, wDuelTempList
	ld de, wHandTempList
	call CopyListWithFFTerminatorFromHLToDE_Bank5
	ld hl, wHandTempList

.next_hand_card
	ld a, [hli]
	cp $ff
	jp z, .done
	ld [wTempAIPokemonCard], a

; check if Prehistoric Power is active
; and if so, skip to next card in hand
	push hl
	call IsPrehistoricPowerActive
	jp c, .done_hand_card

; load evolution data to buffer1
; skip if it's not a Pokémon card
; and if it's a basic stage card
	ld a, [wTempAIPokemonCard]
	call LoadCardDataToBuffer1_FromDeckIndex
	ld a, [wLoadedCard1Type]
	cp TYPE_ENERGY
	jp nc, .done_hand_card
	ld a, [wLoadedCard1Stage]
	or a
	jp z, .done_hand_card

; start looping Pokémon in Play Area
; to find a card to evolve
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	ld c, a
	ld b, 0
.next_bench_pokemon
	push bc
	ld e, b
	ld a, [wTempAIPokemonCard]
	ld d, a
	call CheckIfCanEvolveInto
	pop bc
	push bc
	jp c, .done_bench_pokemon

; store this Play Area location in wTempAI
; and initialize the AI score
	ld a, b
	ld [wTempAI], a
	ldh [hTempPlayAreaLocation_ff9d], a
	ld a, $80
	ld [wAIScore], a
	call AIDecideSpecialEvolutions

; separate evaluation for active spot and bench
	ldh a, [hTempPlayAreaLocation_ff9d]
	or a  ; PLAY_AREA_ARENA
	jr nz, .check_benched_evolution

; active spot
	call AITemporarilyOverwritePlayAreaPokemon
	call AITemporarilyOverwritePlayAreaPokemonHP
; check if the evolution can KO defending Pokémon
; consider possible energy attachments
	call CheckIfAnyAttackCouldKnockOutDefendingCard
	jr nc, .check_defending_can_ko_evolution
; can KO on the active spot, raise score
	ld a, 5
	call AIEncourage
	call AIRestorePlayAreaPokemon_WRAMPlayArea_CardLocationHand
	call AIRestorePlayAreaPokemonHP
	jr .check_status

; if defending Pokémon can KO evolution card, lower AI score
.check_defending_can_ko_evolution
	call CheckIfDefendingPokemonCanKnockOut
	push af
	call AIRestorePlayAreaPokemon_WRAMPlayArea_CardLocationHand
	call AIRestorePlayAreaPokemonHP
	pop af
	jr nc, .check_defending_can_ko_current_card
	ld a, 5
	call AIDiscourage
	jr .check_status

.check_defending_can_ko_current_card
; if defending Pokémon can KO current card, raise AI score
	call CheckIfDefendingPokemonCanKnockOut
	jr nc, .check_status
	ld a, 5
	call AIEncourage

; if current card has a status condition, raise AI score
.check_status
	ld a, DUELVARS_ARENA_CARD_STATUS
	call GetTurnDuelistVariable
	or a
	jr z, .check_2nd_stage_hand
	ld a, 4
	call AIEncourage
	jr .check_2nd_stage_hand

; check if the evolution can KO defending Pokémon
; for the bench, do not consider energy attachment
; because it might be needed to retreat
.check_benched_evolution
	call AITemporarilyOverwritePlayAreaPokemon
	call AITemporarilyOverwritePlayAreaPokemonHP
	call CheckIfAnyAttackKnocksOutDefendingCard
	push af
	call AIRestorePlayAreaPokemon_WRAMPlayArea_CardLocationHand
	call AIRestorePlayAreaPokemonHP
	pop af
	jr nc, .check_2nd_stage_hand
; can KO right now, raise score
	ld a, 5
	call AIEncourage

; common checks for both active spot and benched evolutions
; if hand has 2nd stage card to evolve evolution card, raise AI score
.check_2nd_stage_hand
	ld a, [wTempAIPokemonCard]
	call CheckForEvolutionInList
	jr nc, .check_2nd_stage_deck
	ld a, 2
	call AIEncourage
	jr .check_damage

; if deck has 2nd stage card to evolve evolution card, raise AI score
.check_2nd_stage_deck
	ld a, [wTempAIPokemonCard]
	call CheckForEvolutionInDeck
	jr nc, .check_damage
	ld a, 1
	call AIEncourage

; decrease AI score proportional to damage
; AI score -= floor(Damage / 40)
.check_damage
	ld a, [wTempAI]
	ld e, a
	call GetCardDamageAndMaxHP
	or a
	jr z, .check_mysterious_fossil
	srl a
	srl a
	call ConvertHPToDamageCounters_Bank5
	call AIDiscourage

; if is Mysterious Fossil or
; wLoadedCard1AIInfo is AI_INFO_ENCOURAGE_EVO,
; raise AI score
.check_mysterious_fossil
	ld a, [wTempAI]
	add DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call LoadCardDataToBuffer1_FromDeckIndex
	ld hl, wLoadedCard1ID
	cphl MYSTERIOUS_FOSSIL
	jr z, .mysterious_fossil
	ld a, [wLoadedCard1AIInfo]
	and AI_INFO_ENCOURAGE_EVO
	jr z, .check_score
	ld a, 2
	call AIEncourage
	jr .check_score

.mysterious_fossil
	ld a, 5
	call AIEncourage

; if AI score >= $85, go through with the evolution
.check_score
	ld a, [wAIScore]
	cp $85
	jr c, .done_bench_pokemon
	ld a, [wTempAI]
	ldh [hTempPlayAreaLocation_ffa1], a
	ld a, [wTempAIPokemonCard]
	ldh [hTemp_ffa0], a
	ld a, OPPACTION_EVOLVE_PKMN
	bank1call AIMakeDecision

; disregard PlusPower attack choice in case the Arena card evolved
	ld a, [wTempAI]
	or a
	jr nz, .skip_reset_pluspower_atk
	ld hl, wPreviousAIFlags
	res 0, [hl] ; AI_FLAG_USED_PLUSPOWER
.skip_reset_pluspower_atk
	pop bc
	jr .done_hand_card

.done_bench_pokemon
	pop bc
	inc b
	dec c
	jp nz, .next_bench_pokemon
.done_hand_card
	pop hl
	jp .next_hand_card
.done
	or a
	ret

; determine AI score for evolving certain cards
; wLoadedCard1 is the evolution card
; wLoadedCard2 is the Play Area card being checked
AIDecideSpecialEvolutions:
	ld hl, wLoadedCard1ID
	cphl DRAGONITE_LV41
	ret nz

; trying to evolve into Healing Wind Dragonite
	ldh a, [hTempPlayAreaLocation_ff9d]
	or a ; PLAY_AREA_ARENA
	jr nz, .benched

; if Dragonair is active, check its damage in HP
; if this result is >= 30, encourage evolution
	ld e, PLAY_AREA_ARENA
	call GetCardDamageAndMaxHP
	cp 30
	jr c, .benched
	ld a, 3
	call AIEncourage
	; fallthrough

; if Dragonair is benched, check all Pokémon in Play Area
; and sum all the damage in HP of all cards
; if this result is >= 70, check if there's
; a Muk in any duelist's Play Area
.benched
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	ld b, a
	ld c, 0
.loop
	dec b
	ld e, b
	push bc
	call GetCardDamageAndMaxHP
	pop bc
	add c
	ld c, a
	ld a, b
	or a
	jr nz, .loop
	ld a, 69
	cp c
	jr c, .check_muk
.lower_score
	ld a, 10
	jp AIDiscourage

; if there's no Muk, raise score
.check_muk
	ld de, MUK
	call CountPokemonWithActivePkmnPowerInBothPlayAreas
	jr c, .lower_score
	ld a, 10
	jp AIEncourage


; determine AI score for the legendary cards
; Moltres, Zapdos and Articuno
AIDecidePlayLegendaryBirds:
; check if card applies
	ld hl, wLoadedCard1ID
	cphl ARTICUNO_LV37
	jr z, .articuno
	cphl MOLTRES_LV37
	jr z, .moltres
	cphl ZAPDOS_LV68
	jr z, .zapdos
	ret

.articuno
; exit if not enough Pokemon in Play Area
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	cp 2
	ret c

	call CheckIfActiveCardCanKnockOut
	jr c, .subtract
	call CheckIfActivePokemonCanUseAnyNonResidualAttack
	jr nc, .subtract
	call AIDecideWhetherToRetreat
	jr c, .subtract

; check for player's active card status
	ld a, DUELVARS_ARENA_CARD_STATUS
	call GetNonTurnDuelistVariable
	and CNF_SLP_PRZ
	or a
	jr nz, .subtract

; check for player's Pokémon Power
	; call SwapTurn
	; ld a, DUELVARS_ARENA_CARD
	; call GetTurnDuelistVariable
	; ld d, a
	; ld e, FIRST_ATTACK_OR_PKMN_POWER
	; call CopyAttackDataAndDamage_FromDeckIndex
	; call SwapTurn

; checks for Muk in both Play Areas
	ld de, MUK
	call CountPokemonWithActivePkmnPowerInBothPlayAreas
	jr c, .subtract
; checks if player's active card is Snorlax
	ld a, DUELVARS_ARENA_CARD
	call GetNonTurnDuelistVariable
	call SwapTurn
	call GetCardIDFromDeckIndex
	call SwapTurn
	cp16 SNORLAX
	jr z, .subtract

; add
	; ld a, [wLoadedAttackCategory]
	; cp POKEMON_POWER
	ld a, 70
	jp AIEncourage
.subtract
	ld a, 100
	jp AIDiscourage

.moltres
; check if there are enough cards in deck
	ld a, DUELVARS_NUMBER_OF_CARDS_NOT_IN_DECK
	call GetTurnDuelistVariable
	cp 56 ; max number of cards not in deck to activate
	jr nc, .subtract
	ret

.zapdos
; check for Muk in both Play Areas
	ld de, MUK
	call CountPokemonWithActivePkmnPowerInBothPlayAreas
	jr c, .subtract
	ret
