; have AI choose an attack to use, but do not execute it.
; return carry if an attack is chosen.
AIProcessButDontUseAttack:
	ld a, $01
	ld [wAIExecuteProcessedAttack], a
	call BackupPlayAreaAIScore
	jr AIProcessAttacks


; have AI choose and execute an attack.
; return carry if an attack was chosen and attempted.
AIProcessAndTryToUseAttack:
	xor a
	ld [wAIExecuteProcessedAttack], a
	; fallthrough

; checks which of the Active card's attacks for AI to use.
; If any of the attacks has enough AI score to be used,
; AI will use it if wAIExecuteProcessedAttack is 0.
; in either case, return carry if an attack is chosen to be used.
AIProcessAttacks:
; if AI used Pluspower, load its attack index
	ld a, [wPreviousAIFlags]
	and AI_FLAG_USED_PLUSPOWER
	jr z, .no_pluspower
	ld a, [wAIPluspowerAttack]
	ld [wSelectedAttack], a
	jp .attack_chosen

.no_pluspower
; if Player is running MewtwoLv53 mill deck,
; skip attack if Barrier counter is 0.
	ld a, [wAIBarrierFlagCounter]
	cp AI_MEWTWO_MILL + 0
	jp z, .dont_attack

; Implementation Notes
; We always need to calculate the scores of each attack to evaluate them fairly.
; More often than not, attacks from the current (highest) stage will have the best scores.
; To optimize the maximum score calculation later, it is easier if we start from the Basic stage.
; Thus, we should lay out the scores in WRAM from Basic to Stage 2,
; exactly the same order that wAllStagesIndices uses.
; We define the initial max score as AI_MINIMUM_SCORE_TO_USE_ATTACK.
; Each attack's score is compared to the current max score, and replaces it if higher.
; In addition to replacing the max score, we also store the corresponding attack index,
; as well as the current stage deck index, so that we can overwrite the arena card later.

; initialize attack scores
	xor a
; basic stage
	ld [wAIScoreAllAttacks + 0], a
	ld [wAIScoreAllAttacks + 1], a
; stage 1
	ld [wAIScoreAllAttacks + 2], a
	ld [wAIScoreAllAttacks + 3], a
; stage 2
	ld [wAIScoreAllAttacks + 4], a
	ld [wAIScoreAllAttacks + 5], a

; preload previous stages
	ldh [hTempPlayAreaLocation_ff9d], a  ; PLAY_AREA_ARENA
	bank1call GetCardOneStageBelow

; replace the current card with the Basic
; if the Pokémon is Basic, we overwrite it with itself (non optimal)
; and then skip the rest of the scores
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld a, [wAllStagesIndices + BASIC]
; there is always a Basic, skip checks
	ld [hl], a
; determine AI score of both attacks
	xor a ; FIRST_ATTACK_OR_PKMN_POWER
	call GetAIScoreOfAttack
	ld a, [wAIScore]
	ld [wAIScoreAllAttacks + 0], a
	ld a, SECOND_ATTACK
	call GetAIScoreOfAttack
	ld a, [wAIScore]
	ld [wAIScoreAllAttacks + 1], a

; replace the current card with the Stage 1 Pokémon, if any
; if the Pokémon is an actual Stage 1, we restore it by overwriting here
; if there is no Stage 1, we skip to Stage 2
; the Stage 2 check is necessary because of STAGE2_WITHOUT_STAGE1 cases
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld a, [wAllStagesIndices + STAGE1]
	cp $ff
	jr z, .stage2
; replace the current card with the Stage 1
	ld [hl], a
; determine AI score of both stage 1 attacks
	xor a ; FIRST_ATTACK_OR_PKMN_POWER
	call GetAIScoreOfAttack
	ld a, [wAIScore]
	ld [wAIScoreAllAttacks + 2], a
	ld a, SECOND_ATTACK
	call GetAIScoreOfAttack
	ld a, [wAIScore]
	ld [wAIScoreAllAttacks + 3], a

; replace the current card with the Stage 2 Pokémon, if any
; if the Pokémon is an actual Stage 2, we restore it by overwriting here
.stage2
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld a, [wAllStagesIndices + STAGE2]
	cp $ff
	jr z, .compare_attack_scores
; replace the current card with the Stage 2
	ld [hl], a
; determine AI score of both stage 2 attacks
	xor a ; FIRST_ATTACK_OR_PKMN_POWER
	call GetAIScoreOfAttack
	ld a, [wAIScore]
	ld [wAIScoreAllAttacks + 4], a
	ld a, SECOND_ATTACK
	call GetAIScoreOfAttack
	ld a, [wAIScore]
	ld [wAIScoreAllAttacks + 5], a

; compare attack scores for all stages and pick the best one
.compare_attack_scores
	ld c, 3  ; number of stages we have to go through
	ld b, 0  ; initial max score
	ld hl, wAIScoreAllAttacks
	ld de, wAllStagesIndices + BASIC
; the loop compares both attacks of each stage at once
; this way we only loop 3 times for up to 6 attacks
; it is easier to manage wAllstagesIndices this way
.loop_compare_attack_scores
	ld a, [hli]
	cp b
	jr c, .compare_attack2
; this attack has a higher score
; store the new max score
	ld b, a
; store the attack index
	xor a  ; FIRST_ATTACK_OR_PKMN_POWER
	ld [wSelectedAttack], a
; store the deck index of this stage
	ld a, [de]
	ld [wTempCardDeckIndex], a
.compare_attack2
	ld a, [hli]
	cp b
	jr c, .compare_scores_next_stage
; this attack has a higher score
; store the new max score
	ld b, a
; store the attack index
	ld a, SECOND_ATTACK
	ld [wSelectedAttack], a
; store the deck index of this stage
	ld a, [de]
	ld [wTempCardDeckIndex], a
.compare_scores_next_stage
	inc de
	dec c
	jr nz, .loop_compare_attack_scores

; get the maximum attack score
; check if the chosen attack has at least a minimum score
	ld a, b
	cp AI_MINIMUM_SCORE_TO_USE_ATTACK
	jr c, .dont_attack

; enough score, proceed
; check if first attack is better than second attack
; in case the second one was chosen.
	; ld a, c
	; ld [wSelectedAttack], a
	; or a
	; call nz, CheckWhetherToSwitchToFirstAttack
; FIXME: this section should be removed.
; The score calculation function should take into account
; additional effects of attacks (beneficial or detrimental)
; and update the score accordingly, so that switching attacks
; here is not necessary.

; check whether to execute the chosen attack
.attack_chosen
	ld a, [wAIExecuteProcessedAttack]
	or a
	jr z, .execute

; set carry and reset Play Area AI score
; to the previous values.
	scf
	jp RetrievePlayAreaAIScoreFromBackup2

.execute
	ld a, AI_TRAINER_CARD_PHASE_14
	call AIProcessHandTrainerCards

; load this attack's damage output against the current Defending Pokémon
	xor a ; PLAY_AREA_ARENA
	ldh [hTempPlayAreaLocation_ff9d], a
	ld a, [wSelectedAttack]
	ld e, a
	ld a, [wTempCardDeckIndex]
	call EstimateDamageOfCard_VersusDefendingCard
	ld a, [wDamage]
; if damage is not 0, fallthrough
	or a
	jr z, .check_damage_bench

.can_damage
	xor a
	ld [wAIRetreatScore], a
	jr .use_attack

.check_damage_bench
; check if it can otherwise damage player's bench
	ld a, ATTACK_FLAG1_ADDRESS | DAMAGE_TO_OPPONENT_BENCH_F
	call CheckLoadedAttackFlag
	jr c, .can_damage

; cannot damage Defending Pokémon or Bench
	ld hl, wAIRetreatScore
	inc [hl]

; return carry if attack is chosen and AI tries to use it.
.use_attack
	ld a, TRUE
	ld [wAITriedAttack], a
	ld a, [wTempCardDeckIndex]
	ldh [hTempCardIndex_ff9f], a
; input:
;   [wSelectedAttack] = attack index chosen by AI
;   [wTempCardDeckIndex] = deck index of the card owning the attack
	call AITryUseAttack
	scf
	ret

.dont_attack
	ld a, [wAIExecuteProcessedAttack]
	or a
	jr z, .failed_to_use
; reset Play Area AI score
; to the previous values.
	jp RetrievePlayAreaAIScoreFromBackup2

; return no carry if no viable attack.
.failed_to_use
	ld hl, wAIRetreatScore
	inc [hl]
	or a
	ret


; determines the AI score of attack index in a
; of card in Play Area location hTempPlayAreaLocation_ff9d.
GetAIScoreOfAttack:
; initialize AI score.
	ld [wSelectedAttack], a
	ld a, AI_MINIMUM_SCORE_TO_USE_ATTACK
	ld [wAIScore], a

	call CheckIfSelectedAttackIsUnusable  ; loads attack data
	jr nc, .usable

; return zero AI score.
.unusable
	xor a
	ld [wAIScore], a
	ret

; load arena card IDs
.usable
	xor a  ; FALSE
	ld [wAICannotDamage], a
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
	ld a, e
	ld [wTempTurnDuelistCardID + 0], a
	ld a, d
	ld [wTempTurnDuelistCardID + 1], a
	ld l, DUELVARS_ARENA_CARD_STAGE
	ld a, [hl]
	ld [wTempTurnDuelistCardStage], a
; opponent side
	call SwapTurn
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
	ld a, e
	ld [wTempNonTurnDuelistCardID + 0], a
	ld a, d
	ld [wTempNonTurnDuelistCardID + 1], a
	ld l, DUELVARS_ARENA_CARD_STAGE
	ld a, [hl]
	ld [wTempNonTurnDuelistCardStage], a

; handle the case where the player has No Damage substatus.
; in the case the player does, check if this attack
; has a residual effect, or if it can damage the opposing bench.
; If none of those are true, render the attack unusable.
	call HandleNoDamageOrEffectSubstatus
	call SwapTurn
	jr nc, .check_if_can_ko

; player is under No Damage substatus
	ld a, TRUE
	ld [wAICannotDamage], a
	ld a, [wLoadedAttackCategory]
	and RESIDUAL
	jr nz, .check_if_can_ko
	ld a, ATTACK_FLAG1_ADDRESS | DAMAGE_TO_OPPONENT_BENCH_F
	call CheckLoadedAttackFlag
	jr nc, .unusable

; calculate damage to player to check if attack can KO.
; encourage attack if it's able to KO.
.check_if_can_ko
	ld a, [wSelectedAttack]
	call EstimateDamageOfLoadedAttack_VersusDefendingCard
	ld a, DUELVARS_ARENA_CARD_HP
	call GetNonTurnDuelistVariable
	ld hl, wDamage
	sub [hl]
	jr c, .can_ko
	jr z, .can_ko
	jr .check_damage
.can_ko
	ld a, 20
	call AIEncourage

; raise AI score by the number of damage counters that this attack deals.
; if no damage is dealt, subtract AI score. in case wDamage is zero
; but wMaxDamage is not, then encourage attack afterwards.
; otherwise, if wMaxDamage is also zero, check for damage against
; player's bench, and encourage attack in case there is.
.check_damage
	xor a
	ld [wAIAttackIsNonDamaging], a
	ld a, [wDamage]
	ld [wTempAI], a
	or a
	jr z, .no_damage
	call ConvertHPToDamageCounters_Bank5
	call AIEncourage
	jr .check_recoil

.no_damage
	ld a, TRUE
	ld [wAIAttackIsNonDamaging], a
	; ld a, 1
	call AIDiscourage
	ld a, [wAIMaxDamage]
	or a
	jr z, .no_max_damage
	ld a, 2
	call AIEncourage
	xor a  ; FALSE
	ld [wAIAttackIsNonDamaging], a
.no_max_damage
	ld a, ATTACK_FLAG1_ADDRESS | DAMAGE_TO_OPPONENT_BENCH_F
	call CheckLoadedAttackFlag
	jr nc, .check_recoil
	ld a, 2
	call AIEncourage

; handle recoil attacks (low and high recoil).
.check_recoil
	ld a, ATTACK_FLAG1_ADDRESS | LOW_RECOIL_F
	call CheckLoadedAttackFlag
	jr c, .is_recoil
	ld a, ATTACK_FLAG1_ADDRESS | HIGH_RECOIL_F
	call CheckLoadedAttackFlag
	jp nc, .check_defending_can_ko
.is_recoil
	; sub from AI score number of damage counters
	; that attack deals to itself.
	ld a, [wLoadedAttackEffectParam]
	or a
	jp z, .check_defending_can_ko
	ld [wDamage], a
	call ApplyDamageModifiers_DamageToSelf
	ld a, e
	call ConvertHPToDamageCounters_Bank5
	call AIDiscourage

	push de
	ld a, ATTACK_FLAG1_ADDRESS | HIGH_RECOIL_F
	call CheckLoadedAttackFlag
	pop de
	jr c, .high_recoil

; if LOW_RECOIL KOs self, decrease AI score
	ld a, DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	cp e
	jr c, .kos_self
	jp nz, .check_defending_can_ko

.kos_self
	ld a, 10
	call AIDiscourage

.high_recoil
; dismiss this attack if no benched Pokémon
	ld a, DUELVARS_NUMBER_OF_POKEMON_IN_PLAY_AREA
	call GetTurnDuelistVariable
	cp 2
	jr c, .dismiss_high_recoil_atk

; has benched Pokémon
; here the AI handles high recoil attacks differently
; depending on what deck it's playing.
	ld a, [wOpponentDeckID]
	cp ROCK_CRUSHER_DECK_ID
	jr z, .rock_crusher_deck
	cp ZAPPING_SELFDESTRUCT_DECK_ID
	jr z, .zapping_selfdestruct_deck
	cp BOOM_BOOM_SELFDESTRUCT_DECK_ID
	jr z, .encourage_high_recoil_atk
; Boom Boom Selfdestruct deck always encourages
	cp POWER_GENERATOR_DECK_ID
	jr nz, .high_recoil_generic_checks
; Power Generator deck always dismisses

.dismiss_high_recoil_atk
	xor a
	ld [wAIScore], a
	ret

.encourage_high_recoil_atk
	ld a, 20
	jp AIEncourage

; Zapping Selfdestruct deck only uses this attack
; if number of cards in deck >= 30 and
; HP of active card is < half max HP.
.zapping_selfdestruct_deck
	ld a, DUELVARS_NUMBER_OF_CARDS_NOT_IN_DECK
	call GetTurnDuelistVariable
	cp 31
	jr nc, .high_recoil_generic_checks
	ld e, PLAY_AREA_ARENA
	call GetCardDamageAndMaxHP
	sla a
	cp c
	jr c, .high_recoil_generic_checks
	ld b, 0
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
	cp16 MAGNEMITE_LV13
	jr z, .magnemite1
	ld b, 10 ; bench damage
.magnemite1
	ld a, 10
	add b
	ld b, a ; 20 bench damage if not MagnemiteLv13

; if this attack causes player to win the duel by
; knocking out own Pokémon, dismiss attack.
	ld a, 1 ; count active Pokémon as KO'd
	call .check_if_kos_bench
	jr c, .dismiss_high_recoil_atk
	jr .encourage_high_recoil_atk

; Rock Crusher Deck only uses this attack if
; prize count is below 4 and attack wins (or potentially draws) the duel,
; (i.e. at least gets KOs equal to prize cards left).
.rock_crusher_deck
	call CountPrizes
	cp 4
	jr nc, .dismiss_high_recoil_atk
	; prize count < 4
	ld b, 20 ; damage dealt to bench
	call SwapTurn
	xor a
	call .check_if_kos_bench
	call SwapTurn
	jr c, .encourage_high_recoil_atk

; generic checks for all other deck IDs.
; encourage attack if it wins (or potentially draws) the duel,
; (i.e. at least gets KOs equal to prize cards left).
; dismiss it if it causes the player to win.
.high_recoil_generic_checks
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
	cp16 CHANSEY
	jr z, .chansey
	cp16 MAGNEMITE_LV13
	jr z, .magnemite1_or_weezing
	cp16 WEEZING
	jr z, .magnemite1_or_weezing
	ld b, 20 ; bench damage
	jr .check_bench_kos
.magnemite1_or_weezing
	ld b, 10 ; bench damage
	jr .check_bench_kos
.chansey
	ld b, 0 ; no bench damage

.check_bench_kos
	push bc
	call SwapTurn
	xor a
	call .check_if_kos_bench
	call SwapTurn
	pop bc
	jr c, .wins_the_duel
	push de
	ld a, 1
	call .check_if_kos_bench
	pop bc
	jr nc, .count_own_ko_bench

; attack causes player to draw all prize cards
	xor a
	ld [wAIScore], a
	ret

; attack causes CPU to draw all prize cards
.wins_the_duel
	ld a, 20
	jp AIEncourage

; subtract from AI score number of own benched Pokémon KO'd
.count_own_ko_bench
	push bc
	ld a, d
	or a
	jr z, .count_player_ko_bench
	dec a
	call AIDiscourage

; add to AI score number of player benched Pokémon KO'd
.count_player_ko_bench
	pop bc
	ld a, b
	call AIEncourage
	jr .check_defending_can_ko

; local function that gets called to determine damage to
; benched Pokémon caused by a HIGH_RECOIL attack.
; return carry if using attack causes number of benched Pokémon KOs
; equal to or larger than remaining prize cards.
; this function is independent on duelist turn, so whatever
; turn it is when this is called, it's that duelist's
; bench/prize cards that get checked.
; input:
;	a = initial number of KO's beside benched Pokémon,
;		so that if the active Pokémon is KO'd by the attack,
;		this counts towards the prize cards collected
;	b = damage dealt to bench Pokémon
.check_if_kos_bench
	ld d, a
	ld a, DUELVARS_BENCH
	call GetTurnDuelistVariable
	ld e, PLAY_AREA_ARENA
.loop
	inc e
	ld a, [hli]
	cp $ff
	jr z, .exit_loop
	ld a, e
	add DUELVARS_ARENA_CARD_HP
	push hl
	call GetTurnDuelistVariable
	pop hl
	cp b
	jr z, .increase_count
	jr nc, .loop
.increase_count
	; increase d if damage dealt KOs
	inc d
	jr .loop
.exit_loop
	push de
	call SwapTurn
	call CountPrizes
	call SwapTurn
	pop de
	cp d
	jp c, .set_carry
	jp z, .set_carry
	or a
	ret
.set_carry
	scf
	ret

; if defending card can KO, encourage attack
; unless attack is non-damaging.
.check_defending_can_ko
	ld a, [wSelectedAttack]
	push af
	call CheckIfDefendingPokemonCanKnockOut
	pop bc
	ld a, b
	ld [wSelectedAttack], a
	jr nc, .check_discard
	ld a, 5
	call AIEncourage
	ld a, [wAIAttackIsNonDamaging]
	or a
	jr z, .check_discard
	ld a, 5
	call AIDiscourage

; subtract from AI score if this attack requires discarding energy
.check_discard
; reload attack data because CheckIfDefendingPokemonCanKnockOut clobbers it.
	ld a, [wSelectedAttack]
	ld e, a
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld d, a
	call CopyAttackDataAndDamage_FromDeckIndex
	ld a, ATTACK_FLAG2_ADDRESS | DISCARD_ENERGY_F
	call CheckLoadedAttackFlag
	jr nc, .check_adhoc_score_bonus
	ld a, [wLoadedAttackEffectParam]
	inc a
	call AIDiscourage

.check_adhoc_score_bonus
	ld a, ATTACK_FLAG2_ADDRESS | FLAG_2_BIT_6_F
	call CheckLoadedAttackFlag
	jr nc, .check_nullify_flag
	ld a, [wLoadedAttackEffectParam]
	call AIEncourage

; encourage attack if it has a nullify or weaken attack effect.
.check_nullify_flag
	ld a, ATTACK_FLAG2_ADDRESS | NULLIFY_OR_WEAKEN_ATTACK_F
	call CheckLoadedAttackFlag
	jr nc, .check_draw_flag
	ld a, 1
	call AIEncourage

; encourage attack if it has an effect to draw a card.
.check_draw_flag
	ld a, ATTACK_FLAG1_ADDRESS | DRAW_CARD_F
	call CheckLoadedAttackFlag
	jr nc, .check_heal_flag
	ld a, 1
	call AIEncourage

.check_heal_flag
	ld a, ATTACK_FLAG2_ADDRESS | HEAL_USER_F
	call CheckLoadedAttackFlag
	jr nc, .check_status_effect
	ld a, [wLoadedAttackEffectParam]
	cp 1
	jr z, .tally_heal_score
	ld a, [wTempAI]
	call ConvertHPToDamageCounters_Bank5
	ld b, a
	ld a, [wLoadedAttackEffectParam]
	cp 3
	jr z, .asm_16cec
	srl b
	jr nc, .asm_16cec
	inc b
.asm_16cec
	ld a, DUELVARS_ARENA_CARD_HP
	call GetTurnDuelistVariable
	call ConvertHPToDamageCounters_Bank5
	cp b
	jr c, .tally_heal_score
	ld a, b
.tally_heal_score
	push af
	ld e, PLAY_AREA_ARENA
	call GetCardDamageAndMaxHP
	call ConvertHPToDamageCounters_Bank5
	pop bc
	cp b
	jr c, .add_heal_score
	ld a, b
.add_heal_score
	call AIEncourage

.check_status_effect
	ld a, DUELVARS_ARENA_CARD
	call GetNonTurnDuelistVariable
	call SwapTurn
	call GetCardIDFromDeckIndex
	call SwapTurn
; skip if player has Snorlax
	cp16 SNORLAX
	jp z, .handle_special_atks

	ld a, DUELVARS_ARENA_CARD_STATUS
	call GetNonTurnDuelistVariable
	ld [wTempAI], a

; encourage a poison inflicting attack if opposing Pokémon
; isn't (doubly) poisoned already.
; discourage it if the opposing Pokémon is already poisoned.
	ld a, ATTACK_FLAG1_ADDRESS | INFLICT_POISON_F
	call CheckLoadedAttackFlag
	jr nc, .check_sleep
	ld a, [wTempAI]
	and DOUBLE_POISONED
	jr z, .add_poison_score  ; not poisoned
; already poisoned
	ld a, 1
	call AIDiscourage
	jr .check_sleep
.add_poison_score
	ld a, 2
	call AIEncourage

; encourage sleep-inducing attack if other Pokémon isn't asleep.
.check_sleep
	ld a, ATTACK_FLAG1_ADDRESS | INFLICT_SLEEP_F
	call CheckLoadedAttackFlag
	jr nc, .check_paralysis
	ld a, [wTempAI]
	and CNF_SLP_PRZ
	cp ASLEEP
	jr z, .check_paralysis
	ld a, 1
	call AIEncourage

; encourage paralysis-inducing attack if other Pokémon isn't asleep.
; otherwise, if other Pokémon is asleep, discourage attack.
.check_paralysis
	ld a, ATTACK_FLAG1_ADDRESS | INFLICT_PARALYSIS_F
	call CheckLoadedAttackFlag
	jr nc, .check_confusion
	ld a, [wTempAI]
	and CNF_SLP_PRZ
	cp ASLEEP
	jr z, .sub_prz_score
	ld a, 1
	call AIEncourage
	jr .check_confusion
.sub_prz_score
	ld a, 1
	call AIDiscourage

; encourage confuse-inducing attack if other Pokémon isn't asleep
; or confused already.
; otherwise, if other Pokémon is asleep or confused,
; discourage attack instead.
.check_confusion
	ld a, ATTACK_FLAG1_ADDRESS | INFLICT_CONFUSION_F
	call CheckLoadedAttackFlag
	jr nc, .check_if_confused
	ld a, [wTempAI]
	and CNF_SLP_PRZ
	cp ASLEEP
	jr z, .sub_cnf_score
	ld a, [wTempAI]
	and CNF_SLP_PRZ
	cp CONFUSED
	jr z, .check_if_confused
	ld a, 1
	call AIEncourage
	jr .check_if_confused
.sub_cnf_score
	ld a, 1
	call AIDiscourage

; if this Pokémon is confused, subtract from score.
.check_if_confused
	ld a, DUELVARS_ARENA_CARD_STATUS
	call GetTurnDuelistVariable
	and CNF_SLP_PRZ
	cp CONFUSED
	jr nz, .handle_special_atks
	ld a, 2
	call AIDiscourage

; SPECIAL_AI_HANDLING marks attacks that the AI handles individually.
; each attack has its own checks and modifies AI score accordingly.
.handle_special_atks
	ld a, ATTACK_FLAG3_ADDRESS | SPECIAL_AI_HANDLING_F
	call CheckLoadedAttackFlag
	ret nc

	call HandleSpecialAIAttacks
	cp $80
	jr c, .negative_score
	sub $80
	jp AIEncourage

.negative_score
	ld b, a
	ld a, $80
	sub b
	jp AIDiscourage


; backup wPlayAreaAIScore in wTempPlayAreaAIScore.
; copies wAIScore to wTempAIScore
BackupPlayAreaAIScore:
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
	ret


; copies wTempPlayAreaAIScore to wPlayAreaAIScore
; and loads wAIScore with value in wTempAIScore.
; identical to RetrievePlayAreaAIScoreFromBackup1.
RetrievePlayAreaAIScoreFromBackup2:
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


; called when second attack is determined by AI to have
; more AI score than the first attack, so that it checks
; whether the first attack is a better alternative.
CheckWhetherToSwitchToFirstAttack:
; this checks whether the first attack is also viable
; (has more than minimum score to be used)
	ld a, [wAIScoreAllAttacks]  ; used to be first attack, todo
	cp $50
	jr c, .keep_second_attack

; first attack has more than minimum score to be used,
; check if it can KO, in case it can't
; then the AI keeps second attack as selection.
	xor a ; PLAY_AREA_ARENA
	ldh [hTempPlayAreaLocation_ff9d], a
	; a = FIRST_ATTACK_OR_PKMN_POWER
	call EstimateDamage_VersusDefendingCard
	ld a, DUELVARS_ARENA_CARD_HP
	call GetNonTurnDuelistVariable
	ld hl, wDamage
	sub [hl] ; HP - damage
	jr z, .check_flag
	jr nc, .keep_second_attack ; cannot KO

; first attack can ko, check flags from second attack
; in case its effect is to heal user or nullify/weaken damage
; next turn, keep second attack as the option.
.check_flag
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	ld d, a
	ld e, SECOND_ATTACK
	call CopyAttackDataAndDamage_FromDeckIndex
	ld a, ATTACK_FLAG2_ADDRESS | HEAL_USER_F
	call CheckLoadedAttackFlag
	jr c, .keep_second_attack
	ld a, ATTACK_FLAG2_ADDRESS | NULLIFY_OR_WEAKEN_ATTACK_F
	call CheckLoadedAttackFlag
	jr c, .keep_second_attack
; switch to first attack
	xor a ; FIRST_ATTACK_OR_PKMN_POWER
	ld [wSelectedAttack], a
	ret
.keep_second_attack
	ld a, SECOND_ATTACK
	ld [wSelectedAttack], a
	ret
